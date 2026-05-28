use assert_cmd::cargo::CommandCargoExt;
use assert_cmd::prelude::*;
use predicates::prelude::*;
use tempfile::TempDir;

use std::fs;
use std::path::{Path, PathBuf};

use polars::prelude::*;

mod helpers;

#[derive(Debug, serde::Deserialize)]
struct ProcessingResult {
    branch_id: String,
    sample_size: f64,
    transformation: String,
    outlier_method: String,
    effect_condition: String,
    strip_method: String,
    n_rows_input: usize,
    n_rows_output: usize,
    n_rows_removed: usize,
    processing_time_ms: u128,
    success: bool,
    error_message: Option<String>,
}

// Embed fixtures and write to temp files so tests are self-contained.
const SMALL_INPUT_CSV: &str = include_str!("fixtures/small_input.csv");
const MISSING_COLS_CSV: &str = include_str!("fixtures/missing_cols.csv");

fn write_fixture(tmpdir: &TempDir, name: &str, contents: &str) -> PathBuf {
    let path = tmpdir.path().join(name);
    fs::write(&path, contents).expect("write fixture");
    path
}

fn read_parquet_df(path: &Path) -> DataFrame {
    let file = std::fs::File::open(path).expect("open parquet");
    ParquetReader::new(file).finish().expect("read parquet")
}

#[test]
fn processing_happy_path_produces_expected_files_and_metadata() {
    let tempdir = TempDir::new().expect("create temp dir");
    let output_dir = tempdir.path();

    // Write embedded fixture to temp file
    let input = write_fixture(&tempdir, "small_input.csv", SMALL_INPUT_CSV);

    let mut cmd = std::process::Command::cargo_bin("process").expect("find process binary");
    cmd.arg("--input")
        .arg(&input)
        .arg("--output-dir")
        .arg(output_dir)
        .arg("--sample-sizes")
        .arg("1.0")
        .arg("--transformations")
        .arg("log_rt,no_log_rt")
        .arg("--outliers")
        .arg("none")
        .arg("--effect-conditions")
        .arg("present,null_interaction")
        .arg("--strip-methods")
        .arg("additive_qmap,shuffle")
        .arg("--threads")
        .arg("2")
        .arg("--save-metadata");

    let assert = cmd.output().expect("run process");
    assert!(
        assert.status.success(),
        "process failed: stderr={}",
        String::from_utf8_lossy(&assert.stderr)
    );

    // Count generated parquet files
    let mut parquet_files: Vec<PathBuf> = fs::read_dir(output_dir)
        .expect("list output dir")
        .filter_map(|e| e.ok())
        .map(|e| e.path())
        .filter(|p| p.extension().map(|e| e == "parquet").unwrap_or(false))
        .collect();

    parquet_files.sort();

    // total = transformations(2) * [present(1) + null_interaction*strip(2)] = 2 * 3 = 6
    assert_eq!(
        parquet_files.len(),
        6,
        "unexpected parquet file count: files={:#?}",
        parquet_files
    );

    // No leftover temp files
    let tmp_files: Vec<_> = fs::read_dir(output_dir)
        .unwrap()
        .filter_map(|e| e.ok())
        .map(|e| e.path())
        .filter(|p| p.extension().map(|e| e == "tmp").unwrap_or(false))
        .collect();
    assert!(
        tmp_files.is_empty(),
        "found leftover tmp files: {tmp_files:#?}"
    );

    // Metadata present and matches count
    let metadata_path = output_dir.join("metadata.json");
    assert!(metadata_path.exists(), "metadata.json not found");

    let json = fs::read_to_string(&metadata_path).expect("read metadata.json");
    let results: Vec<ProcessingResult> = serde_json::from_str(&json).expect("parse metadata.json");

    let successes: Vec<&ProcessingResult> = results
        .iter()
        .filter(|r| r.success && r.n_rows_output > 0)
        .collect();
    assert_eq!(
        successes.len(),
        parquet_files.len(),
        "metadata success entries != parquet files"
    );
}

#[test]
fn shuffle_preserves_group_multiset_within_participant_and_cong() {
    let tempdir = TempDir::new().expect("create temp dir");
    let output_dir = tempdir.path();

    let input = write_fixture(&tempdir, "small_input.csv", SMALL_INPUT_CSV);

    let mut cmd = std::process::Command::cargo_bin("process").expect("find process binary");
    cmd.arg("--input")
        .arg(&input)
        .arg("--output-dir")
        .arg(output_dir)
        .arg("--sample-sizes")
        .arg("1.0")
        .arg("--transformations")
        .arg("no_log_rt") // keep RT scale unchanged
        .arg("--outliers")
        .arg("none")
        .arg("--effect-conditions")
        .arg("null_interaction")
        .arg("--strip-methods")
        .arg("shuffle")
        .arg("--threads")
        .arg("2");

    let out = cmd.output().expect("run process");
    assert!(
        out.status.success(),
        "process failed: stderr={}",
        String::from_utf8_lossy(&out.stderr)
    );

    // Find the one output parquet
    let mut candidates: Vec<PathBuf> = fs::read_dir(output_dir)
        .unwrap()
        .filter_map(|e| e.ok())
        .map(|e| e.path())
        .filter(|p| p.extension().map(|e| e == "parquet").unwrap_or(false))
        .filter(|p| {
            let name = p.file_name().unwrap().to_string_lossy();
            name.contains("__null_interaction__") && name.contains("__shuffle")
        })
        .collect();

    assert_eq!(
        candidates.len(),
        1,
        "expected 1 shuffled output parquet, got {}",
        candidates.len()
    );

    let df_in = CsvReadOptions::default()
        .try_into_reader_with_file_path(Some(input))
        .unwrap()
        .finish()
        .unwrap();
    let df_out = read_parquet_df(&candidates[0]);

    // Compare per-group multisets of RTs, ignoring order, per (participant_id, cong)
    let orig_map = helpers::group_rt_multiset_by_pid_cong(&df_in);
    let shuf_map = helpers::group_rt_multiset_by_pid_cong(&df_out);

    assert_eq!(orig_map.len(), shuf_map.len(), "group count must match");
    for (key, orig_vals) in orig_map {
        let shuf_vals = shuf_map
            .get(&key)
            .expect("missing group in shuffled output");
        assert_eq!(
            orig_vals, *shuf_vals,
            "RT multiset must be identical within group {:?}",
            key
        );
    }
}

#[test]
fn missing_columns_cause_validation_error() {
    let tempdir = TempDir::new().expect("create temp dir");
    let output_dir = tempdir.path();

    // Write missing-cols fixture
    let input = write_fixture(&tempdir, "missing_cols.csv", MISSING_COLS_CSV);

    let mut cmd = std::process::Command::cargo_bin("process").expect("find process binary");
    cmd.arg("--input")
        .arg(&input)
        .arg("--output-dir")
        .arg(output_dir);

    let out = cmd.output().expect("run process");
    assert!(
        !out.status.success(),
        "process should fail on missing columns"
    );
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        stderr.contains("Missing required column"),
        "stderr should mention missing columns, got: {}",
        stderr
    );
}
