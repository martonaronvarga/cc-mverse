use assert_cmd::cargo::CommandCargoExt;
use assert_cmd::prelude::*;
use polars::prelude::*;
use std::path::PathBuf;
use tempfile::TempDir;

/// Basic integration check that additive_qmap runs and adjusts RTs (not identical to input),
/// but preserves schema and row count. We don't assert strong distributional properties
/// because additive_qmap is shrinkage-based and depends on data details.
#[test]
fn qmap5_runs_and_adjusts_rts_with_preserved_schema_and_row_count() {
    let tempdir = TempDir::new().expect("create temp dir");
    let output_dir = tempdir.path();
    let input = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fixtures")
        .join("small_input.csv");

    let mut cmd = std::process::Command::cargo_bin("process").expect("find process binary");
    cmd.arg("--input")
        .arg(&input)
        .arg("--output-dir")
        .arg(output_dir)
        .arg("--sample-sizes")
        .arg("1.0")
        .arg("--transformations")
        .arg("no_log_rt") // keep rt scale name
        .arg("--outliers")
        .arg("none")
        .arg("--effect-conditions")
        .arg("null_interaction")
        .arg("--strip-methods")
        .arg("additive_qmap")
        .arg("--threads")
        .arg("2");

    let out = cmd.output().expect("run process");
    assert!(
        out.status.success(),
        "process failed: stderr={}",
        String::from_utf8_lossy(&out.stderr)
    );

    // Find the one output parquet for additive_qmap
    let mut candidates: Vec<_> = std::fs::read_dir(output_dir)
        .unwrap()
        .filter_map(|e| e.ok())
        .map(|e| e.path())
        .filter(|p| p.extension().map(|e| e == "parquet").unwrap_or(false))
        .filter(|p| {
            let name = p.file_name().unwrap().to_string_lossy();
            name.contains("__null_interaction__") && name.contains("__additive_qmap")
        })
        .collect();

    assert_eq!(
        candidates.len(),
        1,
        "expected 1 additive_qmap output parquet, got {}",
        candidates.len()
    );

    let mut df_in = CsvReadOptions::default()
        .try_into_reader_with_file_path(Some(input))
        .unwrap()
        .finish()
        .unwrap();
    df_in = df_in
        .lazy()
        .with_columns([
            col("participant_id").cast(DataType::String),
            col("cong").cast(DataType::String),
            col("prev_cong").cast(DataType::String),
            col("rt").cast(DataType::Float64),
        ])
        .collect()
        .unwrap();
    let file = std::fs::File::open(&candidates[0]).expect("open parquet");
    let df_out = ParquetReader::new(file).finish().expect("read parquet");

    assert_eq!(df_in.height(), df_out.height(), "row count must match");

    // Schema preserved: participant_id, cong, prev_cong, rt present
    for col in ["participant_id", "cong", "prev_cong", "rt"] {
        assert!(
            df_out.get_column_names().iter().any(|n| n == &col),
            "missing column {} in qmap output",
            col
        );
    }

    // RTs should generally change (qmap replaces rt with adjusted values)
    let rt_in: Vec<f64> = df_in
        .column("rt")
        .unwrap()
        .f64()
        .unwrap()
        .into_no_null_iter()
        .collect();
    let rt_out: Vec<f64> = df_out
        .column("rt")
        .unwrap()
        .f64()
        .unwrap()
        .into_no_null_iter()
        .collect();

    assert_ne!(
        rt_in, rt_out,
        "additive_qmap should alter RT values compared to input"
    );
}
