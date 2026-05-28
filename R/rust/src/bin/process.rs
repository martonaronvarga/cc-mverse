// src/bin/process.rs - CLI for batch processing
// R config is the single source of truth; this CLI accepts R-generated arguments.

use anyhow::{Context, Result};
use clap::Parser;
use crossbeam_channel::{bounded};
use multiverse_analysis::{
    BranchConfig, EffectCondition, OutlierMethod, ProcessingResult, StripMethod, Transformation,
};
use polars::prelude::*;
use polars::prelude::{CsvReadOptions, NullValues};
use rayon::prelude::*;
use std::path::PathBuf;
use std::str::FromStr;
use std::sync::{Arc, Mutex};
use std::thread;
use tracing_subscriber::EnvFilter;

#[derive(Parser)]
#[command(
    name = "process",
    about = "Batch process RT data across all analysis branches",
    long_about = "Processes reaction time data applying all combinations of transformations,\
                outlier methods, effect conditions and sample sizes in parallel."
)]
struct Args {
    /// Input CSV file path
    #[arg(short, long, value_name = "FILE")]
    input: PathBuf,

    /// Output directory for processed parquet files
    #[arg(short, long, value_name = "DIR", default_value = "data/processed")]
    output_dir: PathBuf,

    /// Sample sizes as fractions (comma-separated)
    #[arg(long, default_value = "0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0")]
    sample_sizes: String,

    /// Subsamples per size: "fraction:count" pairs (comma-separated).
    /// E.g. "0.1:5,0.2:5,...,1.0:1"
    /// If not provided, defaults to 1 subsample per size.
    #[arg(long)]
    subsamples_per_size: Option<String>,

    /// Transformations to apply (comma-separated)
    #[arg(long, default_value = "log_rt,no_log_rt")]
    transformations: String,

    /// Outlier methods (comma-separated)
    #[arg(
        long,
        default_value = "sd_2,sd_2.5,sd_3,mad_2,mad_2.5,mad_3,range_1000,range_1250,range_1500,none"
    )]
    outliers: String,

    /// Effect stripping conditions
    #[arg(long, default_value = "null_interaction,present")]
    effect_conditions: String,

    /// Effect stripping methods
    #[arg(long, default_value = "additive_qmap, shuffle")]
    strip_methods: String,

    /// Global random seed for reproducible subsampling
    #[arg(long, default_value = "42")]
    seed: u64,

    /// Log level
    #[arg(long, default_value = "info")]
    log_level: String,

    /// Number of threads (0 = auto)
    #[arg(long, default_value = "0")]
    threads: usize,

    #[arg(long, default_value = "0")]
    writer_threads:usize,

    /// Save processing metadata
    #[arg(long)]
    save_metadata: bool,
}

struct WriteTask {
    path: PathBuf,
    df: DataFrame,
    result: ProcessingResult,
}

/// Spawn a pool of writer threads that drain the channel in parallel.
/// Each thread independently receives tasks and writes parquet files.
/// Returns join handles for all writer threads.
fn spawn_writer_pool(
    n_writers: usize,
    rx: crossbeam_channel::Receiver<WriteTask>,
    metadata: Option<Arc<Mutex<Vec<ProcessingResult>>>>,
) -> Vec<thread::JoinHandle<()>> {
    (0..n_writers)
        .map(|id| {
            let rx = rx.clone();
            let metadata = metadata.clone();

            thread::Builder::new()
                .name(format!("writer-{}", id))
                .spawn(move || {
                    // Each writer loops, pulling tasks from the shared channel.
                    // crossbeam Receiver is multi-consumer safe — tasks are
                    // distributed across writers automatically.
                    while let Ok(task) = rx.recv() {
                        let WriteTask {
                            path,
                            mut df,
                            result,
                        } = task;

                        let tmp_path = path.with_extension("parquet.tmp");

                        let write_res: Result<()> = (|| {
                            let tmp_file =
                                std::fs::File::create(&tmp_path).with_context(|| {
                                    format!(
                                        "Failed to create temp file: {}",
                                        tmp_path.display()
                                    )
                                })?;

                            ParquetWriter::new(tmp_file)
                                .with_compression(ParquetCompression::Snappy)
                                .with_row_group_size(Some(64 * 1024))
                                .finish(&mut df)
                                .with_context(|| {
                                    format!(
                                        "Failed to write parquet: {}",
                                        path.display()
                                    )
                                })?;

                            std::fs::rename(&tmp_path, &path).with_context(|| {
                                format!(
                                    "Failed to rename {} -> {}",
                                    tmp_path.display(),
                                    path.display()
                                )
                            })?;

                            Ok(())
                        })();

                        match write_res {
                            Ok(()) => {
                                tracing::info!(
                                    writer = id,
                                    path = ?path,
                                    data_id = result.data_id,
                                    rows = result.n_rows_output,
                                    "Saved branch result"
                                );

                                if let Some(ref meta) = metadata {
                                    meta.lock().unwrap().push(result);
                                }
                            }
                            Err(e) => {
                                let _ = std::fs::remove_file(&tmp_path);
                                tracing::warn!(
                                    writer = id,
                                    data_id = result.data_id,
                                    error = ?e,
                                    "Failed to write parquet"
                                );
                            }
                        }
                    }

                    tracing::debug!(writer = id, "Writer thread exiting");
                })
                .expect("Failed to spawn writer thread")
        })
        .collect()
}

fn main() -> Result<()> {
    let args = Args::parse();

    // Initialize logging
    tracing_subscriber::fmt()
        .with_env_filter(
            EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new(&args.log_level)),
        )
        .with_target(true)
        .with_thread_ids(true)
        .init();

    tracing::info!("Multiverse analysis processor starting");
    tracing::info!(input = ?args.input, output_dir = ?args.output_dir, "Configuration");

    let num_threads = if args.threads > 0 {
        args.threads
    } else {
        std::thread::available_parallelism()
            .map(|n| n.get())
            .unwrap_or_else(|_| num_cpus::get())
    };

    // Writer threads: default to num_threads/4 (clamped to 2..=8).
    // Writing is I/O bound; a few threads saturate disk bandwidth.
    let num_writers = if args.writer_threads > 0 {
        args.writer_threads
    } else {
        (num_threads / 4).clamp(2, 8)
    };

    const STACK_SIZE: usize = 64 * 1024 * 1024; // 64 MB per thread

    let branch_pool = rayon::ThreadPoolBuilder::new()
        .num_threads(num_threads)
        .stack_size(STACK_SIZE)
        .thread_name(|idx| format!("mv-branch-{}", idx))
        .build()
        .context("Failed to build branch thread pool")?;

    tracing::info!(
        threads = num_threads,
        stack_mb = STACK_SIZE / (1024 * 1024),
        "Branch thread pool initialized (dedicated, not global)"
    );

    // Parse arguments
    let sample_sizes: Vec<f64> = parse_csv_floats(&args.sample_sizes)?;
    let transformations: Vec<Transformation> = parse_csv_enums(&args.transformations)?;
    let outlier_methods: Vec<OutlierMethod> = parse_outlier_methods(&args.outliers)?;
    let effect_conditions: Vec<EffectCondition> = parse_effect_conditions(&args.effect_conditions)?;
    let strip_methods: Vec<StripMethod> = parse_strip_methods(&args.strip_methods)?;
    let subsample_map = parse_subsamples_per_size(&args.subsamples_per_size, &sample_sizes);

    tracing::info!(
        sample_sizes = ?sample_sizes,
        subsample_map = ?subsample_map,
        transformations = ?transformations,
        outlier_methods = ?outlier_methods,
        effect_conditions = ?effect_conditions,
        strip_methods = ?strip_methods,
        "Parsed configurations"
    );

    // Load data
    tracing::info!(path = ?args.input, "Loading input data");
    let file = std::fs::File::open(&args.input).context("Failed to open CSV file")?;
    let df = CsvReadOptions::default()
        .with_has_header(true)
        .with_infer_schema_length(None)
        .map_parse_options(|opts| {
            opts.with_null_values(Some(NullValues::AllColumns(vec![
                "NA".into(),
                "NaN".into(),
                "".into(),
            ])))
            .with_missing_is_null(true)
        })
        .into_reader_with_file_handle(file)
        .finish()
        .context("Failed to read CSV into DataFrame")?;
    tracing::info!(rows = df.height(), cols = df.width(), "Data loaded");

    validate_dataframe(&df)?;
    std::fs::create_dir_all(&args.output_dir).context("Failed to create output directory")?;

    // Generate all branch configurations
    let configs = generate_configs(
        sample_sizes,
        &subsample_map,
        transformations,
        outlier_methods,
        effect_conditions,
        strip_methods,
        args.seed,
    );
    tracing::info!(
        n_branches = configs.len(),
        "Generated branch configurations"
    );

    // Optional metadata collector
    let metadata: Option<Arc<Mutex<Vec<ProcessingResult>>>> = if args.save_metadata {
        Some(Arc::new(Mutex::new(Vec::with_capacity(configs.len()))))
    } else {
        None
    };

    // Bounded channel: backpressure kicks in when writers fall behind.
    // Buffer = 2× writer count so producers don't stall immediately but
    // also don't queue unbounded memory.
    let (tx, rx) = bounded::<WriteTask>(num_writers * 2);
    let output_dir = args.output_dir.clone();

     // Spawn the writer pool BEFORE processing starts
    let writer_handles = spawn_writer_pool(num_writers, rx, metadata.clone());
    tracing::info!(
        writers = writer_handles.len(),
        buffer = num_writers * 2,
        "Writer pool started"
    );
   
    let df = normalize_all(&df)?;
    let output_dir = output_dir.clone();

    branch_pool.install(|| {
        configs.par_iter().for_each(|cfg| {
            let df_ref = df.clone();
            let pipeline = multiverse_analysis::BranchPipeline::new(*cfg);

            match pipeline.process(df_ref) {
                Ok((processed_df, result)) => {
                    if result.n_rows_output == 0 {
                        tracing::warn!(
                            data_id = result.data_id,
                            "Skipping write: branch produced zero rows"
                        );
                    } else if result.success {
                        let filename = format!(
                            "processed__{}.parquet", result.data_id
                        );
                        let path = output_dir.join(&filename);
                        let task = WriteTask {path, df: processed_df, result};

                        if let Err(send_err) = tx.send(task) {
                            tracing::warn!(error = ?send_err, "Writer channel closed while sending");
                        } else {
                            tracing::debug!("Sent branch to writer thread");
                        }
                    } else {
                        tracing::warn!(
                            data_id = result.data_id,
                            error = result.error_message,
                            "Branch processing failed"
                        );
                    }
                }
                Err(e) => {
                    tracing::warn!(config = ?cfg, error = ?e, "Branch processing failed");
                }
            }
        });
    });

    drop(tx);
    for handle in writer_handles {
        handle.join().expect("Writer thread panicked");
    }
    tracing::info!("All writers finished");

    if let Some(meta_arc) = metadata {
        let guard = meta_arc.lock().unwrap();
        let metadata_path = args.output_dir.join("metadata.json");
        let json = serde_json::to_string_pretty(&*guard).context("Failed to serialize metadata")?;
        std::fs::write(&metadata_path, json).context("Failed to write metadata")?;

        tracing::info!(path = ?metadata_path, entries = guard.len(), "Saved metadata");
    }

    tracing::info!("Processing complete");
    Ok(())
}

use std::collections::HashMap;

/// Parse "0.1:5,0.2:5,...,1.0:1" into a map of sample_size -> n_subsamples
fn parse_subsamples_per_size(spec: &Option<String>, sample_sizes: &[f64]) -> HashMap<u64, u32> {
    let mut map = HashMap::new();

    if let Some(s) = spec {
        for pair in s.split(',') {
            let parts: Vec<&str> = pair.trim().split(':').collect();
            if parts.len() == 2
                && let (Ok(frac), Ok(n)) = (parts[0].parse::<f64>(), parts[1].parse::<u32>()) {
                    map.insert(frac.to_bits(), n);
                }
        }
    }

    // Default: 1 subsample for any size not specified
    for &ss in sample_sizes {
        map.entry(ss.to_bits()).or_insert(1);
    }

    map
}

fn validate_dataframe(df: &DataFrame) -> Result<()> {
    let required_cols = ["rt", "participant_id", "cong", "prev_cong"];
    for col_name in &required_cols {
        if !df.get_columns().iter().any(|c| c.name() == *col_name) {
            return Err(anyhow::anyhow!("Missing required column: {}", col_name));
        }
    }

    let rt = df.column("rt")?.cast(&DataType::Float64)?;
    let rt = rt.f64()?;
    let missing_rt = rt.into_iter().filter(|v| v.is_none()).count();
    let nonpositive_rt = rt
        .into_iter()
        .filter(|v| v.map(|x| !x.is_finite() || x <= 0.0).unwrap_or(false))
        .count();

    for col_name in ["cong", "prev_cong"] {
        let s = df.column(col_name)?.cast(&DataType::String)?;
        let invalid: Vec<String> = s
            .str()?
            .into_iter()
            .flatten()
            .filter(|v| !matches!(*v, "-1" | "1" | "-1.0" | "1.0"))
            .take(5)
            .map(|v| v.to_string())
            .collect();
        if !invalid.is_empty() {
            return Err(anyhow::anyhow!(
                "Invalid +/-1 coding in {}: {}",
                col_name,
                invalid.join(",")
            ));
        }
    }

    let pid_missing = df.column("participant_id")?.null_count();
    if pid_missing > 0 {
        return Err(anyhow::anyhow!("participant_id contains {} missing values", pid_missing));
    }

    if missing_rt > 0 || nonpositive_rt > 0 {
        tracing::warn!(missing_rt, nonpositive_rt, "Input RT contains rows that downstream diagnostics will treat as invalid");
    }
    if df.column("prev_cong")?.null_count() > 0 {
        tracing::warn!(missing_prev_cong = df.column("prev_cong")?.null_count(), "Input contains first-trial or missing previous-congruency rows");
    }

    tracing::info!(rows = df.height(), cols = df.width(), "Data validation passed");
    Ok(())
}

fn parse_csv_floats(s: &str) -> Result<Vec<f64>> {
    s.split(',')
        .map(|x| {
            x.trim()
                .parse()
                .context(format!("Failed to parse float: {}", x))
        })
        .collect()
}

fn parse_csv_enums(s: &str) -> Result<Vec<Transformation>> {
    s.split(',')
        .map(|x| Transformation::from_str(x.trim()))
        .collect()
}

fn parse_effect_conditions(s: &str) -> Result<Vec<EffectCondition>> {
    s.split(',')
        .map(|x| EffectCondition::from_str(x.trim()))
        .collect()
}

fn parse_strip_methods(s: &str) -> Result<Vec<StripMethod>> {
    s.split(',')
        .map(|x| StripMethod::from_str(x.trim()))
        .collect()
}

fn parse_outlier_methods(s: &str) -> Result<Vec<OutlierMethod>> {
    s.split(',')
        .map(|x| match x.trim() {
            "sd_2" => Ok(OutlierMethod::Sd(2.0)),
            "sd_2.5" => Ok(OutlierMethod::Sd(2.5)),
            "sd_3" => Ok(OutlierMethod::Sd(3.0)),
            "mad_2" => Ok(OutlierMethod::Mad(2.0)),
            "mad_2.5" => Ok(OutlierMethod::Mad(2.5)),
            "mad_3" => Ok(OutlierMethod::Mad(3.0)),
            "range_1000" => Ok(OutlierMethod::Range {
                min: 200.0,
                max: 1000.0,
            }),
            "range_1250" => Ok(OutlierMethod::Range {
                min: 200.0,
                max: 1250.0,
            }),
            "range_1500" => Ok(OutlierMethod::Range {
                min: 200.0,
                max: 1500.0,
            }),
            "none" => Ok(OutlierMethod::None),
            _ => Err(anyhow::anyhow!("Unknown outlier method: {}", x)),
        })
        .collect()
}

fn normalize_all(df: &DataFrame) -> Result<DataFrame> {
        let mut out = df.clone(); // Single clone at entry point
        for (name, target_type) in [
            ("participant_id", DataType::String),
            ("rt", DataType::Float64),
            ("cong", DataType::String),
            ("prev_cong", DataType::String),
        ] {
            let col = out.column(name)?;
            if col.dtype() != &target_type {
                let casted = col.cast(&target_type)?;
                out.with_column(casted)?;
            }
        }
        Ok(out)
    }

fn generate_configs(
    sample_sizes: Vec<f64>,
    subsample_map: &HashMap<u64, u32>,
    transformations: Vec<Transformation>,
    outliers: Vec<OutlierMethod>,
    effect_conditions: Vec<EffectCondition>,
    strip_methods: Vec<StripMethod>,
    global_seed: u64,
) -> Vec<BranchConfig> {
    let mut configs = vec![];

    for &sample_size in &sample_sizes {
        let n_sub = *subsample_map.get(&sample_size.to_bits()).unwrap_or(&1);

        for sub_id in 1..=n_sub {
            for &transformation in &transformations {
                for &outlier_method in &outliers {
                    for &effect_condition in &effect_conditions {
                        match effect_condition {
                            EffectCondition::NullInteraction => {
                                for &strip_method in &strip_methods {
                                    configs.push(BranchConfig {
                                        sample_size,
                                        subsample_id: sub_id,
                                        transformation,
                                        outlier_method,
                                        effect_condition,
                                        strip_method,
                                        global_seed,
                                    });
                                }
                            }
                            EffectCondition::Present => {
                                configs.push(BranchConfig {
                                    sample_size,
                                    subsample_id: sub_id,
                                    transformation,
                                    outlier_method,
                                    effect_condition,
                                    // placeholder; ignored because strip is not applied for present data
                                    strip_method: StripMethod::Shuffle,
                                    global_seed,
                                });
                            }
                        }
                    }
                }
            }
        }
    }

    configs
}

#[cfg(test)]
mod cli_tests {
    use super::*;

    fn make_dummy_df() -> DataFrame {
        let pid = Series::new("participant_id".into(), &["p1", "p1", "p2", "p2"]);
        let cong = Series::new("cong".into(), &["-1", "1", "-1", "1"]);
        let prev = Series::new("prev_cong".into(), &["-1", "1", "-1", "1"]);
        let rt = Series::new("rt".into(), &[500.0, 520.0, 530.0, 545.0]);
        DataFrame::new(vec![pid.into(), cong.into(), prev.into(), rt.into()]).unwrap()
    }

    #[test]
    fn test_generate_configs_strip_only_for_null_interaction() {
        let sample_sizes = vec![1.0];
        let transformations = vec![Transformation::NoLogRt];
        let outliers = vec![OutlierMethod::None];
        let effect_conditions = vec![EffectCondition::Present, EffectCondition::NullInteraction];
        let strip_methods = vec![StripMethod::Shuffle, StripMethod::AdditiveQmap];
        let subsample_map = parse_subsamples_per_size(&Some("0.1:5,1.0:1".into()), &sample_sizes);
        let seed = 42;

        let configs = generate_configs(
            sample_sizes.clone(),
            &subsample_map.clone(),
            transformations.clone(),
            outliers.clone(),
            effect_conditions.clone(),
            strip_methods.clone(),
            seed,
        );

        // Expect: Present -> 1 config, NullInteraction -> 2 configs (one per strip)
        assert_eq!(configs.len(), 1 + 2);

        let mut counts = (0, 0); // present, null_interaction
        for c in configs {
            match c.effect_condition {
                EffectCondition::Present => counts.0 += 1,
                EffectCondition::NullInteraction => counts.1 += 1,
            }
        }
        assert_eq!(counts, (1, 2));
    }

    #[test]
    fn test_validate_dataframe_rejects_bad_congruency_code() {
        let mut df = make_dummy_df();
        df.replace("cong", Series::new("cong".into(), &["0", "1", "-1", "1"]))
            .unwrap();
        assert!(validate_dataframe(&df).is_err());
    }

    #[test]
    fn test_validate_dataframe_allows_missing_prev_cong_for_first_trials() {
        let df = df!(
            "participant_id" => &["p1", "p1"],
            "cong" => &["-1", "1"],
            "prev_cong" => &[None, Some("-1")],
            "rt" => &[500.0, 510.0]
        )
        .unwrap();
        assert!(validate_dataframe(&df).is_ok());
    }

    #[test]
    fn test_filename_contains_none_for_non_interaction() {
        let df = make_dummy_df();
        let cfg_present = BranchConfig {
            sample_size: 1.0,
            subsample_id: 2,
            transformation: Transformation::NoLogRt,
            outlier_method: OutlierMethod::None,
            effect_condition: EffectCondition::Present,
            strip_method: StripMethod::Shuffle,
            global_seed: 42,
        };
        let pipeline = multiverse_analysis::BranchPipeline::new(cfg_present);
        let (_out, _res) = pipeline.process(df).unwrap();
        let filename = format!("processed__{}.parquet", pipeline.data_id());
        assert!(filename.ends_with("__none.parquet"));
    }
}

