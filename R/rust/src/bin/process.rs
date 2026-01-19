// src/bin/process.rs - CLI for batch processing

use anyhow::{Context, Result};
use clap::Parser;
use crossbeam_channel::unbounded;
use multiverse_analysis::{
    BatchProcessor, BranchConfig, EffectCondition, OutlierMethod, ProcessingResult, StripMethod,
    Transformation,
};
use polars::prelude::*;
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
    #[arg(long, default_value = "0.5,0.75,1.0")]
    sample_sizes: String,

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
    #[arg(long, default_value = "null_interaction,null_both,present")]
    effect_conditions: String,

    /// Effect stripping methods
    #[arg(long, default_value = "qmap_5, shuffle")]
    strip_methods: String,

    /// Log level
    #[arg(long, default_value = "info")]
    log_level: String,

    /// Number of threads (0 = auto)
    #[arg(long, default_value = "0")]
    threads: usize,

    /// Save processing metadata
    #[arg(long)]
    save_metadata: bool,
}

struct WriteTask {
    path: PathBuf,
    df: DataFrame,
    result: ProcessingResult,
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
        num_cpus::get() - 1
    };

    // Parse arguments
    let sample_sizes: Vec<f64> = parse_csv_floats(&args.sample_sizes)?;
    let transformations: Vec<Transformation> = parse_csv_enums(&args.transformations)?;
    let outlier_methods: Vec<OutlierMethod> = parse_outlier_methods(&args.outliers)?;
    let effect_conditions: Vec<EffectCondition> = parse_effect_conditions(&args.effect_conditions)?;
    let strip_methods: Vec<StripMethod> = parse_strip_methods(&args.strip_methods)?;

    tracing::info!(
        sample_sizes = ?sample_sizes,
        transformations = ?transformations,
        outlier_methods = ?outlier_methods,
        effect_conditions = ?effect_conditions,
        strip_methods = ?strip_methods,
        "Parsed configurations"
    );

    // Load data
    tracing::info!(path = ?args.input, "Loading input data");
    let file = std::fs::File::open(&args.input).context("Failed to open CSV file")?;

    let df = CsvReader::new(file)
        .finish()
        .context("Failed to read CSV into DataFrame")?;

    tracing::info!(rows = df.height(), cols = df.width(), "Data loaded");

    // Validate data structure
    validate_dataframe(&df)?;

    // Create output directory
    std::fs::create_dir_all(&args.output_dir).context("Failed to create output directory")?;

    // Generate all branch configurations
    let configs = generate_configs(
        sample_sizes,
        transformations,
        outlier_methods,
        effect_conditions,
        strip_methods,
    );
    tracing::info!(
        n_branches = configs.len(),
        "Generated branch configurations"
    );

    // Optional metadata collector
    let metadata: Option<Arc<Mutex<Vec<ProcessingResult>>>> = if args.save_metadata {
        Some(Arc::new(Mutex::new(Vec::new())))
    } else {
        None
    };

    // share input
    let df = Arc::new(df);

    let configs: Vec<Arc<BranchConfig>> = configs.into_iter().map(Arc::new).collect();
    let (tx, rx) = unbounded::<WriteTask>();
    let output_dir = args.output_dir.clone();

    // Spawn single writer thread that performs all blocking IO (parquet writes)
    let writer_handle = thread::spawn(move || {
        while let Ok(task) = rx.recv() {
            let WriteTask {
                path,
                mut df,
                result,
            } = task;

            // write to tmp file + atomic rename
            // keep the write logic local so it does not block Rayon worker threads
            let tmp_path = path.with_extension("parquet.tmp");

            let write_res: Result<()> = (|| {
                let tmp_file = std::fs::File::create(&tmp_path).with_context(|| {
                    format!("Failed to create temp file: {}", tmp_path.display())
                })?;

                // Note: adjust ParquetWriter construction to your project's writer API
                let writer = ParquetWriter::new(tmp_file)
                    .with_compression(ParquetCompression::Snappy)
                    .with_row_group_size(Some(64 * 1024));

                writer
                    .finish(&mut df)
                    .with_context(|| format!("Failed to write parquet for {}", path.display()))?;

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
                    tracing::info!(path = ?path, branch = result.branch_id, rows = result.n_rows_output, "Saved branch result (writer thread)")
                }
                Err(e) => {
                    let _ = std::fs::remove_file(&tmp_path);
                    tracing::warn!(path = ?path, branch = result.branch_id, error = ?e, "Failed to write parquet (writer thread)");
                }
            }
        }
        tracing::debug!("Writer thread exiting: channel closed");
    });

    let pool = rayon::ThreadPoolBuilder::new()
        .num_threads(num_threads)
        .build()?;

    let metadata: Option<Arc<Mutex<Vec<ProcessingResult>>>> = metadata.as_ref().map(Arc::clone);
    let tx_arc = Arc::new(tx);
    let output_dir_arc = Arc::new(output_dir);

    // Process branches in parallel, write results immediately (no big Vec in memory)
    pool.install(|| {
        configs.par_iter().for_each(|config_arc| {
            let cfg = Arc::clone(config_arc);
            let df_ref = Arc::clone(&df);
            let tx = Arc::clone(&tx_arc);
            let out_dir = Arc::clone(&output_dir_arc);

            let pipeline = multiverse_analysis::BranchPipeline::new(*cfg);
            match pipeline.process(&*df_ref) {
                Ok((processed_df, result)) => {
                    if result.n_rows_output == 0 {
                        tracing::warn!(
                            branch = result.branch_id,
                            "Skipping write: branch produced zero rows"
                        );
                    } else if result.success {
                        // Write parquet immediately

                        let filename = format!(
                            "processed__{}__{}__{}__{}__{}.parquet",
                            result.sample_size,
                            result.transformation,
                            result.outlier_method,
                            result.effect_condition,
                            result.strip_method,
                        );
                        let path = out_dir.join(&filename);
                        let task = WriteTask {path, df: processed_df, result: result.clone()};

                        if let Err(send_err) = tx.send(task) {
                            tracing::warn!(error = ?send_err, "Writer channel closed while sending");
                        } else {
                            tracing::debug!("Sent branch to writer thread");
                        }
                    } else {
                        tracing::warn!(
                            branch = result.branch_id,
                            error = result.error_message,
                            "Branch processing failed"
                        );
                    }
                    if let Some(meta_arc) = &metadata {
                        let mut guard = meta_arc.lock().unwrap();
                        guard.push(result);
                    }
                }
                Err(e) => {
                    tracing::warn!(config = ?cfg, error = ?e, "Branch processing failed");
                }
            }
        });
    });

    drop(tx_arc);
    writer_handle.join().expect("Writer thread panicked");

    if let Some(meta_arc) = metadata {
        let guard = meta_arc.lock().unwrap();
        let metadata_path = args.output_dir.join("metadata.json");

        let json = serde_json::to_string_pretty(&*guard).context("Failed to serialize metadata")?;

        std::fs::write(&metadata_path, json).context("Failed to write metadata")?;

        tracing::info!(path = ?metadata_path, entries = guard.len(), "Saved metadata");
    }

    tracing::info!("Processing complete");
    Ok(())

    // // Process all branches
    // let processor = BatchProcessor::new(configs);
    // let results = processor.process_all(&df);

    // tracing::info!(n_results = results.len(), "Batch processing complete");

    // // Save results
    // let mut metadata = vec![];
    // for (i, (mut processed_df, result)) in results.into_iter().enumerate() {
    //     let filename = format!(
    //         "processed__{}__{}__{}__{}__{}.parquet",
    //         result.sample_size,
    //         result.transformation,
    //         result.outlier_method,
    //         result.effect_condition,
    //         result.strip_method,
    //     );
    //     let path = args.output_dir.join(&filename);

    //     if result.success {
    //         let file = std::fs::File::create(&path).context("Failed to create Parquet file")?;
    //         let writer = ParquetWriter::new(file);

    //         writer
    //             .finish(&mut processed_df)
    //             .context(format!("Failed to write parquet for branch {}", i))?;

    //         tracing::info!(
    //             path = ?path,
    //             rows = result.n_rows_output,
    //             removed = result.n_rows_removed,
    //             "Saved branch result"
    //         );
    //     } else {
    //         tracing::warn!(
    //             branch = result.branch_id,
    //             error = result.error_message,
    //             "Branch processing failed"
    //         );
    //     }

    //     metadata.push(result);
    // }

    // // Optionally save metadata
    // if args.save_metadata {
    //     let metadata_path = args.output_dir.join("metadata.json");
    //     let json =
    //         serde_json::to_string_pretty(&metadata).context("Failed to serialize metadata")?;
    //     std::fs::write(&metadata_path, json).context("Failed to write metadata")?;
    //     tracing::info!(path = ?metadata_path, "Metadata saved");
    // }

    // tracing::info!("Processing complete");
    // Ok(())
}

fn validate_dataframe(df: &DataFrame) -> Result<()> {
    let required_cols = ["rt", "participant_id", "cong", "prev_cong"];

    for col_name in &required_cols {
        if !df.get_columns().iter().any(|c| c.name() == *col_name) {
            return Err(anyhow::anyhow!("Missing required column: {}", col_name));
        }
    }

    tracing::info!("Data validation passed");
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

fn generate_configs(
    sample_sizes: Vec<f64>,
    transformations: Vec<Transformation>,
    outliers: Vec<OutlierMethod>,
    effect_conditions: Vec<EffectCondition>,
    strip_methods: Vec<StripMethod>,
) -> Vec<BranchConfig> {
    let mut configs = vec![];

    for &sample_size in &sample_sizes {
        for &transformation in &transformations {
            for &outlier_method in &outliers {
                for &effect_condition in &effect_conditions {
                    match effect_condition {
                        EffectCondition::NullInteraction => {
                            for &strip_method in &strip_methods {
                                configs.push(BranchConfig {
                                    sample_size,
                                    transformation,
                                    outlier_method,
                                    effect_condition,
                                    strip_method,
                                });
                            }
                        }
                        EffectCondition::Present | EffectCondition::NullBoth => {
                            configs.push(BranchConfig {
                                sample_size,
                                transformation,
                                outlier_method,
                                effect_condition,
                                // placeholder; ignored because strip is not applied for these conditions
                                strip_method: StripMethod::Shuffle,
                            });
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
    use polars::prelude::*;

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
        let effect_conditions = vec![
            EffectCondition::Present,
            EffectCondition::NullInteraction,
            EffectCondition::NullBoth,
        ];
        let strip_methods = vec![StripMethod::Shuffle, StripMethod::Qmap5];

        let configs = generate_configs(
            sample_sizes.clone(),
            transformations.clone(),
            outliers.clone(),
            effect_conditions.clone(),
            strip_methods.clone(),
        );

        // Expect: Present -> 1 config, NullBoth -> 1 config, NullInteraction -> 2 configs (one per strip)
        assert_eq!(configs.len(), 1 + 1 + 2);

        let mut counts = (0, 0, 0); // present, null_interaction, null_both
        for c in configs {
            match c.effect_condition {
                EffectCondition::Present => counts.0 += 1,
                EffectCondition::NullInteraction => counts.1 += 1,
                EffectCondition::NullBoth => counts.2 += 1,
            }
        }
        assert_eq!(counts, (1, 2, 1));
    }

    #[test]
    fn test_filename_contains_none_for_non_interaction() {
        let df = make_dummy_df();
        let cfg_present = BranchConfig {
            sample_size: 1.0,
            transformation: Transformation::NoLogRt,
            outlier_method: OutlierMethod::None,
            effect_condition: EffectCondition::Present,
            strip_method: StripMethod::Shuffle,
        };
        let pipeline = multiverse_analysis::BranchPipeline::new(cfg_present);
        let (_out, res) = pipeline.process(&df).unwrap();
        let filename = format!(
            "processed__{}__{}__{}__{}__{}.parquet",
            res.sample_size,
            res.transformation,
            res.outlier_method,
            res.effect_condition,
            res.strip_method,
        );
        assert!(filename.ends_with("__none.parquet"));
    }
}
