// src/bin/process.rs - high-throughput CLI for Rust preprocessing.
//
// The important unit of work is one nullified effect variant streamed across
// base samples. Each effect variant is prepared on the full empirical data once,
// then branches are derived as: effect/nullification -> sample -> outlier -> transform.
// This gives every sample fraction a draw from the same null population while
// bounding live memory to one effect variant plus bounded outlier tasks.

use anyhow::{Context, Result, anyhow};
use clap::Parser;
use multiverse_analysis::{
    BranchConfig, BranchPipeline, EffectCondition, OutlierMethod, ProcessingResult, StripMethod,
    Transformation,
};
use polars::prelude::*;
use polars::prelude::{CsvReadOptions, NullValues};
use rayon::prelude::*;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::sync::atomic::{AtomicU64, Ordering};
use tracing_subscriber::EnvFilter;

static TMP_COUNTER: AtomicU64 = AtomicU64::new(0);

#[derive(Parser)]
#[command(
    name = "process",
    about = "Batch process RT data across analysis branches",
    long_about = "Processes reaction-time data by sharding over effect variants and base samples.\
                  The pipeline is effect/nullification -> sample -> outlier -> transformation."
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
    /// E.g. "0.1:5,0.2:5,...,1.0:1". If absent, defaults to 1 per size.
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

    /// Effect conditions (comma-separated)
    #[arg(long, default_value = "null_interaction,present")]
    effect_conditions: String,

    /// Effect stripping methods for null_interaction (comma-separated)
    #[arg(long, default_value = "additive_qmap,shuffle")]
    strip_methods: String,

    /// Global random seed for reproducible subsampling
    #[arg(long, default_value = "42")]
    seed: u64,

    /// Log level
    #[arg(long, default_value = "info")]
    log_level: String,

    /// Number of Rayon worker threads (0 = auto)
    #[arg(long, default_value = "0")]
    threads: usize,

    /// Backward-compatible option. DataFrames are no longer queued to writer threads.
    /// Writes happen inline in the task that owns the output DataFrame.
    #[arg(long, default_value = "0")]
    writer_threads: usize,

    /// Process this zero-based shard id. Use with SLURM_ARRAY_TASK_ID.
    #[arg(long)]
    task_id: Option<usize>,

    /// Number of array shards. With --task-id and --task-count, this process runs
    /// base tasks whose task_id % task_count == task_id. Without --task-count,
    /// --task-id selects exactly one base task.
    #[arg(long)]
    task_count: Option<usize>,

    /// Optional output path for the internally generated base-task manifest.
    #[arg(long)]
    write_task_manifest: Option<PathBuf>,

    /// Rewrite existing processed__*.parquet files instead of skipping them.
    #[arg(long, default_value_t = false)]
    overwrite: bool,

    /// Save processing metadata. Array tasks write metadata__task_<id>.json.
    #[arg(long)]
    save_metadata: bool,
}

#[derive(Debug, Clone, Copy)]
struct BaseTask {
    task_id: usize,
    sample_size: f64,
    subsample_id: u32,
}

#[derive(Debug, Clone, Copy)]
struct EffectVariant {
    condition: EffectCondition,
    strip_method: StripMethod,
}

struct Axes {
    transformations: Vec<Transformation>,
    outlier_methods: Vec<OutlierMethod>,
    effect_variants: Vec<EffectVariant>,
    seed: u64,
}

fn main() -> Result<()> {
    let args = Args::parse();

    tracing_subscriber::fmt()
        .with_env_filter(
            EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new(&args.log_level)),
        )
        .with_target(true)
        .with_thread_ids(true)
        .init();

    let num_threads = if args.threads > 0 {
        args.threads
    } else {
        std::thread::available_parallelism()
            .map(|n| n.get())
            .unwrap_or_else(|_| num_cpus::get())
    };

    if args.writer_threads > 0 {
        tracing::warn!(
            writer_threads = args.writer_threads,
            "writer_threads is ignored; queued DataFrame writing was removed to bound memory"
        );
    }

    let branch_pool = rayon::ThreadPoolBuilder::new()
        .num_threads(num_threads)
        .thread_name(|idx| format!("mv-task-{}", idx))
        .build()
        .context("Failed to build Rayon thread pool")?;

    tracing::info!(
        input = ?args.input,
        output_dir = ?args.output_dir,
        threads = num_threads,
        "Multiverse Rust processor starting"
    );

    let sample_sizes = parse_csv_floats(&args.sample_sizes)?;
    let transformations = parse_csv_enums(&args.transformations)?;
    let outlier_methods = parse_outlier_methods(&args.outliers)?;
    let effect_conditions = parse_effect_conditions(&args.effect_conditions)?;
    let strip_methods = parse_strip_methods(&args.strip_methods)?;
    let subsample_map = parse_subsamples_per_size(&args.subsamples_per_size, &sample_sizes);

    let base_tasks = generate_base_tasks(&sample_sizes, &subsample_map);
    if let Some(path) = &args.write_task_manifest {
        write_task_manifest(path, &base_tasks)?;
    }

    let tasks_to_run: Vec<BaseTask> =
        match (args.task_id, args.task_count) {
            (Some(task_id), Some(task_count)) => {
                if task_count == 0 {
                    return Err(anyhow!("--task-count must be positive"));
                }
                if task_id >= task_count {
                    return Err(anyhow!(
                        "task_id {} must be smaller than task_count {}",
                        task_id,
                        task_count
                    ));
                }
                base_tasks
                    .iter()
                    .copied()
                    .filter(|task| task.task_id % task_count == task_id)
                    .collect()
            }
            (Some(task_id), None) => vec![*base_tasks.get(task_id).ok_or_else(|| {
                anyhow!("task_id {} out of range 0..{}", task_id, base_tasks.len())
            })?],
            (None, Some(_)) => {
                return Err(anyhow!("--task-count requires --task-id"));
            }
            (None, None) => base_tasks.clone(),
        };

    let axes = Axes {
        transformations,
        outlier_methods,
        effect_variants: generate_effect_variants(&effect_conditions, &strip_methods),
        seed: args.seed,
    };

    tracing::info!(
        total_base_tasks = base_tasks.len(),
        running_base_tasks = tasks_to_run.len(),
        task_id = ?args.task_id,
        task_count = ?args.task_count,
        transformations = axes.transformations.len(),
        outlier_methods = axes.outlier_methods.len(),
        effect_variants = axes.effect_variants.len(),
        "Generated task grid"
    );

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

    validate_dataframe(&df)?;
    let df = normalize_all(&df)?;
    tracing::info!(
        rows = df.height(),
        cols = df.width(),
        "Data loaded and normalized"
    );

    std::fs::create_dir_all(&args.output_dir).context("Failed to create output directory")?;

    let mut all_results = Vec::new();
    for effect in &axes.effect_variants {
        let effect_start = std::time::Instant::now();
        let effect_cfg = BranchConfig {
            sample_size: 1.0,
            subsample_id: 1,
            transformation: Transformation::NoLogRt,
            outlier_method: OutlierMethod::None,
            effect_condition: effect.condition,
            strip_method: effect.strip_method,
            global_seed: axes.seed,
        };

        let effect_df = BranchPipeline::new(effect_cfg)
            .apply_effect_condition(df.clone())
            .with_context(|| {
                format!(
                    "Failed to prepare effect {:?}/{:?} on full data",
                    effect.condition, effect.strip_method
                )
            })?;

        tracing::info!(
            effect_condition = %effect.condition,
            strip_method = %effect.strip_method,
            rows = effect_df.height(),
            ms = effect_start.elapsed().as_millis(),
            "Prepared full-population effect variant"
        );

        for task in &tasks_to_run {
            let results = process_base_task(
                &branch_pool,
                &effect_df,
                df.height(),
                *task,
                *effect,
                &axes,
                &args.output_dir,
                !args.overwrite,
            )?;
            all_results.extend(results);
        }
    }

    if args.save_metadata {
        let metadata_path = match args.task_id {
            Some(task_id) => args
                .output_dir
                .join(format!("metadata__task_{}.json", task_id)),
            None => args.output_dir.join("metadata.json"),
        };
        let json =
            serde_json::to_string_pretty(&all_results).context("Failed to serialize metadata")?;
        std::fs::write(&metadata_path, json).context("Failed to write metadata")?;
        tracing::info!(path = ?metadata_path, entries = all_results.len(), "Saved metadata");
    }

    tracing::info!(processed_outputs = all_results.len(), "Processing complete");
    Ok(())
}

fn process_base_task(
    pool: &rayon::ThreadPool,
    effect_df: &DataFrame,
    full_n_rows: usize,
    task: BaseTask,
    effect: EffectVariant,
    axes: &Axes,
    output_dir: &Path,
    skip_existing: bool,
) -> Result<Vec<ProcessingResult>> {
    let base_start = std::time::Instant::now();
    tracing::info!(
        task_id = task.task_id,
        sample_size = task.sample_size,
        subsample_id = task.subsample_id,
        effect_condition = %effect.condition,
        strip_method = %effect.strip_method,
        "Starting sampled effect task"
    );

    let sample_cfg = BranchConfig {
        sample_size: task.sample_size,
        subsample_id: task.subsample_id,
        transformation: Transformation::NoLogRt,
        outlier_method: OutlierMethod::None,
        effect_condition: effect.condition,
        strip_method: effect.strip_method,
        global_seed: axes.seed,
    };
    let sampled_df = BranchPipeline::new(sample_cfg)
        .sample_data(effect_df)
        .with_context(|| format!("Failed to sample base task {}", task.task_id))?;

    tracing::info!(
        task_id = task.task_id,
        rows = sampled_df.height(),
        full_rows = full_n_rows,
        "Sampled effect task"
    );

    let per_outlier: Vec<Result<Vec<ProcessingResult>>> = pool.install(|| {
        axes.outlier_methods
            .par_iter()
            .map(|outlier| {
                process_outlier_variant(
                    sampled_df.clone(),
                    full_n_rows,
                    task,
                    effect,
                    *outlier,
                    &axes.transformations,
                    axes.seed,
                    output_dir,
                    skip_existing,
                )
            })
            .collect()
    });

    let mut task_results = Vec::new();
    for result in per_outlier {
        task_results.extend(result?);
    }

    tracing::info!(
        task_id = task.task_id,
        effect_condition = %effect.condition,
        strip_method = %effect.strip_method,
        outputs = task_results.len(),
        ms = base_start.elapsed().as_millis(),
        "Finished sampled effect task"
    );

    Ok(task_results)
}

#[allow(clippy::too_many_arguments)]
fn process_outlier_variant(
    effect_df: DataFrame,
    full_n_rows: usize,
    task: BaseTask,
    effect: EffectVariant,
    outlier_method: OutlierMethod,
    transformations: &[Transformation],
    seed: u64,
    output_dir: &Path,
    skip_existing: bool,
) -> Result<Vec<ProcessingResult>> {
    let outlier_start = std::time::Instant::now();
    let outlier_cfg = BranchConfig {
        sample_size: task.sample_size,
        subsample_id: task.subsample_id,
        transformation: Transformation::NoLogRt,
        outlier_method,
        effect_condition: effect.condition,
        strip_method: effect.strip_method,
        global_seed: seed,
    };

    let filtered_df = BranchPipeline::new(outlier_cfg)
        .filter_outliers(effect_df)
        .with_context(|| {
            format!(
                "Failed outlier filter {} for task {}",
                outlier_method.as_string(),
                task.task_id
            )
        })?;

    let mut results = Vec::with_capacity(transformations.len());
    for &transformation in transformations {
        let branch_start = std::time::Instant::now();
        let cfg = BranchConfig {
            sample_size: task.sample_size,
            subsample_id: task.subsample_id,
            transformation,
            outlier_method,
            effect_condition: effect.condition,
            strip_method: effect.strip_method,
            global_seed: seed,
        };
        let pipeline = BranchPipeline::new(cfg);
        let data_id = pipeline.data_id();
        let filename = format!("processed__{}.parquet", data_id);
        let path = output_dir.join(filename);

        if skip_existing && path.exists() {
            tracing::debug!(path = ?path, data_id = %data_id, "Skipping existing output");
            continue;
        }

        let transformed_df = pipeline
            .apply_transformation(filtered_df.clone())
            .with_context(|| format!("Failed transformation {} for {}", transformation, data_id))?;

        let n_rows_output = transformed_df.height();
        if n_rows_output == 0 {
            tracing::warn!(data_id = %data_id, "Skipping empty output");
            continue;
        }

        write_parquet_atomic(transformed_df, &path)
            .with_context(|| format!("Failed to write {}", path.display()))?;

        let strip_label = match effect.condition {
            EffectCondition::NullInteraction => effect.strip_method.to_string(),
            EffectCondition::Present => "none".to_string(),
        };

        let result = ProcessingResult {
            branch_id: data_id.clone(),
            data_id: data_id.clone(),
            sample_size: task.sample_size,
            subsample_id: task.subsample_id,
            transformation: transformation.to_string(),
            outlier_method: outlier_method.as_string(),
            effect_condition: effect.condition.to_string(),
            strip_method: strip_label,
            n_rows_input: full_n_rows,
            n_rows_output,
            n_rows_removed: full_n_rows.saturating_sub(n_rows_output),
            processing_time_ms: branch_start.elapsed().as_millis(),
            success: true,
            error_message: None,
        };

        tracing::info!(
            data_id = %data_id,
            rows = n_rows_output,
            ms = branch_start.elapsed().as_millis(),
            "Wrote branch output"
        );
        results.push(result);
    }

    tracing::debug!(
        task_id = task.task_id,
        outlier_method = %outlier_method.as_string(),
        outputs = results.len(),
        ms = outlier_start.elapsed().as_millis(),
        "Finished outlier variant"
    );
    Ok(results)
}

fn write_parquet_atomic(mut df: DataFrame, path: &Path) -> Result<()> {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)
            .with_context(|| format!("Failed to create output dir {}", parent.display()))?;
    }

    let file_name = path
        .file_name()
        .and_then(|s| s.to_str())
        .ok_or_else(|| anyhow!("Invalid output path: {}", path.display()))?;
    let counter = TMP_COUNTER.fetch_add(1, Ordering::Relaxed);
    let tmp_name = format!(".{}.{}.{}.tmp", file_name, std::process::id(), counter);
    let tmp_path = path.with_file_name(tmp_name);

    let tmp_file = std::fs::File::create(&tmp_path)
        .with_context(|| format!("Failed to create temp file: {}", tmp_path.display()))?;

    let write_result = ParquetWriter::new(tmp_file)
        .with_compression(ParquetCompression::Snappy)
        .with_row_group_size(Some(64 * 1024))
        .finish(&mut df)
        .with_context(|| format!("Failed to write parquet: {}", tmp_path.display()));

    if let Err(e) = write_result {
        let _ = std::fs::remove_file(&tmp_path);
        return Err(e);
    }

    std::fs::rename(&tmp_path, path).with_context(|| {
        format!(
            "Failed to rename {} -> {}",
            tmp_path.display(),
            path.display()
        )
    })?;
    Ok(())
}

fn generate_base_tasks(sample_sizes: &[f64], subsample_map: &HashMap<u64, u32>) -> Vec<BaseTask> {
    let mut tasks = Vec::new();
    for &sample_size in sample_sizes {
        let n_sub = *subsample_map.get(&sample_size.to_bits()).unwrap_or(&1);
        for subsample_id in 1..=n_sub {
            let task_id = tasks.len();
            tasks.push(BaseTask {
                task_id,
                sample_size,
                subsample_id,
            });
        }
    }
    tasks
}

fn generate_effect_variants(
    effect_conditions: &[EffectCondition],
    strip_methods: &[StripMethod],
) -> Vec<EffectVariant> {
    let mut out = Vec::new();
    for &condition in effect_conditions {
        match condition {
            EffectCondition::Present => out.push(EffectVariant {
                condition,
                strip_method: StripMethod::Shuffle,
            }),
            EffectCondition::NullInteraction => {
                for &strip_method in strip_methods {
                    out.push(EffectVariant {
                        condition,
                        strip_method,
                    });
                }
            }
        }
    }
    out
}

fn write_task_manifest(path: &Path, tasks: &[BaseTask]) -> Result<()> {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)
            .with_context(|| format!("Failed to create manifest dir {}", parent.display()))?;
    }
    let mut body = String::from("task_id\tsample_size\tsubsample_id\n");
    for task in tasks {
        body.push_str(&format!(
            "{}\t{}\t{}\n",
            task.task_id, task.sample_size, task.subsample_id
        ));
    }
    std::fs::write(path, body).with_context(|| format!("Failed to write {}", path.display()))?;
    Ok(())
}

/// Parse "0.1:5,0.2:5,...,1.0:1" into sample_size -> n_subsamples.
fn parse_subsamples_per_size(spec: &Option<String>, sample_sizes: &[f64]) -> HashMap<u64, u32> {
    let mut map = HashMap::new();

    if let Some(s) = spec {
        for pair in s.split(',') {
            let parts: Vec<&str> = pair.trim().split(':').collect();
            if parts.len() == 2 {
                if let (Ok(frac), Ok(n)) = (parts[0].parse::<f64>(), parts[1].parse::<u32>()) {
                    map.insert(frac.to_bits(), n);
                }
            }
        }
    }

    for &ss in sample_sizes {
        map.entry(ss.to_bits()).or_insert(1);
    }

    map
}

fn validate_dataframe(df: &DataFrame) -> Result<()> {
    let required_cols = ["rt", "participant_id", "cong", "prev_cong"];
    for col_name in &required_cols {
        if !df.get_columns().iter().any(|c| c.name() == *col_name) {
            return Err(anyhow!("Missing required column: {}", col_name));
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
            return Err(anyhow!(
                "Invalid +/-1 coding in {}: {}",
                col_name,
                invalid.join(",")
            ));
        }
    }

    let pid_missing = df.column("participant_id")?.null_count();
    if pid_missing > 0 {
        return Err(anyhow!(
            "participant_id contains {} missing values",
            pid_missing
        ));
    }

    if missing_rt > 0 || nonpositive_rt > 0 {
        tracing::warn!(missing_rt, nonpositive_rt, "Input RT contains invalid rows");
    }
    if df.column("prev_cong")?.null_count() > 0 {
        tracing::warn!(
            missing_prev_cong = df.column("prev_cong")?.null_count(),
            "Input contains first-trial or missing previous-congruency rows"
        );
    }

    tracing::info!(
        rows = df.height(),
        cols = df.width(),
        "Data validation passed"
    );
    Ok(())
}

fn parse_csv_floats(s: &str) -> Result<Vec<f64>> {
    s.split(',')
        .map(|x| {
            x.trim()
                .parse()
                .with_context(|| format!("Failed to parse float: {}", x))
        })
        .collect()
}

fn parse_csv_enums(s: &str) -> Result<Vec<Transformation>> {
    s.split(',')
        .filter(|x| !x.trim().is_empty())
        .map(|x| Transformation::from_str(x.trim()))
        .collect()
}

fn parse_effect_conditions(s: &str) -> Result<Vec<EffectCondition>> {
    s.split(',')
        .filter(|x| !x.trim().is_empty())
        .map(|x| EffectCondition::from_str(x.trim()))
        .collect()
}

fn parse_strip_methods(s: &str) -> Result<Vec<StripMethod>> {
    s.split(',')
        .filter(|x| !x.trim().is_empty())
        .map(|x| StripMethod::from_str(x.trim()))
        .collect()
}

fn parse_outlier_methods(s: &str) -> Result<Vec<OutlierMethod>> {
    s.split(',')
        .filter(|x| !x.trim().is_empty())
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
            _ => Err(anyhow!("Unknown outlier method: {}", x)),
        })
        .collect()
}

fn normalize_all(df: &DataFrame) -> Result<DataFrame> {
    let mut out = df.clone();
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

#[cfg(test)]
mod cli_tests {
    use super::*;

    #[test]
    fn test_base_task_count() {
        let sample_sizes = vec![0.1, 1.0];
        let subsample_map = parse_subsamples_per_size(&Some("0.1:5,1.0:1".into()), &sample_sizes);
        let tasks = generate_base_tasks(&sample_sizes, &subsample_map);
        assert_eq!(tasks.len(), 6);
        assert_eq!(tasks[0].task_id, 0);
        assert_eq!(tasks[5].subsample_id, 1);
    }

    #[test]
    fn test_effect_variants_present_only_once() {
        let effects = vec![EffectCondition::Present, EffectCondition::NullInteraction];
        let strips = vec![StripMethod::Shuffle, StripMethod::AdditiveQmap];
        let variants = generate_effect_variants(&effects, &strips);
        assert_eq!(variants.len(), 3);
        assert_eq!(
            variants
                .iter()
                .filter(|v| v.condition == EffectCondition::Present)
                .count(),
            1
        );
    }
}
