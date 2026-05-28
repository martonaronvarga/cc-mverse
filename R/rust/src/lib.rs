// src/lib.rs - Core library

use anyhow::{anyhow, Result};
use polars::prelude::*;
use rand::rngs::StdRng;
use rand::seq::SliceRandom;
use rand::SeedableRng;
use rayon::prelude::*;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::fmt;
use tracing::{debug, info};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Transformation {
    LogRt,
    NoLogRt,
}

impl fmt::Display for Transformation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Transformation::LogRt => write!(f, "log_rt"),
            Transformation::NoLogRt => write!(f, "no_log_rt"),
        }
    }
}

impl std::str::FromStr for Transformation {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self> {
        match s {
            "log_rt" => Ok(Transformation::LogRt),
            "no_log_rt" => Ok(Transformation::NoLogRt),
            _ => Err(anyhow::anyhow!("Unknown transformation: {}", s)),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OutlierSpec {
    pub method: OutlierMethod,
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum OutlierMethod {
    Sd(f64),
    Mad(f64),
    Range { min: f64, max: f64 },
    None,
}

impl OutlierMethod {
    pub fn as_string(&self) -> String {
        match self {
            OutlierMethod::Sd(t) => format!("sd_{}", t),
            OutlierMethod::Mad(t) => format!("mad_{}", t),
            OutlierMethod::Range {
                min: 200.0,
                max: 1000.0,
            } => "range_1000".to_string(),
            OutlierMethod::Range {
                min: 200.0,
                max: 1250.0,
            } => "range_1250".to_string(),
            OutlierMethod::Range {
                min: 200.0,
                max: 1500.0,
            } => "range_1500".to_string(),
            OutlierMethod::None => "none".to_string(),
            _ => format!("{:?}", self),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BranchConfig {
    pub sample_size: f64,
    pub subsample_id: u32,
    pub transformation: Transformation,
    pub outlier_method: OutlierMethod,
    pub effect_condition: EffectCondition,
    pub strip_method: StripMethod,
    pub global_seed: u64,
}

fn stable_seed_mix(mut hash: u64, value: u64) -> u64 {
    for byte in value.to_le_bytes() {
        hash ^= byte as u64;
        hash = hash.wrapping_mul(0x100000001b3);
    }
    hash
}

impl BranchConfig {
    /// Deterministic seed derived from global seed + branch identity.
    /// Two branches with different subsample_id get different seeds.
    pub fn sampling_seed(&self) -> u64 {
        let mut hash = 0xcbf29ce484222325_u64;
        hash = stable_seed_mix(hash, self.global_seed);
        hash = stable_seed_mix(hash, self.sample_size.to_bits());
        stable_seed_mix(hash, self.subsample_id as u64)
    }

    /// Seed for effect stripping (shuffle / qmap).
    /// Depends on subsample_id so different subsamples get different null realizations.
    pub fn strip_seed(&self) -> u64 {
        let mut hash = 0xcbf29ce484222325_u64;
        hash = stable_seed_mix(hash, self.global_seed);
        hash = stable_seed_mix(hash, self.subsample_id as u64);
        // Include effect condition and strip method so present branches don't collide with null_interaction seeds
        hash = stable_seed_mix(hash, self.effect_condition as u64);
        stable_seed_mix(hash, self.strip_method as u64)
    }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct ProcessingResult {
    pub branch_id: String,
    pub data_id: String,
    pub sample_size: f64,
    pub subsample_id: u32,
    pub transformation: String,
    pub outlier_method: String,
    pub effect_condition: String,
    pub strip_method: String,
    pub n_rows_input: usize,
    pub n_rows_output: usize,
    pub n_rows_removed: usize,
    pub processing_time_ms: u128,
    pub success: bool,
    pub error_message: Option<String>,
}

// ============================================================================
// BRANCH PROCESSING PIPELINE
// ============================================================================

#[derive(Debug, Clone)]
pub struct BranchPipeline {
    pub config: BranchConfig,
}

impl BranchPipeline {
    pub fn new(config: BranchConfig) -> Self {
        BranchPipeline { config }
    }

    /// The data_id used for filenames (model-agnostic).
    /// Format: sample_size__subsample_id__transformation__outlier__effect_condition__strip_method
    pub fn data_id(&self) -> String {
        let strip_label = match self.config.effect_condition {
            EffectCondition::NullInteraction => self.config.strip_method.to_string(),
            _ => "none".to_string(),
        };
        format!(
            "{}__{}__{}__{}__{}__{}",
            self.config.sample_size,
            self.config.subsample_id,
            self.config.transformation,
            self.config.outlier_method.as_string(),
            self.config.effect_condition,
            strip_label,
        )
    }

    /// Process a single branch: sampling -> transformation -> outlier filtering
    pub fn process(&self, mut data: DataFrame) -> Result<(DataFrame, ProcessingResult)> {
        let start_time = std::time::Instant::now();
        let n_rows_input = data.height();

        debug!(
            branch = ?self.config,
            n_rows = n_rows_input,
            "Starting branch processing"
        );

        // Step 0: Condition data
        data = self.apply_effect_condition(data)?;
        debug!(n_rows = data.height(), "After conditioning");

        // Step 1: Sample data
        data = self.sample_data(&data)?;
        debug!(n_rows = data.height(), "After sampling");

        // Step 2: Filter outliers
        data = self.filter_outliers(data)?;
        debug!(n_rows = data.height(), "After outlier filtering");

        // Step 3: Transform
        data = self.apply_transformation(data)?;
        debug!(n_rows = data.height(), "After transformation");

        let n_rows_output = data.height();
        let processing_time_ms = start_time.elapsed().as_millis();
        let data_id = self.data_id();

        let strip_label = match self.config.effect_condition {
            EffectCondition::NullInteraction => self.config.strip_method.to_string(),
            _ => "none".to_string(),
        };

        let result = ProcessingResult {
            branch_id: data_id.clone(),
            data_id: data_id.clone(),
            sample_size: self.config.sample_size,
            subsample_id: self.config.subsample_id,
            transformation: self.config.transformation.to_string(),
            outlier_method: self.config.outlier_method.as_string(),
            effect_condition: self.config.effect_condition.to_string(),
            strip_method: strip_label.clone(),
            n_rows_input,
            n_rows_output,
            n_rows_removed: n_rows_input - n_rows_output,
            processing_time_ms,
            success: true,
            error_message: None,
        };

        info!(
            data_id = data_id,
            rows_kept = n_rows_output,
            rows_removed = n_rows_input - n_rows_output,
            time_ms = processing_time_ms,
            "Branch processing complete"
        );

        Ok((data, result))
    }

    // fn branch_id_string(&self, strip_label: &str) -> String {
    //     format!(
    //         "{}__{}__{}__{}__{}",
    //         self.config.sample_size,
    //         self.config.transformation,
    //         self.config.outlier_method.as_string(),
    //         self.config.effect_condition,
    //         strip_label,
    //     )
    // }

    fn sample_data(&self, data: &DataFrame) -> Result<DataFrame> {
        if (self.config.sample_size - 1.0).abs() < 1e-6 {
            return Ok(data.clone());
        }

        let pid_col = data.column("participant_id")?.str()?;
        let unique_pids: Vec<String> = pid_col
            .unique()?
            .into_no_null_iter()
            .map(|s| s.to_string())
            .collect();

        let n_keep = ((unique_pids.len() as f64) * self.config.sample_size).ceil() as usize;
        let seed = self.config.sampling_seed();
        let mut rng = StdRng::seed_from_u64(seed);

        let mut selected = unique_pids;
        selected.shuffle(&mut rng);
        selected.truncate(n_keep);

        // Use a HashSet<&str> to avoid per-row String allocation
        use ahash::AHashSet;
        let id_set: AHashSet<&str> = selected.iter().map(|s| s.as_str()).collect();

        let mask: BooleanChunked = pid_col
            .into_iter()
            .map(|opt_id| opt_id.map(|id| id_set.contains(id)).unwrap_or(false))
            .collect();

        data.filter(&mask).map_err(Into::into)
    }

    fn apply_transformation(&self, mut data: DataFrame) -> Result<DataFrame> {
        match self.config.transformation {
            Transformation::LogRt => {
                let rt = data.column("rt")?.f64()?;
                let log_rt = rt.apply(|opt_v| opt_v.map(|v| v.ln()));

                data.replace("rt", log_rt.into_series())?;
                Ok(data)
            }
            Transformation::NoLogRt => Ok(data),
        }
    }

    fn filter_outliers(&self, data: DataFrame) -> Result<DataFrame> {
        match self.config.outlier_method {
            OutlierMethod::None => Ok(data.clone()),
            OutlierMethod::Sd(threshold) => self.filter_sd(data, threshold),
            OutlierMethod::Mad(threshold) => self.filter_mad(data, threshold),
            OutlierMethod::Range { min, max } => self.filter_range(data, min, max),
        }
    }

    fn filter_sd(&self, data: DataFrame, threshold: f64) -> Result<DataFrame> {
        let rt = data.column("rt")?.f64()?;

        // Welford's single-pass mean + variance
        let mut count = 0u64;
        let mut mean = 0.0f64;
        let mut m2 = 0.0f64;
        for val in rt.into_iter().flatten() {
            count += 1;
            let delta = val - mean;
            mean += delta / count as f64;
            let delta2 = val - mean;
            m2 += delta * delta2;
        }
        let std = if count > 1 {
            (m2 / (count - 1) as f64).sqrt()
        } else {
            0.0
        };

        let lower = mean - threshold * std;
        let upper = mean + threshold * std;

        let mask: BooleanChunked = rt
            .into_iter()
            .map(|opt_v| opt_v.map(|v| v >= lower && v <= upper).unwrap_or(false))
            .collect();

        data.filter(&mask).map_err(Into::into)
    }

    fn filter_mad(&self, data: DataFrame, threshold: f64) -> Result<DataFrame> {
        let rt = data.column("rt")?.f64()?;
        let values: Vec<_> = rt.into_iter().flatten().collect();

        if values.is_empty() {
            return Ok(data);
        }

        let mut sorted = values.clone();
        sorted.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
        let median = if sorted.len() % 2 == 0 {
            (sorted[sorted.len() / 2 - 1] + sorted[sorted.len() / 2]) / 2.0
        } else {
            sorted[sorted.len() / 2]
        };

        let deviations: Vec<_> = values.iter().map(|x| (x - median).abs()).collect();
        let mut sorted_dev = deviations.clone();
        sorted_dev.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
        let mad = if sorted_dev.len() % 2 == 0 {
            (sorted_dev[sorted_dev.len() / 2 - 1] + sorted_dev[sorted_dev.len() / 2]) / 2.0
        } else {
            sorted_dev[sorted_dev.len() / 2]
        };

        let lower = median - threshold * mad;
        let upper = median + threshold * mad;

        debug!(median, mad, lower, upper, "MAD filtering");

        let mask: BooleanChunked = rt
            .into_iter()
            .map(|opt_v| opt_v.map(|v| v >= lower && v <= upper).unwrap_or(false))
            .collect();

        Ok(data.filter(&mask)?)
    }

    fn filter_range(&self, data: DataFrame, min: f64, max: f64) -> Result<DataFrame> {
        let mask = data
            .column("rt")?
            .f64()?
            .into_iter()
            .map(|opt_v| opt_v.map(|v| v >= min && v <= max).unwrap_or(false))
            .collect();

        debug!(min, max, "Range filtering");

        Ok(data.filter(&mask)?)
    }

    pub fn apply_effect_condition(&self, data: DataFrame) -> Result<DataFrame> {
        match self.config.effect_condition {
            EffectCondition::Present => {
                // No modification
                Ok(data)
            }
            EffectCondition::NullInteraction => {
                // Estimate and remove interaction effect
                self.remove_interaction_effect(data)
            }

        }
    }

    /// Remove cong * prev_cong interaction effect
    /// Quantile mapping with kappa shrinkage
    fn remove_interaction_effect(&self, data: DataFrame) -> Result<DataFrame> {
        use tracing::debug;
        debug!("Removing interaction effect from RT");

        let seed = self.config.strip_seed();
        match self.config.strip_method {
            StripMethod::Shuffle => shuffle_null(data, "rt", seed),
            StripMethod::AdditiveQmap => quantile_map_once(
                data,
                QuantileMapParams {
                    scale_col: "rt".into(),
                    kappa: 5.0,
                    ngrid: 200,
                },
            ),
            StripMethod::AdditiveQmapTrialBin => quantile_map_trial_bin(
                data,
                QuantileMapParams {
                    scale_col: "rt".into(),
                    kappa: 5.0,
                    ngrid: 200,
                },
                5,
                4,
            ),
            StripMethod::LocalMeanResidual => local_mean_residual_strip(data, 5, 4),
            StripMethod::LocalMedianResidual => local_median_residual_strip(data, 5, 4),
        }
    }
}

// ============================================================================
// BATCH PROCESSING
// ============================================================================

// pub struct BatchProcessor {
//     configs: Vec<BranchConfig>,
// }

// impl BatchProcessor {
//     pub fn new(configs: Vec<BranchConfig>) -> Self {
//         BatchProcessor { configs }
//     }

// Process all branches in parallel
// pub fn process_all(&self, data: &DataFrame) -> Vec<(DataFrame, ProcessingResult)> {
//     info!(
//         n_branches = self.configs.len(),
//         "Starting parallel batch processing"
//     );

//     self.configs
//         .par_iter()
//         .map(|config| {
//             let pipeline = BranchPipeline::new(*config);
//             match pipeline.process(data) {
//                 Ok(result) => result,
//                 Err(e) => {
//                     warn!(error = ?e, config = ?config, "Branch processing failed");
//                     let mut result_data = data.clone();
//                     if let Ok(col) = data.column("rt") {
//                         result_data = result_data.filter(&col.is_null()).unwrap_or_default();
//                     }
//                     (
//                         result_data,
//                         ProcessingResult {
//                             branch_id: pipeline.data_id(),
//                             data_id: pipeline.data_id(),
//                             sample_size: config.sample_size,
//                             subsample_id: config.subsample_id,
//                             transformation: config.transformation.to_string(),
//                             outlier_method: config.outlier_method.as_string(),
//                             effect_condition: config.effect_condition.to_string(),
//                             strip_method: config.strip_method.to_string(),
//                             n_rows_input: data.height(),
//                             n_rows_output: 0,
//                             n_rows_removed: data.height(),
//                             processing_time_ms: 0,
//                             success: false,
//                             error_message: Some(e.to_string()),
//                         },
//                     )
//                 }
//             }
//         })
//         .collect()
// }
// }

use std::str::FromStr;

/// Effect condition: whether to include, null, or modify effects
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum EffectCondition {
    /// Include all effects as present in data (baseline)
    Present,
    /// Nullify cong*prev_cong interaction only (for FDR investigation)
    NullInteraction,
}

impl FromStr for EffectCondition {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self> {
        match s {
            "present" => Ok(EffectCondition::Present),
            "null_interaction" => Ok(EffectCondition::NullInteraction),
            _ => Err(anyhow::anyhow!("Unknown effect condition: {}", s)),
        }
    }
}

impl std::fmt::Display for EffectCondition {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            EffectCondition::Present => write!(f, "present"),
            EffectCondition::NullInteraction => write!(f, "null_interaction"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum StripMethod {
    /// Shuffle condition-wise
    Shuffle,
    /// Stationary participant-level additive quantile mapping
    AdditiveQmap,
    /// Trial-bin local additive quantile mapping with stationary fallback
    AdditiveQmapTrialBin,
    /// Trial-bin local mean-residual stripping with participant fallback
    LocalMeanResidual,
    /// Trial-bin local median-residual stripping with participant fallback
    LocalMedianResidual,
}

impl StripMethod {
    pub fn as_string(&self) -> String {
        match self {
            StripMethod::Shuffle => "shuffle".to_string(),
            StripMethod::AdditiveQmap => "additive_qmap".to_string(),
            StripMethod::AdditiveQmapTrialBin => "additive_qmap_trial_bin".to_string(),
            StripMethod::LocalMeanResidual => "local_mean_residual".to_string(),
            StripMethod::LocalMedianResidual => "local_median_residual".to_string(),
        }
    }
}

impl FromStr for StripMethod {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self> {
        match s {
            "additive_qmap" | "qmap_5" => Ok(StripMethod::AdditiveQmap),
            "additive_qmap_trial_bin" | "qmap_5_trial_bin" => Ok(StripMethod::AdditiveQmapTrialBin),
            "local_mean_residual" => Ok(StripMethod::LocalMeanResidual),
            "local_median_residual" => Ok(StripMethod::LocalMedianResidual),
            "shuffle" => Ok(StripMethod::Shuffle),
            _ => Err(anyhow::anyhow!("Unknown strip method: {}", s)),
        }
    }
}

impl std::fmt::Display for StripMethod {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            StripMethod::Shuffle => write!(f, "shuffle"),
            StripMethod::AdditiveQmap => write!(f, "additive_qmap"),
            StripMethod::AdditiveQmapTrialBin => write!(f, "additive_qmap_trial_bin"),
            StripMethod::LocalMeanResidual => write!(f, "local_mean_residual"),
            StripMethod::LocalMedianResidual => write!(f, "local_median_residual"),
        }
    }
}
/// # Strategy
///
/// // Remove ONLY the congruency × previous_congruency interaction per participant.
/// This is the theory-correct (stationary) CSE / Gratton nullification.
///
/// Required columns:
/// - "participant"  (u32/u64/i32/i64)
/// - "cong"         (-1 or 1)
/// - "prev_cong"    (-1 or 1)
/// - "rt"           (f64)
fn stable_group_seed(seed: u64, pid: &str, cong: &str) -> u64 {
    // FNV-1a style mixing gives stable cross-platform group seeds.
    let mut hash = 0xcbf29ce484222325_u64 ^ seed;
    for byte in pid.bytes().chain([0xff]).chain(cong.bytes()) {
        hash ^= byte as u64;
        hash = hash.wrapping_mul(0x100000001b3);
    }
    hash
}

pub fn shuffle_null(df: DataFrame, scale_col: &str, seed: u64) -> Result<DataFrame> {
    info!("Applying shuffle null on column: {}", scale_col);

    let shuffled = df.group_by(["participant_id", "cong"])?.apply(|mut g| {
        let pid = g.column("participant_id")?.str()?.get(0).ok_or_else(|| {
            PolarsError::ComputeError("empty participant_id group during shuffle".into())
        })?;
        let cong =
            g.column("cong")?.str()?.get(0).ok_or_else(|| {
                PolarsError::ComputeError("empty cong group during shuffle".into())
            })?;
        let mut rng = StdRng::seed_from_u64(stable_group_seed(seed, pid, cong));

        let col = g.column(scale_col)?.clone();
        let mut vals: Vec<f64> = col.f64()?.into_no_null_iter().collect();
        vals.shuffle(&mut rng);
        g.replace("rt", Series::from_vec("rt".into(), vals))?;
        Ok(g)
    })?;

    info!("Shuffle complete: {} rows", shuffled.height());
    Ok(shuffled)
}

pub struct QuantileMapParams {
    pub scale_col: String, // e.g., "rt"
    pub kappa: f64,
    pub ngrid: usize,
}

pub fn quantile_map_once(df: DataFrame, params: QuantileMapParams) -> Result<DataFrame> {
    let scale = params.scale_col.as_str();
    let ngrid = params.ngrid.max(2);
    let kappa = params.kappa;

    // Validate required columns
    for c in [scale, "cong", "prev_cong", "participant_id"] {
        if !df.get_column_names().iter().any(|n| n == &c) {
            return Err(anyhow!("missing required column: {}", c));
        }
    }

    // Coerce types we need:
    // - scale_col to Float64
    // - cong, prev_cong, participant_id to Utf8 for easy grouping
    let mut df = df.clone();
    if df.column(scale)?.dtype() != &DataType::Float64 {
        let casted = df.column(scale)?.cast(&DataType::Float64)?;
        df.with_column(casted)?;
    }
    for name in ["cong", "prev_cong", "participant_id"] {
        if df.column(name)?.dtype() != &DataType::String {
            let casted = df.column(name)?.cast(&DataType::String)?;
            df.with_column(casted)?;
        }
    }

    // Create taus grid in [0,1]
    let taus: Vec<f64> = (0..ngrid)
        .map(|i| i as f64 / (ngrid as f64 - 1.0))
        .collect();

    // Precompute GLOBAL per-cell quantiles for shrinkage: group_by(cong, prev_cong) over whole df
    // For each (cong, prev_cong) => quantile curve q_global[taus]
    let global_cell_q = compute_cell_quantiles(&df, scale, &taus)?;

    // Partition df by participants
    let participants = df
        .column("participant_id")?
        .str()?
        .into_no_null_iter()
        .collect::<Vec<_>>();
    // unique participants
    let mut pats_unique = participants.clone();
    pats_unique.sort_unstable();
    pats_unique.dedup();

    // Build index of rows per participant to slice quickly
    let pid_rows: HashMap<String, Vec<usize>> = {
        let mut map: HashMap<String, Vec<usize>> = HashMap::new();
        let pid_col = df.column("participant_id")?.str()?;
        for (idx, val) in pid_col.into_no_null_iter().enumerate() {
            map.entry(val.to_string()).or_default().push(idx);
        }
        map
    };

    // For parallel processing, collect needed columns to owned vectors
    let rt_vec: Vec<f64> = df.column(scale)?.f64()?.into_no_null_iter().collect();
    let cong_vec: Vec<String> = df
        .column("cong")?
        .str()?
        .into_no_null_iter()
        .map(|s| s.to_string())
        .collect();
    let prev_vec: Vec<String> = df
        .column("prev_cong")?
        .str()?
        .into_no_null_iter()
        .map(|s| s.to_string())
        .collect();
    // Process each participant in parallel, producing replacement RTs by original row index.
    let per_pid_outputs: Vec<Vec<PerRowOut>> = pats_unique
        .par_iter()
        .map(|pid| {
            let row_idxs = pid_rows.get(*pid).cloned().unwrap_or_default();
            if row_idxs.len() < 2 {
                // Return minimal outputs with tau from global inverse and other fields from single-point quantiles
                return per_pid_minimal(
                    &row_idxs,
                    &rt_vec,
                    &cong_vec,
                    &prev_vec,
                    pid,
                    &taus,
                    &global_cell_q,
                );
            }

            // Build sub-data for this participant
            let mut sub_rows: Vec<RowData> = Vec::with_capacity(row_idxs.len());
            for &i in &row_idxs {
                sub_rows.push(RowData {
                    idx: i,
                    rt: rt_vec[i],
                    cong: cong_vec[i].clone(),
                    prev: prev_vec[i].clone(),
                });
            }

            // Compute per-cell ECDFs and n_cell, then join global quantiles
            let cell_groups = group_by_cell(&sub_rows);
            // local ECDF for cells with n>=1; we need special fallback when n<2
            let mut cell_local: HashMap<(String, String), CellLocal> = HashMap::new();
            for (key, rows) in &cell_groups {
                let xs: Vec<f64> = rows.iter().map(|r| r.rt).collect();
                let ecdf = SimpleEcdf::new(&xs);
                // Sort for quantile curve
                let q_curve_local = quantiles_type8(&xs, &taus);
                // Global curve for this cell
                let q_global = global_cell_q
                    .get(key)
                    .cloned()
                    .unwrap_or_else(|| taus.iter().map(|_| f64::NAN).collect());
                // Apply kappa shrinkage for quantile curves
                let q_shrunk: Vec<f64> = q_curve_local
                    .iter()
                    .zip(q_global.iter())
                    .map(|(l, g)| shrink(*l, *g, kappa))
                    .collect();
                cell_local.insert(
                    key.clone(),
                    CellLocal {
                        n: rows.len(),
                        ecdf,
                        q_shrunk,
                    },
                );
            }

            let mut tau_per_row: Vec<f64> = Vec::with_capacity(sub_rows.len());
            for r in &sub_rows {
                if let Some(cell) = cell_local.get(&(r.cong.clone(), r.prev.clone())) {
                    if cell.n < 2 {
                        // inverse quantile via interpolation over q_shrunk (global-influenced)
                        let tau = invert_monotone_interp(&taus, &cell.q_shrunk, r.rt);
                        tau_per_row.push(tau);
                    } else {
                        let tau = cell.ecdf.eval(r.rt);
                        tau_per_row.push(tau);
                    }
                } else {
                    // no local cell (shouldn't happen) => global-only inverse
                    let q_global = global_cell_q
                        .get(&(r.cong.clone(), r.prev.clone()))
                        .cloned()
                        .unwrap_or_else(|| taus.iter().map(|_| f64::NAN).collect());
                    let tau = invert_monotone_interp(&taus, &q_global, r.rt);
                    tau_per_row.push(tau);
                }
            }

            let grand_curve = {
                let xs: Vec<f64> = sub_rows.iter().map(|r| r.rt).collect();
                quantiles_type8(&xs, &taus)
            };

            let mut row_curves: HashMap<String, Vec<f64>> = HashMap::new();
            let mut col_curves: HashMap<String, Vec<f64>> = HashMap::new();

            {
                // row (by cong)
                let row_groups = group_by_key(&sub_rows, |r| r.cong.clone());
                for (cong, rows) in row_groups {
                    let xs: Vec<f64> = rows.iter().map(|r| r.rt).collect();
                    let q_local = quantiles_type8(&xs, &taus);
                    row_curves.insert(cong, q_local);
                }
            }
            {
                // column (by prev_cong)
                let col_groups = group_by_key(&sub_rows, |r| r.prev.clone());
                for (prev, rows) in col_groups {
                    let xs: Vec<f64> = rows.iter().map(|r| r.rt).collect();
                    let q_local = quantiles_type8(&xs, &taus);
                    col_curves.insert(prev, q_local);
                }
            }

            // Interpolate the additive row + column - grand quantile structure per row.
            let mut out: Vec<PerRowOut> = Vec::with_capacity(sub_rows.len());
            for (i, r) in sub_rows.iter().enumerate() {
                let tau = tau_per_row[i];
                let q_row_tau = interp(&taus, row_curves.get(&r.cong).unwrap(), tau);
                let q_col_tau = interp(&taus, col_curves.get(&r.prev).unwrap(), tau);
                let q_grand_tau = interp(&taus, &grand_curve, tau);
                let lrt_adj = q_row_tau + q_col_tau - q_grand_tau;

                out.push(PerRowOut {
                    idx: r.idx,
                    lrt_adj,
                });
            }

            out
        })
        .collect();

    // Flatten and reorder by original row index
    let mut flat: Vec<PerRowOut> = Vec::new();
    for v in per_pid_outputs {
        flat.extend(v);
    }
    flat.sort_by_key(|r| r.idx);

    let new_rt: Vec<f64> = flat.iter().map(|r| r.lrt_adj).collect();

    // Use take or direct replace to ensure alignment with original row count
    assert_eq!(new_rt.len(), df.height(), "output length mismatch");

    // Create a new Series for rt and replace the column in-place
    let mut out_df = df.clone();
    let new_rt_series = Series::new("rt".into(), new_rt);
    out_df.replace("rt", new_rt_series)?;

    // Return the DataFrame with identical columns/order as input, only rt replaced
    Ok(out_df)
}

fn parse_pm1(value: Option<&str>) -> Option<f64> {
    match value? {
        "1" | "1.0" => Some(1.0),
        "-1" | "-1.0" => Some(-1.0),
        _ => None,
    }
}

fn local_interaction_beta(rows: &[usize], rt: &[f64], cong: &[Option<f64>], prev: &[Option<f64>], min_cell_n: usize) -> Option<f64> {
    let mut sums: HashMap<(i8, i8), (f64, usize)> = HashMap::new();
    for &idx in rows {
        let (Some(c), Some(p)) = (cong[idx], prev[idx]) else { continue };
        let y = rt[idx];
        if !y.is_finite() { continue }
        let key = (c as i8, p as i8);
        let entry = sums.entry(key).or_insert((0.0, 0));
        entry.0 += y;
        entry.1 += 1;
    }
    for c in [-1_i8, 1_i8] {
        for p in [-1_i8, 1_i8] {
            if sums.get(&(c, p)).map(|(_, n)| *n).unwrap_or(0) < min_cell_n {
                return None;
            }
        }
    }
    let mean = |c: i8, p: i8| -> f64 {
        let (sum, n) = sums.get(&(c, p)).copied().unwrap();
        sum / n as f64
    };
    Some((mean(1, 1) - mean(1, -1) - mean(-1, 1) + mean(-1, -1)) / 4.0)
}

fn local_median_interaction_beta(rows: &[usize], rt: &[f64], cong: &[Option<f64>], prev: &[Option<f64>], min_cell_n: usize) -> Option<f64> {
    let mut cells: HashMap<(i8, i8), Vec<f64>> = HashMap::new();
    for &idx in rows {
        let (Some(c), Some(p)) = (cong[idx], prev[idx]) else { continue };
        let y = rt[idx];
        if y.is_finite() {
            cells.entry((c as i8, p as i8)).or_default().push(y);
        }
    }
    for c in [-1_i8, 1_i8] {
        for p in [-1_i8, 1_i8] {
            if cells.get(&(c, p)).map(|v| v.len()).unwrap_or(0) < min_cell_n {
                return None;
            }
        }
    }
    let median = |c: i8, p: i8| -> f64 {
        let mut xs = cells.get(&(c, p)).cloned().unwrap();
        xs.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
        let n = xs.len();
        if n % 2 == 0 { (xs[n / 2 - 1] + xs[n / 2]) / 2.0 } else { xs[n / 2] }
    };
    Some((median(1, 1) - median(1, -1) - median(-1, 1) + median(-1, -1)) / 4.0)
}

pub fn local_mean_residual_strip(df: DataFrame, min_cell_n: usize, n_bins: usize) -> Result<DataFrame> {
    for c in ["rt", "cong", "prev_cong", "participant_id"] {
        if !df.get_column_names().iter().any(|n| *n == c) {
            return Err(anyhow!("missing required column: {}", c));
        }
    }
    let rt_cast = df.column("rt")?.cast(&DataType::Float64)?;
    let rt: Vec<f64> = rt_cast
        .f64()?
        .into_iter()
        .map(|v| v.unwrap_or(f64::NAN))
        .collect();
    let cong_cast = df.column("cong")?.cast(&DataType::String)?;
    let prev_cast = df.column("prev_cong")?.cast(&DataType::String)?;
    let cong: Vec<Option<f64>> = cong_cast.str()?.into_iter().map(parse_pm1).collect();
    let prev: Vec<Option<f64>> = prev_cast.str()?.into_iter().map(parse_pm1).collect();
    let pid_col = df.column("participant_id")?.str()?;

    let mut by_pid: HashMap<String, Vec<usize>> = HashMap::new();
    for i in 0..df.height() {
        if let Some(pid) = pid_col.get(i) {
            by_pid.entry(pid.to_string()).or_default().push(i);
        }
    }
    let participant_beta: HashMap<String, Option<f64>> = by_pid
        .iter()
        .map(|(pid, rows)| (pid.clone(), local_interaction_beta(rows, &rt, &cong, &prev, min_cell_n)))
        .collect();

    let mut groups: HashMap<(String, usize), Vec<usize>> = HashMap::new();
    if df.get_column_names().iter().any(|n| *n == "trial_index") && n_bins >= 2 {
        let trial_cast = df.column("trial_index")?.cast(&DataType::Float64)?;
        let trial_col = trial_cast.f64()?;
        for (pid, mut rows) in by_pid.clone() {
            rows.sort_by(|a, b| {
                let ta = trial_col.get(*a).unwrap_or(*a as f64);
                let tb = trial_col.get(*b).unwrap_or(*b as f64);
                ta.partial_cmp(&tb).unwrap_or(std::cmp::Ordering::Equal)
            });
            let n = rows.len().max(1);
            for (pos, idx) in rows.into_iter().enumerate() {
                let bin = ((pos * n_bins) / n).min(n_bins - 1);
                groups.entry((pid.clone(), bin)).or_default().push(idx);
            }
        }
    } else {
        for (pid, rows) in by_pid.clone() {
            groups.insert((pid, 0), rows);
        }
    }

    let mut new_rt = rt.clone();
    for ((pid, _bin), rows) in groups {
        let beta = local_interaction_beta(&rows, &rt, &cong, &prev, min_cell_n)
            .or_else(|| participant_beta.get(&pid).copied().flatten());
        let Some(beta) = beta else { continue };
        for idx in rows {
            if let (Some(c), Some(p)) = (cong[idx], prev[idx]) {
                if new_rt[idx].is_finite() {
                    new_rt[idx] -= beta * c * p;
                }
            }
        }
    }

    let mut out_df = df.clone();
    out_df.replace("rt", Series::new("rt".into(), new_rt))?;
    Ok(out_df)
}

pub fn local_median_residual_strip(df: DataFrame, min_cell_n: usize, n_bins: usize) -> Result<DataFrame> {
    for c in ["rt", "cong", "prev_cong", "participant_id"] {
        if !df.get_column_names().iter().any(|n| *n == c) {
            return Err(anyhow!("missing required column: {}", c));
        }
    }
    let rt_cast = df.column("rt")?.cast(&DataType::Float64)?;
    let rt: Vec<f64> = rt_cast
        .f64()?
        .into_iter()
        .map(|v| v.unwrap_or(f64::NAN))
        .collect();
    let cong_cast = df.column("cong")?.cast(&DataType::String)?;
    let prev_cast = df.column("prev_cong")?.cast(&DataType::String)?;
    let cong: Vec<Option<f64>> = cong_cast.str()?.into_iter().map(parse_pm1).collect();
    let prev: Vec<Option<f64>> = prev_cast.str()?.into_iter().map(parse_pm1).collect();
    let pid_col = df.column("participant_id")?.str()?;

    let mut by_pid: HashMap<String, Vec<usize>> = HashMap::new();
    for i in 0..df.height() {
        if let Some(pid) = pid_col.get(i) {
            by_pid.entry(pid.to_string()).or_default().push(i);
        }
    }
    let participant_beta: HashMap<String, Option<f64>> = by_pid
        .iter()
        .map(|(pid, rows)| (pid.clone(), local_median_interaction_beta(rows, &rt, &cong, &prev, min_cell_n)))
        .collect();

    let mut groups: HashMap<(String, usize), Vec<usize>> = HashMap::new();
    if df.get_column_names().iter().any(|n| *n == "trial_index") && n_bins >= 2 {
        let trial_cast = df.column("trial_index")?.cast(&DataType::Float64)?;
        let trial_col = trial_cast.f64()?;
        for (pid, mut rows) in by_pid.clone() {
            rows.sort_by(|a, b| {
                let ta = trial_col.get(*a).unwrap_or(*a as f64);
                let tb = trial_col.get(*b).unwrap_or(*b as f64);
                ta.partial_cmp(&tb).unwrap_or(std::cmp::Ordering::Equal)
            });
            let n = rows.len().max(1);
            for (pos, idx) in rows.into_iter().enumerate() {
                let bin = ((pos * n_bins) / n).min(n_bins - 1);
                groups.entry((pid.clone(), bin)).or_default().push(idx);
            }
        }
    } else {
        for (pid, rows) in by_pid.clone() {
            groups.insert((pid, 0), rows);
        }
    }

    let mut new_rt = rt.clone();
    for ((pid, _bin), rows) in groups {
        let beta = local_median_interaction_beta(&rows, &rt, &cong, &prev, min_cell_n)
            .or_else(|| participant_beta.get(&pid).copied().flatten());
        let Some(beta) = beta else { continue };
        for idx in rows {
            if let (Some(c), Some(p)) = (cong[idx], prev[idx]) {
                if new_rt[idx].is_finite() {
                    new_rt[idx] -= beta * c * p;
                }
            }
        }
    }

    let mut out_df = df.clone();
    out_df.replace("rt", Series::new("rt".into(), new_rt))?;
    Ok(out_df)
}

pub fn quantile_map_trial_bin(
    df: DataFrame,
    params: QuantileMapParams,
    min_cell_n: usize,
    n_bins: usize,
) -> Result<DataFrame> {
    let stationary = quantile_map_once(df.clone(), QuantileMapParams {
        scale_col: params.scale_col.clone(),
        kappa: params.kappa,
        ngrid: params.ngrid,
    })?;
    if !df.get_column_names().iter().any(|n| *n == "trial_index") || n_bins < 2 {
        return Ok(stationary);
    }

    let mut groups: HashMap<(String, usize), Vec<usize>> = HashMap::new();
    let pid_col = df.column("participant_id")?.str()?;
    let trial_cast = df.column("trial_index")?.cast(&DataType::Float64)?;
    let trial_col = trial_cast.f64()?;

    let mut by_pid: HashMap<String, Vec<(usize, f64)>> = HashMap::new();
    for i in 0..df.height() {
        let Some(pid) = pid_col.get(i) else { continue };
        let Some(trial) = trial_col.get(i) else { continue };
        if trial.is_finite() {
            by_pid.entry(pid.to_string()).or_default().push((i, trial));
        }
    }
    for (pid, mut rows) in by_pid {
        rows.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap_or(std::cmp::Ordering::Equal));
        let n = rows.len().max(1);
        for (pos, (idx, _trial)) in rows.into_iter().enumerate() {
            let bin = ((pos * n_bins) / n).min(n_bins - 1);
            groups.entry((pid.clone(), bin)).or_default().push(idx);
        }
    }

    let mut new_rt: Vec<f64> = stationary
        .column("rt")?
        .f64()?
        .into_no_null_iter()
        .collect();
    let cong_col = df.column("cong")?.cast(&DataType::String)?;
    let cong_col = cong_col.str()?;
    let prev_col = df.column("prev_cong")?.cast(&DataType::String)?;
    let prev_col = prev_col.str()?;

    for rows in groups.values() {
        let mut counts: HashMap<(String, String), usize> = HashMap::new();
        for &idx in rows {
            if let (Some(c), Some(p)) = (cong_col.get(idx), prev_col.get(idx)) {
                *counts.entry((c.to_string(), p.to_string())).or_default() += 1;
            }
        }
        let adequate = ["-1", "1"].iter().all(|c| {
            ["-1", "1"].iter().all(|p| {
                counts
                    .get(&(c.to_string(), p.to_string()))
                    .copied()
                    .unwrap_or(0)
                    >= min_cell_n
            })
        });
        if !adequate {
            continue;
        }

        let idx_set: HashSet<usize> = rows.iter().copied().collect();
        let mask: BooleanChunked = (0..df.height()).map(|i| idx_set.contains(&i)).collect();
        let local_df = df.filter(&mask)?;
        let local_mapped = quantile_map_once(local_df, QuantileMapParams {
            scale_col: params.scale_col.clone(),
            kappa: params.kappa,
            ngrid: params.ngrid,
        })?;
        let local_rt: Vec<f64> = local_mapped
            .column("rt")?
            .f64()?
            .into_no_null_iter()
            .collect();
        for (&idx, rt) in rows.iter().zip(local_rt.into_iter()) {
            new_rt[idx] = rt;
        }
    }

    let mut out_df = df.clone();
    out_df.replace("rt", Series::new("rt".into(), new_rt))?;
    Ok(out_df)
}

// Utilities and helpers

#[derive(Clone)]
struct RowData {
    idx: usize,
    rt: f64,
    cong: String,
    prev: String,
}

struct CellLocal {
    n: usize,
    ecdf: SimpleEcdf,
    q_shrunk: Vec<f64>,
}

#[derive(Clone)]
struct PerRowOut {
    idx: usize,
    lrt_adj: f64,
}

fn shrink(local: f64, global: f64, kappa: f64) -> f64 {
    if !local.is_finite() && !global.is_finite() {
        return f64::NAN;
    }
    if !global.is_finite() {
        return local;
    }
    if !local.is_finite() {
        return global;
    }
    (local + kappa * global) / (1.0 + kappa)
}

// Compute per-cell global quantile curves over entire df: group_by (cong, prev_cong)
fn compute_cell_quantiles(
    df: &DataFrame,
    scale: &str,
    taus: &[f64],
) -> Result<HashMap<(String, String), Vec<f64>>> {
    // Build a map (cong, prev) -> Vec<f64> quantiles
    let cong = df.column("cong")?.str()?;
    let prev = df.column("prev_cong")?.str()?;
    let rt = df.column(scale)?.f64()?;

    // Collect rows by cell
    let mut by_cell: HashMap<(String, String), Vec<f64>> = HashMap::new();
    for ((c, p), r) in cong
        .into_no_null_iter()
        .zip(prev.into_no_null_iter())
        .zip(rt.into_no_null_iter())
    {
        if r.is_finite() {
            by_cell
                .entry((c.to_string(), p.to_string()))
                .or_default()
                .push(r);
        }
    }

    let mut out: HashMap<(String, String), Vec<f64>> = HashMap::new();
    for (key, xs) in by_cell {
        let q_curve = quantiles_type8(&xs, taus);
        out.insert(key, q_curve);
    }
    Ok(out)
}

// Compute quantile curve for a sample xs at given taus (0..1), approximating R type=8.
// xs can be unsorted; we sort internally.

pub fn quantiles_type8(xs: &[f64], ps: &[f64]) -> Vec<f64> {
    // filter NaNs
    let mut s: Vec<f64> = xs.iter().copied().filter(|v| v.is_finite()).collect();
    if s.is_empty() {
        return ps.iter().map(|_| f64::NAN).collect();
    }
    s.sort_by(|a, b| a.partial_cmp(b).unwrap());

    let n = s.len() as f64;
    ps.iter()
        .map(|&p| {
            let p = p.clamp(0.0, 1.0);
            let h = (n + 1.0 / 3.0) * p + 1.0 / 3.0;
            if h <= 1.0 {
                return s[0];
            }
            if h >= n {
                return s[s.len() - 1];
            }
            let k = h.floor(); // 1-based
            let gamma = h - k;
            let k0 = (k as usize) - 1; // to 0-based lower index
            let k1 = k0 + 1;
            s[k0] + gamma * (s[k1] - s[k0])
        })
        .collect()
}

// Interpolate y(x) at xout using linear interpolation, with rule=2 boundary handling:
// if xout < min(x) -> y(min), if xout > max(x) -> y(max).
pub fn interp(x: &[f64], y: &[f64], xout: f64) -> f64 {
    if x.is_empty() || y.is_empty() || x.len() != y.len() {
        return f64::NAN;
    }
    if xout <= x[0] {
        return y[0];
    }
    if xout >= x[x.len() - 1] {
        return y[y.len() - 1];
    }
    // find interval
    let mut i = 1usize;
    while i < x.len() && x[i] < xout {
        i += 1;
    }
    let i0 = i - 1;
    let x0 = x[i0];
    let x1 = x[i];
    let y0 = y[i0];
    let y1 = y[i];
    if (x1 - x0).abs() < f64::EPSILON {
        return y0;
    }
    y0 + (y1 - y0) * (xout - x0) / (x1 - x0)
}

// Invert monotone y(x) to get x such that y ~ yout, via linear search and interpolation.
// If y is not strictly monotone, we still do piecewise linear find.
// Boundary rule=2: return min(x) if yout < min(y), max(x) if yout > max(y).
pub fn invert_monotone_interp(x: &[f64], y: &[f64], yout: f64) -> f64 {
    if x.is_empty() || y.is_empty() || x.len() != y.len() {
        return f64::NAN;
    }
    // Assume y is non-decreasing
    let ymin = y[0];
    let ymax = y[y.len() - 1];
    if yout <= ymin {
        return x[0];
    }
    if yout >= ymax {
        return x[x.len() - 1];
    }
    // find interval
    let mut i = 1usize;
    while i < y.len() && y[i] < yout {
        i += 1;
    }
    let i0 = i - 1;
    let y0 = y[i0];
    let y1 = y[i];
    let x0 = x[i0];
    let x1 = x[i];
    if (y1 - y0).abs() < f64::EPSILON {
        return x0;
    }
    x0 + (x1 - x0) * (yout - y0) / (y1 - y0)
}

// Simple ECDF over f64 using upper_bound via partition_point
#[derive(Clone)]
struct SimpleEcdf {
    xs: Vec<f64>, // sorted, finite
}

impl SimpleEcdf {
    fn new(xs_in: &[f64]) -> Self {
        let mut xs: Vec<f64> = xs_in.iter().copied().filter(|v| v.is_finite()).collect();
        xs.sort_by(|a, b| a.partial_cmp(b).unwrap());
        Self { xs }
    }

    fn eval(&self, x: f64) -> f64 {
        if self.xs.is_empty() {
            return f64::NAN;
        }
        // count of elements <= x
        let idx = self.xs.partition_point(|v| *v <= x);
        idx as f64 / self.xs.len() as f64
    }
}

// Group rows by (cong, prev)
fn group_by_cell(rows: &[RowData]) -> HashMap<(String, String), Vec<&RowData>> {
    let mut map: HashMap<(String, String), Vec<&RowData>> = HashMap::new();
    for r in rows {
        map.entry((r.cong.clone(), r.prev.clone()))
            .or_default()
            .push(r);
    }
    map
}

// Generic grouping by key
fn group_by_key<F: Fn(&RowData) -> String>(
    rows: &[RowData],
    key_fn: F,
) -> HashMap<String, Vec<&RowData>> {
    let mut map: HashMap<String, Vec<&RowData>> = HashMap::new();
    for r in rows {
        map.entry(key_fn(r)).or_default().push(r);
    }
    map
}

// Minimal per-pid outputs for participant with <2 rows.
// tau via global per-cell inverse; row/col/grand quantiles degenerate to single curve.
fn per_pid_minimal(
    row_idxs: &Vec<usize>,
    rt_vec: &[f64],
    cong_vec: &[String],
    prev_vec: &[String],
    _pid: &str,
    taus: &[f64],
    global_cell_q: &HashMap<(String, String), Vec<f64>>,
) -> Vec<PerRowOut> {
    // grand curve from the available rt(s)
    let xs: Vec<f64> = row_idxs.iter().map(|&i| rt_vec[i]).collect();
    let grand_curve = quantiles_type8(&xs, taus);

    let mut out = Vec::with_capacity(row_idxs.len());
    for &i in row_idxs {
        let cong = &cong_vec[i];
        let prev = &prev_vec[i];
        let rt = rt_vec[i];
        let q_global = global_cell_q
            .get(&(cong.clone(), prev.clone()))
            .cloned()
            .unwrap_or_else(|| taus.iter().map(|_| f64::NAN).collect());
        let tau = invert_monotone_interp(taus, &q_global, rt);
        let lrt_adj = interp(taus, &grand_curve, tau);
        out.push(PerRowOut { idx: i, lrt_adj });
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_dummy_df() -> DataFrame {
        // Build a small deterministic dataset with two participants and four trials each
        let pid = Series::new(
            "participant_id".into(),
            &["p1", "p1", "p1", "p1", "p2", "p2", "p2", "p2"],
        );
        let cong = Series::new("cong".into(), &["-1", "-1", "1", "1", "-1", "-1", "1", "1"]);
        let prev = Series::new(
            "prev_cong".into(),
            &["-1", "1", "-1", "1", "-1", "1", "-1", "1"],
        );
        // RTs chosen to be distinct per cell, not critical
        let rt = Series::new(
            "rt".into(),
            &[500.0, 520.0, 480.0, 510.0, 530.0, 545.0, 490.0, 515.0],
        );
        DataFrame::new(vec![pid.into(), cong.into(), prev.into(), rt.into()]).unwrap()
    }

    #[test]
    fn test_strip_label_present_is_none() {
        let df = make_dummy_df();

        let cfg_present = BranchConfig {
            sample_size: 1.0,
            subsample_id: 1,
            transformation: Transformation::NoLogRt,
            outlier_method: OutlierMethod::None,
            effect_condition: EffectCondition::Present,
            strip_method: StripMethod::Shuffle, // ignored
            global_seed: 42,
        };

        let pipeline_present = BranchPipeline::new(cfg_present);
        let (_df_out_p, res_p) = pipeline_present
            .process(df.clone())
            .expect("present branch ok");
        assert_eq!(res_p.effect_condition, "present");
        assert_eq!(res_p.strip_method, "none");
        assert!(res_p.branch_id.ends_with("__none"));
    }

    #[test]
    fn test_strip_label_null_interaction_reflects_method() {
        let df = make_dummy_df();

        let cfg_shuffle = BranchConfig {
            sample_size: 1.0,
            subsample_id: 1,
            transformation: Transformation::NoLogRt,
            outlier_method: OutlierMethod::None,
            effect_condition: EffectCondition::NullInteraction,
            strip_method: StripMethod::Shuffle,
            global_seed: 42,
        };
        let cfg_qmap = BranchConfig {
            sample_size: 1.0,
            subsample_id: 1,
            transformation: Transformation::NoLogRt,
            outlier_method: OutlierMethod::None,
            effect_condition: EffectCondition::NullInteraction,
            strip_method: StripMethod::AdditiveQmap,
            global_seed: 42,
        };

        let pipeline_shuffle = BranchPipeline::new(cfg_shuffle);
        let (_df_s, res_s) = pipeline_shuffle
            .process(df.clone())
            .expect("shuffle branch ok");
        assert_eq!(res_s.effect_condition, "null_interaction");
        assert_eq!(res_s.strip_method, "shuffle");
        assert!(res_s.branch_id.ends_with("__shuffle"));

        let pipeline_qmap = BranchPipeline::new(cfg_qmap);
        let (_df_q, res_q) = pipeline_qmap.process(df).expect("qmap branch ok");
        assert_eq!(res_q.effect_condition, "null_interaction");
        assert_eq!(res_q.strip_method, "additive_qmap");
        assert!(res_q.branch_id.ends_with("__additive_qmap"));
    }

    fn group_rt_multiset(df: &DataFrame) -> HashMap<(String, String), Vec<f64>> {
        let pid = df.column("participant_id").unwrap().str().unwrap();
        let cong = df.column("cong").unwrap().str().unwrap();
        let rt = df.column("rt").unwrap().f64().unwrap();

        let mut map: HashMap<(String, String), Vec<f64>> = HashMap::new();
        for ((p, c), r) in pid
            .into_no_null_iter()
            .zip(cong.into_no_null_iter())
            .zip(rt.into_no_null_iter())
        {
            map.entry((p.to_string(), c.to_string()))
                .or_default()
                .push(r);
        }
        for v in map.values_mut() {
            v.sort_by(|a, b| a.partial_cmp(b).unwrap());
        }
        map
    }

    #[test]
    fn test_effect_application_semantics() {
        let df = make_dummy_df();

        // Present should keep original RT distribution aside from downstream steps (none here)
        let cfg_present = BranchConfig {
            sample_size: 1.0,
            subsample_id: 1,
            transformation: Transformation::NoLogRt,
            outlier_method: OutlierMethod::None,
            effect_condition: EffectCondition::Present,
            strip_method: StripMethod::Shuffle,
            global_seed: 42,
        };
        let (df_present, _res_p) = BranchPipeline::new(cfg_present)
            .process(df.clone())
            .unwrap();
        // RTs should match original exactly
        assert_eq!(
            df.column("rt")
                .unwrap()
                .f64()
                .unwrap()
                .into_no_null_iter()
                .collect::<Vec<_>>(),
            df_present
                .column("rt")
                .unwrap()
                .f64()
                .unwrap()
                .into_no_null_iter()
                .collect::<Vec<_>>()
        );

        // NullInteraction + shuffle should permute within (participant_id, cong) groups
        let cfg_shuffle = BranchConfig {
            sample_size: 1.0,
            subsample_id: 1,
            transformation: Transformation::NoLogRt,
            outlier_method: OutlierMethod::None,
            effect_condition: EffectCondition::NullInteraction,
            strip_method: StripMethod::Shuffle,
            global_seed: 42,
        };
        let (df_shuffle, _res_s) = BranchPipeline::new(cfg_shuffle)
            .process(df.clone())
            .unwrap();

        // Compare per-group multisets of RTs, ignoring order and group ordering
        let orig_map = group_rt_multiset(&df);
        let shuf_map = group_rt_multiset(&df_shuffle);

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
    fn test_branch_pipeline_identity() {
        // Test with no transformations/filtering
        let config = BranchConfig {
            sample_size: 1.0,
            subsample_id: 1,
            transformation: Transformation::NoLogRt,
            outlier_method: OutlierMethod::None,
            effect_condition: EffectCondition::Present,
            strip_method: StripMethod::Shuffle,
            global_seed: 42,
        };

        let _pipeline = BranchPipeline::new(config);
        // Actual test would require test data
    }
    #[test]
    fn test_effect_condition_parsing() {
        assert_eq!(
            "present".parse::<EffectCondition>().unwrap(),
            EffectCondition::Present
        );
        assert_eq!(
            "null_interaction".parse::<EffectCondition>().unwrap(),
            EffectCondition::NullInteraction
        );
        assert!("null_both".parse::<EffectCondition>().is_err());
    }

    #[test]
    fn test_effect_condition_display() {
        assert_eq!(EffectCondition::Present.to_string(), "present");
        assert_eq!(
            EffectCondition::NullInteraction.to_string(),
            "null_interaction"
        );
    }
}
