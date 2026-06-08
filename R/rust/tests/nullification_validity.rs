use multiverse_analysis::{
    BranchConfig, BranchPipeline, EffectCondition, OutlierMethod, QuantileMapParams, StripMethod,
    Transformation, local_mean_residual_strip, local_median_residual_strip, quantile_map_once,
    quantile_map_trial_bin, quantiles_type8, shuffle_null,
};
use polars::prelude::*;
use std::collections::HashMap;

fn synthetic_location_cse_df(n_participants: usize, reps_per_cell: usize, cse: f64) -> DataFrame {
    let mut trial_id = Vec::new();
    let mut participant_id = Vec::new();
    let mut cong = Vec::new();
    let mut prev_cong = Vec::new();
    let mut rt = Vec::new();

    let mut trial = 0_i64;
    for pid in 0..n_participants {
        let pid_offset = pid as f64 * 7.0;
        for &c in &[-1_i32, 1_i32] {
            for &p in &[-1_i32, 1_i32] {
                for rep in 0..reps_per_cell {
                    let centered = rep as f64 - (reps_per_cell as f64 - 1.0) / 2.0;
                    // Identical within-cell shape; only the location carries the CSE.
                    let noise = centered * 0.35;
                    trial_id.push(trial);
                    participant_id.push(format!("p{pid:02}"));
                    cong.push(c.to_string());
                    prev_cong.push(p.to_string());
                    rt.push(
                        600.0
                            + pid_offset
                            + 18.0 * c as f64
                            + 11.0 * p as f64
                            + cse * (c * p) as f64
                            + noise,
                    );
                    trial += 1;
                }
            }
        }
    }

    DataFrame::new(vec![
        Series::new("trial_id".into(), trial_id).into(),
        Series::new("participant_id".into(), participant_id).into(),
        Series::new("cong".into(), cong).into(),
        Series::new("prev_cong".into(), prev_cong).into(),
        Series::new("rt".into(), rt).into(),
    ])
    .unwrap()
}

fn small_cell_df() -> DataFrame {
    DataFrame::new(vec![
        Series::new("trial_id".into(), &[0_i64, 1, 2, 3, 4]).into(),
        Series::new("participant_id".into(), &["p1", "p1", "p1", "p2", "p2"]).into(),
        Series::new("cong".into(), &["-1", "-1", "1", "-1", "1"]).into(),
        Series::new("prev_cong".into(), &["-1", "1", "1", "-1", "1"]).into(),
        Series::new("rt".into(), &[500.0, 510.0, 530.0, 610.0, 620.0]).into(),
    ])
    .unwrap()
}

fn synthetic_shape_cse_df(
    n_participants: usize,
    reps_per_cell: usize,
    interaction_shape: impl Fn(f64, usize) -> f64,
) -> DataFrame {
    let mut trial_id = Vec::new();
    let mut participant_id = Vec::new();
    let mut cong = Vec::new();
    let mut prev_cong = Vec::new();
    let mut rt = Vec::new();

    let mut trial = 0_i64;
    for pid in 0..n_participants {
        let pid_offset = pid as f64 * 3.0;
        for &c in &[-1_i32, 1_i32] {
            for &p in &[-1_i32, 1_i32] {
                for rep in 0..reps_per_cell {
                    let z = (rep as f64 + 0.5) / reps_per_cell as f64;
                    let baseline =
                        560.0 + pid_offset + 200.0 * z + 18.0 * c as f64 + 9.0 * p as f64;
                    rt.push(baseline + (c * p) as f64 * interaction_shape(z, rep));
                    trial_id.push(trial);
                    participant_id.push(format!("p{pid:02}"));
                    cong.push(c.to_string());
                    prev_cong.push(p.to_string());
                    trial += 1;
                }
            }
        }
    }

    DataFrame::new(vec![
        Series::new("trial_id".into(), trial_id).into(),
        Series::new("participant_id".into(), participant_id).into(),
        Series::new("cong".into(), cong).into(),
        Series::new("prev_cong".into(), prev_cong).into(),
        Series::new("rt".into(), rt).into(),
    ])
    .unwrap()
}

fn synthetic_time_varying_cse_df() -> DataFrame {
    let mut trial_id = Vec::new();
    let mut trial_index = Vec::new();
    let mut participant_id = Vec::new();
    let mut cong = Vec::new();
    let mut prev_cong = Vec::new();
    let mut rt = Vec::new();

    let mut trial = 0_i64;
    for pid in 0..4 {
        let pid_offset = pid as f64 * 5.0;
        for bin in 0..4 {
            let cse = [35.0, 15.0, -10.0, 25.0][bin];
            for _rep in 0..12 {
                for &c in &[-1_i32, 1_i32] {
                    for &p in &[-1_i32, 1_i32] {
                        let ar_like = if trial > 0 { 8.0 * ((trial % 7) as f64 / 6.0) } else { 0.0 };
                        trial_id.push(trial);
                        trial_index.push(trial);
                        participant_id.push(format!("p{pid:02}"));
                        cong.push(c.to_string());
                        prev_cong.push(p.to_string());
                        rt.push(
                            620.0
                                + pid_offset
                                - 0.04 * trial as f64
                                + 12.0 * c as f64
                                + 7.0 * p as f64
                                + cse * (c * p) as f64
                                + ar_like,
                        );
                        trial += 1;
                    }
                }
            }
        }
    }

    DataFrame::new(vec![
        Series::new("trial_id".into(), trial_id).into(),
        Series::new("trial_index".into(), trial_index).into(),
        Series::new("participant_id".into(), participant_id).into(),
        Series::new("cong".into(), cong).into(),
        Series::new("prev_cong".into(), prev_cong).into(),
        Series::new("rt".into(), rt).into(),
    ])
    .unwrap()
}

fn synthetic_imbalanced_location_cse_df() -> DataFrame {
    let mut frames = Vec::new();
    for (pid, reps) in [("p00", 8_usize), ("p01", 30), ("p02", 75), ("p03", 120)] {
        let mut part = synthetic_location_cse_df(1, reps, 42.0);
        part.replace(
            "participant_id",
            Series::new("participant_id".into(), vec![pid; part.height()]),
        )
        .unwrap();
        frames.push(part);
    }

    let mut out = frames.remove(0);
    for frame in frames {
        out = out.vstack(&frame).unwrap();
    }
    out
}

fn tied_rt_df() -> DataFrame {
    DataFrame::new(vec![
        Series::new("trial_id".into(), 0_i64..16).into(),
        Series::new(
            "participant_id".into(),
            &[
                "p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1", "p2", "p2", "p2", "p2", "p2", "p2",
                "p2", "p2",
            ],
        )
        .into(),
        Series::new(
            "cong".into(),
            &[
                "-1", "-1", "-1", "-1", "1", "1", "1", "1", "-1", "-1", "-1", "-1", "1", "1", "1",
                "1",
            ],
        )
        .into(),
        Series::new(
            "prev_cong".into(),
            &[
                "-1", "-1", "1", "1", "-1", "-1", "1", "1", "-1", "-1", "1", "1", "-1", "-1", "1",
                "1",
            ],
        )
        .into(),
        Series::new(
            "rt".into(),
            &[
                500.0, 500.0, 520.0, 520.0, 510.0, 510.0, 530.0, 530.0, 600.0, 600.0, 620.0, 620.0,
                610.0, 610.0, 630.0, 630.0,
            ],
        )
        .into(),
    ])
    .unwrap()
}

fn sorted_multisets_by_pid_cong(df: &DataFrame) -> HashMap<(String, String), Vec<f64>> {
    let pid = df.column("participant_id").unwrap().str().unwrap();
    let cong = df.column("cong").unwrap().str().unwrap();
    let rt = df.column("rt").unwrap().f64().unwrap();

    let mut out: HashMap<(String, String), Vec<f64>> = HashMap::new();
    for ((pid, cong), rt) in pid
        .into_no_null_iter()
        .zip(cong.into_no_null_iter())
        .zip(rt.into_no_null_iter())
    {
        out.entry((pid.to_string(), cong.to_string()))
            .or_default()
            .push(rt);
    }
    for vals in out.values_mut() {
        vals.sort_by(|a, b| a.partial_cmp(b).unwrap());
    }
    out
}

fn rt_by_trial(df: &DataFrame) -> HashMap<i64, f64> {
    let trial = df.column("trial_id").unwrap().i64().unwrap();
    let rt = df.column("rt").unwrap().f64().unwrap();
    trial
        .into_no_null_iter()
        .zip(rt.into_no_null_iter())
        .collect()
}

fn cell_mean(df: &DataFrame, c: &str, p: &str) -> f64 {
    let cong = df.column("cong").unwrap().str().unwrap();
    let prev = df.column("prev_cong").unwrap().str().unwrap();
    let rt = df.column("rt").unwrap().f64().unwrap();

    let mut sum = 0.0;
    let mut n = 0_usize;
    for ((cong, prev), rt) in cong
        .into_no_null_iter()
        .zip(prev.into_no_null_iter())
        .zip(rt.into_no_null_iter())
    {
        if cong == c && prev == p {
            sum += rt;
            n += 1;
        }
    }
    sum / n as f64
}

fn mean_cse(df: &DataFrame) -> f64 {
    (cell_mean(df, "1", "1") - cell_mean(df, "1", "-1"))
        - (cell_mean(df, "-1", "1") - cell_mean(df, "-1", "-1"))
}

fn mean_cong_effect(df: &DataFrame) -> f64 {
    (cell_mean(df, "1", "1") + cell_mean(df, "1", "-1")) / 2.0
        - (cell_mean(df, "-1", "1") + cell_mean(df, "-1", "-1")) / 2.0
}

fn mean_prev_cong_effect(df: &DataFrame) -> f64 {
    (cell_mean(df, "1", "1") + cell_mean(df, "-1", "1")) / 2.0
        - (cell_mean(df, "1", "-1") + cell_mean(df, "-1", "-1")) / 2.0
}

fn cell_values(df: &DataFrame) -> HashMap<(String, String), Vec<f64>> {
    let cong = df.column("cong").unwrap().str().unwrap();
    let prev = df.column("prev_cong").unwrap().str().unwrap();
    let rt = df.column("rt").unwrap().f64().unwrap();

    let mut cells: HashMap<(String, String), Vec<f64>> = HashMap::new();
    for ((cong, prev), rt) in cong
        .into_no_null_iter()
        .zip(prev.into_no_null_iter())
        .zip(rt.into_no_null_iter())
    {
        cells
            .entry((cong.to_string(), prev.to_string()))
            .or_default()
            .push(rt);
    }
    cells
}

fn quantile_cse_curve(df: &DataFrame, taus: &[f64]) -> Vec<f64> {
    let cells = cell_values(df);
    let curve = |c: &str, p: &str| -> Vec<f64> {
        quantiles_type8(cells.get(&(c.to_string(), p.to_string())).unwrap(), taus)
    };
    let q11 = curve("1", "1");
    let q1m = curve("1", "-1");
    let qm1 = curve("-1", "1");
    let qmm = curve("-1", "-1");

    q11.iter()
        .zip(q1m.iter())
        .zip(qm1.iter())
        .zip(qmm.iter())
        .map(|(((q11, q1m), qm1), qmm)| (q11 - q1m) - (qm1 - qmm))
        .collect()
}

fn max_abs_quantile_cse(df: &DataFrame) -> f64 {
    let taus = [0.1, 0.25, 0.5, 0.75, 0.9];
    quantile_cse_curve(df, &taus)
        .into_iter()
        .map(f64::abs)
        .fold(0.0_f64, f64::max)
}

fn conditional_prev_diffs_by_pid_cong(df: &DataFrame) -> Vec<f64> {
    let pid = df.column("participant_id").unwrap().str().unwrap();
    let cong = df.column("cong").unwrap().str().unwrap();
    let prev = df.column("prev_cong").unwrap().str().unwrap();
    let rt = df.column("rt").unwrap().f64().unwrap();

    let mut sums: HashMap<(String, String, String), (f64, usize)> = HashMap::new();
    for (((pid, cong), prev), rt) in pid
        .into_no_null_iter()
        .zip(cong.into_no_null_iter())
        .zip(prev.into_no_null_iter())
        .zip(rt.into_no_null_iter())
    {
        let entry = sums
            .entry((pid.to_string(), cong.to_string(), prev.to_string()))
            .or_insert((0.0, 0));
        entry.0 += rt;
        entry.1 += 1;
    }

    let mut diffs = Vec::new();
    for ((pid, cong, prev), (sum_pos, n_pos)) in &sums {
        if prev != "1" {
            continue;
        }
        let key_neg = (pid.clone(), cong.clone(), "-1".to_string());
        if let Some((sum_neg, n_neg)) = sums.get(&key_neg) {
            diffs.push(sum_pos / *n_pos as f64 - sum_neg / *n_neg as f64);
        }
    }
    diffs
}

fn qmap(df: DataFrame, kappa: f64) -> DataFrame {
    qmap_with_ngrid(df, kappa, 101)
}

fn qmap_with_ngrid(df: DataFrame, kappa: f64, ngrid: usize) -> DataFrame {
    quantile_map_once(
        df,
        QuantileMapParams {
            scale_col: "rt".to_string(),
            kappa,
            ngrid,
        },
    )
    .unwrap()
}

fn assert_all_rt_finite(df: &DataFrame) {
    let rt: Vec<f64> = df
        .column("rt")
        .unwrap()
        .f64()
        .unwrap()
        .into_no_null_iter()
        .collect();
    assert!(rt.iter().all(|v| v.is_finite()));
}

#[test]
fn shuffle_preserves_rt_multiset_within_each_participant_by_cong_group() {
    let df = synthetic_location_cse_df(4, 20, 45.0);
    let shuffled = shuffle_null(df.clone(), "rt", 1729).unwrap();

    assert_eq!(
        sorted_multisets_by_pid_cong(&df),
        sorted_multisets_by_pid_cong(&shuffled),
        "shuffle must preserve each participant_id x cong RT multiset"
    );
}

#[test]
fn shuffle_never_crosses_participant_boundaries() {
    let df = synthetic_location_cse_df(3, 12, 40.0);
    let shuffled = shuffle_null(df.clone(), "rt", 19).unwrap();
    let original = sorted_multisets_by_pid_cong(&df);

    let pid = shuffled.column("participant_id").unwrap().str().unwrap();
    let cong = shuffled.column("cong").unwrap().str().unwrap();
    let rt = shuffled.column("rt").unwrap().f64().unwrap();
    for ((pid, cong), rt) in pid
        .into_no_null_iter()
        .zip(cong.into_no_null_iter())
        .zip(rt.into_no_null_iter())
    {
        let allowed = original
            .get(&(pid.to_string(), cong.to_string()))
            .expect("participant/cong group must exist");
        assert!(
            allowed.iter().any(|v| (*v - rt).abs() < f64::EPSILON),
            "RT {rt} crossed into participant {pid}, cong {cong} from another group"
        );
    }
}

#[test]
fn shuffle_is_deterministic_for_fixed_seed() {
    let df = synthetic_location_cse_df(5, 30, 35.0);
    let shuffled_a = shuffle_null(df.clone(), "rt", 12345).unwrap();
    let shuffled_b = shuffle_null(df, "rt", 12345).unwrap();

    assert_eq!(rt_by_trial(&shuffled_a), rt_by_trial(&shuffled_b));
}

#[test]
fn shuffle_removes_prev_cong_association_conditional_on_participant_by_cong() {
    let df = synthetic_location_cse_df(12, 400, 60.0);
    let before = conditional_prev_diffs_by_pid_cong(&df);
    let shuffled = shuffle_null(df, "rt", 20240522).unwrap();
    let after = conditional_prev_diffs_by_pid_cong(&shuffled);

    let mean_abs_before = before.iter().map(|v| v.abs()).sum::<f64>() / before.len() as f64;
    let mean_abs_after = after.iter().map(|v| v.abs()).sum::<f64>() / after.len() as f64;
    assert!(
        mean_abs_before > 100.0,
        "synthetic data must contain a strong association"
    );
    assert!(
        mean_abs_after < 6.0 && mean_abs_after / mean_abs_before < 0.05,
        "residual conditional prev_cong association after shuffle was {mean_abs_after} ms"
    );
}

#[test]
fn shuffle_nullification_is_shared_across_full_sample_subsample_ids() {
    let df = synthetic_location_cse_df(4, 30, 35.0);
    let cfg = |subsample_id| BranchConfig {
        sample_size: 1.0,
        subsample_id,
        transformation: Transformation::NoLogRt,
        outlier_method: OutlierMethod::None,
        effect_condition: EffectCondition::NullInteraction,
        strip_method: StripMethod::Shuffle,
        global_seed: 20240522,
    };

    let (out_a, _) = BranchPipeline::new(cfg(1)).process(df.clone()).unwrap();
    let (out_b, _) = BranchPipeline::new(cfg(2)).process(df).unwrap();

    assert_eq!(
        rt_by_trial(&out_a),
        rt_by_trial(&out_b),
        "subsample_id must not create a different null population at sample_size=1"
    );
}

#[test]
fn qmap_reduces_known_location_only_cse_to_approximately_zero() {
    let df = synthetic_location_cse_df(10, 80, 45.0);
    let before = mean_cse(&df);
    let qmapped = qmap(df, 5.0);
    let after = mean_cse(&qmapped);

    assert!(
        before.abs() > 150.0,
        "synthetic data must contain a strong CSE"
    );
    assert!(
        after.abs() < 1.0,
        "additive_qmap should approximately remove location CSE; residual was {after} ms"
    );
}

#[test]
fn qmap_preserves_marginal_main_effects_while_removing_cse() {
    let df = synthetic_location_cse_df(6, 50, 30.0);
    let before_cong = mean_cong_effect(&df);
    let before_prev = mean_prev_cong_effect(&df);
    let before_cse = mean_cse(&df);
    let qmapped = qmap(df, 5.0);
    let after_cong = mean_cong_effect(&qmapped);
    let after_prev = mean_prev_cong_effect(&qmapped);
    let after_cse = mean_cse(&qmapped);

    assert!(before_cse.abs() > 100.0);
    assert!(
        (after_cong - before_cong).abs() < 0.5,
        "additive_qmap should preserve the current-congruency main effect; before {before_cong}, after {after_cong}"
    );
    assert!(
        (after_prev - before_prev).abs() < 1.0,
        "additive_qmap should preserve the previous-congruency main effect; before {before_prev}, after {after_prev}"
    );
    assert!(
        after_cse.abs() < 1.0,
        "additive_qmap should still remove the location interaction; residual was {after_cse} ms"
    );
}

#[test]
fn qmap_reduces_scale_only_quantile_cse() {
    let df = synthetic_shape_cse_df(8, 100, |z, _rep| 70.0 * (z - 0.5));
    let before = max_abs_quantile_cse(&df);
    let qmapped = qmap(df, 5.0);
    let after = max_abs_quantile_cse(&qmapped);

    assert!(
        before > 60.0,
        "synthetic scale-only data must contain quantile CSE"
    );
    assert!(
        after < 4.0 && after / before < 0.06,
        "additive_qmap should reduce scale-only quantile CSE; residual was {after} ms"
    );
}

#[test]
fn qmap_reduces_tail_only_quantile_cse() {
    let df = synthetic_shape_cse_df(8, 100, |z, _rep| if z > 0.8 { 90.0 } else { 0.0 });
    let before = max_abs_quantile_cse(&df);
    let qmapped = qmap(df, 5.0);
    let after = max_abs_quantile_cse(&qmapped);

    assert!(
        before > 90.0,
        "synthetic tail-only data must contain quantile CSE"
    );
    assert!(
        after < 5.0 && after / before < 0.06,
        "additive_qmap should reduce tail-only quantile CSE; residual was {after} ms"
    );
}

#[test]
fn qmap_reduces_quantile_crossing_cse() {
    let df = synthetic_shape_cse_df(8, 100, |z, _rep| 70.0 * (0.5 - z));
    let before_curve = quantile_cse_curve(&df, &[0.1, 0.9]);
    let before = max_abs_quantile_cse(&df);
    let qmapped = qmap(df, 5.0);
    let after = max_abs_quantile_cse(&qmapped);

    assert!(
        before_curve[0] > 40.0 && before_curve[1] < -40.0,
        "synthetic quantile-crossing data must flip interaction sign across quantiles"
    );
    assert!(
        after < 3.0 && after / before < 0.06,
        "additive_qmap should reduce quantile-crossing CSE; residual was {after} ms"
    );
}

#[test]
fn qmap_reduces_mixed_location_and_shape_cse() {
    let df = synthetic_shape_cse_df(8, 100, |z, _rep| 35.0 + 50.0 * (z - 0.5));
    let before_mean = mean_cse(&df).abs();
    let before_quantile = max_abs_quantile_cse(&df);
    let qmapped = qmap(df, 5.0);
    let after_mean = mean_cse(&qmapped).abs();
    let after_quantile = max_abs_quantile_cse(&qmapped);

    assert!(before_mean > 130.0 && before_quantile > 100.0);
    assert!(
        after_mean < 3.0 && after_quantile < 4.0,
        "additive_qmap should reduce mixed mean/shape CSE; residual mean {after_mean}, quantile {after_quantile} ms"
    );
}

#[test]
fn qmap_reduces_location_cse_with_participant_imbalance() {
    let df = synthetic_imbalanced_location_cse_df();
    let before = mean_cse(&df).abs();
    let qmapped = qmap(df, 5.0);
    let after = mean_cse(&qmapped).abs();

    assert!(before > 150.0);
    assert!(
        after < 3.0,
        "additive_qmap should reduce location CSE under participant imbalance; residual was {after} ms"
    );
}

#[test]
fn qmap_reduces_shape_cse_across_grid_and_shrinkage_settings() {
    let df = synthetic_shape_cse_df(6, 80, |z, _rep| 30.0 + 60.0 * (z - 0.5));
    let before = max_abs_quantile_cse(&df);
    assert!(before > 90.0);

    for (kappa, ngrid) in [(0.0, 31_usize), (5.0, 31), (5.0, 151)] {
        let qmapped = qmap_with_ngrid(df.clone(), kappa, ngrid);
        let after = max_abs_quantile_cse(&qmapped);
        assert!(
            after < 5.0,
            "qmap should reduce mixed-shape CSE for kappa={kappa}, ngrid={ngrid}; residual was {after} ms"
        );
    }
}

#[test]
fn qmap_handles_ties_without_nonfinite_output() {
    let qmapped = qmap(tied_rt_df(), 5.0);
    assert_eq!(qmapped.height(), 16);
    assert_all_rt_finite(&qmapped);
}

#[test]
fn qmap_missing_cell_behavior_preserves_rows_and_returns_finite_values() {
    let qmapped = qmap(small_cell_df(), 5.0);

    assert_eq!(qmapped.height(), 5);
    assert_all_rt_finite(&qmapped);
}

#[test]
fn additive_qmap_trial_bin_reduces_time_varying_autocorrelated_cse() {
    let df = synthetic_time_varying_cse_df();
    let before = mean_cse(&df).abs();
    let qmapped = quantile_map_trial_bin(
        df.clone(),
        QuantileMapParams {
            scale_col: "rt".into(),
            kappa: 5.0,
            ngrid: 101,
        },
        5,
        4,
    )
    .expect("trial-bin additive qmap should run");
    let after = mean_cse(&qmapped).abs();

    assert_eq!(qmapped.height(), df.height());
    assert!(before > 50.0);
    assert!(
        after < 5.0,
        "trial-bin additive qmap should reduce time-varying location CSE; residual was {after} ms"
    );
}

#[test]
fn local_median_residual_reduces_time_varying_autocorrelated_cse() {
    let df = synthetic_time_varying_cse_df();
    let before = mean_cse(&df).abs();
    let stripped = local_median_residual_strip(df.clone(), 5, 4).expect("local median residual should run");
    let after = mean_cse(&stripped).abs();

    assert_eq!(stripped.height(), df.height());
    assert!(before > 50.0);
    assert!(
        after < 5.0,
        "local median residual should reduce time-varying location CSE; residual was {after} ms"
    );
}

#[test]
fn qmap_trial_bin_reduces_cse_and_preserves_rows_with_trial_index() {
    let df = synthetic_location_cse_df(4, 24, 20.0);
    let before = mean_cse(&df).abs();
    let qmapped = quantile_map_trial_bin(
        df.clone(),
        QuantileMapParams {
            scale_col: "rt".into(),
            kappa: 5.0,
            ngrid: 101,
        },
        4,
        4,
    )
    .expect("trial-bin qmap should run");
    let after = mean_cse(&qmapped).abs();

    assert_eq!(qmapped.height(), df.height());
    assert!(before > 70.0);
    assert!(
        after < 2.0,
        "trial-bin qmap should reduce location CSE; residual was {after} ms"
    );
}

#[test]
fn local_mean_residual_reduces_cse_and_preserves_marginal_effects() {
    let df = synthetic_location_cse_df(4, 24, 20.0);
    let before_cse = mean_cse(&df).abs();
    let before_cong = mean_cong_effect(&df);
    let before_prev = mean_prev_cong_effect(&df);
    let stripped = local_mean_residual_strip(df.clone(), 4, 4).expect("local mean residual should run");
    let after_cse = mean_cse(&stripped).abs();
    let after_cong = mean_cong_effect(&stripped);
    let after_prev = mean_prev_cong_effect(&stripped);

    assert_eq!(stripped.height(), df.height());
    assert!(before_cse > 70.0);
    assert!(after_cse < 1.0, "residual CSE was {after_cse} ms");
    assert!((after_cong - before_cong).abs() < 1.0);
    assert!((after_prev - before_prev).abs() < 1.0);
}

#[test]
fn local_median_residual_reduces_median_cse_and_preserves_marginal_effects() {
    let df = synthetic_location_cse_df(4, 24, 20.0);
    let before_cse = mean_cse(&df).abs();
    let before_cong = mean_cong_effect(&df);
    let before_prev = mean_prev_cong_effect(&df);
    let stripped = local_median_residual_strip(df.clone(), 4, 4).expect("local median residual should run");
    let after_cse = mean_cse(&stripped).abs();
    let after_cong = mean_cong_effect(&stripped);
    let after_prev = mean_prev_cong_effect(&stripped);

    assert_eq!(stripped.height(), df.height());
    assert!(before_cse > 70.0);
    assert!(after_cse < 1.0, "residual CSE was {after_cse} ms");
    assert!((after_cong - before_cong).abs() < 1.0);
    assert!((after_prev - before_prev).abs() < 1.0);
}

#[test]
fn qmap_trial_bin_falls_back_to_stationary_without_trial_index() {
    let df = synthetic_location_cse_df(4, 24, 20.0);
    let binned = quantile_map_trial_bin(
        df.clone(),
        QuantileMapParams {
            scale_col: "rt".into(),
            kappa: 5.0,
            ngrid: 101,
        },
        4,
        4,
    )
    .expect("fallback qmap should run");
    let stationary = quantile_map_once(
        df,
        QuantileMapParams {
            scale_col: "rt".into(),
            kappa: 5.0,
            ngrid: 101,
        },
    )
    .expect("stationary qmap should run");

    let rt_binned: Vec<f64> = binned
        .column("rt")
        .unwrap()
        .f64()
        .unwrap()
        .into_no_null_iter()
        .collect();
    let rt_stationary: Vec<f64> = stationary
        .column("rt")
        .unwrap()
        .f64()
        .unwrap()
        .into_no_null_iter()
        .collect();
    assert_eq!(rt_binned, rt_stationary);
}
