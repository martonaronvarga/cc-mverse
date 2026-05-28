use multiverse_analysis::{
    BranchConfig, BranchPipeline, EffectCondition, OutlierMethod, StripMethod, Transformation,
};
use polars::prelude::*;

/// Construct a simple DataFrame with multiple participants and trials for sampling tests.
fn make_df(n_participants: usize, trials_per_participant: usize) -> DataFrame {
    let mut pids: Vec<String> = Vec::new();
    let mut cong: Vec<String> = Vec::new();
    let mut prev: Vec<String> = Vec::new();
    let mut rt: Vec<f64> = Vec::new();

    for i in 0..n_participants {
        for t in 0..trials_per_participant {
            pids.push(format!("p{}", i + 1));
            cong.push(if t % 2 == 0 {
                "1".to_string()
            } else {
                "-1".to_string()
            });
            prev.push(if t % 2 == 0 {
                "-1".to_string()
            } else {
                "1".to_string()
            });
            rt.push(500.0 + (i as f64) * 10.0 + (t as f64));
        }
    }

    DataFrame::new(vec![
        Series::new("participant_id".into(), pids).into(),
        Series::new("cong".into(), cong).into(),
        Series::new("prev_cong".into(), prev).into(),
        Series::new("rt".into(), rt).into(),
    ])
    .unwrap()
}

#[test]
fn sample_data_identity_at_1_0() {
    let df = make_df(5, 10);
    let cfg = BranchConfig {
        sample_size: 1.0,
        subsample_id: 1,
        transformation: Transformation::NoLogRt,
        outlier_method: OutlierMethod::None,
        effect_condition: EffectCondition::Present,
        strip_method: StripMethod::Shuffle,
        global_seed: 42,
    };
    let pipeline = BranchPipeline::new(cfg);
    let input_n = df.height();
    let (out_df, _res) = pipeline.process(df).unwrap();
    assert_eq!(
        out_df.height(),
        input_n,
        "sample_size=1.0 should keep all rows"
    );
}

#[test]
fn sample_data_zero_yields_empty() {
    let df = make_df(4, 5);
    let cfg = BranchConfig {
        sample_size: 0.0,
        subsample_id: 1,
        transformation: Transformation::NoLogRt,
        outlier_method: OutlierMethod::None,
        effect_condition: EffectCondition::Present,
        strip_method: StripMethod::Shuffle,
        global_seed: 42,
    };
    let pipeline = BranchPipeline::new(cfg);
    let (out_df, res) = pipeline.process(df).unwrap();
    assert_eq!(
        out_df.height(),
        0,
        "sample_size=0.0 should produce zero rows"
    );
    assert_eq!(res.n_rows_output, 0, "result should reflect zero rows");
}

#[test]
fn sample_data_fraction_keeps_expected_participants() {
    // 10 participants, 10 trials each -> 100 rows total
    let df = make_df(10, 10);
    let cfg = BranchConfig {
        sample_size: 0.5,
        subsample_id: 1,
        transformation: Transformation::NoLogRt,
        outlier_method: OutlierMethod::None,
        effect_condition: EffectCondition::Present,
        strip_method: StripMethod::Shuffle,
        global_seed: 42,
    };
    let pipeline = BranchPipeline::new(cfg);
    let (out_df, _res) = pipeline.process(df.clone()).unwrap();

    // Expect approximately half the participants kept; sampling truncates after ceil(p * frac)
    let orig_pids: Vec<String> = df
        .column("participant_id")
        .unwrap()
        .str()
        .unwrap()
        .into_no_null_iter()
        .map(|s| s.to_string())
        .collect();
    let mut uniq_orig = orig_pids.clone();
    uniq_orig.sort();
    uniq_orig.dedup();

    let out_pids: Vec<String> = out_df
        .column("participant_id")
        .unwrap()
        .str()
        .unwrap()
        .into_no_null_iter()
        .map(|s| s.to_string())
        .collect();
    let mut uniq_out = out_pids.clone();
    uniq_out.sort();
    uniq_out.dedup();

    let expected_keep = ((uniq_orig.len() as f64) * cfg.sample_size).ceil() as usize;
    assert_eq!(
        uniq_out.len(),
        expected_keep,
        "kept participants should match ceil(P * frac)"
    );
}
