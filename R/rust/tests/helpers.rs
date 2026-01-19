use polars::prelude::*;
use std::collections::HashMap;

/// Groups RTs by (participant_id, cong) and returns sorted multisets per group.
/// Robust to participant_id/cong being numeric or string, and rt being integer or float.
pub fn group_rt_multiset_by_pid_cong(df: &DataFrame) -> HashMap<(String, String), Vec<f64>> {
    let mut df = df.clone();

    // Ensure types: participant_id, cong -> Utf8; rt -> Float64
    for col_name in ["participant_id", "cong"] {
        if df
            .column(col_name)
            .map(|c| c.dtype() != &DataType::String)
            .unwrap_or(true)
        {
            df = df
                .lazy()
                .with_columns([col(col_name).cast(DataType::String)])
                .collect()
                .expect("cast to String");
        }
    }
    if df
        .column("rt")
        .map(|c| c.dtype() != &DataType::Float64)
        .unwrap_or(true)
    {
        df = df
            .lazy()
            .with_columns([col("rt").cast(DataType::Float64)])
            .collect()
            .expect("cast rt to Float64");
    }

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
