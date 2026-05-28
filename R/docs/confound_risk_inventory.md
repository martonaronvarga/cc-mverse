# Confound Risk Inventory

Generated artifacts:

- `R/outputs/analysis/raw_column_inventory.csv`
- `R/outputs/analysis/confound_risk_table.csv`

Regenerate from the repository root with:

```sh
Rscript R/bin/write_data_inventory.R R/all_indexed.csv R/outputs/analysis
```

## Main Findings

The combined `R/all_indexed.csv` contains enough information for the statistical CSE location interaction (`cong`, `prev_cong`, `participant_id`, `rt`) after previous-trial fields are recomputed and audited. It does not contain enough information to separate many psychological mechanisms that can masquerade as CSE.

Available but requiring care:

- `prev_cong` and `prev_correct`: present in all studies but must be recomputed because construction used ungrouped `lag()` in `R/mverse_indexed.Rmd`.
- `correct`: available for accuracy filtering, but filtering changes cell counts and can affect FPR/TPR.
- `trial_index`: available for fatigue/practice trends.
- `block_index`: available but study-specific in meaning.
- `is_practice`, `name`, `congruency`, `condition`: sparse or study-specific and should not be pooled as global covariates without harmonization.

Unavailable or insufficient in the combined file:

- exact stimulus identity across all studies;
- response identity and response repetition;
- feature/event-file integration variables;
- item-level contingency learning variables.

## Consequence

The operating-characteristics implementation can defensibly target CSE as a statistical `cong:prev_cong` location interaction. It cannot claim to separate cognitive-control adaptation from feature integration, response repetition, stimulus repetition, or contingency learning in the combined dataset.
