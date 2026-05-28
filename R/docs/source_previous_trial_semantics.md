# Source Previous-Trial Semantics

`R/all_indexed.csv` is built from heterogeneous source files, so previous-trial fields are not audited with one universal row-lag rule.

- `zeng`: previous fields are computed in `R/mverse_indexed.Rmd` after sorting by participant and `trial_index`; first participant trial has missing previous fields.
- `zhang`: previous fields are computed after filtering to response trials, then sorting by participant, `block_index`, and `trial_index`; first participant response trial has missing previous fields.
- `yang`: probe rows inherit previous probe semantics from the source task structure. The construction groups by `subj_code`, sorts by `trial_index`, and uses the probe-offset lag (`n = 5`) before filtering to probe rows.
- `primeprobe`: the Bognar source file already supplies `prev_congruent`; `mverse_indexed.Rmd` preserves that source field as `prev_cong` and computes `prev_correct` within participant row order.
- `flanker`: the Bognar source file already supplies `prev_congruent`; `mverse_indexed.Rmd` preserves that source field as `prev_cong` and computes `prev_correct` within participant row order.

The design-integrity audit therefore treats `flanker`, `primeprobe`, and `yang` previous-congruency fields as source-supplied/task-structured semantics rather than simple previous row lags in the reduced combined table. Any future stronger audit should compare against the original task event logs, not only against adjacent rows in `all_indexed.csv`.
