# Shuffle Exchangeability Policy

Unrestricted shuffle within `(participant_id, cong)` is retained only as a secondary robustness check. It is not the primary empirical CSE counterfactual.

## Why

CSE data are time-series-like RT streams. A shuffle that permutes RTs within `(participant_id, cong)` preserves a current-congruency marginal distribution, but it destroys or distorts temporal structure, post-error structure, and any previous-congruency marginal/history structure.

## Empirical Diagnostics

`R/functions/shuffle_exchangeability_diagnostics.R` and `R/bin/write_shuffle_exchangeability_diagnostics.R` produce `R/outputs/analysis/shuffle_exchangeability_diagnostics.csv`.

Current full `R/all_indexed.csv` diagnostics show non-negligible temporal/history structure:

- lag-1 autocorrelation by study ranges from about `0.08` to `0.23`;
- trial RT slopes are nonzero in all studies, especially flanker and primeprobe;
- post-error slowing/reversal is large by study, ranging roughly from `-66 ms` to `98 ms`;
- block mean spread is sizeable where block structure is available.

Therefore `unrestricted_shuffle_plausible = FALSE` for every study.

## Interpretation

Use `shuffle` rows as an exchangeability stress test only. They can answer whether a result depends on a very strong within-current-congruency permutation null, but they must not be pooled with `local_median_residual` primary rows or labeled as primary nullification-based FPR.

Primary nullification-based FPR should use branches with `nullification_verdict == "interpretable_nullifier"`, currently expected from `local_median_residual` after diagnostics pass.
