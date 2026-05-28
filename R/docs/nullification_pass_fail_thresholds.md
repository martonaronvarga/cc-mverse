# Nullification Pass/Fail Thresholds

This note defines the current gate for interpreting empirical nullification as a credible `CSE = 0` counterfactual for primary FPR tables.

## Scope

The primary gate applies to raw-RT empirical nullification before participant subsampling, outlier filtering, transformation, and model fitting. It is designed for the central-location CSE estimand used by common rmANOVA/LM/LMM analyses.

A nullified branch can support primary `nullification-based FPR` only when its diagnostics row has:

```text
nullification_verdict == "interpretable_nullifier"
preservation_pass == TRUE
```

Branches that fail the gate may still be reported as sensitivity or diagnostic nullification results, but they must not drive the primary FPR interpretation.

## Current Thresholds

The implemented thresholds live in `R/functions/nullification_diagnostics.R`.

A paired nullification branch fails if any of the following are true:

- absolute residual mean CSE is greater than 5 ms;
- absolute residual median/`q050` quantile CSE is greater than 5 ms;
- current-congruency marginal effect shifts by more than 5 ms from the matching `present` branch;
- previous-congruency marginal effect shifts by more than 5 ms from the matching `present` branch;
- mean lag-1 autocorrelation shifts by more than 0.05;
- mean trial RT slope shifts by more than 0.05 ms/trial;
- block mean spread shifts by more than 10 ms;
- transition imbalance shifts by more than 0.01;
- post-error slowing shifts by more than 10 ms;
- max absolute time-bin median/`q050` CSE is greater than 10 ms.

Warnings are stored in `preservation_warnings`; passing branches get an empty warning string.

## Primary Candidate Status

Current full-data no-transform/no-outlier diagnostics identify `local_median_residual` as the primary candidate nullifier:

- `mean_cse = 0.83 ms`
- `q050_cse = 0.5 ms`
- `max_abs_timebin_q050_cse = 4.31 ms`
- `preservation_pass = TRUE`
- `nullification_verdict = interpretable_nullifier`

This evidence supports using `local_median_residual` for primary nullification-based FPR under the central-location CSE estimand.

## Limitations

These thresholds are pragmatic audit thresholds, not universal cognitive-model claims. They do not prove all distributional CSE is absent. `additive_qmap`, `additive_qmap_trial_bin`, `local_mean_residual`, and `shuffle` remain sensitivity mechanisms unless they pass the same gate in the relevant full branch set.

The final report should stratify or filter primary FPR tables by `nullification_verdict` and separately report failed/nullification-diagnostic branches.
