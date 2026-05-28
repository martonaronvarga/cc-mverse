# Time-Local CSE Stripping Design

The stationary implemented stripper, `additive_qmap`, operates at the participant level. The diagnostics show that it removes the target CSE on a bounded empirical smoke subset, but it can shift non-CSE structure such as current-congruency marginal effects and post-error slowing. Therefore stationary `additive_qmap` is not sufficient as a fully reliable primary counterfactual for all final FPR claims.

## Implemented Time-Local Stripper

### `additive_qmap_trial_bin`

`additive_qmap_trial_bin` splits each participant by `trial_index` quantile bins and runs the same additive quantile projection as stationary `additive_qmap` within adequate participant bins. Sparse bins fall back to stationary participant-level qmap values, so the method preserves output row count and remains usable when local cells are incomplete.

Current Rust defaults are four trial bins and minimum five observations in each `cong x prev_cong` cell before using a local bin.

## Required Next Strippers

### Block/trial-bin qmap refinements

Definition:

1. Split each participant's raw RT stream into time-local strata using study, participant, block, and/or trial quantile bins.
2. Within each stratum, run the same additive quantile projection as stationary `additive_qmap` when all four `cong x prev_cong` cells meet a minimum count.
3. Fall back hierarchically when cells are sparse:
   - participant x block/bin qmap;
   - participant-level stationary qmap;
   - study-level or global diagnostic failure when even participant-level cells are inadequate.
4. Write fallback level and minimum cell count into diagnostics.

Acceptance criteria:

- residual participant mean CSE close to zero;
- residual median quantile CSE close to zero;
- marginal `cong` and `prev_cong` effects close to present branch;
- lag-1 autocorrelation, trial slopes, block mean spread, and post-error slowing close to present branch;
- row order and source rows preserved.

### `local_mean_residual` and `local_median_residual`

Implemented as conservative location-only local residual strippers. Within participant trial bins, they estimate the 2x2 cell interaction coefficient and subtract only `beta_interaction * cong * prev_cong` from each row. Sparse bins fall back to participant-level interaction estimates; if participant cells are inadequate, rows are left unchanged.

- `local_mean_residual` estimates the interaction from cell means and targets mean/location CSE.
- `local_median_residual` estimates the interaction from cell medians and targets the central quantile CSE while preserving marginal/post-error structure. In the full `R/all_indexed.csv` no-transform/no-outlier smoke run, it passed all current preservation gates and is the candidate primary empirical nullifier.

These methods preserve row order and within-cell residual shape better than qmap, but they only apply a scalar cell-interaction correction per local stratum. They are not full distributional CSE strippers.

### Future model-based local residual stripping

Definition:

1. Fit a richer nuisance model on raw RT in original row order with participant, study, block/trial trend, post-error, current-congruency, and previous-congruency terms.
2. Include the target `cong:prev_cong` term explicitly.
3. Construct stripped RT as observed RT minus only the fitted target interaction contribution.
4. Recompose in original row order without resampling residuals.

Acceptance criteria are the same as block/trial-bin qmap, with additional diagnostics for model fit failures and extrapolation.

## Current Status

Implemented diagnostics now expose whether stationary `additive_qmap` is sufficient. On the bounded all-indexed smoke subset, `additive_qmap` removes mean CSE but fails preservation gates for current-congruency marginal effect and post-error slowing. This motivates implementing the time-local methods above before treating nullification-based FPR as final.
