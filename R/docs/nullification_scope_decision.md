# Nullification Scope Decision

This note defines the scientific/statistical scope of the current nullification methods for an empirical counterfactual: observed RT data as they might have looked if the population CSE were zero while non-CSE structure remained as intact as possible.

## Decision

For FPR operating-characteristic work, use empirical nullification as the primary counterfactual source when the question is how the existing complex RT data would behave with CSE stripped away. These rates are interpretable as nullification-based FPR estimates only if the stripped datasets pass preservation diagnostics showing that non-CSE RT structure remains credible.

For TPR/power work, compare the observed empirical branches against the nullified branches as a detection/attenuation contrast. A known-alternative simulation can remain a calibration tool, but it is not the primary source of realism.

Current nullification roles:

1. `additive_qmap` is the primary stationary participant-level distributional CSE stripper because it removes the `cong:prev_cong` interaction while preserving participant-conditioned `cong` and `prev_cong` marginal effects.
2. `shuffle` is a mechanical exchangeability stress test; it preserves `(participant_id, cong)` RT multisets but does not guarantee preservation of previous-congruency marginal effects or temporal structure.
3. Trial-bin/blockwise or model-based local nullification is a future sensitivity path for relaxing stationarity.

## Stationary participant-level removal

Definition:

```text
Remove one participant-level cong:prev_cong effect over the full session.
```

Current implementation:

- `additive_qmap`: participant-conditioned quantile reconstruction.
- `shuffle`: permutes RT within `(participant_id, cong)`.

Benefits:

- Simple and reproducible.
- Matches much common CSE practice that estimates one interaction per participant/session.
- Works as a counterfactual for whether analysis strategies produce detections after stationary location/distributional CSE has been stripped.

Risks:

- Assumes stationarity over trial time and block.
- Can obscure fatigue, practice, post-error effects, and autocorrelation.
- Shuffle breaks local temporal dependence.

Use as:

- the primary empirical nullification denominator for FPR-like detection rates once preservation diagnostics pass;
- a sensitivity family that must remain labeled by strip method (`additive_qmap`, `shuffle`, future local methods).

## Trial-bin or blockwise removal

Definition:

```text
Remove CSE within local time windows, blocks, or trial bins.
```

Benefits:

- Better respects fatigue/practice/nonstationarity.
- Keeps nullification closer to local experimental context.

Risks:

- Sparse `cong x prev_cong` cells within participant x bin.
- Block semantics differ across studies in `R/all_indexed.csv`.
- Bin count and shrinkage rules become analysis degrees of freedom.

Use as:

- future sensitivity analysis after design integrity and cell-count diagnostics pass;
- not required before the first known-ground-truth simulation smoke runs.

## Trial-to-trial/model-based removal

Definition:

```text
Estimate local nuisance structure and remove the model-implied cong:prev_cong component while preserving trial order.
```

Benefits:

- Most compatible with autocorrelation, fatigue, and history-dependent RT processes.
- Can preserve row order and nuisance covariates.

Risks:

- Requires an explicit nuisance model; not assumption-free.
- Model choice becomes part of the multiverse.
- The combined dataset lacks global stimulus/response identity variables needed to separate some psychological mechanisms.

Use as:

- later sensitivity family if GAMM/LMM nuisance modeling is added;
- not as a claim that CSE has been removed without assumptions.

## Processing-order decision

The current Rust order is:

```text
nullify -> participant subsample -> outlier filter -> transform
```

This order is required. The counterfactual event is that the collected raw RT stream itself had population CSE = 0; participant subsampling, outlier filtering, transformation, and model fitting are later analytical procedures whose behavior is being audited. Do not move stripping after those procedures.

Required diagnostics before final interpretation:

- verify that downstream log-transform and outlier branches do not create unacceptable residual CSE after raw-data stripping;
- report residual mean/quantile CSE by transformation/outlier branch;
- keep these diagnostics tied to the empirical nullification source rather than treating them as known-truth simulation checks.

## Implemented supports

- `R/docs/cse_estimand.md` defines the location and quantile CSE estimands.
- `R/docs/shuffle_exchangeability_policy.md` documents shuffle exchangeability limits.
- `R/docs/additive_qmap_policy.md` documents qmap shrinkage and stationarity assumptions.
- `R/functions/nullification_diagnostics.R` computes residual mean and quantile CSE diagnostics.
- `R/functions/location_operating_characteristics.R` starts the known-ground-truth empirical-design simulation path.
