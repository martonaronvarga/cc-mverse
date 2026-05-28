# Model Convergence And Singularity Policy

This policy applies before interpreting FPR/TPR or multiverse detection proportions. Model failure, non-convergence, and singularity are outcomes of an analysis strategy, not rows to silently discard.

## Primary Principle

Fit the prespecified model for each branch. Store its numerical status and inference result separately. Do not silently replace a failed or singular primary model with a simpler model in the primary estimand.

Primary operating-characteristic denominators must include all planned branches:

```text
unconditional FPR/TPR = significant primary fits / all planned fits
```

Conditional rates may also be reported, but only with explicit labels:

```text
conditional FPR/TPR = significant primary fits / numerically usable primary fits
```

Failure-aware summaries must report the full composition:

```text
significant, usable nonsignificant, singular, non-converged, extraction error, preprocessing failure, other error
```

## LMM Fitting Policy

1. Fit the maximal/prespecified LMM from the branch config first.
2. Record convergence status from optimizer messages and convergence codes.
3. Record singularity with `lme4::isSingular(model, tol = 1e-4)` when available.
4. Record variance diagnostics for `(Intercept)`, `cong`, `prev_cong`, and `cong:prev_cong` random effects where present.
5. Record rank diagnostics with `lme4::rePCA(model)` for LMMs where this is available.
6. Extract the `cong:prev_cong` fixed effect only with explicit term matching; do not select the first term containing `:`.

A boundary variance near zero under a nullified interaction is not automatically a data failure. It is evidence about the compatibility of the maximal random-effects structure with the null-generating mechanism and must be counted.

## Prespecified Fallback Sensitivity

Fallback models are convenience/sensitivity fits, not inferential targets and never replacements for the primary model.

Recommended fallback sequence for LMM branches:

1. Primary maximal model as configured.
2. If the primary fit is singular or non-converged, fit a prespecified reduced random-effects model that removes the highest-order random slope for `cong:prev_cong` while retaining random intercepts and lower-order slopes when estimable. Fallbacks may only reduce random-effects structure; fixed-effects structure, especially the `cong:prev_cong` estimand, must not change.
3. If that reduced model fails, fit random-intercept-only LMM.
4. If random-intercept-only LMM fails, record failure; do not substitute rmANOVA as an LMM result.

A branch with a singular/non-converged primary fit remains invalid for primary analysis even when a fallback convenience fit succeeds. Each fallback fit must get separate result fields, for example:

```text
primary_model_status
primary_is_singular
primary_converged
primary_p_value
fallback_level
fallback_formula
fallback_converged
fallback_is_singular
fallback_p_value
inference_source = primary_maximal | primary_maximal_invalid_fallback_sensitivity_available | none
```

Primary tables must use only primary maximal-model inference. Sensitivity tables may display fallback convenience fits, but they must be labeled as fallback analyses and must not contribute to primary FPR/TPR denominators or significance counts.

## rmANOVA Policy

Repeated-measures ANOVA has no optimizer convergence, but it can fail because of preprocessing or data geometry.

Record as failures:

- missing `cong x prev_cong` cells for a participant after preprocessing;
- too few complete participants;
- non-finite cell means;
- extraction failure for the named `cong:prev_cong` effect.

Do not rely on row order in the ANOVA table; select the named interaction effect.

## Basic Model Policy

For trial-level fixed-effect/basic models, record:

- missing/non-finite RT filtering;
- participant fixed-effect rank deficiency;
- coefficient aliasing for `cong:prev_cong`;
- heteroskedasticity or clustered-SE availability if used.

If the interaction coefficient is aliased or absent, count the branch as an extraction/model-rank failure.

## Reporting Requirements

Every final FPR/TPR table must include:

- planned branch count;
- primary significant count;
- primary usable nonsignificant count;
- singular count;
- non-converged count;
- extraction/preprocessing error count;
- unconditional rate;
- conditional usable-only rate;
- fallback sensitivity rate, if implemented.

Plots must label whether they show primary-only, conditional primary-only, or fallback sensitivity rates.

## Implementation Status

Implemented before full runs:

- explicit fixed-effect CSE term selection in `R/functions/cse_term_extraction.R` and `R/functions/results.R`;
- named random-effect variance extraction in `R/functions/random_effect_extraction.R` and `R/functions/results.R`;
- smoke operating-characteristics summary with unconditional and conditional denominators in `R/functions/operating_characteristics_summary.R`.

Still to implement before final full runs:

- broader end-to-end fallback reporting plots/tables;
- failure-aware final tables across the full branch grid.

Implemented on 2026-05-22:

- `lme4::isSingular()` and `rePCA()` minimum-SD diagnostics are stored via `full_is_singular`, `null_is_singular`, `full_repca_min_sd`, and `null_repca_min_sd` in `R/functions/results.R`; `R/functions/analysis.R` now prefers `full_is_singular` when deriving `is_singular`.
- Sensitivity-only LMM fallback storage is implemented in `R/functions/models.R` and `R/functions/results.R`. Fallbacks only reduce random-effects structure, leave fixed effects unchanged, and are marked as convenience/sensitivity outputs rather than primary inference.
