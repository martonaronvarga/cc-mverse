# Rate Estimands And Labels

This audit uses three related classes of rates. They must not be pooled or labeled interchangeably.

## Empirical multiverse detection proportions

Definition:

```text
proportion of empirical analysis branches with p(cong:prev_cong) < alpha
```

These rates are computed over dependent branches that reuse the same empirical participants and trials under different analysis choices. They are not independent experimental replications.

Use labels such as:

- `empirical multiverse detection proportion`
- `empirical nullified-branch detection proportion`
- `conditional empirical detection proportion among usable fits`

Avoid labels such as `FPR`, `TPR`, or `power` for unstripped empirical branches because their population truth is not known.

## Nullification-based FPR

Definition:

```text
significant cong:prev_cong tests under empirical RT data after a prespecified CSE stripping method has removed the target interaction while preserving non-CSE RT structure as far as the method permits
```

Use labels such as:

- `nullification-based FPR`
- `additive_qmap nullification-based FPR`
- `unconditional nullification-based FPR`
- `conditional nullification-based FPR among usable fits`

Denominator policy:

- unconditional: all planned fits for a given stripping method;
- conditional: usable primary fits only;
- failure-aware: significant, usable nonsignificant, singular, non-converged, extraction/preprocessing error.

Interpretability requirement: every nullification-based FPR must be paired with residual-CSE and preservation diagnostics for the same stripping method. `additive_qmap`, `shuffle`, and future local methods define different empirical counterfactuals and must stay stratified.

## Known-null simulation calibration

Definition:

```text
significant cong:prev_cong tests under simulated data with true interaction = 0
```

Use labels such as:

- `known-null simulated FPR`
- `known-null calibration FPR`

These rows calibrate analysis behavior under simple known truth, but they are not the primary source of realism when empirical nullification diagnostics pass.

## Known-alternative TPR

Definition:

```text
significant cong:prev_cong tests under simulated data with a known nonzero interaction
```

Use labels such as:

- `known-alternative TPR`
- `known-alternative TPR at truth_interaction_log = 0.02`
- `conditional known-alternative TPR among usable fits`

Always stratify TPR by effect size (`truth_interaction_log` or raw-scale equivalent).

## Current implementation

- `R/functions/operating_characteristics_summary.R` summarizes known-null/known-alternative smoke outputs with unconditional, conditional, and failure-composition columns.
- `R/outputs/analysis/smoke_operating_characteristics_summary.csv` includes `truth_type`, `rate_name`, and `truth_interaction_log` to prevent mixing empirical detection proportions with known-ground-truth FPR/TPR.
- `R/functions/analysis_plot_c.R` now uses FPR labels for null-condition significance proportions instead of FDR.

## Reporting rule

Final reports must state the source of every rate:

```text
rate_source = empirical_multiverse | empirical_nullification | known_null_simulation | known_alternative_simulation
```

A figure or table that mixes sources must facet or otherwise label them explicitly.
