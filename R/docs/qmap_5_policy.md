# additive_qmap Nullification Policy

## Active implementation

`additive_qmap` is the `null_interaction` strip method that calls `quantile_map_once()` from `R/rust/src/lib.rs` with `kappa = 5` and `ngrid = 200`.

The method works within participants. It estimates quantile curves for each `cong x prev_cong` cell, row margins (`cong`), column margins (`prev_cong`), and the participant grand distribution. Each trial is assigned a cell-specific quantile position, then its replacement RT is built from the additive row-plus-column-minus-grand quantile structure:

```text
rt_null(tau) = q_cong(tau) + q_prev_cong(tau) - q_grand(tau)
```

## Target estimand

`additive_qmap` targets the `cong:prev_cong` interaction in the RT distribution under a stationary, participant-level 2x2 design. Its counterfactual goal is to create RT data that are interpretable as the observed empirical data with CSE set to zero while preserving participant-conditioned marginal `cong` and `prev_cong` effects. It is intended to remove more than a mean/location CSE: because the operation is quantile-based, it can also reduce distributional CSE expressed as quantile-specific departures from additivity.

This target is still narrower than every plausible scientific CSE. It does not validate removal of time-varying CSE, block-specific CSE, feature/repetition confounds, post-error effects, or trial-to-trial dependence. See `R/docs/cse_estimand.md` for the formal validity criteria and reference list.

## Shrinkage rule

For local quantile curve `q_local(tau)` and its shrinkage target `q_target(tau)`, the active shrinkage rule is:

```text
q_shrunk(tau) = (q_local(tau) + kappa * q_target(tau)) / (1 + kappa)
```

- Cell curves shrink toward global `cong x prev_cong` curves only for sparse-cell quantile assignment.
- Row and column marginal curves remain local participant curves in the additive reconstruction, matching the R reference implementation and preserving main effects.
- `kappa = 0` means no cell-curve shrinkage.
- `kappa = 5` gives the target curve five times the weight of the local curve where sparse-cell shrinkage is used.
- If one side of the shrinkage pair is non-finite, the finite side is used.

The current tests include a regression check that `additive_qmap` preserves current- and previous-congruency main effects while removing the location interaction. The Rust implementation previously shrank row/column marginal curves toward the participant grand curve, which incorrectly attenuated the main `cong` effect by roughly `1 / (1 + kappa)`.

## Small cells and ties

Small cells are handled explicitly rather than silently dropped. Cells with fewer than two local observations use the shrunk curve for inverse interpolation when assigning quantile positions. Tests require qmap output to preserve rows and return finite RT values on small-cell and tied-RT examples.

Tied RT values are accepted. The interpolation helpers are monotone but not strictly monotone; flat segments return the lower grid value on inversion. This is deterministic and avoids non-finite output, but it is not a full validation of all tied or discrete RT distributions.

## Remaining unvalidated assumptions

- Distributional nullification has only synthetic tests so far, not full empirical residual diagnostics.
- Synthetic tests now cover location-only, scale-only, tail-only, quantile-crossing, mixed location+shape, missing/small-cell, tied-RT, participant-imbalance, and `ngrid`/`kappa` sensitivity cases.
- Empirical residual diagnostics still need broader adversarial coverage.
- Time dependence is not preserved or validated by additive_qmap.
- The appropriate CSE estimand remains a scientific decision: location-only, distributional, time-varying, or a sensitivity family.
- `ngrid = 200` and `kappa = 5` remain tuning choices; sensitivity to these values still needs reporting.
