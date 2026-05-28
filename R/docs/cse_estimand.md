# CSE Estimand And Nullification Validity

## Decision

For this audit, the primary estimand is a participant-conditioned, stationary congruency sequence effect (CSE): the departure from additivity of current-trial congruency (`cong`) and previous-trial congruency (`prev_cong`) in reaction time (RT), after preserving participant identity.

The primary location estimand for participant `i` is:

```text
CSE_i = [E(RT | cong=1, prev_cong=1, i) - E(RT | cong=1, prev_cong=-1, i)]
      - [E(RT | cong=-1, prev_cong=1, i) - E(RT | cong=-1, prev_cong=-1, i)]
```

With +/-0.5 coding this is equivalent to the `cong:prev_cong` contrast up to the coding scale. The pooled diagnostic should report both participant-level CSE values and a clearly labeled aggregation over participants.

The distributional extension replaces cell means with cell quantiles:

```text
CSE_i(tau) = [Q_i,1,1(tau) - Q_i,1,-1(tau)]
           - [Q_i,-1,1(tau) - Q_i,-1,-1(tau)]
```

Report at minimum `tau = 0.1, 0.25, 0.5, 0.75, 0.9` plus the maximum absolute quantile CSE. This is the estimand that `additive_qmap` is attempting to remove; `shuffle` targets conditional association with `prev_cong` by permutation and is not a quantile-additivity method.

## What counts as valid stripping

A stripping method is valid only relative to the estimand and the data-generating assumptions it preserves. A nullified branch is scientifically interpretable if it meets all of these criteria:

1. It removes the target CSE diagnostic to near zero in empirical stripped branches and in known-effect fixtures.
2. It preserves the design constraints needed by the downstream model: participant clustering, current-trial congruency labels, canonical previous-trial labels, row order, trial/block geometry, planned branch identity, and usable cell counts.
3. It strips CSE before participant subsampling, outlier filtering, RT transformation, and model fitting because the counterfactual is defined on the collected raw RT stream.
4. It does not silently replace the estimand with a different null. For example, unrestricted shuffle creates an exchangeability null within `(participant_id, cong)` groups; it is not a time-series null.
5. It reports failures, missing cells, singular fits, and non-convergence as outcomes rather than filtering them out of FPR/TPR denominators.
6. It keeps `shuffle`, `additive_qmap`, and `present` separated in analysis tables because they answer different scientific questions.

## Inclusion criteria

The current primary CSE includes:

- previous-trial congruency as encoded in `prev_cong`;
- current-trial congruency as encoded in `cong`;
- participant-conditioned location CSE diagnostics;
- participant-conditioned distributional CSE diagnostics for `additive_qmap` validity checks;
- stationary effects over the full session unless a branch explicitly bins or models trial time.

## Exclusion criteria and confound risks

The primary estimand does not by itself distinguish CSE from:

- exact stimulus repetitions or alternations;
- response repetitions or alternations;
- feature integration and event-file retrieval;
- contingency learning and item-specific proportion-congruency effects;
- post-error slowing or post-error accuracy adjustments;
- block, fatigue, practice, or trial-index trends;
- local autocorrelation or long-range RT dependence.

If raw data lack columns for these mechanisms, the audit should say that the processed `cong:prev_cong` contrast is a behavioral sequential-modulation contrast, not definitive evidence of cognitive-control adaptation.

## Method-specific validity

### `shuffle`

`shuffle` is valid for the location estimand only under exchangeability of RTs within `(participant_id, cong)` groups. It preserves the RT multiset in each group and breaks the association between RT and `prev_cong`, but it also disrupts trial order. Therefore it is invalid as a sole null when trial trends, blocks, post-error effects, autocorrelation, or changing transition frequencies are part of the scientific null.

Required diagnostics:

- RT multiset preservation by `(participant_id, cong)`;
- conditional mean difference by `prev_cong` after shuffling;
- lag-1 and longer-lag autocorrelation before/after shuffling;
- trial-index and block trends before/after shuffling;
- comparison with blockwise, trial-bin, circular-shift, or nuisance-residual permutation sensitivity methods.

### `additive_qmap`

`additive_qmap` is valid for a stationary participant-level distributional CSE if the additive row-plus-column-minus-grand quantile reconstruction is the intended null. It is stronger than mean stripping because it targets quantile-specific non-additivity, but it still does not preserve local temporal dependence.

Required diagnostics:

- mean CSE and quantile CSE before/after qmap;
- small-cell and missing-cell warnings;
- participant imbalance sensitivity;
- `ngrid` and `kappa` sensitivity;
- empirical residual-effect table by branch.

## References checked

- Gratton, Coles, and Donchin (1992), `Optimizing the use of information: Strategic control of activation of responses`, Journal of Experimental Psychology: General, DOI `10.1037/0096-3445.121.4.480`. Introduces the sequential congruency/control framing commonly called the Gratton effect.
- Mayr, Awh, and Laurey (2003), `Conflict adaptation effects in the absence of executive control`, Nature Neuroscience, DOI `10.1038/nn1051`. Canonical challenge that apparent conflict adaptation can arise without executive-control adaptation.
- Egner (2007), `Congruency sequence effects and cognitive control`, Cognitive, Affective, & Behavioral Neuroscience, DOI `10.3758/CABN.7.4.380`. Review framing CSE as informative but mechanistically heterogeneous.
- Duthoo, Abrahamse, Braem, Boehler, and Notebaert (2014), `The heterogeneous world of congruency sequence effects: an update`, Frontiers in Psychology, DOI `10.3389/fpsyg.2014.01001`. Supports treating CSE definition and confounds as central validity questions.
- Schmidt and De Houwer (2011), `Now you see it, now you don't: Controlling for contingencies and stimulus repetitions eliminates the Gratton effect`, Acta Psychologica, DOI `10.1016/j.actpsy.2011.06.002`. Key warning that contingency and repetition confounds can masquerade as CSE.
- Hommel (1998), `Event files: Evidence for automatic integration of stimulus-response episodes`, Visual Cognition, DOI `10.1080/713756773`. Canonical source for event-file/feature-integration confounds.
- Winkler et al. (2014), `Permutation inference for the general linear model`, NeuroImage, DOI `10.1016/j.neuroimage.2014.01.060`. Supports explicit exchangeability restrictions for permutation-based nulls.
