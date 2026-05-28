# Statistical Operating Characteristics Plan

This project estimates FPR and TPR for the CSE location interaction that common forced-choice RT analyses test. The goal is methodological: evaluate how transformations, outlier filters, sample fractions, and model specifications change the detection behavior of `cong:prev_cong` under known null and known alternative data.

## Implemented Smoke Simulator

The first implementation slice is an isolated smoke simulator, not a replacement for the core pipeline.

Run from the repository root:

```sh
Rscript R/bin/location_smoke_simulation.R R/all_indexed.csv R/outputs/analysis flanker 3 8
```

Arguments:

1. input CSV path;
2. output directory;
3. comma-separated study list;
4. number of replicates;
5. maximum participants per study;
6. optional comma-separated log-scale alternative interaction sizes, e.g. `0.02,0.06`.

Output:

- `R/outputs/analysis/smoke_operating_characteristics.csv`

Summarize the smoke output with conditional, unconditional, and failure-aware denominators:

```sh
Rscript R/bin/summarise_operating_characteristics.R R/outputs/analysis/smoke_operating_characteristics.csv R/outputs/analysis
```

Summary output:

- `R/outputs/analysis/smoke_operating_characteristics_summary.csv`

## Current Smoke Scope

The smoke simulator:

- uses canonical previous-trial fields from `R/functions/design_integrity.R`;
- keeps the empirical participant/trial structure for selected studies;
- filters missing/nonpositive RT rows for this smoke stage;
- estimates a nuisance log-RT model with current congruency, previous congruency, trial trend, and participant fixed effects;
- simulates `null_location` with zero log-scale `cong:prev_cong` interaction;
- simulates `present_location` with one or more prespecified log-scale interactions;
- runs two simple smoke analyses: participant fixed-effect trial-level LM and participant cell-mean t-tests on raw/log RT.

This intentionally does not modify core config, targets, model specs, or result extraction.

## Interpretation

The smoke output is a gate, not the final operating-characteristics estimate. It verifies that known-ground-truth data can be generated and passed through simple CSE extractors with failures recorded. The current default smoke size is intentionally too small to estimate stable FPR/TPR.

## Required Next Upgrades

- Replace the smoke nuisance model with a documented generator family for full runs.
- Add study-wise runs and compare RT scale behavior across studies.
- Add the existing core model families only after extractor tests verify the intended `cong:prev_cong` term. The isolated helper/test pair `R/functions/cse_term_extraction.R` and `R/bin/test_cse_term_extraction.R` now defines the intended selection rule without changing core extraction code.
- Add unconditional, conditional, and failure-aware denominators.
- Add larger replicate counts only after smoke diagnostics pass.
