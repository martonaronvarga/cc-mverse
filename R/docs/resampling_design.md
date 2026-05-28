# Resampling Design Memo

Requested empirical resampling expansion:

- 50 resamples at 5% participant fraction;
- 50 resamples at 10% participant fraction;
- 50 resamples at 20% participant fraction.

Generate the schedule with:

```sh
Rscript R/bin/write_resampling_design.R R/outputs/analysis/resampling_design_table.csv
```

Generated artifact:

- `R/outputs/analysis/resampling_design_table.csv`

## Interpretation

These empirical resamples are dependent multiverse paths, not independent experiments. They reuse the same empirical participant pool and task design. Therefore the primary label is:

```text
empirical multiverse detection proportion
```

Do not label these empirical resampling proportions as population FPR/TPR unless the data are simulated with known ground truth.

## Uncertainty policy

For empirical resampling summaries, uncertainty should be described with dependence-aware summaries such as participant-clustered bootstrap or study-stratified summaries. Treating the 50 resamples as independent experimental replications is not valid.

Known-ground-truth simulation remains the source for estimated FPR/TPR operating characteristics.
