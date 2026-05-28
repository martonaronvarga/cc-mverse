# Audit Data Contract

This contract documents the intended data lifecycle for the CSE operating-characteristics audit. It is descriptive and does not change core configs or model definitions.

## Lifecycle

1. Source study files are assembled by `R/mverse_indexed.Rmd`.
2. `R/all_indexed.csv` is the primary combined empirical design table for CSE stripping and diagnostics; reduced merged CSVs are not the primary data source because they discard study/trial/block semantics.
3. `R/bin/design_integrity_audit.R` produces study-aware design integrity artifacts:
   - `R/outputs/analysis/design_inventory.csv`
   - `R/outputs/analysis/previous_trial_column_audit.csv`
4. `R/bin/write_data_inventory.R` produces confound and column inventory artifacts:
   - `R/outputs/analysis/raw_column_inventory.csv`
   - `R/outputs/analysis/confound_risk_table.csv`
5. Branch generation defines the analysis/data multiverse. `R/functions/branch_validation.R` checks intended branch invariants.
6. Processed/nullified data branches are expected to be named:

```text
processed__{sample_size}__{subsample_id}__{transformation}__{outlier}__{effect_condition}__{strip_method}.parquet
```

7. `R/bin/run_nullification_diagnostics.R` computes residual CSE diagnostics for processed branches.
8. Model fitting writes branch-level results with schema defined in `R/functions/results.R`.
9. `R/functions/analysis.R` prepares result tables for operating-characteristic summaries and plots.
10. Operating-characteristic summaries must distinguish nullification-based FPR from simulation calibration and must include conditional, unconditional, and failure-aware denominators.

## Required columns in `all_indexed.csv`

Required for current implementation:

- `participant_id`
- `trial_index`
- `block_index`
- `cong`
- `prev_cong` or enough structure to recompute canonical previous congruency
- `rt`
- `correct`
- `prev_correct` or enough structure to recompute canonical previous correctness

Optional/study-specific:

- `is_practice`
- `name`
- `congruency`
- `condition`

The unnamed first CSV column from `write.csv()` is an export row index and must not be treated as a design variable.

## Canonical previous-trial fields

Any new simulation, nullification, or diagnostic code should use canonical previous-trial fields recomputed by study and participant after sorting by:

```text
study, participant_id, block_index, trial_index, source_row
```

Provided `prev_cong`, `prev_correct`, and `is_first_trial` are audit inputs, not trusted ground truth.

## Required columns in processed branch data

Processed branch diagnostics require:

- `participant_id`
- `cong`
- `prev_cong`
- `rt`

Recommended additional metadata is encoded in processed filenames and/or branch tables:

- `sample_size`
- `subsample_id`
- `transformation`
- `outlier`
- `effect_condition`
- `strip_method`
- `data_id`

## Result schema requirements

The branch-level result schema must include:

- branch metadata (`branch_id`, `data_id`, sample/transformation/outlier/model/effect/strip fields);
- convergence fields (`full_converged`, `null_converged`);
- singularity fields (`full_is_singular`, `null_is_singular`);
- random-effect diagnostics (`full_repca_min_sd`, `null_repca_min_sd`, random variance fields);
- fixed-effect CSE estimate and p-value fields;
- error fields (`error`, `error_message`).

Failures are data in the operating-characteristics analysis and must remain in the denominator for unconditional FPR/TPR.

## Known limitations

- The combined data supports statistical `cong:prev_cong` location analyses, not a full cognitive mechanism decomposition.
- Stimulus identity, response identity, feature integration, and item-level contingency learning are not globally available in `all_indexed.csv`.
- Block and practice variables are study-specific.
- RT units and impossible values require study-aware validation before full simulation calibration.

## Implemented contract checks

- Design integrity: `R/functions/design_integrity.R`
- Column/confound inventory: `R/functions/data_inventory.R`
- Branch invariants: `R/functions/branch_validation.R`
- CSE term extraction: `R/functions/cse_term_extraction.R`
- Random-effect extraction: `R/functions/random_effect_extraction.R`
- LMM diagnostics: `R/functions/lmm_diagnostics.R`
- Nullification diagnostics: `R/functions/nullification_diagnostics.R`
- Smoke operating characteristics: `R/functions/location_operating_characteristics.R`
- Rate summaries: `R/functions/operating_characteristics_summary.R`
