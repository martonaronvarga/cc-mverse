# Data Construction Constraints

`R/mverse_indexed.Rmd` builds `R/all_indexed.csv` by row-binding five forced-choice RT datasets: flanker, primeprobe, yang, zhang, and zeng. The combined file is useful as a common design table, but nullification and empirical-design simulation must treat it as a heterogeneous multi-study dataset.

## Constraints

- The unnamed first CSV column is the `write.csv()` export row index and is not a design variable.
- Study identity is encoded in the suffix of `participant_id`.
- `cong` is harmonized to +/-1, but the original meanings differ; Zhang maps `no_conflict` to +1 and both `low_conflict`/`high_conflict` to -1.
- `block_index` is study-specific: condition changes in Zeng, pause markers in Zhang, inferred probe-sequence starts in Yang, and raw block IDs in flanker/primeprobe.
- `flanker` and `primeprobe` compute RT from response timestamp minus stimulus/probe onset; `zeng`, `zhang`, and `yang` use provided RT columns. RT units and impossible values must be audited by study before simulation.
- `prev_cong`, `prev_correct`, and `is_first_trial` should not be trusted blindly because `mverse_indexed.Rmd` often uses `lag()` outside `group_by(participant_id)`.

## Implemented Audit

Run from the repository root:

```sh
Rscript R/bin/design_integrity_audit.R R/all_indexed.csv R/outputs/analysis
```

Outputs:

- `R/outputs/analysis/design_inventory.csv`
- `R/outputs/analysis/previous_trial_column_audit.csv`

The audit recomputes canonical previous-trial fields after sorting by `study`, `participant_id`, `block_index`, `trial_index`, and original row. It reports mismatch rates, missing/nonpositive RT counts, participant counts, and four-cell adequacy by study and participant.

## Current Findings

The 2026-05-22 audit found all studies have complete four-cell participant coverage, but previous-trial mismatches exist in every study. This is expected from ungrouped `lag()` at participant boundaries and must be resolved before using previous-trial columns as ground-truth design variables.

Study-level highlights from `design_inventory.csv`:

- `flanker`: 147,204 rows; no missing/nonpositive RT; `prev_cong` mismatch rate 0.29%.
- `primeprobe`: 221,656 rows; 2 nonpositive RT rows; `prev_cong` mismatch rate 0.24%.
- `yang`: 229,315 rows; 12,947 missing RT rows; 40 nonpositive RT rows; `prev_cong` mismatch rate 1.21%.
- `zeng`: 188,160 rows; 3,816 missing RT rows; 1 nonpositive RT row; `prev_cong` mismatch rate 0.21%.
- `zhang`: 193,506 rows; 3,668 missing RT rows; 4 nonpositive RT rows; `prev_cong` mismatch rate 0.15%.

## Consequence For Nullification

Use canonical recomputed previous-trial fields for any new simulation/nullification diagnostics. Existing core configs and model definitions are unchanged, but any implementation of `null_location`, design integrity diagnostics, or residual CSE diagnostics should fail early or warn if it receives unaudited previous-trial fields.
