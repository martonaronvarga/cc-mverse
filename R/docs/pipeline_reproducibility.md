# Pipeline Reproducibility And Cache Policy

This note defines the lightweight local smoke command and cache invalidation policy for the audit. It does not replace full `targets` orchestration.

## Local smoke command

Run from the repository root:

```sh
Rscript R/bin/run_local_smoke_checks.R
```

The smoke command runs narrow assertion scripts for:

- fixed-effect CSE term extraction;
- random-effect variance extraction;
- LMM diagnostic helpers;
- branch-generation invariants;
- CSE definition comparisons;
- defensive data validation;
- branch/data/path IDs;
- processed parquet cache signatures;
- pipeline readiness reporting;
- shuffle adversarial diagnostics;
- nullification diagnostics;
- processed-style nullification diagnostic filename parsing.

It intentionally avoids heavy model fitting, full `targets`, and Rust recompilation.

A bounded all-indexed nullification smoke is also available:

```sh
Rscript R/bin/run_all_indexed_nullification_smoke.R
```

This command samples participants from each study, runs Rust on `present` and `null_interaction:local_median_residual`, writes paired nullification diagnostics, and fails only if pooled mean/median CSE remains large. Because the bounded subset is intentionally small, strict preservation gates may warn even when the full-data `local_median_residual` diagnostic passes.

## Cache invalidation policy

Processed Rust outputs and downstream model results should be considered stale when any of these change:

- `R/all_indexed.csv` or upstream data construction in `R/mverse_indexed.Rmd`;
- `pipeline.yaml` or resolved branch config;
- branch ID/data ID formatters in `R/functions/config.R` or `R/functions/paths.R`;
- Rust nullification code in `R/rust/src/lib.rs`;
- Rust dependencies or `R/rust/Cargo.lock`;
- qmap/shuffle policy decisions affecting nullification semantics;
- preprocessing order, outlier definitions, transformation definitions, or model formulas;
- R package versions affecting model fitting or parquet IO;
- Arrow/Polars/parquet compatibility changes.

## Targets policy

`R/_targets.R` now treats processed parquet outputs as concrete file dependencies and computes a processed-cache signature before model fitting. The signature is stored at `R/data/metadata/processed_cache_signature.rds` when Rust arguments are generated or the Rust processor is called. The signature includes:

- raw input path, size, and checksum;
- Rust CLI arguments derived from the active config;
- expected data IDs;
- `pipeline.yaml` / `.pipeline.resolved.yaml` when present;
- branch/path/Rust interop helpers that define IDs and Rust arguments;
- Rust source files plus `Cargo.toml` and `Cargo.lock`.

`processed_data_dir` refuses to continue if required parquet files exist but the stored signature does not match the current raw/config/Rust inputs. `R/bin/check_processed_cache_signature.R` exposes the same validation for launch scripts, and `R/multiverse.sh hpc` now reruns the Rust job when the signature is stale or missing instead of checking only the number of parquet files. This prevents a controller run from silently fitting models against stale processed files.

## Current status

Implemented:

- local smoke orchestrator: `R/bin/run_local_smoke_checks.R`;
- dependency reports: `R/bin/write_dependency_report.R`;
- data contract: `R/docs/audit_data_contract.md`;
- processed parquet cache signatures: `R/functions/cache_signature.R`;
- targets signature validation before model fitting: `R/_targets.R`;
- launcher-side signature validation: `R/bin/check_processed_cache_signature.R`;
- Rust/HPC signature writing from argument generation and SLURM script generation;
- pipeline readiness report: `R/bin/check_pipeline_readiness.R` writes `pipeline_readiness_report.csv` with raw/config/target/cache/dependency/HPC checks;
- CSV-wide visual coverage: `R/bin/plot_csv_outputs.R` and target `csv_output_plot_files` render a PNG for every analysis CSV, write `figures/csv_outputs/csv_output_plot_manifest.csv`, and build `figures/csv_outputs/index.html` as a quick visual gallery. Saturated `TPR == 1` ROC tables switch to an FPR-centered view so all-perfect detection does not hide false-positive differences.

Still open:

- full tiny end-to-end `targets` smoke mode.
