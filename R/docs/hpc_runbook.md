# HPC Runbook

This runbook prepares the empirical CSE nullification pipeline for SLURM without submitting jobs.

## Preflight

From the repository root:

```sh
Rscript R/bin/hpc_dry_run.R
Rscript R/bin/check_pipeline_readiness.R hpc R/pipeline.yaml R/outputs/analysis/pipeline_readiness_report.csv
```

The dry run checks:

- `R/pipeline.yaml` loads in `hpc` mode;
- configured input resolves to `R/all_indexed.csv`;
- `null_both` is absent;
- current strip methods include `local_median_residual` and `additive_qmap` sensitivities;
- `rust_threads + writer_threads <= slurm.rust.cpus`;
- generated Rust CLI arguments are printable and ready for SLURM wrapping.

The readiness report records pass/warn/fail rows for raw CSV availability, branch contract invariants, `_targets.R` parsing, expected processed parquet coverage, processed cache signature status, dependency availability, and HPC Rust/worker resource settings.

## Rust Processing Job

Generate or inspect the Rust processing command before submission. The dry run prints the equivalent arguments. The SLURM wrapper generator is `R/gen_rust_slurm.sh`; it now resolves `cfg$raw_csv` against the project root, so `all_indexed.csv` is used rather than `data/raw/merged_data.csv`.

Current Rust resource settings in `R/pipeline.yaml`:

- partition: `hpc2019`
- CPUs: `18`
- memory: `32G`
- wall time: `360` minutes
- Rust worker threads: `14`
- writer threads: `4`

## Controller/Analysis Job

The controller settings remain in `R/pipeline.yaml` under `slurm.controller` and `slurm.worker`. Submit only after Rust processing outputs exist and the diagnostic pass/fail policy is confirmed.

## Targets/HPC Behavior

The `_targets.R` graph follows the current `{targets}` file-target guidance: targets that return existing paths use `format = "file"`, so `raw_data_path`, `processed_data_dir`, and generated plot files are tracked by file state rather than hidden side effects. The Rust processed parquet target returns the expected parquet file vector, not just the containing directory, so downstream model targets depend on concrete processed outputs.

The graph also includes `dependency_preflight`, which fails before model work if required R/Rust dependencies are unavailable. `{crew}` remains the orchestration layer for model-fitting targets; per the targets/crew docs, controller configuration is set with `tar_option_set(controller = ...)` in `configure_targets()`.

## Required Evidence Before Submission

1. `Rscript R/bin/hpc_dry_run.R` passes.
2. `Rscript R/bin/run_all_indexed_nullification_smoke.R` completes.
3. `Rscript R/bin/test_branch_validation.R` passes.
4. `cargo test --manifest-path R/rust/Cargo.toml --bin process validate_dataframe -- --nocapture` passes locally or on the login node.

## Open Items

- Full `tar_make()` has not been run in this session.
- Generated SLURM controller script still needs site-specific review before submission.
- Cluster module/Rust/R library environment must match the dependency report.
