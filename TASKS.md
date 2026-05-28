# Agentic Audit Workflow

This workflow turns the open questions in `tasks.md` into auditable, reproducible work packages. The goal is to decide whether TPR/FPR claims are scientifically interpretable, especially for `null_interaction` branches produced by `shuffle` and `additive_qmap`.

## Ground Rules

- Treat every rate as an estimand: define the numerator and denominator before computing TPR/FPR.
- Treat CSE definition as a scientific decision, not a code detail.
- Preserve participant-level clustering unless a task explicitly tests the consequence of removing it.
- Never pool `present`, `null_interaction:shuffle`, `null_interaction:additive_qmap`, `null_interaction:additive_qmap_trial_bin`, `null_interaction:local_mean_residual`, and `null_interaction:local_median_residual` without labeling the null-generation mechanism.
- Report model failure, non-convergence, and singularity as outcomes, not just exclusions.
- Every task must produce a small artifact: table, plot, test, decision note, or code patch.

## Current Statistical Aim And Implementation Plan

The primary scientific target for the next implementation pass is methodological, not a claim that a cognitive-control mechanism has been removed. Treat CSE as the location interaction term `cong:prev_cong` that common forced-choice RT analyses test. The main operating-characteristic questions are:

- Under a defensible no-location-interaction null, which common analytical strategies inflate FPR?
- Under known location-interaction alternatives, which strategies deflate TPR?
- How do outlier filtering, RT transformation, model specification, singularity handling, and participant subsampling change these rates?
- Which failures are statistical failures of the analysis strategy versus failures of the null-generation mechanism?

Empirical design inventory from `R/all_indexed.csv` (streamed 2026-05-22): 979,841 rows, 1,817 participant IDs, five study tags inferred from `participant_id` (`yang`, `primeprobe`, `zhang`, `zeng`, `flanker`). Core columns are `trial_index`, `block_index`, `cong`, `prev_cong`, `participant_id`, `rt`, `correct`, and `prev_correct`. `rt` has 20,431 `NA` values (2.1%); `prev_cong`/`prev_correct` have 2,945 `NA` values (0.3%). `is_first_trial`, `name`, `congruency`, and `condition` are study-specific or sparse and should be used only after study-aware validation.

Data construction inventory from `R/mverse_indexed.Rmd` (inspected 2026-05-22): `all_indexed` is a row-bind of five forced-choice conflict/prime-probe RT datasets. `flanker` and `primeprobe` compute RT as response timestamp minus stimulus/probe onset; `zeng`, `zhang`, and `yang` use provided RT columns. `cong` is harmonized to +/-1, but Zhang collapses `low_conflict` and `high_conflict` into -1 and `no_conflict` into +1. Block indices are not semantically identical across studies: Zeng uses condition changes, Zhang uses pause markers, Yang uses inferred probe sequence starts, and flanker/primeprobe use raw block labels. The file was written with `write.csv()`, so the unnamed first column is an export row index, not a design variable.

### Data-construction constraints for empirical nullification

- [x] Do not trust `prev_cong`, `prev_correct`, or `is_first_trial` until they are audited/recomputed study-wise.
  - In `R/mverse_indexed.Rmd`, lagged previous-trial fields are generally computed with `lag()` outside `group_by(participant_id)`; this can leak previous-trial values across participant boundaries and, after filtering, across non-task rows.
  - Recompute canonical previous-trial fields by study and participant after sorting by the appropriate trial/block order, then compare against the provided columns.
  - Artifact: `R/outputs/analysis/previous_trial_column_audit.csv` with mismatch rates by study and participant-boundary checks.
  - Debrief 2026-05-22: implemented `R/functions/design_integrity.R` and `R/bin/design_integrity_audit.R`; generated `R/outputs/analysis/previous_trial_column_audit.csv` with 1,817 participant rows and nonzero `prev_cong`/`prev_correct` mismatch rates in every study.

- [x] Make all nullification study-aware and source it from `R/all_indexed.csv`.
  - Preserve each study's observed trial counts, participant structure, block definition, practice labeling, congruency balance, correctness pattern, and missingness policy separately.
  - Do not pool study-specific columns (`condition`, `congruency`, `name`, `is_practice`, `is_first_trial`) unless the plan defines a harmonized meaning.
  - Validate RT units and impossible values by study before fitting nuisance distributions; current `rt` includes zero/nonpositive values and extreme maxima.
  - Artifact: `R/docs/data_construction_constraints.md` plus study-level RT/missingness tables.
  - Debrief 2026-05-22: added `R/docs/data_construction_constraints.md` and generated `R/outputs/analysis/design_inventory.csv`; current audit flags nonpositive RT rows in primeprobe/yang/zeng/zhang and missing RT concentrated in yang/zeng/zhang.

- [x] Upgrade nullification diagnostics to include design-integrity checks before residual CSE checks.
  - Required checks: no cross-participant previous-trial leakage, first trial per participant/block has missing previous-trial fields, all four `cong x prev_cong` cells have adequate counts after preprocessing, and row order is preserved by nullification.
  - Required output columns: study, participant count, row count, missing RT rate, nonpositive RT count, previous-trial mismatch rate, cell-count minimum, and warnings.
  - Artifact: add these fields to `R/outputs/analysis/nullification_diagnostics.csv` or an upstream `design_integrity_diagnostics.csv`.

### Primary estimand for FPR/TPR work

- [x] Make the primary estimand the participant-level location interaction in RT or log RT:
  - Cell-mean contrast: `(mean_11 - mean_1m) - (mean_m1 - mean_mm)` using `cong` and `prev_cong` coded as +/-1.
  - Model coefficient: the `cong:prev_cong` fixed effect under the specified coding and transformation.
  - Artifact: update `R/docs/cse_estimand.md` with a short addendum saying distributional CSE is diagnostic/sensitivity, while FPR/TPR operating characteristics are primary for the location interaction most common in rmANOVA/lmer/basic models.

### Empirical nullification as the primary counterfactual

- [x] Build the empirical CSE-stripping path from `R/all_indexed.csv`, not reduced merged CSVs.
  - Preserve observed participant IDs, study labels, row order, `trial_index`, `block_index`, `cong`, recomputed canonical `prev_cong`, correctness columns, missing first-trial structure, source rows, and participant cell counts.
  - Drop or flag rows with missing/nonpositive RT only according to an explicit preprocessing policy, but keep that policy upstream of all analytical branches.
  - Strip CSE in the raw collected data before participant subsampling, outlier filtering, RT transformation, or model fitting.
  - Artifact: canonical nullification input table derived from `R/all_indexed.csv` plus `R/outputs/analysis/design_inventory.csv`.
  - Debrief 2026-05-22: implemented isolated smoke simulation generator in `R/functions/location_operating_characteristics.R`; retain it only as calibration. Full empirical nullification integration with `all_indexed.csv` remains open.

- [x] Treat CSE stripping as the primary empirical counterfactual, not as assumption-free simulation.
  - `additive_qmap`: stationary distributional CSE stripper when preservation diagnostics pass.
  - `additive_qmap_trial_bin`: time-local qmap stripper with stationary fallback for sparse participant bins; candidate distributional stripper once diagnostics pass.
  - `local_mean_residual`: time-local location-only residual stripper with participant fallback; best for mean CSE in bounded smoke, but leaves residual time-bin quantile CSE.
  - `local_median_residual`: time-local central-quantile residual stripper with participant fallback; current primary candidate after passing full `R/all_indexed.csv` no-transform/no-outlier preservation gates.
  - `shuffle`: mechanical exchangeability stress test because it preserves `(participant_id, cong)` RT multisets but not all non-CSE structure.
  - simulated `null_location`: calibration/smoke path for known-truth behavior, not the primary realism source.
  - Artifact: labels in result tables must distinguish empirical nullification, empirical unstripped branches, and known-truth simulation calibration.

### Analysis strategies to evaluate

- [x] Implement a focused analysis multiverse aligned with common CSE practice.
  - rmANOVA on participant-level 2x2 cell means, with complete-cell participant policy documented.
  - Basic fixed-effect regression/ANOVA variants on trial-level RT/log RT with participant fixed effects or participant-clustered standard errors if available.
  - LMM variants: random intercept only; random intercept plus by-participant `cong`, `prev_cong`, and `cong:prev_cong` slopes; prespecified reduced fallback for singular/non-converged maximal fits.
  - Transformations: raw RT and log RT as primary; other transformations only as sensitivity.
  - Outliers: none, fixed plausible range, SD, and MAD filters as currently planned, with row and participant loss recorded.
  - Artifact: strategy table with exact formulas, denominator rules, and coefficient extraction rules.
  - Debrief 2026-05-22: added `R/functions/analysis_strategy_table.R`, `R/bin/write_analysis_strategy_table.R`, `R/docs/analysis_strategy_table.md`, and generated `R/outputs/analysis/analysis_strategy_table.csv` covering rmANOVA, participant fixed-effect LM, random-intercept LMM, and maximal LMM on raw/log RT.
  - Debrief 2026-05-28: added `focused` mode to `R/pipeline.yaml` with sample size 1, raw/log RT, none/sd_2.5 outliers, `present` plus `null_interaction`, and primary `local_median_residual` plus `shuffle` sensitivity. Added `R/bin/write_focused_branch_manifest.R`; dry run writes `R/outputs/analysis/focused_branch_manifest.csv` with 48 model branches and 12 unique data branches.

### Operating-characteristic outputs

- [x] Estimate FPR-like and detection operating characteristics over nullified empirical datasets and analysis branches.
  - Nullification-based FPR: significant `cong:prev_cong` under a validated stripped empirical counterfactual divided by all planned branches, with conditional and failure-aware variants reported separately.
  - Detection/attenuation contrast: compare unstripped empirical branches against stripped branches, stratified by strip method and diagnostics rather than treating simulation as the sole source of truth.
  - Report failure composition: usable nonsignificant, significant, singular, non-converged, extraction error, preprocessing empty-cell failure, and other error.
  - Stratify by model family, transformation, outlier method, sample fraction, study, and nullification mechanism.
  - Artifact: `R/outputs/analysis/operating_characteristics.csv` and plots with labels such as `local_median_residual nullification-based FPR`, plus separate calibration labels only for simulation rows.
  - Debrief 2026-05-28: added `R/functions/nullification_operating_characteristics.R` and `R/bin/write_nullification_operating_characteristics.R`; builder joins model results with nullification diagnostics, gates by `nullification_verdict`, and writes coarse/sample/outlier/failure-aware FPR tables. Synthetic smoke with full `local_median_residual` diagnostics wrote CSV outputs to `/tmp/tdk_nullification_oc`. `_targets.R` now includes file targets for `nullification_diagnostics_file` and `nullification_operating_characteristics_files`, so full pipeline runs produce diagnostics and FPR tables as tracked artifacts.
  - Debrief 2026-05-22: implemented isolated summary layer in `R/functions/operating_characteristics_summary.R` and `R/bin/summarise_operating_characteristics.R`; generated `R/outputs/analysis/smoke_operating_characteristics_summary.csv` with unconditional, conditional, failure-composition, and `truth_interaction_log` effect-size stratification columns for smoke FPR/TPR rows. Full branch stratification remains open.
  - Debrief 2026-05-26: `R/functions/analysis.R` now carries `rate_source`, `nullification_verdict`, `is_interpretable_nullifier`, and FPR `rate_label`; FPR tables distinguish `nullification-based FPR` from diagnostic nullification detection rates and stratify by preservation-gate status.

### Decision gates before full runs

- [x] Run a tiny smoke simulation before any broad target run.
  - Use a small subset of studies/participants and a small number of simulated replicates.
  - Verify generated null has near-zero mean interaction before analysis.
  - Verify generated alternative has the requested interaction size.
  - Verify every model extractor selects the intended `cong:prev_cong` term.
  - Verify failures are counted in unconditional denominators.
  - Artifact: smoke-test command and `R/outputs/analysis/smoke_operating_characteristics.csv`.
  - Debrief 2026-05-22: smoke command `Rscript R/bin/location_smoke_simulation.R R/all_indexed.csv R/outputs/analysis flanker 3 8` runs and writes `smoke_operating_characteristics.csv` with `null_location` and `present_location` rows for simple participant-fixed LM and cell-mean t-test extractors. The runner now also accepts comma-separated alternative log-interaction sizes, e.g. `0.02,0.06`.

## Phase 0 - Natural Next Steps

- [x] Add residual-nullification diagnostics for every processed parquet.
  - Compute participant-level and pooled CSE after `present`, `shuffle`, and `additive_qmap` processing.
  - Artifact: `R/outputs/analysis/nullification_diagnostics.csv`.
  - Debrief 2026-05-22: added `R/functions/nullification_diagnostics.R`, `R/bin/test_nullification_diagnostics.R`, and `R/bin/run_nullification_diagnostics.R`; smoke outputs verify zero CSE under synthetic null, known location CSE under synthetic present data, and metadata parsing over processed-style filenames.
  - Debrief 2026-05-26: diagnostics now report studies, row-order monotonicity, current/previous marginal effects, lag-1 autocorrelation, trial RT slope, block mean spread, transition imbalance, post-error slowing, max time-bin median quantile CSE, paired deltas from the matching `present` branch, `preservation_pass`/`preservation_warnings`, and `nullification_verdict`. Bounded `R/all_indexed.csv` smoke runs show stationary `additive_qmap` removes mean CSE but fails preservation gates for current-congruency marginal effect, time-bin quantile CSE, and post-error slowing; `local_mean_residual` removes mean CSE with better marginal preservation but still fails the time-bin quantile CSE gate; full `R/all_indexed.csv` no-transform/no-outlier smoke shows `local_median_residual` passes all current gates (`mean_cse = 0.83 ms`, `q050_cse = 0.5 ms`, `max_abs_timebin_q050_cse = 4.31 ms`) and is marked `interpretable_nullifier`.

- [x] Add tests proving `shuffle` and `additive_qmap` remove known simulated CSE.
  - Include location-only CSE, quantile-dependent CSE, time-varying CSE, and autocorrelated RT noise.
  - Artifact: Rust tests and a small R simulation script.
  - Debrief 2026-05-22: added Rust adversarial tests in `R/rust/tests/nullification_validity.rs` for shuffle mechanics and qmap location/scale/tail/quantile-crossing/mixed-shape nullification.
  - Debrief 2026-05-27: added synthetic time-varying/autocorrelated CSE fixture and targeted Rust tests for `additive_qmap_trial_bin` and `local_median_residual`; `cargo test --manifest-path R/rust/Cargo.toml --test nullification_validity time_varying -- --nocapture` passes.

- [x] Add conditional, unconditional, and failure-aware TPR/FPR tables.
  - Conditional: significant among numerically usable branches.
  - Unconditional: significant among all planned branches, failures counted separately or as non-detections by policy.
  - Failure-aware: significant, usable non-significant, singular, non-converged, and error proportions.
  - Artifact: new analysis tables and plot labels.
  - Debrief 2026-05-26: added `compute_failure_aware_nullification_rates()` to `R/functions/analysis.R` and included `nullification_failure_aware_rates` in analysis outputs. Synthetic check verifies significant, usable-nonsignificant, singular, non-converged, unconditional, and conditional components.

- [x] Define the LMM singularity/fallback policy before rerunning final results.
  - Fit maximal model first; record singularity; fit a prespecified reduced model only as an explicit fallback/sensitivity result.
  - Artifact: `R/docs/model_convergence_policy.md`.
  - Debrief 2026-05-22: added `R/docs/model_convergence_policy.md` defining primary unconditional denominators, conditional rates, failure-aware composition, maximal-first LMM fitting, `isSingular()`/`rePCA()` requirements, and fallback-as-sensitivity rules. Schema/fallback implementation remains open.

- [ ] Stabilize the pipeline run, target orchestration, cache invalidation, and dependencies.
  - Make the pipeline reliably rerunnable locally before scientific interpretation.
  - Artifact: reproducible smoke-test command and documented cache/dependency policy.
  - Debrief 2026-05-22: added lightweight local smoke orchestrator `R/bin/run_local_smoke_checks.R` and `R/docs/pipeline_reproducibility.md`; smoke command passes without full targets/Rust compilation. Full targets orchestration and automatic cache invalidation remain open.
  - Debrief 2026-05-28: added processed parquet cache signatures in `R/functions/cache_signature.R`; `R/_targets.R` now computes the current signature before `processed_data_dir` and refuses model fitting if existing parquet files do not match current raw/config/Rust inputs. `R/functions/rust_interop.R` and `R/gen_rust_slurm.sh` write `R/data/metadata/processed_cache_signature.rds` when Rust arguments are generated. Added launcher validator `R/bin/check_processed_cache_signature.R`; `R/multiverse.sh hpc` now reruns Rust when the signature is stale/missing instead of checking only parquet file count. Added `R/bin/test_processed_cache_signature.R` to local smoke checks; narrow signature check, launcher validator, `_targets.R` parse check, shell syntax check, and `R/bin/hpc_dry_run.R` pass.
  - Debrief 2026-05-28: added `R/functions/pipeline_readiness.R`, `R/bin/check_pipeline_readiness.R`, and `R/bin/test_pipeline_readiness.R`. The readiness report writes pass/warn/fail checks for raw CSV availability, branch counts/contracts, `_targets.R` parsing, processed parquet coverage, processed cache signature status, dependency readiness, and HPC Rust/worker resources. Focused no-dependency readiness check writes `pipeline_readiness_report.csv` and currently warns only that focused processed parquet files are absent.

## Phase 1 - What Exactly Is CSE?

- [x] Define the CSE estimand before selecting a nullification method.
  - Is CSE only the mean/location interaction `cong:prev_cong`?
  - Is CSE a distributional effect: location plus scale, skew, tails, or quantile-specific differences?
  - Is CSE stationary across the experiment, or does it vary over trial number, block, fatigue, practice, or conflict history?
  - Does the target effect include only previous-trial congruency, or also feature repetitions, response repetitions, stimulus repetitions, contingency learning, and episodic binding?
  - Artifact: `R/docs/cse_estimand.md` with a mathematical definition and inclusion/exclusion criteria.
  - Debrief 2026-05-22: added `R/docs/cse_estimand.md` defining participant-conditioned location and quantile CSE diagnostics, method-specific validity criteria, inclusion/exclusion rules, and canonical CSE/exchangeability references. Empirical column inventory and final sign-off remain open.

- [x] Compare location-only and distributional definitions of CSE.
  - Location-only: participant-level 2x2 cell-mean interaction after +/-0.5 coding.
  - Distributional: quantile interaction curves, delta plots, shift functions, variance/tail differences, and cell-specific RT distribution shapes.
  - Check whether qmap is trying to remove distributional CSE or only mean CSE.
  - Artifact: CSE diagnostics by mean, median, selected quantiles, and distributional distance.
  - Debrief 2026-05-28: added `R/functions/cse_definition_comparison.R`, `R/bin/write_cse_definition_comparison.R`, and `R/bin/test_cse_definition_comparison.R`. The comparison pivots nullification diagnostics into location, distributional, and time-local distributional metric families, writes `cse_definition_metrics_long.csv` plus `cse_definition_comparison.csv`, and flags branches where quantile/time-local residuals remain despite near-zero mean CSE. `_targets.R` now includes `cse_definition_comparison_files` after nullification diagnostics so full pipeline runs track these artifacts. Smoke, CLI, and `_targets.R` parse checks pass, and local smoke checks now include the comparison test.

- [x] Decide whether CSE should be removed trial-to-trial, within bins, or altogether.
  - Trial-to-trial: model or residualize local sequential effects while preserving temporal dependence.
  - Binned: remove CSE within trial-number bins, blocks, or time windows to respect nonstationarity.
  - Altogether: remove one stationary participant-level effect over the full session.
  - Artifact: decision note comparing scientific validity and statistical consequences.
  - Debrief 2026-05-25: `R/docs/nullification_scope_decision.md` now defines empirical raw-data-first CSE stripping as the primary counterfactual and keeps `shuffle` as an exchangeability stress test. Debrief 2026-05-26: implemented Rust `additive_qmap_trial_bin` as a time-local qmap stripper with stationary fallback for sparse participant bins, and `local_mean_residual` as a time-local location-only residual stripper with participant fallback.

- [x] Audit confounds that can masquerade as CSE.
  - Feature integration/repetition, response repetition, stimulus repetition, contingency learning, post-error slowing, conflict frequency, block effects, and trial index.
  - Verify whether raw data has columns needed to distinguish these mechanisms; if not, document the limitation.
  - Artifact: raw-data column inventory plus confound-risk table.
  - Debrief 2026-05-22: added `R/functions/data_inventory.R`, `R/bin/write_data_inventory.R`, `R/docs/confound_risk_inventory.md`, `R/outputs/analysis/raw_column_inventory.csv`, and `R/outputs/analysis/confound_risk_table.csv`. Combined data supports statistical `cong:prev_cong` location analyses but lacks global stimulus identity, response identity/repetition, feature-integration, and item-level contingency-learning variables.

## Phase 2 - Pipeline And Data Contract Audit

- [x] Map the complete data lifecycle from raw CSV to final analysis tables.
  - Files: `R/run.R`, `R/_targets.R`, `R/functions/config.R`, `R/functions/paths.R`, `R/functions/results.R`, `R/rust/src/lib.rs`.
  - Verify raw columns, processed parquet columns, branch IDs, data IDs, result schema, and analysis table names.
  - Artifact: `R/docs/audit_data_contract.md` with one row per pipeline step.
  - Debrief 2026-05-22: added `R/docs/audit_data_contract.md` documenting `R/mverse_indexed.Rmd` -> `R/all_indexed.csv` -> design/confound inventories -> branch validation -> processed branch naming -> nullification diagnostics -> result schema -> operating-characteristic summaries, with required columns and current implemented checks.

- [x] Validate branch generation against the intended multiverse.
  - Check that `present` only uses `strip_method = none`, while `null_interaction` only uses `shuffle`, `additive_qmap`, `additive_qmap_trial_bin`, `local_mean_residual`, and `local_median_residual`; `null_both` must be absent.
  - Check expected branch counts by sample size, transformation, outlier, model, effect condition, and strip method.
  - Artifact: branch-count table and assertion script.
  - Debrief 2026-05-22: added isolated branch invariant checks in `R/functions/branch_validation.R` and `R/bin/test_branch_validation.R`; narrow test validates strip-method constraints, unique branch IDs, model-agnostic data branch counts, and expected rows for a synthetic config.

- [x] Lock processing order to match the empirical counterfactual estimand.
  - Required Rust order: nullify CSE in raw collected data, then participant subsample, then outlier filter, then transform.
  - Do not move stripping after subsampling, filtering, or transformation: the counterfactual event is that the collected raw data itself had population CSE = 0.
  - Diagnostics should still report whether downstream log-transform/outlier branches reintroduce apparent residual CSE.
  - Artifact: decision note and sensitivity plan.
  - Debrief 2026-05-25: `R/docs/nullification_scope_decision.md` records that raw-data-first stripping is intentional and primary, not a provisional ordering choice.

- [ ] Solidify targets orchestration, caching, and dependency tracking.
  - Make `R/run.R` and `R/_targets.R` agree on project root, script path, state files, and generated config.
  - Ensure Rust output files are real `targets` dependencies, not only external side effects.
  - Decide when processed parquet files should be invalidated: Rust source changes, config changes, raw data changes, qmap algorithm changes, or dependency version changes.
  - Add a local smoke-test mode that runs end-to-end on a tiny dataset without HPC assumptions.
  - Artifact: `R/docs/pipeline_reproducibility.md` plus a smoke-test command.
  - Debrief 2026-05-22: `R/docs/pipeline_reproducibility.md` records cache invalidation triggers and local smoke command `Rscript R/bin/run_local_smoke_checks.R`; command currently covers lightweight assertion scripts, not a full tiny targets end-to-end run.
  - Debrief 2026-05-27: `R/pipeline.yaml` now points `raw_csv` to `all_indexed.csv`, strip methods use current names (`additive_qmap`, `additive_qmap_trial_bin`, `local_mean_residual`, `local_median_residual`), `null_both` is absent, `load_config()` no longer depends on unexported `.makeCC`, and Rust argument construction smoke-checks include `local_median_residual`. Added bounded empirical smoke `R/bin/run_all_indexed_nullification_smoke.R`; it runs Rust on `all_indexed` subset and validates pooled residual CSE, while strict preservation gates can warn on the subset. Added HPC dry run `R/bin/hpc_dry_run.R` and `R/docs/hpc_runbook.md`; dry run validates `all_indexed.csv`, current strip methods, no `null_both`, and Rust/SLURM thread counts without submitting jobs. `_targets.R` now follows `{targets}` file-target semantics by returning concrete processed parquet paths from the Rust output target and adds a `dependency_preflight` target before raw/model work. Full expensive `tar_make()`/SLURM submission remains open.
  - Debrief 2026-05-28: added `R/functions/cache_signature.R`, `R/bin/test_processed_cache_signature.R`, and signature validation in `R/_targets.R`; processed parquet files are now gated by a signature over raw input, Rust args, expected data IDs, config files, branch/path/Rust interop helpers, Rust source, and Cargo manifests. Rust argument generation and HPC SLURM generation write the signature file so controller-side targets can detect stale or missing processed caches before model fitting. Added `R/bin/check_processed_cache_signature.R` and wired `R/multiverse.sh hpc` to submit Rust when the signature is not current, not just when parquet count is low. Narrow checks pass: cache-signature unit script, launcher validator after writing a focused signature, focused signature build, `_targets.R` parse, `bash -n R/multiverse.sh`, and `R/bin/hpc_dry_run.R`.
  - Debrief 2026-05-28: added `R/functions/pipeline_readiness.R`, `R/bin/check_pipeline_readiness.R`, and `R/bin/test_pipeline_readiness.R`. The readiness report writes pass/warn/fail checks for raw CSV availability, branch counts/contracts, `_targets.R` parsing, processed parquet coverage, processed cache signature status, dependency readiness, and HPC Rust/worker resources. Focused no-dependency readiness check writes `pipeline_readiness_report.csv` and currently warns only that focused processed parquet files are absent.

- [ ] Design a canonical project CLI and package layout.
  - Replace ad hoc `Rscript R/bin/*.R` and shell entry points with a structured CLI in a language with a stronger module/package system.
  - Evaluate Python + `pyproject.toml`/`uv` as the orchestration layer, with R called through a kernel/session bridge where possible and Rust kept as the high-throughput processing engine.
  - Define stable commands for dependency checks, data indexing, Rust processing, diagnostics, dashboards, targets preparation, and HPC dry-run/submission.
  - Artifact: `R/docs/canonical_cli_design.md` with command taxonomy, module layout, dependency strategy, migration plan, and which existing `R/bin`/`.sh` scripts become deprecated wrappers.

- [x] Make dependency management explicit.
  - Record R package versions, Rust toolchain version, Cargo lock status, Polars version, and Arrow/parquet compatibility.
  - Avoid runtime package installation inside analysis runs unless intentionally sandboxed.
  - Add a dependency check target that fails early with actionable messages.
  - Artifact: dependency report and setup instructions.
  - Debrief 2026-05-22: added `R/functions/dependency_report.R`, `R/bin/write_dependency_report.R`, `R/bin/check_dependencies.R`, `R/docs/dependency_setup.md`, and generated dependency report CSVs. `Rscript R/bin/check_dependencies.R` passes locally and fails early on missing R packages, Rust tools, Cargo lock, or Cargo metadata errors. Full targets integration remains open.

## Phase 3 - Nullification Validity Checks

- [ ] Add residual-effect diagnostics for every processed data branch.
  - For each processed parquet, fit a lightweight diagnostic model or compute cell-mean/quantile contrasts before the main model.
  - Required columns: data ID, effect condition, strip method, sample size, outlier, transformation, cell counts, mean CSE, quantile CSEs, distributional distances, and warnings.
  - Artifact: `nullification_diagnostics.csv`.
  - Debrief 2026-05-22: implemented dataframe-level diagnostic helper and directory runner for participant mean CSE, pooled quantile CSEs, cell-count minimum, invalid-row counts, warnings, and processed filename metadata parsing. Needs execution over all real processed parquet branches.

- [x] Test whether `shuffle` is valid for time-dependent data.
  - Shuffling within `(participant_id, cong)` assumes exchangeability within those groups.
  - Check whether RT has autocorrelation, trial trends, block trends, post-error effects, or slow drifts that make unrestricted within-group shuffling invalid.
  - Compare unrestricted shuffle with blockwise shuffle, trial-bin shuffle, circular shifts, and model-based residual permutation.
  - Artifact: exchangeability diagnosis and shuffle sensitivity table.
  - Debrief 2026-05-27: added `R/functions/shuffle_exchangeability_diagnostics.R` and `R/bin/write_shuffle_exchangeability_diagnostics.R`; full `R/all_indexed.csv` diagnostics show lag-1 autocorrelation, trial slopes, post-error effects, and block spread, so unrestricted shuffle is marked `unrestricted_shuffle_plausible = FALSE` for every study and retained only as a secondary robustness check.

- [x] Test `shuffle` nullification adversarially.
  - Verify it preserves RT multiset within `(participant_id, cong)` groups.
  - Verify it removes the association between RT and `prev_cong` conditional on `(participant_id, cong)` within Monte Carlo tolerance.
  - Check determinism across repeated runs with the same seed and independence across different `subsample_id` values.
  - Artifact: Rust tests plus empirical diagnostics.
  - Debrief 2026-05-22: Rust tests in `R/rust/tests/nullification_validity.rs` verify multiset preservation, participant boundaries, fixed-seed determinism, `subsample_id`-dependent strip seeds, and synthetic conditional association removal. Empirical exchangeability diagnostics remain open; see `R/docs/shuffle_exchangeability_policy.md`.
  - Debrief 2026-05-28: added empirical processed-output checks in `R/functions/shuffle_adversarial_diagnostics.R`, CLI wrapper `R/bin/write_shuffle_adversarial_diagnostics.R`, and smoke test `R/bin/test_shuffle_adversarial_diagnostics.R`. Diagnostics pair each `null_interaction:shuffle` branch with its matching `present:none` branch, verify RT multiset preservation within `(participant_id, cong)`, estimate the residual `prev_cong` RT slope conditional on participant x current congruency, and write `shuffle_adversarial_diagnostics.csv`. `_targets.R` now tracks `shuffle_adversarial_diagnostics_file`; smoke, CLI, and parse checks pass.

- [x] Test `additive_qmap` nullification adversarially.
  - Verify additive qmap reduces the `cong:prev_cong` interaction to approximately zero on known-effect fixtures while preserving marginal `cong` and `prev_cong` effects.
  - Test separate cases: location-only CSE, scale-only CSE, tail-only CSE, quantile-crossing CSE, mixed location+shape CSE, time-varying CSE, and autocorrelated RT noise.
  - Check that row/column marginal curves remain unshrunk in the additive reconstruction and that sparse-cell shrinkage is only used for cell-level quantile assignment.
  - Check small-cell behavior, missing-cell behavior, ties, monotonic interpolation, participant imbalance, source-row order, and sensitivity to `ngrid`/`kappa`.
  - Artifact: Rust unit/property tests and empirical residual-effect table.
  - Debrief 2026-05-22: added Rust tests in `R/rust/tests/nullification_validity.rs` for location-only, scale-only, tail-only, quantile-crossing, mixed location+shape, ties, missing/small cells, participant imbalance, and `ngrid` sensitivity. Debrief 2026-05-25: aligned Rust `additive_qmap` with the R reference by keeping row/column marginal curves unshrunk, preserving the current-congruency main effect while removing the interaction. Debrief 2026-05-26: renamed qmap methods to `additive_qmap`/`additive_qmap_trial_bin` with backward-compatible aliases and removed obsolete shrinkage helpers; property/adversarial qmap tests pass.

- [ ] Decide whether nullification without participant grouping is scientifically useful.
  - Treat ungrouped nullification as a sensitivity check unless the design justifies exchangeability across participants.
  - Compare participant-grouped vs ungrouped nullification on residual CSE, RT distribution preservation, temporal structure, and FPR.
  - Artifact: sensitivity note answering the `tasks.md` question.

## Phase 4 - Resampling And Rate Estimand Audit

- [x] Separate multiverse rates from population operating characteristics.
  - Current subsampling reuses the empirical dataset, so branches are dependent analytic paths, not independent simulated experiments.
  - Decide labels: e.g. `multiverse detection proportion` vs `estimated TPR/FPR`.
  - Artifact: estimand note used in reports and plot labels.
  - Debrief 2026-05-25: `R/docs/rate_estimand_labels.md` now separates unstripped empirical detection proportions, empirical nullification-based FPR, and simulation calibration rows, with denominator labels and required `rate_source` semantics.

- [x] Add unconditional and conditional rates.
  - Conditional rate: significant among numerically usable branches.
  - Unconditional conservative rate: significant among all planned branches, with failures counted as non-significant or separately by policy.
  - Failure-aware rate: significant, non-significant usable, non-converged, singular, and error proportions all reported together.
  - Artifact: rate tables by model, transformation, sample size, outlier, effect condition, and null type.

- [ ] Evaluate requested resampling expansions.
  - Test 50x resampling at 5%, 10%, and 20% participant fractions.
  - For empirical data, report dependence and uncertainty via clustered/bootstrap intervals, not as independent experiment replications.
  - If true TPR/FPR operating characteristics are needed, add simulated datasets with known ground truth.
  - Artifact: resampling design memo and pilot results.
  - Debrief 2026-05-22: added `R/functions/resampling_design.R`, `R/bin/write_resampling_design.R`, `R/docs/resampling_design.md`, and generated `R/outputs/analysis/resampling_design_table.csv` with 150 planned empirical paths. Actual pilot model results remain open.

## Phase 5 - Model Fitting, Extraction, And Convergence Audit

- [x] Audit LMM convergence and singularity handling.
  - Current analysis excludes non-converged and singular fits from TPR/FPR denominators.
  - Decide whether a boundary random-slope variance of zero under nullification is a failure, an expected result, or a trigger for a preregistered simpler model.
  - Use `lme4::isSingular()` and `rePCA()` rather than exact zero checks only.
  - Artifact: convergence policy with primary and sensitivity estimands.
  - Debrief 2026-05-22: policy decision recorded in `R/docs/model_convergence_policy.md`: boundary/singular primary fits are analysis outcomes included in unconditional denominators; fallback models are sensitivity-only and must not overwrite primary inference. Added `full_is_singular`, `null_is_singular`, `full_repca_min_sd`, and `null_repca_min_sd` result fields in `R/functions/results.R`, with `R/functions/analysis.R` using `full_is_singular` when available.

- [x] Implement a principled fallback for maximal LMM failures.
  - Recommended policy: fit maximal model; if singular because a random slope is estimated at zero, record it and fit a prespecified reduced random-effects model for inference.
  - Do not silently replace the primary result; store both maximal and fallback outcomes.
  - Artifact: model-policy note plus result schema proposal.
  - Debrief 2026-05-22: `R/docs/model_convergence_policy.md` now proposes fallback levels and result fields (`fallback_level`, `fallback_formula`, `fallback_p_value`, `inference_source`). Debrief 2026-05-26: implemented convenience/sensitivity-only reduced-random-effects fallback storage for LMMs in `R/functions/models.R`/`R/functions/results.R`; primary inference remains maximal-model output, fallback p-values are stored separately when singular/non-converged maximal fits trigger a derivable reduced random-effects formula, and fallback fits are invalid for main-analysis FPR/TPR denominators.

- [x] Check random-effect variance extraction.
  - Inspect `R/functions/results.R` around random slope extraction; verify `VarCorr()` parsing for `cong`, `prev_cong`, and `cong:prev_cong` slopes.
  - Confirm singularity detection catches near-zero variances and rank-deficient covariance matrices.
  - Artifact: extraction tests using synthetic fitted models.
  - Debrief 2026-05-22: added `R/functions/random_effect_extraction.R`, integrated named `VarCorr()` stddev extraction into `R/functions/results.R`, and added mock tests in `R/bin/test_random_effect_extraction.R`. This fixes name-based extraction for `(Intercept)` and `cong:prev_cong`; near-zero/rank-deficient singularity policy still remains for `R/docs/model_convergence_policy.md`.
  - Debrief 2026-05-26: expanded result schema/extraction to store named `cong`, `prev_cong`, and `cong:prev_cong` random-slope variances separately (`random_cong_slope_var`, `random_prev_cong_slope_var`, `random_cse_slope_var`, with null-model counterparts) while keeping `random_slope_var` as the legacy CSE alias. Narrow schema and mock extraction checks pass.

- [x] Check fixed-effect extraction.
  - Verify that selecting the first coefficient containing `:` always returns the intended CSE term for all models.
  - Verify rmANOVA extraction does not rely on `slice(3)` if row order can change.
  - Artifact: extraction tests and a safer term-selection rule.
  - Debrief 2026-05-22: added safer selection helpers in `R/functions/cse_term_extraction.R`, integrated them into `R/functions/results.R`, and added tests in `R/bin/test_cse_term_extraction.R`; tests verify exact/reversed `cong:prev_cong`, ignore unrelated interactions, error on duplicates, avoid rmANOVA row-order dependence, and source-load with `results.R`.

## Phase 6 - Code Hygiene And Maintainability

- [x] Add narrow tests around branch IDs, data IDs, and path helpers.
  - Include sample sizes with floating-point formatting and model names containing separators.
  - Artifact: R unit tests or standalone assertion scripts.
  - Debrief 2026-05-22: added `R/bin/test_id_path_helpers.R`; checks branch ID sample-size formatting, model names containing safe separators, model-free `data_id` derivation, bad branch ID errors, project path initialization, processed-data paths, results paths, and timestamped analysis paths.

- [x] Remove or quarantine obsolete implementations.
  - `quantile_map_shrink()` appears unused and misleading; either delete it after tests or move it to an archive with a warning.
  - Artifact: code cleanup patch with tests.
  - Debrief 2026-05-26: deleted obsolete Rust `quantile_map_shrink()`, `process_participant_qmap()`, and old `compute_quantiles()` helper after the active additive qmap path passed property/adversarial checks.

- [ ] Separate scientific logic from orchestration glue.
  - Keep pure branch-generation, result-extraction, and analysis-summary functions testable without running `targets`.
  - Artifact: small refactor plan and test list.

- [x] Standardize terminology.
  - Use FPR for null-condition significant proportion, not FDR, unless actually estimating false discovery rate among discoveries.
  - Align labels across `analysis.R`, `analysis_plots.R`, and `analysis_plot_c.R`.
  - Artifact: terminology patch and plot-label review.
  - Debrief 2026-05-22: patched `R/functions/analysis_plot_c.R` to replace FDR labels/columns/titles with FPR terminology and clarify sample-size plot as significance proportion by condition; source-load check passes. `analysis.R` and `analysis_plots.R` already use FPR/TPR terminology in searched locations.

- [x] Add defensive data validation.
  - Validate required columns, condition coding, participant IDs, trial order, missingness, cell counts, RT bounds, and transformed RT validity.
  - Fail early with branch/data IDs in error messages.
  - Artifact: validation functions and smoke tests.
  - Debrief 2026-05-22: added `R/functions/data_validation.R` and `R/bin/test_data_validation.R`; smoke tests cover required columns, +/-1 coding, positive/log-transform-valid RT bounds, nondecreasing participant trial order, and complete participant `cong x prev_cong` cells with context-rich errors.
  - Debrief 2026-05-26: integrated defensive CSV validation into the Rust processor: required columns and participant IDs are fatal, invalid `cong`/`prev_cong` coding is fatal, missing first-trial `prev_cong` and invalid RT rows are logged as explicit warnings for downstream missingness policy. Added Rust validation unit tests.

## Phase 7 - Reporting And Decision Gates

- [x] Create a nullification reliability dashboard.
  - Panels: residual CSE by null method, cell-count imbalance, distribution preservation, temporal autocorrelation preservation, qmap-vs-shuffle differences, and failure rates.
  - Artifact: dashboard files in `R/outputs/analysis/figures`.
  - Debrief 2026-05-27: added `R/functions/nullification_dashboard.R` and `R/bin/plot_nullification_dashboard.R`. Dashboard smoke on full `local_median_residual` diagnostics wrote residual-CSE and preservation-delta PNGs to `/tmp/tdk_nullification_dashboard`; broader multi-method dashboard panels can be generated from full diagnostics CSV after the HPC run.

- [x] Establish pass/fail thresholds before interpreting FPR.
  - Example: median residual CSE near zero, no systematic residual by transformation/outlier/sample size, deterministic output for fixed seed, acceptable cell-count coverage, and acceptable temporal-structure preservation.
  - Artifact: signed-off threshold note.
  - Debrief 2026-05-27: added `R/docs/nullification_pass_fail_thresholds.md` documenting the implemented `preservation_pass`/`nullification_verdict` gate and thresholds from `R/functions/nullification_diagnostics.R`. Primary FPR tables should use only branches with `nullification_verdict == "interpretable_nullifier"`.

- [ ] Recompute final tables only after nullification and convergence policies pass.
  - Required final outputs: conditional TPR/FPR, unconditional TPR/FPR, failure-aware composition, residual-nullification diagnostics, and sensitivity comparisons.
  - Artifact: final audit report with limitations.

## Immediate Red Flags To Investigate First

- [x] The project must first decide whether CSE is a mean/location-only interaction or a distributional/time-dependent effect.
- [x] `additive_qmap` may not be using shrinkage for row/column curves even though `kappa = 5` is configured.
- [x] `additive_qmap` has no direct test proving that it nullifies CSE on known-effect data.
- [x] `shuffle` may be invalid for time-dependent RT data if exchangeability within `(participant_id, cong)` is violated.
- [x] Current TPR/FPR are conditional on usable fits, which can bias inference when nullification causes singularity or non-convergence.
- [x] Full random-slope LMM singularity under a nullified interaction may be expected boundary behavior, not necessarily bad data.
- [x] Empirical participant subsampling creates dependent branches; it should not be described as independent Monte Carlo FPR/TPR without qualification.
- [x] Pipeline cache invalidation and dependency tracking need to be solid before rerunning expensive analyses.

## Sources And Online References To Check

### CSE, Conflict Adaptation, And Alternative Mechanisms

- Gratton, Coles, and Donchin (1992), optimizing information processing and strategic control: https://doi.org/10.1037/0096-1523.18.2.379
- Mayr, Awh, and Laurey (2003), conflict adaptation effects and sequential priming: https://doi.org/10.1038/nn1051
- Egner (2007), conflict adaptation review: https://doi.org/10.1016/j.conb.2007.05.001
- Duthoo, Abrahamse, Braem, Boehler, and Notebaert (2014), review/meta-analysis of congruency sequence effects: search the exact title `The congruency sequence effect 3 decades after Gratton et al. (1992)`.
- Schmidt and De Houwer work on contingency learning and CSE confounds: search `Schmidt De Houwer contingency learning congruency sequence effect`.
- Hommel and feature-integration/event-file accounts: search `Hommel feature integration congruency sequence effect response repetition`.

### RT Distributions, Quantiles, And Time Dependence

- Ratcliff-style RT distribution analysis and quantile/delta-plot methods: search `Ratcliff reaction time distribution quantile delta plot`.
- Shift functions and distributional comparisons for RT data: search `shift function reaction time distributions quantiles`.
- Gilden (2001), cognitive emissions of 1/f noise and long-range RT dependence: https://doi.org/10.1037/0033-2909.108.1.33
- Wagenmakers and Brown work on RT autocorrelation/1/f noise: search `Wagenmakers Brown 1/f noise reaction time`.

### Permutation, Shuffling, And Exchangeability

- Winkler et al. (2014), permutation inference and exchangeability blocks: https://doi.org/10.1016/j.neuroimage.2014.01.060
- Anderson (2001), permutation tests for complex designs: search `Anderson 2001 permutation tests exchangeability experimental design`.
- Maris and Oostenveld (2007), nonparametric permutation for dependent/time-structured data: https://doi.org/10.1016/j.jneumeth.2007.03.024
- Search specifically: `restricted permutation time series circular shift block permutation exchangeability`.

### Mixed Models And Singularity

- `lme4::isSingular()` documentation and references: https://lme4.github.io/lme4/reference/isSingular.html
- Barr et al. (2013), keep it maximal: listed in the `isSingular()` references.
- Bates et al. (2015), parsimonious mixed models: https://arxiv.org/abs/1506.04967
- Matuschek et al. (2017), balancing type I error and power in LMMs: listed in the `isSingular()` references.

### Pipeline, Targets, And Reproducibility

- `{targets}` user manual: https://books.ropensci.org/targets/
- `{targets}` debugging guide: https://books.ropensci.org/targets/debugging.html
- `{targets}` function reference: https://docs.ropensci.org/targets/reference/index.html
- `tarchetypes` reference: https://docs.ropensci.org/tarchetypes/
- `crew` and `crew.cluster` orchestration: https://wlandau.github.io/crew/ and https://wlandau.github.io/crew.cluster/
- Rust/Cargo reproducibility: https://doc.rust-lang.org/cargo/guide/cargo-toml-vs-cargo-lock.html
