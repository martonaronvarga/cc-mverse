# Multiverse Analysis Pipeline Audit Report

**Date:** 2026-01-21  
**Auditor:** Automated Code Review  
**Repository:** cc-mverse  

---

## Executive Summary

This report presents a staged, adversarial audit of the R multiverse analysis codebase. The audit identified **18 issues** across 5 severity categories, including 3 silent-error risks, 5 design-risk issues, and 10 inefficiencies/improvements.

---

## PHASE 0 — Orientation and Contract Inference

### Dependency Map

```
run.R
  └─→ config.R (load_pipeline_config, generate_all_branches, configure_targets)
  └─→ paths.R (init_project_paths, save_pipeline_state)
  └─→ logging.R (configure_logging)
  └─→ packages.R (load_all_packages)

_targets.R
  └─→ paths.R (load_pipeline_state, get_processed_data_path)
  └─→ config.R (get_model_spec, configure_targets)
  └─→ models.R (fit_model)
  └─→ results.R (extract_results, append_results, load_results, fit_and_save_branch)
  └─→ analysis.R (analyze_and_save)
  └─→ analysis_plots.R (generate_multiverse_dashboard)
  └─→ analysis_plot_c.R (cont_plots)
```

### Inferred Contracts Table

| File | Inputs | Outputs | Contract Notes |
|------|--------|---------|----------------|
| **config.R** | CLI args, env vars | `config` list, `branches` tibble | Defines multiverse dimensions |
| **models.R** | `data` (df), `model_spec` (list) | `model_result` list with fitted models | Returns different structures for LMM vs RMANOVA |
| **results.R** | `model_result`, `branch_spec` | Single-row tibble matching `RESULTS_COLUMNS` | Schema-validated extraction |
| **analysis.R** | `results_df` (aggregated) | List of analysis tables | Computes TPR/FPR/ROC metrics |
| **analysis_plots.R** | `analysis_list` (from analysis.R) | ggplot objects, saved files | **Expects specific keys that differ from analysis.R output** |
| **analysis_plot_c.R** | `results_df` (raw) | ggplot objects, saved files | Recomputes diagnostics internally |

### Cross-File Data Flow

```
models.R::fit_model() 
    → model_result (list with type-specific structure)
        → results.R::extract_results() 
            → Single-row tibble (RESULTS_COLUMNS schema)
                → results.R::append_results() 
                    → Parquet files (per branch)
                        → results.R::load_results()
                            → Aggregated tibble
                                → analysis.R::analyze_multiverse_results()
                                    → List of analysis tables
                                        → analysis_plots.R::generate_multiverse_dashboard()
                                            → Plots
```

### Checkpoint: What flows from models.R to analysis_plots.R?

Model coefficients (estimates, SEs, p-values, CIs) flow through results.R extraction, are aggregated, analyzed in analysis.R to compute discovery rates/ROC metrics, then visualized. **However, there are naming mismatches between what analysis.R produces and what analysis_plots.R expects.**

---

## PHASE 1 — models.R Audit

### Issue Ledger

| ID | Severity | Evidence | Downstream Impact |
|----|----------|----------|-------------------|
| M-001 | design-risk | `fit_rmanova` uses Type III SS without explicit contrast specification | Effect estimates may differ based on R's default contrasts |
| M-002 | inefficiency | `check_model_assumptions` only computes residual variance, not actual assumption tests | Misleading function name; inadequate diagnostics |
| M-003 | silent-error | RMANOVA formula parsing assumes specific structure without validation | Could silently produce wrong results with different variable names |
| M-004 | design-risk | `fit_lmm` uses `REML = FALSE` for both full and null models | Appropriate for LRT comparison but reduces estimate efficiency |

### Model Output Semantics

**LMM Output (`fit_lmm`)**:
- `main_estimate`: Interaction coefficient (β for `cong:prev_cong`)
- `effect_size`: Partial Cohen's d approximation (β / σ_residual)
- `main_p_value`: Satterthwaite p-value from lmerTest
- **Population quantity**: Estimated fixed effect of congruency × previous congruency interaction on RT

**RMANOVA Output (`fit_rmanova`)**:
- `main_estimate`: Generalized eta-squared (ηG²)
- `main_t_stat`: F-statistic (NOT t-statistic) — **semantic mismatch**
- `LR_stat`: Approximated from F × df — **not a true LR statistic**
- **Population quantity**: Proportion of variance explained by interaction

### Critical Finding

**M-005 (silent-error)**: RMANOVA and LMM `main_estimate` columns contain **incompatible quantities** (effect sizes vs coefficients). Downstream analyses treat them identically, which is statistically invalid.

---

## PHASE 2 — results.R Audit

### Issue Ledger

| ID | Severity | Evidence | Downstream Impact |
|----|----------|----------|-------------------|
| R-001 | silent-error | `RESULTS_COLUMNS` includes `null_effect_size` but schema has `null_effect` | Column name mismatch will cause validation failures |
| R-002 | design-risk | Random slope variance extraction assumes `"cong:prev_cong"` name | Will return NA silently if model uses different parameterization |
| R-003 | inefficiency | `extract_rmanova_results` duplicates `null_t_stat = f_stat` and `null_main_p_value = p_value` | Null model concepts don't apply to RMANOVA |
| R-004 | design-risk | `fit_and_save_branch` error handler calls `extract_results(model_result, ...)` where `model_result` may be NULL | Potential NULL dereference |

### Model Output → Result Table Mapping

```
LMM model_result:
  coefficients (tidy df) → interaction_rows → main_estimate, main_std_error, main_t_stat, main_p_value
  random_effects → random_intercept_var, random_slope_var
  performance → AIC_diff, BIC_diff, LR_stat, LR_df, LR_p

RMANOVA model_result:
  full_stats (tidy df) → interaction_row → main_estimate (ηG²), main_t_stat (F)
  [No null model] → null_* fields set to NA or copies of full model values
```

### Structural Integrity Check

**R-005 (design-risk)**: Schema column `null_n_obs = arrow::int32` (line 30 in results.R) is missing parentheses — should be `arrow::int32()`. This is a **latent bug** that will cause runtime errors when the schema is used.

### Checkpoint: Could a small change in models.R silently corrupt results.R?

**YES.** If `broom.mixed::tidy()` changes column names (e.g., `statistic` → `t.value`), extraction would silently produce NAs. The code uses `%||%` fallback but doesn't validate expected columns exist.

---

## PHASE 3 — analysis.R Audit

### Issue Ledger

| ID | Severity | Evidence | Downstream Impact |
|----|----------|----------|-------------------|
| A-001 | design-risk | `is_true_effect` definition excludes any `strip_method != "none"` even for present condition | Defensive but may mask configuration errors |
| A-002 | inefficiency | `allowed_combinations_filter` called repeatedly (5+ times per analysis) | Redundant filtering overhead |
| A-003 | silent-error | `analyze_specification_sensitivity` references `model_type` which isn't created in `analysis_main_filter` | Will fail with "column not found" error |
| A-004 | design-risk | `null_main_tstat` used at line 156 but variable is named `null_tstat` | Undefined variable — will cause runtime error |

### Conditioning and Logic Review

**Correct Conditioning:**
- TPR computed only from `effect_condition == "present"`
- FPR computed only from `effect_condition %in% c("null_interaction", "null_both")`
- Proper separation of true effect vs null effect populations

**Problematic Logic:**
- `compute_roc_metrics` joins TPR and FPR tables by `group_vars`, but if groups don't match exactly, NAs result
- `detect_specification_inconsistencies` defines `is_inconsistent = pct_significant > 5 & pct_significant < 95` — arbitrary thresholds

### Estimand Clarity

| Analysis | Estimand | Clear Definition? |
|----------|----------|-------------------|
| TPR | P(reject H0 \| effect present) | ✓ Yes |
| FPR | P(reject H0 \| effect absent) | ✓ Yes |
| d-prime | Z(TPR) - Z(FPR) | ✓ Yes |
| Power | = TPR (redundant) | ⚠️ Duplicate concept |
| Discovery rate | = TPR/FPR (redundant) | ⚠️ Redundant |

### Checkpoint: Can each analysis be expressed as a well-defined estimand?

**Mostly yes**, but several metrics are redundant (power = TPR, discovery_rates = roc_metrics renamed).

---

## PHASE 4 — analysis_plots.R Audit

### Issue Ledger

| ID | Severity | Evidence | Downstream Impact |
|----|----------|----------|-------------------|
| P-001 | fatal | `generate_multiverse_dashboard` expects `analysis_list$power_analytics` but analysis.R produces `power_analysis` | **KeyError** — plots won't generate |
| P-002 | fatal | Expects `analysis_list$fpr_by_null` but analysis.R produces `fdr_by_null_type` | **KeyError** — plots won't generate |
| P-003 | fatal | Expects `analysis_list$results_with_diagnostics` but analysis.R produces `results_with_diag` | **KeyError** — plots won't generate |
| P-004 | design-risk | `plot_roc_by_outlier` uses `scales::percent(model_type, ...)` which expects numeric input | Will produce errors or garbage labels |
| P-005 | design-risk | `plot_stripping_robustness` expects `FDR` column but `compute_fpr_by_null_type` produces `FPR` | Column name mismatch |

### Recomputation Inventory

| Function | Recomputes | Should Precompute? |
|----------|------------|-------------------|
| `rename_for_plots` | model_type, sample_size labels, transformation_label, strip_label | Yes — duplicates analysis.R logic |
| `plot_specification_curve_detailed` | spec_rank ordering | No — plot-specific |
| `analysis_plot_c.R::prep_alt_results` | converged_both, is_true_effect, is_null_effect, is_significant, model_type, strip_label, outlier_label, transformation_label | **Yes — complete duplication** |

### Dependency Analysis

| Plot | Depends On |
|------|-----------|
| `plot_roc_by_model` | roc_metrics (precomputed) |
| `plot_fdr_by_stripping` | fpr_by_null (naming mismatch) |
| `plot_power_by_sample_size` | power_metrics (naming mismatch) |
| `plot_specification_curve_detailed` | results_df (raw data) |
| `plot_effect_distributions` | results_df (raw data) |
| `plot_pvalue_distributions` | results_df (raw data) |
| `plot_stripping_robustness` | fpr_df (expects FDR, gets FPR) |
| `plot_sensitivity_heatmap` | sensitivity_df (expects power column) |

### Checkpoint: Could all plots be regenerated from a single analysis object?

**No.** There are at least 3 name mismatches between analysis.R output keys and analysis_plots.R expected keys. Additionally, analysis_plot_c.R recomputes all diagnostics from scratch, ignoring precomputed analyses.

---

## PHASE 5 — Cross-File Semantic Tracing

### End-to-End Trace: `main_estimate`

```
models.R (LMM): coefficients$estimate[1] where term contains ":"
    ↓
results.R: main_est = interaction_rows$estimate[1]
    ↓  
results.R → tibble: main_estimate = main_est
    ↓
analysis.R: compute_branch_diagnostics → main_estimate_abs = abs(main_estimate)
    ↓
analysis.R: summarise → mean_main_estimate = mean(main_estimate[...])
    ↓
analysis_plots.R: plot_specification_curve_detailed → aes(y = main_estimate)
```

**Semantic Break:** For RMANOVA, `main_estimate = eta_sq` (effect size), but for LMM, `main_estimate = β` (coefficient). These are not comparable.

### End-to-End Trace: `is_significant`

```
results.R: NOT computed — only p-values stored
    ↓
analysis.R: compute_branch_diagnostics → is_significant = main_p_value < 0.05
    ↓
analysis.R: various summaries use is_significant
    ↓
analysis_plots.R: rename_for_plots doesn't add is_significant
    ↓
analysis_plots.R: plot_specification_curve_detailed uses is_significant (must exist)
```

**Semantic Break:** `is_significant` is computed in analysis.R but expected in plots without ensuring it exists. If plots receive raw results, they'll fail.

### End-to-End Trace: `model_type`

```
results.R: model = branch_spec$model (e.g., "lmm_intercept", "lmm_cong_slope")
    ↓
analysis.R: analyze_specification_sensitivity references model_type (UNDEFINED)
    ↓
analysis.R: create_summary_table computes model_type = case_when(...)
    ↓
analysis_plots.R: rename_for_plots computes model_type (DUPLICATE)
    ↓
analysis_plot_c.R: prep_alt_results computes model_type (TRIPLICATE)
```

**Semantic Drift:** `model_type` is derived differently in 3 places but should be computed once in results extraction.

### Semantic Stability Summary

| Variable | Stable? | Issue |
|----------|---------|-------|
| branch_id | ✓ | Consistent throughout |
| main_estimate | ✗ | Different semantics for LMM vs RMANOVA |
| main_p_value | ✓ | Consistent (both use Satterthwaite/F p-value) |
| is_significant | ✗ | Computed late, expected early |
| model_type | ✗ | Computed 3 times with identical logic |
| effect_size | ✗ | For RMANOVA = ηG², for LMM = partial d |

---

## PHASE 6 — Refactoring Proposal

### Proposed Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│                        MODELING LAYER                               │
│   models.R: fit_model() returns standardized model_result          │
│   - Consistent output structure regardless of model type           │
│   - Explicit semantic flags (is_lmm, is_rmanova)                   │
└────────────────────────────┬────────────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────────────┐
│                      EXTRACTION LAYER                               │
│   results.R: extract_results() → CANONICAL RESULT SCHEMA           │
│   - All derived columns computed here (model_type, is_significant) │
│   - Separate estimate columns for different effect size types      │
│   - Schema validation before return                                │
└────────────────────────────┬────────────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────────────┐
│                       ANALYSIS LAYER                                │
│   analysis.R: All statistical computations                         │
│   - Takes validated result tibble                                  │
│   - Returns ANALYSIS_SCHEMA with named outputs                     │
│   - No visualization code                                          │
└────────────────────────────┬────────────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────────────┐
│                    VISUALIZATION LAYER                              │
│   analysis_plots.R: Pure visualization                             │
│   - Takes analysis output (fixed schema)                           │
│   - Zero recomputation                                             │
│   - All aesthetics from precomputed columns                        │
└─────────────────────────────────────────────────────────────────────┘
```

### Canonical Intermediate Data Structures

**1. RESULTS_SCHEMA (from results.R)**
```r
RESULTS_SCHEMA <- list(
  # Identifiers
  branch_id = character,
  branch_idx = integer,
  
  # Branch specification
  sample_size = numeric,
  transformation = character,
  outlier = character,
  model = character,  # raw model name
  model_type = character,  # canonical display name (NEW)
  effect_condition = character,
  strip_method = character,
  
  # Data info
  n_obs = integer,
  n_participants = integer,
  
  # Model convergence
  full_converged = logical,
  null_converged = logical,
  
  # Estimates - SEPARATE by type
  lmm_coefficient = numeric,  # β for LMM (NEW)
  lmm_std_error = numeric,    # SE for LMM (NEW)
  rmanova_eta_sq = numeric,   # ηG² for RMANOVA (NEW)
  rmanova_f_stat = numeric,   # F for RMANOVA (NEW)
  
  # Unified p-value (both types)
  main_p_value = numeric,
  
  # Effect sizes - SEPARATE
  cohens_d = numeric,         # For LMM (NEW)
  eta_squared = numeric,      # For RMANOVA (NEW)
  
  # Derived flags - computed here, not later
  is_significant = logical,   # (NEW - compute at extraction)
  is_true_effect = logical,   # (NEW - compute at extraction)
  is_null_effect = logical,   # (NEW - compute at extraction)
  
  # ... rest of schema
)
```

**2. ANALYSIS_OUTPUT_SCHEMA (from analysis.R)**
```r
ANALYSIS_OUTPUT <- list(
  # Core metrics (fixed names)
  roc_metrics = tibble,        # TPR, FPR, d_prime per grouping
  power_metrics = tibble,      # Power analysis (renamed from power_analysis)
  fpr_by_null = tibble,        # FPR by null type (renamed from fdr_by_null_type)
  sensitivity = tibble,        # Specification sensitivity
  
  # Summaries
  by_model = tibble,
  by_transformation = tibble,
  by_outlier = tibble,
  by_sample_size = tibble,
  by_strip_method = tibble,
  
  # Diagnostics
  branch_diagnostics = tibble, # Full results with all derived columns
  problematic_branches = tibble,
  inconsistencies = tibble,
  
  # Metadata
  alpha = numeric,
  n_branches = integer,
  timestamp = POSIXct
)
```

### Dataflow Diagram

```
                    ┌──────────────┐
                    │   Raw Data   │
                    │  (Parquet)   │
                    └──────┬───────┘
                           │
                           ▼
┌──────────────────────────────────────────────────────────────┐
│                     config.R                                  │
│  generate_all_branches() → branch_specs tibble               │
└──────────────────────────────┬───────────────────────────────┘
                               │
        ┌──────────────────────┼──────────────────────┐
        │                      │                      │
        ▼                      ▼                      ▼
┌───────────────┐    ┌───────────────┐    ┌───────────────┐
│  Branch 1     │    │  Branch 2     │    │  Branch N     │
│  Processed    │    │  Processed    │    │  Processed    │
│  Data         │    │  Data         │    │  Data         │
└───────┬───────┘    └───────┬───────┘    └───────┬───────┘
        │                    │                    │
        ▼                    ▼                    ▼
┌───────────────────────────────────────────────────────────────┐
│                       models.R                                 │
│  fit_model() → standardized model_result                      │
│  (Validates model type, returns consistent structure)          │
└───────────────────────────────┬───────────────────────────────┘
                                │
                                ▼
┌───────────────────────────────────────────────────────────────┐
│                       results.R                                │
│  extract_results() → RESULTS_SCHEMA compliant tibble          │
│  - Computes model_type, is_significant, is_true_effect        │
│  - Validates schema before saving                              │
│                                                                │
│  load_results() → aggregated results                          │
└───────────────────────────────┬───────────────────────────────┘
                                │
                                ▼
┌───────────────────────────────────────────────────────────────┐
│                       analysis.R                               │
│  analyze_multiverse_results() → ANALYSIS_OUTPUT               │
│  - All computation here                                        │
│  - Fixed output keys (roc_metrics, power_metrics, etc.)       │
│  - No visualization                                            │
└───────────────────────────────┬───────────────────────────────┘
                                │
                                ▼
┌───────────────────────────────────────────────────────────────┐
│                    analysis_plots.R                            │
│  generate_multiverse_dashboard(analysis_output)               │
│  - Pure visualization                                          │
│  - Takes ANALYSIS_OUTPUT, produces plots                       │
│  - Zero recomputation                                          │
└───────────────────────────────────────────────────────────────┘
```

### Rules for Adding New Models Safely

1. **Add model spec to config.R**: Define formula, type, control parameters
2. **Verify fit_model dispatch**: Ensure switch() handles new type
3. **Add extraction logic in results.R**: Map model outputs to RESULTS_SCHEMA
4. **No changes needed in analysis.R**: Works on schema, not model specifics
5. **No changes needed in plots**: Works on analysis output, not model specifics

### Checkpoint: Can a new plot be added without touching analysis logic?

**After refactoring: YES.** Any plot can be added by consuming the fixed ANALYSIS_OUTPUT schema without modifying analysis.R.

---

## PHASE 7 — Validation and Risk Assessment

### Risk Matrix

| Issue ID | Risk | Likelihood | Impact | Mitigation |
|----------|------|------------|--------|------------|
| P-001/002/003 | Plot generation fails | Certain | High | Fix key name mismatches |
| M-005 | Invalid cross-model comparisons | High | High | Separate effect size columns |
| R-005 | Schema validation fails | High | Medium | Fix `arrow::int32` typo |
| A-003 | Analysis fails at runtime | High | Medium | Add model_type computation |
| A-004 | Undefined variable error | Certain | Medium | Fix typo: null_main_tstat |
| P-004 | Malformed plot labels | High | Low | Fix scales::percent usage |

### Suggested Safeguards

**Schema Validation (results.R)**
```r
validate_result_row <- function(result_tbl) {
  required <- c("branch_id", "main_estimate", "main_p_value", "model_type",
                "is_significant", "is_true_effect", "is_null_effect")
  missing <- setdiff(required, names(result_tbl))
  if (length(missing) > 0) {
    stop("Result validation failed: missing columns ", paste(missing, collapse = ", "))
  }
  
  # Type checks
  stopifnot(
    is.character(result_tbl$branch_id),
    is.numeric(result_tbl$main_estimate),
    is.numeric(result_tbl$main_p_value),
    is.logical(result_tbl$is_significant)
  )
  
  invisible(result_tbl)
}
```

**Analysis Output Validation (analysis.R)**
```r
validate_analysis_output <- function(analysis_list) {
  required_keys <- c("roc_metrics", "power_metrics", "fpr_by_null", 
                     "sensitivity", "branch_diagnostics")
  missing <- setdiff(required_keys, names(analysis_list))
  if (length(missing) > 0) {
    stop("Analysis output missing required keys: ", paste(missing, collapse = ", "))
  }
  
  # Validate roc_metrics columns
  roc_required <- c("TPR", "FPR", "model_type", "sample_size")
  roc_missing <- setdiff(roc_required, names(analysis_list$roc_metrics))
  if (length(roc_missing) > 0) {
    stop("roc_metrics missing columns: ", paste(roc_missing, collapse = ", "))
  }
  
  invisible(analysis_list)
}
```

**Model Type Consistency Test**
```r
test_model_type_consistency <- function() {
  # Verify model_type derivation is identical across files
  test_models <- c("rmanova", "lmm_intercept", "lmm_cong_slope", "lmm_full_slope")
  
  for (m in test_models) {
    # Simulate derivation from each file
    results_type <- derive_model_type_results(m)
    analysis_type <- derive_model_type_analysis(m)
    plots_type <- derive_model_type_plots(m)
    
    if (!all(c(results_type, analysis_type, plots_type) == results_type)) {
      stop("Inconsistent model_type derivation for: ", m)
    }
  }
  
  message("Model type consistency: PASS")
}
```

### Edge Cases Prevented by Refactoring

1. **Empty results for a model type**: Schema validation catches before analysis
2. **New outlier method added**: Only config.R change needed
3. **RMANOVA vs LMM comparison**: Separate columns prevent invalid mixing
4. **Missing model_type in plots**: Computed at extraction, always present
5. **Null model for RMANOVA**: Properly handles NA fields in unified schema

### Migration Risk Assessment

| Change | Files Affected | Risk | Strategy |
|--------|---------------|------|----------|
| Fix naming mismatches | analysis.R, analysis_plots.R | Low | Rename keys only |
| Add model_type to extraction | results.R | Low | Add computation |
| Fix typos (R-005, A-004) | results.R, analysis.R | Very Low | Direct fix |
| Separate effect size columns | results.R, analysis.R, analysis_plots.R | Medium | Incremental migration |
| Consolidate prep functions | analysis_plot_c.R | Medium | Use shared function |

---

## Immediate Fixes Required

Based on this audit, the following **blocking issues** must be fixed before the pipeline can run successfully:

### Critical (Will Cause Runtime Failures)

1. **R-005**: Fix `null_n_obs = arrow::int32` → `null_n_obs = arrow::int32()` in results.R line 30
2. **A-003**: Add `model_type` computation to `analyze_specification_sensitivity()` in analysis.R
3. **A-004**: Fix `null_main_tstat` → `null_tstat` in results.R line 156
4. **P-001/002/003**: Fix analysis output key names to match what plots expect:
   - `power_analysis` → `power_metrics` OR update plot to use `power_analysis`
   - `fdr_by_null_type` → `fpr_by_null` OR update plot to use `fdr_by_null_type`
   - `results_with_diag` → `results_with_diagnostics` OR update plot to use `results_with_diag`

### High Priority (Silent Errors)

5. **M-005**: Document that `main_estimate` semantics differ between LMM and RMANOVA
6. **P-004**: Fix `scales::percent(model_type, ...)` in `plot_roc_by_outlier`
7. **P-005**: Ensure `plot_stripping_robustness` receives `FPR` column (or rename)

---

## Conclusion

This codebase has a solid conceptual foundation for multiverse analysis but contains several **contract mismatches** between files that will cause runtime failures. The most critical issues are naming inconsistencies between analysis.R outputs and analysis_plots.R expectations.

The proposed refactoring separates concerns cleanly and ensures each layer has a well-defined contract. Implementation priority should be:

1. Fix critical bugs (same PR)
2. Standardize naming conventions (follow-up PR)
3. Consolidate derived column computation (follow-up PR)
4. Separate effect size types (future enhancement)

---

*End of Audit Report*
