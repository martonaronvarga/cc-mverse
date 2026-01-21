# Multiverse Analysis Pipeline - Issue Ledger

## CRITICAL ISSUES (Must Fix)

### config.R

| ID | Line | Severity | Principle | Issue | Impact |
|----|------|----------|-----------|-------|--------|
| C001 | 171 | SILENT-ERROR | Branch uniqueness | Inconsistent branch_id composition (includes model) vs compose_branch_id() (excludes model) | Medium - Potential branch ID mismatch if compose_branch_id() used |
| C002 | 179 | CONCEPTUAL | Branch validation | Missing validation assertions for strip_method/effect_condition constraints | Medium - No runtime guarantee of multiverse integrity |

### results.R

| ID | Line | Severity | Principle | Issue | Impact |
|----|------|----------|-----------|-------|--------|
| R001 | 149 | SILENT-ERROR | Losslessness | null_effect_size uses nobs instead of residual SD (asymmetric with full model) | HIGH - Cannot recompute effect sizes; stored values not comparable |
| R002 | 245, 163 | FATAL | Losslessness | Random slope variance never populated (always NA_real_) | HIGH - Missing model complexity information for likelihood reconstruction |
| R003 | 335-343 | FATAL | Losslessness | RMANOVA duplicates full stats as null stats; no separate null model | CRITICAL - Cannot reconstruct TPR/FPR for RMANOVA |
| R004 | 341 | FATAL | Schema consistency | Typo "nul_t_stat" instead of "null_t_stat" | CRITICAL - Schema mismatch, column creation failure |
| R005 | 143, 148 | CONCEPTUAL | Metadata | Missing test directionality documentation (one vs two-tailed) | MEDIUM - Cannot determine proper FPR thresholds |
| R006 | 152-156 | CONCEPTUAL | Metadata | Missing CI methodology documentation (level, type) | MEDIUM - Cannot verify significance thresholds |
| R007 | 281-293 | SILENT-ERROR | Multiple comparisons | RMANOVA term selection via heuristics; no recording of tests conducted | HIGH - Cannot assess multiple comparison problem |

### analysis.R

| ID | Line | Severity | Principle | Issue | Impact |
|----|------|----------|-----------|-------|--------|
| A001 | 335 | FATAL | Runtime | Variable typo: references undefined `true_discovery_rate` instead of `true_positive_rate` | CRITICAL - Function crashes at runtime |
| A002 | 264 | FATAL | Runtime | References undefined `fdr_df` instead of `fpr_df` | CRITICAL - Function crashes |
| A003 | 210 | FATAL | FPR/TPR computation | Mismatched join keys: FPR grouped by (group_vars + strip_method), joined only on group_vars | CRITICAL - Produces incorrect FPR estimates via cartesian product |
| A004 | 423-427 | SILENT-ERROR | Vectorization | Scalar if() inside dplyr::summarise (should be vectorized) | HIGH - May cause silent errors or incorrect NA assignment |
| A005 | 373-374, 386-387, 400 | CONCEPTUAL | Strip method treatment | Aggregates across strip_method, mixing substantive with robustness checks | HIGH - Violates design principle that strip methods are robustness checks |
| A006 | 71 vs 182 | INEFFICIENCY | Parameter consistency | Hard-coded alpha=0.05 in compute_branch_diagnostics vs parameterized in compute_roc_metrics | MEDIUM - Parameter not honored in early computation |
| A007 | 199 | CONCEPTUAL | Strip method treatment | FPR grouped by strip_method contradicts robustness principle | MEDIUM - Should report strip methods separately, not as grouping dimension |
| A008 | General | CONCEPTUAL | Multiple testing | No accounting for multiple comparisons within branches | MEDIUM - Type I error rates may be inflated |

### analysis_plots.R

| ID | Line | Severity | Principle | Issue | Impact |
|----|------|----------|-----------|-------|--------|
| P001 | 111-151 | FATAL | Visualization purity | Duplicate compute_roc_metrics() function (also in analysis.R) | CRITICAL - Recomputation in plots violates separation of concerns |
| P002 | 157-175 | FATAL | Visualization purity | Duplicate compute_fdr_by_null_type() function (also in analysis.R as compute_fpr_by_null_type) | CRITICAL - Recomputation in plots |
| P003 | 244-250 | FATAL | Visualization purity | Computes mean_FDR and se_FDR statistics inside plot function | CRITICAL - Statistical computation in visualization layer |
| P004 | 514-521 | FATAL | Visualization purity | Computes power statistics inside plot function | CRITICAL - Statistical computation in visualization layer |
| P005 | 638-647 | FATAL | Visualization purity | create_summary_table() performs complex statistical aggregation | CRITICAL - Analysis function misplaced in plotting file |
| P006 | 186, 295, 456 | CONCEPTUAL | Visualization purity | Plot functions call compute_* functions instead of using pre-computed objects | HIGH - Violates declarative plotting principle |
| P007 | 190, 299 | CONCEPTUAL | Analytical decisions | Implicit filtering strip_method == "none" encodes baseline selection | HIGH - Analytical decision hidden in plot code |
| P008 | 332-334, 394 | INEFFICIENCY | Visualization purity | Data preparation (prepare_results, ranking) in plot functions | MEDIUM - Should be upstream |

## SEMANTIC DRIFT ISSUES

### FPR/TPR Definition Consistency

| Location | Definition | Status | Issue |
|----------|------------|--------|-------|
| analysis.R lines 51-55 | is_true_effect / is_null_effect branch classification | ✓ CORRECT | Mutually exclusive, properly defined |
| analysis.R lines 184-194 | TPR = n_detected / n_true for is_true_effect | ✓ CORRECT | Proper frequentist definition |
| analysis.R lines 197-206 | FPR = n_false_positive / n_null for is_null_effect | ⚠️ FLAWED | Grouped by strip_method, causing join mismatch (A003) |
| analysis_plots.R lines 118-127 | TPR computation (duplicate) | ⚠️ REDUNDANT | Should use pre-computed from analysis.R |
| analysis_plots.R lines 130-138 | FPR computation (duplicate) | ⚠️ REDUNDANT | Should use pre-computed from analysis.R |

## REFACTORING REQUIREMENTS

### 1. config.R
- [ ] Add validation assertions after line 169 to enforce strip_method/effect_condition rules
- [ ] Resolve branch_id inconsistency: remove or document compose_branch_id()
- [ ] Rename n_combinations to n_total_branches for clarity

### 2. results.R
- [ ] Fix null_effect_size computation (line 149) to use residual SD
- [ ] Implement random slope variance extraction (currently always NA)
- [ ] Fix RMANOVA null model handling - compute separate null or document limitation
- [ ] Fix typo "nul_t_stat" → "null_t_stat" (line 341)
- [ ] Document test directionality and CI methodology
- [ ] Add logging when interaction term is missing (line 140)
- [ ] Add validation for effect_condition consistency

### 3. analysis.R
- [ ] Fix variable name typos (lines 335, 264)
- [ ] Fix join key mismatch (line 210) - include strip_method in join or restructure
- [ ] Replace scalar if() with vectorized alternative (lines 423-427)
- [ ] Add alpha parameter to compute_branch_diagnostics()
- [ ] Separate strip method robustness analysis from main TPR/FPR computation
- [ ] Add multiple comparison accounting/documentation
- [ ] Apply allowed_combinations_filter() consistently across all aggregations

### 4. analysis_plots.R
- [ ] Remove duplicate compute_roc_metrics() and compute_fdr_by_null_type()
- [ ] Remove all statistical computations from plot functions
- [ ] Move create_summary_table() to analysis.R
- [ ] Refactor all plot functions to accept pre-computed analysis tibbles
- [ ] Make strip_method filtering explicit via function parameters
- [ ] Remove prepare_results() calls from plot functions

### 5. Pipeline Architecture
- [ ] Ensure branch creation is explicit and immutable
- [ ] Move all inferential computation to analysis.R
- [ ] Make plotting declarative and side-effect-free
- [ ] Document pipeline invariants
- [ ] Create plot-ready data structures specification

## STOP CONDITION

Pipeline transparently answers:
> "How do FPR and TPR depend on transformation and outlier strategy, and are conclusions robust to stripping method?"

Verified when:
- [ ] FPR and TPR can be traced from branch → model → result → analysis → plot without redefinition
- [ ] Strip methods are reported as robustness checks, not mixed into primary estimates
- [ ] Plots regenerable identically from saved analysis objects
- [ ] No recomputation in visualization layer
- [ ] All critical and high-severity issues resolved
