# R/functions/analysis.R - Results Aggregation & Discovery Rate Analysis

# ==============================================================================
# BRANCH-LEVEL ANALYSIS
# ==============================================================================
# Analysis of individual branch results (single models, convergence, fit)

analysis_main_filter <- function(
    df,
    remove_singular = TRUE # remove singular fits via lme4 message or random effects var
    ) {
  out <- df


  if (remove_singular) {
    # Often found as "singular fit" message in error_message or model_result$message
    if ("error_message" %in% names(out)) {
      out <- out %>%
        dplyr::filter(
          is.na(error_message) | !grepl("singular", error_message, ignore.case = TRUE)
        )
    }

    # Or if random_intercept_var/random_slope_var exists and is exactly zero (typical lme4 indicator)
    if (all(c("random_intercept_var", "random_slope_var") %in% names(out))) {
      out <- out %>%
        dplyr::filter(
          is.na(random_intercept_var) | random_intercept_var > 0,
          is.na(random_slope_var) | random_slope_var > 0
        )
    }
  }
  out
}

#' Compute branch-level diagnostics
#'
#' Single-branch metrics: convergence, fit, effect estimates
#'
#' @param results_df Results tibble (can be multiple branches)
#'
#' @return Tibble with one row per branch containing diagnostics
#'
compute_branch_diagnostics <- function(results_df) {
  diagn <- results_df %>%
    dplyr::mutate(
      # Convergence status
      converged_both = full_converged & dplyr::coalesce(null_converged, TRUE),

      # Effect classification (define early for downstream use)
      is_true_effect = (effect_condition == "present" & strip_method == "none"),
      is_null_effect = (
        (effect_condition == "null_interaction" & strip_method %in% c("shuffle", "qmap_5")) |
          (effect_condition == "null_both" & strip_method == "none")
      ),

      # Effect size magnitude
      main_estimate_abs = abs(main_estimate),

      # Model fit delta (relative improvement of full vs null)
      aic_improvement = -AIC_diff, # Negative AIC_diff = better full model
      bic_improvement = -BIC_diff,

      # Effect size precision (inverse of SE)
      effect_precision = 1 / (main_std_error + 1e-6),

      # Confidence interval width
      ci_width = effect_ci_upper - effect_ci_lower,

      # Statistical significance
      is_significant = main_p_value < 0.05,

      # Model quality flags
      small_sample = n_obs < 100,
      poor_ci = ci_width > 2 * main_estimate_abs, # Very wide CI

      .keep = "all"
    )

  diagn <- analysis_main_filter(diagn)

  stopifnot(
    "Overlapping effect definitions" = !any(diagn$is_true_effect & diagn$is_null_effect, na.rm = TRUE)
  )
  diagn
}

#' Summarize branch-level results by model specification
#'
#' @param results_df Results tibble with branch diagnostics
#'
#' @return Tibble: one row per model type
#'
branch_summary_by_model <- function(results_df) {
  results_df %>%
    analysis_main_filter() %>%
    dplyr::filter(!error) %>%
    dplyr::group_by(model) %>%
    dplyr::summarise(
      n_branches = dplyr::n(),
      n_converged = sum(converged_both, na.rm = TRUE),
      convergence_rate = mean(converged_both, na.rm = TRUE),
      # Effect estimates (only converged)
      mean_main_estimate = mean(main_estimate[converged_both], na.rm = TRUE),
      median_main_estimate = median(main_estimate[converged_both], na.rm = TRUE),
      sd_main_estimate = sd(main_estimate[converged_both], na.rm = TRUE),

      # Model fit
      mean_aic_diff = mean(AIC_diff[converged_both], na.rm = TRUE),
      mean_bic_diff = mean(BIC_diff[converged_both], na.rm = TRUE),

      # Inference precision
      mean_main_se = mean(main_std_error[converged_both], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(mean_main_estimate)
}

#' Identify problematic branches
#'
#' Flags branches with convergence failures, small samples, or wide CIs
#'
#' @param results_df Results tibble with diagnostics
#'
#' @return Tibble of problematic branches
#'
identify_problematic_branches <- function(results_df) {
  results_df %>%
    analysis_main_filter() %>%
    dplyr::filter(error | !converged_both | poor_ci | small_sample) %>%
    dplyr::select(
      branch_id, model, effect_condition,
      error, converged_both, small_sample, poor_ci,
      main_estimate, main_std_error, ci_width,
      main_p_value, n_obs
    ) %>%
    dplyr::mutate(
      problem = dplyr::case_when(
        error ~ "Model fitting error",
        !converged_both ~ "Non-convergence",
        poor_ci ~ "Very wide confidence interval",
        small_sample ~ "Small sample size",
        TRUE ~ "Unknown issue"
      )
    ) %>%
    dplyr::arrange(branch_id)
}

allowed_combinations_filter <- function(df) {
  df %>%
    dplyr::filter(
      (effect_condition == "present" & strip_method == "none") |
        (effect_condition == "null_both" & strip_method == "none") |
        (effect_condition == "null_interaction" & strip_method != "none")
    )
}


#' Compute ROC metrics (TPR and FPR) properly
#'
#' @details
#' - TPR computed ONLY from effect_condition == "present"
#' - FPR computed ONLY from effect_condition %in% c("null_interaction", "null_both")
#' - Grouping is appropriate to avoid single-row groups
#'
#' @param results_df Results tibble with diagnostics
#' @param alpha Significance threshold
#' @param group_vars Variables to group by for aggregation
#'
#' @return Tibble with TPR, FPR, and derived metrics
#'
compute_roc_metrics <- function(
    results_df,
    alpha = 0.05,
    group_vars = c(
      "model", "sample_size",
      "transformation"
    )) {
  prepared <- results_df %>%
    dplyr::filter(!error, converged_both) %>%
    allowed_combinations_filter() %>%
    dplyr::mutate(is_significant = main_p_value < alpha)

  # TPR: proportion significant when effect is PRESENT
  tpr_df <- prepared %>%
    dplyr::filter(is_true_effect, strip_method == "none") %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::summarise(
      n_true = dplyr::n(),
      n_detected = sum(is_significant, na.rm = TRUE),
      TPR = ifelse(n_true > 0, n_detected / n_true, NA_real_),
      mean_effect_present = mean(main_estimate, na.rm = TRUE),
      .groups = "drop"
    )

  # FPR: proportion significant when effect is NULL
  fpr_df <- prepared %>%
    dplyr::filter(is_null_effect) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_vars, "strip_method")))) %>%
    dplyr::summarise(
      n_null = dplyr::n(),
      n_false_positive = sum(is_significant, na.rm = TRUE),
      FPR = ifelse(n_null > 0, n_false_positive / n_null, NA_real_),
      mean_effect_null = mean(main_estimate, na.rm = TRUE),
      .groups = "drop"
    )

  # Join TPR and FPR
  # Note: FPR is grouped by strip_method for robustness analysis
  # For primary ROC, aggregate FPR across strip_methods by taking mean
  fpr_aggregated <- fpr_df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::summarise(
      n_null = sum(n_null, na.rm = TRUE),
      n_false_positive = sum(n_false_positive, na.rm = TRUE),
      FPR = ifelse(n_null > 0, n_false_positive / n_null, NA_real_),
      mean_effect_null = mean(mean_effect_null, na.rm = TRUE),
      .groups = "drop"
    )
  
  roc_df <- tpr_df %>%
    dplyr::left_join(fpr_aggregated, by = group_vars, suffix = c("_present", "_null")) %>%
    dplyr::mutate(
      # d-prime (sensitivity index)
      d_prime = qnorm(pmin(TPR, 0.999)) - qnorm(pmax(FPR, 0.001)),
      # Youden's J statistic
      youden_j = TPR - FPR,
      # Diagnostic odds ratio
      DOR = dplyr::case_when(
        FPR == 0 | TPR == 1 ~ NA_real_,
        TRUE ~ (TPR / (1 - TPR)) / (FPR / (1 - FPR))
      )
    )

  logger::log_info("Computed ROC metrics for {nrow(roc_df)} groupings")

  roc_df
}

#' Compute FPR by null condition type
#'
#' @details
#' Separates null_interaction vs null_both to verify stripping method works
#' and doesn't artificially inflate false discoveries
#'
#' @param results_df Results tibble
#' @param alpha Significance threshold
#' @param group_vars Grouping variables
#'
#' @return Tibble with FPR by null type
#'
compute_fpr_by_null_type <- function(
    results_df,
    alpha = 0.05,
    group_vars = c(
      "strip_method"
    )) {
  prepared <- results_df %>%
    dplyr::filter(!error, converged_both, is_null_effect) %>%
    allowed_combinations_filter() %>%
    dplyr::mutate(is_significant = main_p_value < alpha)

  # NOTE: i think in some null models the interaction doesn't exist so i have to grace that in results.R/models.R
  fpr_df <- prepared %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_vars, "effect_condition")))) %>%
    dplyr::summarise(
      n = dplyr::n(),
      n_significant = sum(is_significant, na.rm = TRUE),
      FPR = ifelse(n > 0, n_significant / n, NA_real_),
      mean_p = mean(main_p_value, na.rm = TRUE),
      median_p = median(main_p_value, na.rm = TRUE),
      mean_effect = mean(main_estimate, na.rm = TRUE),
      .groups = "drop"
    )

  logger::log_info("Computed FPR for {nrow(fpr_df)} null-condition groupings")

  fpr_df
}

#' Compute power (TPR) across specifications
#'
#' @param results_df Results tibble
#' @param alpha Significance threshold
#' @param group_vars Grouping variables
#'
#' @return Tibble with power metrics
#'
compute_power <- function(
    results_df,
    alpha = 0.05,
    group_vars = c(
      "model", "sample_size",
      "transformation"
    )) {
  prepared <- results_df %>%
    dplyr::filter(!error, converged_both, is_true_effect, strip_method == "none") %>%
    allowed_combinations_filter() %>%
    dplyr::mutate(is_significant = main_p_value < alpha)

  power_df <- prepared %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::summarise(
      n = dplyr::n(),
      n_detected = sum(is_significant, na.rm = TRUE),
      power = ifelse(n > 0, n_detected / n, NA_real_),
      mean_p = mean(main_p_value, na.rm = TRUE),
      median_p = median(main_p_value, na.rm = TRUE),
      mean_effect = mean(main_estimate, na.rm = TRUE),
      sd_effect = sd(main_estimate, na.rm = TRUE),
      .groups = "drop"
    )

  logger::log_info("Computed power for {nrow(power_df)} present-condition groupings")

  power_df
}

# ==============================================================================
# MULTIVERSE-LEVEL ANALYSIS
# ==============================================================================

#' Compute discovery rates
#'
#' @param results_df Results tibble with all branches
#' @param alpha Significance threshold
#'
#' @return Tibble with discovery metrics
#'
compute_discovery_rates <- function(results_df, alpha = 0.05) {
  logger::log_info("Computing discovery rates at alpha={alpha}")

  roc_metrics <- compute_roc_metrics(
    results_df,
    alpha = alpha,
    group_vars = c("model", "strip_method", "sample_size", "transformation", "outlier")
  )


  # Combine into single summary
  discovery_rates <- roc_metrics %>%
    dplyr::rename(
      true_positive_rate = TPR,
      false_positive_rate = FPR
    ) %>%
    dplyr::mutate(
      pct_tpr = 100 * true_positive_rate,
      pct_fpr = 100 * false_positive_rate
    )

  logger::log_info("Computed discovery rates for {nrow(discovery_rates)} combinations")

  discovery_rates
}


#' Analyze multiverse results across all branches
#'
#' @param results_df Full results tibble
#'
#' @return List containing multiple analysis tables
#'

analyze_multiverse_results <- function(results_df, alpha = 0.05) {
  logger::log_info("Performing multiverse analysis on {nrow(results_df)} branches")

  results_with_diag <- compute_branch_diagnostics(results_df)
  results_with_diag <- allowed_combinations_filter(results_with_diag)

  analyses <- list(
    # Branch-level summaries
    branch_by_model = branch_summary_by_model(results_with_diag),
    branch_issues = identify_problematic_branches(results_with_diag),

    # Summary by model
    by_model = results_with_diag %>%
      dplyr::filter(!error, converged_both) %>%
      dplyr::group_by(model) %>%
      dplyr::summarise(
        n_branches = dplyr::n(),
        n_converged = sum(converged_both, na.rm = TRUE),
        mean_p = mean(main_p_value, na.rm = TRUE),
        median_p = median(main_p_value, na.rm = TRUE),
        # Separate power (true) from FDR (null)
        power = mean(is_significant[is_true_effect], na.rm = TRUE),
        fpr = mean(is_significant[is_null_effect], na.rm = TRUE),
        mean_main_estimate = mean(main_estimate, na.rm = TRUE),
        sd_main_estimate = sd(main_estimate, na.rm = TRUE),
        .groups = "drop"
      ),

    # Summary by transformation (separate true/null)
    by_transformation = results_with_diag %>%
      dplyr::filter(!error, converged_both) %>%
      dplyr::group_by(transformation) %>%
      dplyr::summarise(
        n_branches = dplyr::n(),
        power = mean(is_significant[is_true_effect], na.rm = TRUE),
        fpr = mean(is_significant[is_null_effect], na.rm = TRUE),
        mean_effect_present = mean(main_estimate[is_true_effect], na.rm = TRUE),
        mean_effect_null = mean(main_estimate[is_null_effect], na.rm = TRUE),
        .groups = "drop"
      ),

    # Summary by outlier method
    by_outlier = results_with_diag %>%
      dplyr::filter(!error, converged_both) %>%
      dplyr::group_by(outlier) %>%
      dplyr::summarise(
        n_branches = dplyr::n(),
        power = mean(is_significant[is_true_effect & strip_method == "none"], na.rm = TRUE),
        fpr = mean(is_significant[is_null_effect], na.rm = TRUE),
        mean_effect_present = mean(main_estimate[is_true_effect & strip_method == "none"], na.rm = TRUE),
        .groups = "drop"
      ),

    # Summary by sample size
    by_sample_size = results_with_diag %>%
      dplyr::filter(!error, converged_both) %>%
      dplyr::group_by(sample_size) %>%
      dplyr::summarise(
        n_branches = dplyr::n(),
        power = mean(is_significant[is_true_effect], na.rm = TRUE),
        fpr = mean(is_significant[is_null_effect], na.rm = TRUE),
        mean_effect_present = mean(main_estimate[is_true_effect], na.rm = TRUE),
        .groups = "drop"
      ),

    # Summary by strip method (CRITICAL for robustness)
    by_strip_method = results_with_diag %>%
      dplyr::filter(!error, converged_both) %>%
      dplyr::group_by(strip_method) %>%
      dplyr::summarise(
        n_branches = dplyr::n(),
        power = ifelse(
          strip_method[1] == "none",
          mean(is_significant[is_true_effect], na.rm = TRUE),
          NA_real_
        ),
        fpr = mean(is_significant[is_null_effect], na.rm = TRUE),
        mean_effect_present = mean(main_estimate[is_true_effect], na.rm = TRUE),
        mean_effect_null = mean(main_estimate[is_null_effect], na.rm = TRUE),
        .groups = "drop"
      ),

    # ROC metrics (proper TPR/FPR)
    roc_metrics = compute_roc_metrics(results_with_diag, alpha = alpha),

    # FDR by null type (robustness check)
    fdr_by_null_type = compute_fpr_by_null_type(results_with_diag, alpha = alpha),

    # Power analysis
    power_analysis = compute_power(results_with_diag, alpha = alpha),
    discovery_rates = compute_discovery_rates(results_with_diag, alpha = alpha),

    # Specification inconsistencies
    spec_inconsistencies = detect_specification_inconsistencies(results_with_diag, alpha = alpha),

    # Sensitivity analysis
    spec_sensitivity = analyze_specification_sensitivity(results_with_diag),
    
    # Summary table for reporting
    summary_table = create_summary_table(results_with_diag),
    
    # Include diagnostics for plotting
    results_with_diagnostics = results_with_diag
  )

  logger::log_info("Multiverse analysis complete:  {length(analyses)} tables generated")

  analyses
}

#' Analyze results and save to disk
#'
#' @param results_df Aggregated results tibble
#' @param paths Project paths object
#' @param alpha Significance threshold
#'
#' @return List of written file paths
#'
analyze_and_save <- function(results_df, paths, alpha = 0.05) {
  logger::log_info("Saving multiverse analysis results...")

  # Ensure output directory
  if (!dir.exists(paths$outputs_analysis)) {
    dir.create(paths$outputs_analysis, recursive = TRUE, showWarnings = FALSE)
  }

  output_files <- list()

  # Run analyses
  analyses <- analyze_multiverse_results(results_df, alpha = alpha)

  # Save each analysis table
  for (name in names(analyses)) {
    df <- analyses[[name]]

    if (!is.data.frame(df) || nrow(df) == 0) {
      logger::log_warn("Skipping empty analysis:  {name}")
      next
    }

    filename <- glue::glue("{name}_{format(Sys.time(), '%Y%m%d_%H%M%S')}.csv")
    filepath <- file.path(paths$outputs_analysis, filename)

    readr::write_csv(df, filepath)
    logger::log_info("Wrote analysis:  {filepath}")
    output_files[[name]] <- filepath
  }

  logger::log_info("Analysis complete, {length(output_files)} files saved")

  invisible(output_files)
}


#' Detect inconsistent findings
#'
#' @param results_df Results tibble
#' @param alpha Significance threshold
#'
#' @return Tibble with inconsistent specification sets
#'
detect_specification_inconsistencies <- function(results_df, alpha = 0.05) {
  logger::log_info("Detecting specification inconsistencies")

  # Check within same (model, effect_condition) if different preprocessing
  # leads to different conclusions

  inconsistencies <- results_df %>%
    allowed_combinations_filter() %>%
    analysis_main_filter() %>%
    dplyr::filter(!error, !is.na(main_p_value)) %>%
    dplyr::filter(converged_both) %>%
    # Group by model and effect condition, vary preprocessing
    dplyr::group_by(model, effect_condition) %>%
    dplyr::summarise(
      n_specs = dplyr::n(),
      n_significant = sum(is_significant, na.rm = TRUE),
      pct_significant = 100 * n_significant / n_specs,

      # P-value range indicates sensitivity
      p_min = min(main_p_value, na.rm = TRUE),
      p_max = max(main_p_value, na.rm = TRUE),
      p_range = p_max - p_min,
      p_iqr = IQR(main_p_value, na.rm = TRUE),

      # Effect size variation
      effect_mean = mean(main_estimate, na.rm = TRUE),
      effect_sd = sd(main_estimate, na.rm = TRUE),
      effect_cv = effect_sd / (abs(effect_mean) + 1e-6),

      # Inconsistent if not all agree
      is_inconsistent = pct_significant > 5 & pct_significant < 95,
      .groups = "drop"
    ) %>%
    dplyr::filter(is_inconsistent) %>%
    dplyr::arrange(dplyr::desc(effect_cv))

  logger::log_info("Found {nrow(inconsistencies)} inconsistent specification sets")

  inconsistencies
}


#' Analyze specification sensitivity
#'
#' @param results_df Results tibble
#'
#' @return Sensitivity summary tibble
#'
analyze_specification_sensitivity <- function(results_df) {
  results_df %>%
    allowed_combinations_filter() %>%
    analysis_main_filter() %>%
    dplyr::filter(!error, converged_both, !is.na(main_p_value)) %>%
    dplyr::group_by(transformation, outlier, sample_size) %>%
    dplyr::summarise(
      n_results = dplyr::n(),
      # Separate by condition type
      power = mean(is_significant[is_true_effect], na.rm = TRUE),
      fdr = mean(is_significant[is_null_effect], na.rm = TRUE),
      mean_effect_present = mean(main_estimate[is_true_effect], na.rm = TRUE),
      mean_effect_null = mean(main_estimate[is_null_effect], na.rm = TRUE),
      mean_rp = mean(main_p_value, na.rm = TRUE),
      .groups = "drop"
    )
}

# ==============================================================================
# SUMMARY TABLE FOR REPORTING
# ==============================================================================

#' Generate summary statistics table
#'
#' @param results_df Results with diagnostics
#'
#' @return Summary tibble
#'
create_summary_table <- function(results_df) {
  # Add model_type grouping for display
  prepared <- results_df %>%
    dplyr::mutate(
      model_type = dplyr::case_when(
        grepl("rmanova", model) ~ "rmANOVA",
        grepl("full_slope", model) ~ "LMM (full)",
        grepl("cong_slope", model) ~ "LMM (cong slope)",
        grepl("intercept", model) ~ "LMM (intercept)",
        TRUE ~ model
      )
    )

  summary_tbl <- prepared %>%
    dplyr::group_by(model_type, transformation, effect_condition) %>%
    dplyr::summarise(
      n = dplyr::n(),
      n_converged = sum(converged_both, na.rm = TRUE),
      pct_significant = mean(is_significant, na.rm = TRUE) * 100,
      mean_effect = mean(main_estimate, na.rm = TRUE),
      sd_effect = sd(main_estimate, na.rm = TRUE),
      median_p = median(main_p_value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      metric_type = dplyr::case_when(
        effect_condition == "present" ~ "Power (TPR)",
        TRUE ~ "FPR"
      )
    ) %>%
    dplyr::arrange(model_type, transformation, effect_condition)

  summary_tbl
}
