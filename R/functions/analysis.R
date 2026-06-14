# R/functions/analysis.R - Results Aggregation & Discovery Rate Analysis
# Multiverse Analysis: Results Aggregation & Discovery Rate Analysis
#
# Design principles:
#   1. Non-convergence is data, not dirt.
#   2. Singular fits are flagged, not dropped.
#   3. Scales (log_rt vs no_log_rt) are never mixed.
#   4. Null types are never pooled.
#   5. Power and FPR are modeled separately.
#   6. subsample_id provides dependent empirical resampling paths, not
#      independent Monte Carlo replications.
#
# Cell sizes with maximal design (11 fractional sample sizes x 100 subsamples + 1x1):
#   Per (model, transformation, effect_condition, null_type): up to 11,010 branches before usability filters
#   Per (model, transformation, sample_size): usually 1,000 branches across outliers, 10 at full sample
#   Per (model, transformation, sample_size, outlier): usually 100 branches, 1 at full sample
#   Per full spec (model, transform, sample_size, outlier, subsample): 1 branch


#' Assign semantic name for model object in analysis
#' @param model name from config, in model result list
derive_model <- function(model) {
  dplyr::case_when(
    grepl("rmanova", model) ~ "rmANOVA",
    grepl("full_slope", model) ~ "LMM (full)",
    grepl("cong_slope", model) ~ "LMM (random congruency slope)",
    grepl("intercept", model) ~ "LMM (random intercept)",
    TRUE ~ model
  )
}

#' Filter to allowed (effect_condition, strip_method) combinations.
#'
#' Ensures that:
#'   - "present"          only appears with strip_method == "none"
#'   - "null_both"        is absent
#'   - "null_interaction"  only appears with strip_method != "none"
allowed_combinations_filter <- function(df) {
  df %>%
    dplyr::filter(
      (effect_condition == "present" & strip_method == "none") |
        (effect_condition == "null_interaction" & strip_method != "none")
    )
}

#' Build the analysis-ready data frame.
#'
#' Applies allowed-combination and singularity filters, then attaches all
#' derived columns used downstream. Every subsequent function receives this
#' prepared frame and does not re-filter.
prepare_analysis_df <- function(results_df) {
  if (!"fallback_level" %in% names(results_df)) results_df$fallback_level <- NA_character_
  if (!"preservation_pass" %in% names(results_df)) results_df$preservation_pass <- NA
  if (!"nullification_verdict" %in% names(results_df)) results_df$nullification_verdict <- NA_character_
  out <- results_df %>%
    allowed_combinations_filter() %>%
    dplyr::mutate(
      # --- convergence (rmANOVA: null_converged is NA → treat as TRUE) ---
      converged_both = full_converged & dplyr::coalesce(null_converged, TRUE),
      is_singular = dplyr::case_when(
        error ~ NA,
        "full_is_singular" %in% names(.) & !is.na(full_is_singular) ~ full_is_singular,
        !is.na(error_message) &
          grepl("singular", error_message, ignore.case = TRUE) ~ TRUE,
        !is.na(random_intercept_var) & random_intercept_var == 0 ~ TRUE,
        !is.na(random_slope_var) & random_slope_var == 0 ~ TRUE,
        TRUE ~ FALSE
      ),
      numerically_usable = !error & converged_both & !is_singular,

      # --- effect-type flags (mutually exclusive after allowed_combinations) ---
      is_true_effect = (effect_condition == "present"),
      is_null_effect = (effect_condition == "null_interaction"),

      # --- null sub-type: encodes the null-generation mechanism ---
      null_type = dplyr::case_when(
        effect_condition == "null_interaction" ~
          paste0("null_interaction:", strip_method),
        TRUE ~ NA_character_
      ),
      rate_source = dplyr::case_when(
        effect_condition == "null_interaction" ~ "empirical_nullification",
        effect_condition == "present" ~ "empirical_present",
        TRUE ~ NA_character_
      ),
      preservation_pass = dplyr::coalesce(preservation_pass, NA),
      nullification_verdict = dplyr::coalesce(nullification_verdict, NA_character_),
      has_fallback_sensitivity = !is.na(fallback_level),
      primary_inference_only = TRUE,
      is_interpretable_nullifier = effect_condition == "null_interaction" &
        nullification_verdict == "interpretable_nullifier",
      model_type = derive_model(model),
      is_significant = dplyr::if_else(
        numerically_usable, main_p_value < 0.05, NA
      ),
      aic_improvement = dplyr::if_else(numerically_usable, -AIC_diff, NA_real_),
      bic_improvement = dplyr::if_else(numerically_usable, -BIC_diff, NA_real_),
      ci_width = dplyr::if_else(
        numerically_usable,
        effect_ci_upper - effect_ci_lower,
        NA_real_
      ),
      small_sample = n_obs < 100,
      poor_ci = dplyr::if_else(
        numerically_usable & is_true_effect &
          !is.na(main_estimate) & abs(main_estimate) > 1e-6 &
          !is.na(ci_width),
        ci_width > 2 * abs(main_estimate),
        FALSE
      ),
      subsample_id = as.integer(subsample_id),
      .keep = "all"
    )

  # sanity: the two flags must be mutually exclusive.
  stopifnot(
    "Overlapping effect definitions" =
      !any(out$is_true_effect & out$is_null_effect, na.rm = TRUE)
  )

  out
}

branch_health <- function(prepared_df) {
  prepared_df %>%
    dplyr::group_by(model_type, transformation, effect_condition, null_type) %>%
    dplyr::summarise(
      n_total = dplyr::n(),
      n_error = sum(error, na.rm = TRUE),
      n_no_error = sum(!error, na.rm = TRUE),
      n_converged = sum(converged_both & !error, na.rm = TRUE),
      n_singular = sum(is_singular & !error, na.rm = TRUE),
      n_usable = sum(numerically_usable, na.rm = TRUE),
      pct_error = 100 * n_error / n_total,
      pct_converged = dplyr::if_else(
        n_no_error > 0, 100 * n_converged / n_no_error, NA_real_
      ),
      pct_singular = dplyr::if_else(
        n_no_error > 0, 100 * n_singular / n_no_error, NA_real_
      ),
      pct_usable = 100 * n_usable / n_total,
      .groups = "drop"
    ) %>%
    dplyr::arrange(model_type, transformation, effect_condition, null_type)
}

branch_health_by_spec <- function(prepared_df) {
  prepared_df %>%
    dplyr::group_by(
      model_type, transformation, effect_condition, null_type,
      sample_size, outlier
    ) %>%
    dplyr::summarise(
      n_total = dplyr::n(),
      n_usable = sum(numerically_usable, na.rm = TRUE),
      n_subsamples = dplyr::n_distinct(subsample_id),
      pct_usable = 100 * n_usable / n_total,
      .groups = "drop"
    ) %>%
    dplyr::arrange(
      model_type, transformation, effect_condition, sample_size, outlier
    )
}

identify_problematic_branches <- function(prepared_df) {
  prepared_df %>%
    dplyr::filter(error | !converged_both | is_singular | poor_ci | small_sample) %>%
    dplyr::select(
      branch_id, model_type, effect_condition, null_type, transformation,
      subsample_id, error, converged_both, is_singular, numerically_usable,
      small_sample, poor_ci,
      main_estimate, main_std_error, ci_width,
      main_p_value, n_obs
    ) %>%
    dplyr::mutate(
      problems = paste0(
        dplyr::if_else(error, "error;", ""),
        dplyr::if_else(!error & !converged_both, "non-converged;", ""),
        dplyr::if_else(!error & is_singular, "singular;", ""),
        dplyr::if_else(poor_ci, "wide-CI;", ""),
        dplyr::if_else(small_sample, "small-n;", "")
      )
    ) %>%
    dplyr::arrange(branch_id)
}

estimate_summary <- function(prepared_df) {
  prepared_df %>%
    dplyr::filter(numerically_usable) %>%
    dplyr::group_by(model_type, transformation, effect_condition, null_type) %>%
    dplyr::summarise(
      n_usable = dplyr::n(),
      mean_estimate = mean(main_estimate, na.rm = TRUE),
      median_estimate = median(main_estimate, na.rm = TRUE),
      sd_estimate = sd(main_estimate, na.rm = TRUE),
      mad_estimate = mad(main_estimate, na.rm = TRUE),
      mean_se = mean(main_std_error, na.rm = TRUE),
      mean_aic_improvement = mean(-AIC_diff, na.rm = TRUE),
      mean_bic_improvement = mean(-BIC_diff, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(transformation, effect_condition, null_type, model_type)
}

# POWER ANALYSIS (true-effect branches only)

compute_power_tables <- function(prepared_df, alpha = 0.05) {
  usable_present <- prepared_df %>%
    dplyr::filter(numerically_usable, is_true_effect) %>%
    dplyr::mutate(sig = main_p_value < alpha)

  coarse <- usable_present %>% # maximal design: high-count cells across sample sizes/outliers
    dplyr::group_by(model_type, transformation) %>%
    dplyr::summarise(
      n = dplyr::n(), detected = sum(sig),
      power = detected / n,
      mean_p = mean(main_p_value, na.rm = TRUE),
      median_p = median(main_p_value, na.rm = TRUE),
      mean_estimate = mean(main_estimate, na.rm = TRUE),
      sd_estimate = sd(main_estimate, na.rm = TRUE),
      .groups = "drop"
    )

  # Usually 100 subsamples x 10 outliers per fractional sample-size cell; full sample is deterministic.
  medium <- usable_present %>%
    dplyr::group_by(model_type, transformation, sample_size) %>%
    dplyr::summarise(
      n = dplyr::n(), detected = sum(sig),
      power = detected / n,
      mean_p = mean(main_p_value, na.rm = TRUE),
      mean_estimate = mean(main_estimate, na.rm = TRUE),
      .groups = "drop"
    )

  # Fine: ~5 per cell (one per subsample)
  fine <- usable_present %>%
    dplyr::transmute(
      model_type, transformation, sample_size, outlier,
      significant = sig,
      p_value = main_p_value,
      estimate = main_estimate,
      std_error = main_std_error
    )

  per_branch <- usable_present %>%
    dplyr::transmute(
      model_type, transformation, sample_size, subsample_id, outlier,
      significant = sig, p_value = main_p_value,
      estimate = main_estimate, std_error = main_std_error
    )

  list(coarse = coarse, by_sample_size = medium, by_outlier = fine, per_branch = per_branch)
}

compute_fpr_tables <- function(prepared_df, alpha = 0.05) {
  usable_null <- prepared_df %>%
    dplyr::filter(numerically_usable, is_null_effect) %>%
    dplyr::mutate(
      sig = main_p_value < alpha,
      interpretable_fpr_source = dplyr::coalesce(is_interpretable_nullifier, FALSE),
      rate_label = dplyr::if_else(
        interpretable_fpr_source,
        "nullification-based FPR",
        "diagnostic nullification detection rate"
      )
    )

  coarse <- usable_null %>%
    dplyr::group_by(rate_source, rate_label, interpretable_fpr_source, effect_condition, strip_method, null_type, model_type, transformation) %>%
    dplyr::summarise(
      n = dplyr::n(), false_positives = sum(sig, na.rm = TRUE),
      FPR = false_positives / n,
      mean_p = mean(main_p_value, na.rm = TRUE),
      median_p = median(main_p_value, na.rm = TRUE),
      mean_estimate = mean(main_estimate, na.rm = TRUE),
      .groups = "drop"
    )

  by_sample_size <- usable_null %>%
    dplyr::group_by(
      rate_source,
      rate_label,
      interpretable_fpr_source,
      null_type,
      effect_condition,
      strip_method,
      model_type,
      transformation,
      sample_size
    ) %>%
    dplyr::summarise(
      n = dplyr::n(),
      false_positives = sum(sig, na.rm = TRUE),
      FPR = false_positives / n,
      mean_p = mean(main_p_value, na.rm = TRUE),
      mean_estimate = mean(main_estimate, na.rm = TRUE),
      .groups = "drop"
    )

  by_outlier <- usable_null %>%
    dplyr::group_by(
      rate_source,
      rate_label,
      interpretable_fpr_source,
      null_type,
      effect_condition,
      strip_method,
      model_type,
      transformation,
      sample_size,
      outlier
    ) %>%
    dplyr::summarise(
      n = dplyr::n(),
      false_positives = sum(sig, na.rm = TRUE),
      FPR = false_positives / n,
      mean_estimate = mean(main_estimate, na.rm = TRUE),
      .groups = "drop"
    )

  per_branch <- usable_null %>%
    dplyr::transmute(
      rate_source,
      rate_label,
      interpretable_fpr_source,
      null_type,
      effect_condition,
      strip_method,
      model_type,
      transformation,
      sample_size,
      subsample_id,
      outlier,
      significant = sig,
      p_value = main_p_value,
      estimate = main_estimate,
      std_error = main_std_error
    )

  list(
    coarse = coarse,
    by_sample_size = by_sample_size,
    by_outlier = by_outlier,
    per_branch = per_branch
  )
}

compute_failure_aware_nullification_rates <- function(prepared_df, alpha = 0.05) {
  null_df <- prepared_df %>%
    dplyr::filter(is_null_effect) %>%
    dplyr::mutate(
      sig_primary = numerically_usable & !is.na(main_p_value) & main_p_value < alpha,
      usable_nonsignificant = numerically_usable & !sig_primary,
      non_converged = !error & !converged_both,
      singular = !error & converged_both & dplyr::coalesce(is_singular, FALSE),
      extraction_or_preprocessing_error = error,
      other_invalid = !sig_primary & !usable_nonsignificant & !non_converged & !singular & !extraction_or_preprocessing_error,
      interpretable_fpr_source = dplyr::coalesce(is_interpretable_nullifier, FALSE),
      rate_label = dplyr::if_else(
        interpretable_fpr_source,
        "nullification-based FPR",
        "diagnostic nullification detection rate"
      )
    )

  group_vars <- c(
    "rate_source",
    "rate_label",
    "interpretable_fpr_source",
    "null_type",
    "effect_condition",
    "strip_method",
    "model_type",
    "transformation",
    "sample_size",
    "outlier"
  )

  null_df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::summarise(
      n_planned = dplyr::n(),
      significant_primary = sum(sig_primary, na.rm = TRUE),
      usable_nonsignificant = sum(usable_nonsignificant, na.rm = TRUE),
      singular = sum(singular, na.rm = TRUE),
      non_converged = sum(non_converged, na.rm = TRUE),
      extraction_or_preprocessing_error = sum(extraction_or_preprocessing_error, na.rm = TRUE),
      other_invalid = sum(other_invalid, na.rm = TRUE),
      unconditional_rate = significant_primary / n_planned,
      conditional_n_usable = significant_primary + usable_nonsignificant,
      conditional_rate = dplyr::if_else(conditional_n_usable > 0, significant_primary / conditional_n_usable, NA_real_),
      singular_rate = singular / n_planned,
      non_converged_rate = non_converged / n_planned,
      error_rate = extraction_or_preprocessing_error / n_planned,
      .groups = "drop"
    )
}

compute_roc_metrics <- function(prepared_df, alpha = 0.05) {
  usable <- prepared_df %>%
    dplyr::filter(numerically_usable)

  join_vars <- c("model_type", "transformation", "sample_size")

  tpr_df <- usable %>%
    dplyr::filter(is_true_effect) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(join_vars))) %>%
    dplyr::summarise(
      n_true = dplyr::n(),
      detected = sum(main_p_value < alpha),
      TPR = detected / n_true,
      mean_effect_present = mean(main_estimate, na.rm = TRUE),
      .groups = "drop"
    )

  fpr_df <- usable %>%
    dplyr::filter(is_null_effect) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(join_vars, "null_type")))) %>%
    dplyr::summarise(
      n_null = dplyr::n(),
      false_positives = sum(main_p_value < alpha),
      FPR = false_positives / n_null,
      mean_effect_null = mean(main_estimate, na.rm = TRUE),
      .groups = "drop"
    )

  roc_df <- fpr_df %>%
    dplyr::left_join(tpr_df, by = join_vars) %>%
    dplyr::mutate(
      has_tpr = !is.na(TPR),
      tpr_adj = pmin(pmax(dplyr::coalesce(TPR, 0), 1 / (2 * n_true)), 1 - 1 / (2 * n_true)),
      fpr_adj = pmin(pmax(FPR, 1 / (2 * n_null)), 1 - 1 / (2 * n_null)),
      d_prime = stats::qnorm(tpr_adj) - stats::qnorm(fpr_adj),
      youden_j = dplyr::coalesce(TPR, 0) - FPR,
      DOR = dplyr::case_when(
        is.na(TPR) | is.na(FPR) ~ NA_real_,
        TPR == 0 ~ 0,
        FPR == 0 | TPR == 1 | FPR == 1 ~ NA_real_,
        TRUE ~ (TPR / (1 - TPR)) / (FPR / (1 - FPR))
      )
    ) %>%
    dplyr::select(-tpr_adj, -fpr_adj)

  log_pipeline(logger::INFO, "Computed ROC metrics for {nrow(roc_df)} groupings")
  roc_df
}

#' ROC metrics at outlier granularity (for outlier-comparison plots)
compute_roc_metrics_by_outlier <- function(prepared_df, alpha = 0.05) {
  usable <- prepared_df %>%
    dplyr::filter(numerically_usable)

  join_vars <- c("model_type", "transformation", "sample_size", "outlier")

  tpr_df <- usable %>%
    dplyr::filter(is_true_effect) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(join_vars))) %>%
    dplyr::summarise(
      n_true = dplyr::n(),
      detected = sum(main_p_value < alpha),
      TPR = detected / n_true,
      .groups = "drop"
    )

  fpr_df <- usable %>%
    dplyr::filter(is_null_effect) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(join_vars, "null_type")))) %>%
    dplyr::summarise(
      n_null = dplyr::n(),
      false_positives = sum(main_p_value < alpha),
      FPR = false_positives / n_null,
      .groups = "drop"
    )

  fpr_df %>%
    dplyr::left_join(tpr_df, by = join_vars)
}


# ==============================================================================
# META-ANALYTIC MODELS OF THE MULTIVERSE
# ==============================================================================

#' Prepare model data frame for a logistic regression.
#'
#' Encodes sample_size as an ordered factor (not continuous, because three
#' unevenly-spaced levels spanning [0.5, 1.0] lack the leverage for a linear
#' log-odds slope — the continuous parameterization produces a near-zero,
#' undetectable effect). As an ordered factor, each level gets its own
#' contrast, which can capture nonlinear jumps (e.g., power plateau between
#' 0.75 and 1.0).
#'
#' Consolidates outlier methods that cause complete separation into a
#' combined level. Methods with zero (or all) significant outcomes across
#' the modeled subset produce infinite log-odds and enormous standard
#' errors. Grouping them prevents separation while preserving the
#' information that these methods behave differently.
prepare_model_data <- function(df, alpha = 0.05) {
  df %>%
    dplyr::mutate(
      sig = as.integer(main_p_value < alpha),
      model_type_f = factor(model_type),
      transformation_f = factor(transformation),
      sample_size_c = sample_size,
      outlier_f = factor(outlier),
      subsample_id_f = factor(subsample_id)
    ) %>%
    # Detect and collapse outlier levels with complete separation
    collapse_separated_levels(
      outcome_col = "sig",
      factor_col = "outlier_f"
    )
}

#' Collapse factor levels that cause complete separation.
#'
#' A factor level causes separation when all its observations have the same
#' outcome (all 0 or all 1). The logistic regression cannot estimate a finite
#' coefficient for such levels.
#'
#' Strategy: identify separated levels and merge them into a single
#' "other_separated" bucket. This loses information about which specific
#' level it was, but the original per-branch tables preserve that.
#' The coefficient for "other_separated" is interpretable as "the group of
#' methods that uniformly produced (non-)significance".
collapse_separated_levels <- function(df, outcome_col, factor_col) {
  separation_check <- df %>%
    dplyr::group_by(.data[[factor_col]]) %>%
    dplyr::summarise(
      n = dplyr::n(),
      n_pos = sum(.data[[outcome_col]], na.rm = TRUE),
      n_neg = n - n_pos,
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      is_separated = (n_pos == 0) | (n_neg == 0)
    )

  separated_levels <- separation_check %>%
    dplyr::filter(is_separated) %>%
    dplyr::pull(!!rlang::sym(factor_col))

  if (length(separated_levels) > 0) {
    log_pipeline(
      logger::INFO,
      "Collapsing {length(separated_levels)} separated level(s) in {factor_col}: {paste(separated_levels, collapse = ', ')}"
    )

    current_levels <- levels(df[[factor_col]])
    new_levels <- c(
      setdiff(current_levels, as.character(separated_levels)),
      "other_separated"
    )
    df[[factor_col]] <- forcats::fct_other(
      df[[factor_col]],
      drop = as.character(separated_levels),
      other_level = "other_separated"
    )
  }

  df
}


#' Fit separate models for power, each FPR type, and convergence.
#'
#' Design decisions:
#'
#'   1. NO random effect for branch_id. Each branch_id is unique (n=1 per
#'      group), making a random intercept unidentifiable. The variance
#'      collapses to zero and adds nothing.
#'
#'   2. SEPARATE models for present vs each null_type. These conditions have
#'      completely different base rates (~70% vs ~2-5%). A combined model
#'      requires intercept + null_type offsets that span ~20 log-odds units,
#'      causing quasi-complete separation on the null_type dimension.
#'
#'   3. sample_size as ordered factor. With only 3 levels in [0.5, 1.0],
#'      a continuous parameterization has negligible leverage. An ordered
#'      factor can capture nonlinear jumps between levels.
#'
#'   4. model_type × transformation interaction. Log-transformation changes
#'      the error distribution and may affect which model structures succeed.
#'      This is the only scientifically motivated interaction; adding more
#'      would risk overfitting on ~60-120 branches per model.
#'
#'   5. Collapsed separated outlier levels. Methods like range_1000/range_1250
#'      may produce zero significant results, causing infinite coefficients.
#'      These are merged into "other_separated" to allow estimation.
model_multiverse <- function(prepared_df, alpha = 0.05) {
  usable <- prepared_df %>% dplyr::filter(numerically_usable)

  formula_sig <- sig ~ model_type_f * transformation_f +
    outlier_f + sample_size_c

  formula_sig_re <- sig ~ model_type_f * transformation_f +
    outlier_f + sample_size_c + (1 | subsample_id_f)

  models <- list()

  # ---- POWER model (present condition only) ----
  present_df <- usable %>%
    dplyr::filter(is_true_effect) %>%
    prepare_model_data(alpha = alpha)

  models$power <- fit_with_optional_re(
    present_df, formula_sig_re, formula_sig,
    label = "power"
  )

  # ---- FPR models (one per null_type) ----
  null_types <- unique(usable$null_type[usable$is_null_effect])

  for (nt in null_types) {
    null_df <- usable %>%
      dplyr::filter(null_type == nt) %>%
      prepare_model_data(alpha = alpha)

    safe_name <- gsub(":", "_", nt)
    models[[paste0("fpr_", safe_name)]] <- fit_with_optional_re(
      null_df, formula_sig_re, formula_sig,
      label = paste("FPR", nt)
    )
  }

  # ---- CONVERGENCE model (all non-error branches, all conditions) ----
  conv_df <- prepared_df %>%
    dplyr::filter(!error) %>%
    dplyr::mutate(
      failed = as.integer(!converged_both | is_singular),
      model_type_f = factor(model_type),
      transformation_f = factor(transformation),
      sample_size_c = sample_size,
      outlier_f = factor(outlier),
      effect_condition_f = factor(effect_condition),
      subsample_id_f = factor(subsample_id)
    ) %>%
    collapse_separated_levels(
      outcome_col = "failed",
      factor_col = "outlier_f"
    )

  formula_conv <- failed ~ model_type_f * transformation_f +
    outlier_f + sample_size_c + effect_condition_f

  formula_conv_re <-
    failed ~ model_type_f * transformation_f +
    outlier_f + sample_size_c + effect_condition_f + (1 | subsample_id_f)

  models$convergence <- fit_with_optional_re(
    conv_df, formula_conv_re, formula_conv,
    label = "convergence"
  )

  log_pipeline(
    logger::INFO,
    "Fit {sum(!sapply(models, is.null))} multiverse logistic models"
  )
  models
}


#' Fit a single logistic model with diagnostics.
#'
#' Returns coefficients (odds ratios), predicted probabilities,
#' and model diagnostics. Returns NULL on failure with a warning.
fit_with_optional_re <- function(df, formula_re, formula_fixed, label = "") {
  if (nrow(df) < 10) {
    log_pipeline(logger::WARN, "{label}: too few rows ({nrow(df)}), skipping")
    return(NULL)
  }

  outcome_var <- all.vars(formula_re)[1]
  outcome_vals <- df[[outcome_var]]
  if (all(outcome_vals == 0) || all(outcome_vals == 1)) {
    log_pipeline(logger::WARN, "{label}: constant outcome, skipping")
    return(NULL)
  }

  # Check if there are multiple subsamples
  has_re <- dplyr::n_distinct(df$subsample_id_f) > 1

  fit <- NULL
  used_re <- FALSE

  if (has_re) {
    fit <- tryCatch(
      {
        m <- lme4::glmer(formula_re,
          data = df, family = binomial(link = "logit"),
          control = lme4::glmerControl(optimizer = "bobyqa")
        )
        # Check for singular fit (RE variance ~ 0)
        re_var <- lme4::VarCorr(m)
        if (any(sapply(re_var, function(x) attr(x, "stddev")) < 1e-4)) {
          log_pipeline(logger::INFO, "{label}: RE singular, falling back to GLM")
          NULL
        } else {
          used_re <- TRUE
          m
        }
      },
      error = function(e) {
        log_pipeline(logger::INFO, "{label}: GLMER failed ({e$message}), falling back to GLM")
        NULL
      }
    )
  }

  if (is.null(fit)) {
    fit <- tryCatch(
      stats::glm(formula_fixed, data = df, family = binomial(link = "logit")),
      error = function(e) {
        log_pipeline(logger::WARN, "{label}: GLM also failed: {e$message}")
        NULL
      }
    )
  }

  if (is.null(fit)) {
    return(NULL)
  }

  coef_tbl <- broom.mixed::tidy(fit, conf.int = TRUE)
  has_separation <- any(
    abs(coef_tbl$estimate) > 10 & coef_tbl$std.error > 100,
    na.rm = TRUE
  )

  if (has_separation) {
    log_pipeline(logger::WARN, "{label}: quasi-separation in {sum(abs(coef_tbl$estimate) > 10 & coef_tbl$std.error > 100)} term(s)")
  }

  or_tbl <- coef_tbl %>%
    dplyr::filter(term != "(Intercept)", effects != "ran_pars" | is.na(effects)) %>%
    dplyr::mutate(
      odds_ratio = exp(estimate),
      or_ci_lower = exp(conf.low),
      or_ci_upper = exp(conf.high),
      has_separation = abs(estimate) > 10 & std.error > 100
    )

  pred_tbl <- tryCatch(
    broom::augment(fit, type.predict = "response") %>%
      dplyr::rename(predicted_prob = .fitted, pred_se = .se.fit),
    error = function(e) tibble::tibble()
  )

  deviance <- if (inherits(fit, "glmerMod")) fit@devcomp$cmp["dev"] else fit$deviance
  null_deviance <- if (inherits(fit, "glmerMod")) NA_real_ else fit$null.deviance
  aic_val <- AIC(fit)

  list(
    label = label, fit = fit, used_re = used_re,
    n_obs = nrow(df), n_positive = sum(outcome_vals == 1),
    base_rate = mean(outcome_vals), has_separation = has_separation,
    coefficients = coef_tbl, odds_ratios = or_tbl, predictions = pred_tbl,
    deviance = as.numeric(deviance), null_deviance = as.numeric(null_deviance),
    aic = aic_val,
    pseudo_r2_mcfadden = if (!is.na(null_deviance) && null_deviance > 0) {
      1 - (as.numeric(deviance) / as.numeric(null_deviance))
    } else {
      NA_real_
    }
  )
}

specification_curve_data <- function(prepared_df) {
  prepared_df %>%
    dplyr::transmute(
      branch_id, model_type, transformation, outlier, sample_size, subsample_id,
      effect_condition, null_type, strip_method,
      status = dplyr::case_when(
        error ~ "error",
        !converged_both ~ "non-converged",
        is_singular ~ "singular",
        TRUE ~ "usable"
      ),
      numerically_usable,
      significant = is_significant,
      p_value = dplyr::if_else(numerically_usable, main_p_value, NA_real_),
      estimate = dplyr::if_else(numerically_usable, main_estimate, NA_real_),
      std_error = dplyr::if_else(numerically_usable, main_std_error, NA_real_),
      ci_lower = dplyr::if_else(numerically_usable, effect_ci_lower, NA_real_),
      ci_upper = dplyr::if_else(numerically_usable, effect_ci_upper, NA_real_)
    ) %>%
    dplyr::arrange(
      effect_condition, model_type, transformation, sample_size, outlier
    )
}

detect_specification_inconsistencies <- function(prepared_df, alpha = 0.05) {
  prepared_df %>%
    dplyr::group_by(
      model_type, effect_condition, null_type, transformation, sample_size
    ) %>%
    dplyr::summarise(
      n_total = dplyr::n(),
      n_usable = sum(numerically_usable, na.rm = TRUE),
      n_subsamples = dplyr::n_distinct(subsample_id[numerically_usable]),
      n_sig = sum(
        main_p_value[numerically_usable] < alpha,
        na.rm = TRUE
      ),
      n_nonsig = n_usable - n_sig,
      pct_significant = dplyr::if_else(
        n_usable > 0, 100 * n_sig / n_usable, NA_real_
      ),
      is_inconsistent = (n_sig > 0) & (n_nonsig > 0),
      n_failed = n_total - n_usable,
      pct_failed = 100 * n_failed / n_total,
      effect_mean = mean(main_estimate[numerically_usable], na.rm = TRUE),
      effect_sd = sd(main_estimate[numerically_usable], na.rm = TRUE),
      effect_cv = dplyr::if_else(
        n_usable >= 2 & abs(effect_mean) > 1e-6,
        effect_sd / abs(effect_mean),
        NA_real_
      ),
      p_min = min(main_p_value[numerically_usable], na.rm = TRUE),
      p_max = max(main_p_value[numerically_usable], na.rm = TRUE),
      p_range = p_max - p_min,
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(is_inconsistent), dplyr::desc(pct_failed))
}

summarise_by_factor <- function(prepared_df, group_vars, label = "factor") {
  health <- prepared_df %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c(group_vars, "effect_condition", "null_type")))
    ) %>%
    dplyr::summarise(
      n_total = dplyr::n(),
      n_usable = sum(numerically_usable, na.rm = TRUE),
      pct_usable = 100 * n_usable / n_total,
      .groups = "drop"
    )

  rates <- prepared_df %>%
    dplyr::filter(numerically_usable) %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c(group_vars, "effect_condition", "null_type")))
    ) %>%
    dplyr::summarise(
      n = dplyr::n(),
      n_significant = sum(main_p_value < 0.05, na.rm = TRUE),
      pct_significant = 100 * n_significant / n,
      mean_estimate = mean(main_estimate, na.rm = TRUE),
      median_estimate = median(main_estimate, na.rm = TRUE),
      mean_p = mean(main_p_value, na.rm = TRUE),
      .groups = "drop"
    )

  health %>%
    dplyr::left_join(
      rates,
      by = c(group_vars, "effect_condition", "null_type")
    ) %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(c(
      "effect_condition", "null_type", group_vars
    ))))
}

create_summary_table <- function(prepared_df) {
  prepared_df %>%
    dplyr::group_by(model_type, transformation, effect_condition, null_type) %>%
    dplyr::summarise(
      n_total = dplyr::n(),
      n_usable = sum(numerically_usable, na.rm = TRUE),
      pct_usable = 100 * n_usable / n_total,
      pct_significant = dplyr::if_else(
        n_usable > 0,
        100 * sum(
          main_p_value[numerically_usable] < 0.05,
          na.rm = TRUE
        ) / n_usable,
        NA_real_
      ),
      mean_effect = mean(main_estimate[numerically_usable], na.rm = TRUE),
      sd_effect = sd(main_estimate[numerically_usable], na.rm = TRUE),
      median_p = median(main_p_value[numerically_usable], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      metric_type = dplyr::case_when(
        effect_condition == "present" ~ "Power (TPR)",
        effect_condition == "null_interaction" ~ paste0("FPR (", null_type, ")"),
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::arrange(model_type, transformation, effect_condition, null_type)
}


# MAIN ENTRY POINT


#' Analyze multiverse results across all branches
#'
#' @param results_df Full results tibble
#'
#' @return List containing multiple analysis tables
#'
analyze_multiverse_results <- function(results_df, diagnostics_df = NULL, alpha = 0.05) {
  log_pipeline(
    logger::INFO,
    "Performing multiverse analysis on {nrow(results_df)} branches"
  )

  if (!is.null(diagnostics_df)) {
    if (!exists("join_nullification_diagnostics", mode = "function")) {
      oc_path <- file.path("functions", "nullification_operating_characteristics.R")
      if (!file.exists(oc_path)) oc_path <- file.path("R", "functions", "nullification_operating_characteristics.R")
      source(oc_path)
    }
    results_df <- join_nullification_diagnostics(results_df, diagnostics_df)
  }
  prepared <- prepare_analysis_df(results_df)
  power_tables <- compute_power_tables(prepared, alpha = alpha)
  fpr_tables <- compute_fpr_tables(prepared, alpha = alpha)
  # mv_models <- model_multiverse(prepared, alpha = alpha)

  # # Extract exportable tables from model objects
  model_coefs <- list()
  model_odds <- list()
  model_predictions <- list()
  model_diagnostics <- list()

  # for (nm in names(mv_models)) {
  #   m <- mv_models[[nm]]
  #   if (is.null(m)) next

  #   model_coefs[[paste0("coef_", nm)]] <- m$coefficients
  #   model_odds[[paste0("or_", nm)]] <- m$odds_ratios
  #   model_predictions[[paste0("pred_", nm)]] <- m$predictions
  #   model_diagnostics[[nm]] <- tibble::tibble(
  #     model = nm,
  #     label = m$label,
  #     used_re = m$used_re,
  #     n_obs = m$n_obs,
  #     n_positive = m$n_positive,
  #     base_rate = m$base_rate,
  #     has_separation = m$has_separation,
  #     deviance = m$deviance,
  #     null_deviance = m$null_deviance,
  #     aic = m$aic,
  #     pseudo_r2 = m$pseudo_r2_mcfadden
  #   )
  # }

  diagnostics_tbl <- if (length(model_diagnostics) > 0) {
    dplyr::bind_rows(model_diagnostics)
  } else {
    tibble::tibble()
  }

  analyses <- c(
    list(
      branch_health = branch_health(prepared),
      branch_health_by_spec = branch_health_by_spec(prepared),
      branch_issues = identify_problematic_branches(prepared),
      estimate_summary = estimate_summary(prepared),
      power_coarse = power_tables$coarse,
      power_by_sample_size = power_tables$by_sample_size,
      power_by_outlier = power_tables$by_outlier,
      power_per_branch = power_tables$per_branch,
      fpr_coarse = fpr_tables$coarse,
      fpr_by_sample_size = fpr_tables$by_sample_size,
      fpr_by_outlier = fpr_tables$by_outlier,
      fpr_per_branch = fpr_tables$per_branch,
      nullification_failure_aware_rates = compute_failure_aware_nullification_rates(prepared, alpha = alpha),
      roc_metrics = compute_roc_metrics(prepared, alpha = alpha),
      roc_metrics_by_outlier = compute_roc_metrics_by_outlier(prepared, alpha = alpha),
      by_outlier = summarise_by_factor(prepared, c("outlier", "transformation")),
      by_sample_size = summarise_by_factor(prepared, c("sample_size", "transformation")),
      by_model = summarise_by_factor(prepared, c("model_type", "transformation")),
      spec_curve = specification_curve_data(prepared),
      spec_inconsistencies = detect_specification_inconsistencies(prepared, alpha = alpha),
      summary_table = create_summary_table(prepared),
      model_diagnostics = diagnostics_tbl,
      results_with_diag = prepared
    ),
    model_coefs, model_odds, model_predictions
  )

  log_pipeline(
    logger::INFO,
    "Multiverse analysis complete: {length(analyses)} tables generated"
  )
  analyses
}

get_analysis_run_id <- function() {
  Sys.getenv(
    "PIPELINE_RUN_ID",
    unset = format(Sys.time(), "%Y%m%d_%H%M%S")
  )
}

analysis_run_dir <- function(paths, run_id = get_analysis_run_id()) {
  file.path(paths$outputs_analysis, "runs", run_id)
}

analysis_latest_dir <- function(paths) {
  latest <- file.path(paths$outputs_analysis, "latest")
  link <- Sys.readlink(latest)

  if (nzchar(link)) {
    if (grepl("^/", link)) {
      return(normalizePath(link, mustWork = FALSE))
    }
    return(normalizePath(file.path(dirname(latest), link), mustWork = FALSE))
  }

  latest
}

update_analysis_latest_link <- function(paths, run_dir) {
  root <- paths$outputs_analysis
  latest <- file.path(root, "latest")
  rel_target <- file.path("runs", basename(run_dir))
  tmp <- file.path(root, paste0(".latest.tmp.", Sys.getpid()))

  dir.create(root, recursive = TRUE, showWarnings = FALSE)

  if (file.exists(tmp) || nzchar(Sys.readlink(tmp))) {
    unlink(tmp, recursive = TRUE)
  }

  if (!file.symlink(rel_target, tmp)) {
    stop("Failed to create temporary latest symlink: ", tmp, " -> ", rel_target)
  }

  latest_is_symlink <- nzchar(Sys.readlink(latest))

  if (file.exists(latest) && !latest_is_symlink) {
    backup <- file.path(
      root,
      paste0("latest.backup.", format(Sys.time(), "%Y%m%d_%H%M%S"))
    )

    if (!file.rename(latest, backup)) {
      unlink(tmp)
      stop("Refusing to overwrite non-symlink latest path: ", latest)
    }

    log_pipeline(logger::WARN, "Moved non-symlink latest path to {backup}")
  }

  if (file.exists(latest) || latest_is_symlink) {
    unlink(latest, recursive = TRUE)
  }

  if (!file.rename(tmp, latest)) {
    unlink(tmp)
    stop("Failed to promote latest symlink: ", latest)
  }

  invisible(latest)
}

write_analysis_manifest <- function(output_files, analyses, run_dir, run_id) {
  manifest <- purrr::imap_dfr(output_files, function(path, name) {
    df <- analyses[[name]]

    tibble::tibble(
      run_id = run_id,
      table = name,
      file = basename(path),
      path = path,
      rows = nrow(df),
      columns = ncol(df),
      column_names = paste(names(df), collapse = ","),
      size_bytes = file.info(path)$size,
      md5 = unname(tools::md5sum(path))
    )
  })

  manifest_path <- file.path(run_dir, "analysis_manifest.csv")
  readr::write_csv(manifest, manifest_path)
  manifest_path
}

#' Analyze results and save to disk
#'
#' @param results_df Aggregated results tibble
#' @param paths Project paths object
#' @param alpha Significance threshold
#'
#' @return List of written file paths
#'
analyze_and_save <- function(results_df, paths, diagnostics_csv = NULL, alpha = 0.05) {
  run_id <- get_analysis_run_id()
  run_dir <- analysis_run_dir(paths, run_id = run_id)

  log_pipeline(logger::INFO, "Saving multiverse analysis results to {run_dir}")

  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)

  diagnostics_df <- NULL
  if (!is.null(diagnostics_csv)) {
    diagnostics_df <- readr::read_csv(diagnostics_csv, show_col_types = FALSE)
  }
  analyses <- analyze_multiverse_results(results_df, diagnostics_df = diagnostics_df, alpha = alpha)
  output_files <- list()

  for (name in names(analyses)) {
    df <- analyses[[name]]
    if (!is.data.frame(df) || nrow(df) == 0) {
      log_pipeline(logger::WARN, "Skipping empty or non-df analysis: {name}")
      next
    }

    filename <- paste0(name, ".csv")
    filepath <- file.path(run_dir, filename)
    readr::write_csv(df, filepath)
    log_pipeline(logger::INFO, "Wrote analysis: {filepath}")
    output_files[[name]] <- filepath
  }

  manifest_path <- write_analysis_manifest(
    output_files = output_files,
    analyses = analyses,
    run_dir = run_dir,
    run_id = run_id
  )

  log_pipeline(logger::INFO, "Wrote analysis manifest: {manifest_path}")

  update_analysis_latest_link(paths, run_dir)

  log_pipeline(
    logger::INFO,
    "Analysis complete, {length(output_files)} files saved under run_id={run_id}"
  )

  invisible(analyses)
}
