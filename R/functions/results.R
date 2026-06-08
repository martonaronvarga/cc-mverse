RESULTS_COLUMNS <- c(
  "branch_id", "branch_idx", "sample_size", "subsample_id", "transformation", "outlier",
  "model", "effect_condition", "strip_method", "n_obs", "null_n_obs", "n_participants",
  "full_converged", "null_converged", "full_is_singular", "null_is_singular",
  "full_repca_min_sd", "null_repca_min_sd", "AIC_diff", "BIC_diff", "LR_stat",
  "LR_df", "LR_p", "main_estimate", "null_main_estimate", "main_std_error", "null_std_error", "main_t_stat",
  "null_t_stat",
  "main_p_value", "null_main_p_value", "effect_size", "null_effect", "effect_ci_lower", "null_effect_ci_lower",
  "effect_ci_upper", "null_effect_ci_upper",
  "random_intercept_var", "null_random_intercept_var", "random_slope_var", "null_random_slope_var",
  "random_cong_slope_var", "null_random_cong_slope_var", "random_prev_cong_slope_var", "null_random_prev_cong_slope_var",
  "random_cse_slope_var", "null_random_cse_slope_var", "residual_var",
  "null_residual_var",
  "loglik_full", "loglik_null", "npar_full", "npar_null",
  "fallback_level", "fallback_formula", "fallback_p_value", "inference_source",
  "error", "error_message", "stage_completed", "timestamp"
)

# Function to create Arrow schema when needed (not at load time)
get_results_schema <- function() {
  arrow::schema(
    branch_id = arrow::string(),
    branch_idx = arrow::int32(),
    sample_size = arrow::float64(),
    subsample_id = arrow::int32(),
    transformation = arrow::string(),
    outlier = arrow::string(),
    model = arrow::string(),
    effect_condition = arrow::string(),
    strip_method = arrow::string(),
    n_obs = arrow::int32(),
    null_n_obs = arrow::int32(),
    n_participants = arrow::int32(),
    full_converged = arrow::bool(),
    null_converged = arrow::bool(),
    full_is_singular = arrow::bool(),
    null_is_singular = arrow::bool(),
    full_repca_min_sd = arrow::float64(),
    null_repca_min_sd = arrow::float64(),
    AIC_diff = arrow::float64(),
    BIC_diff = arrow::float64(),
    LR_stat = arrow::float64(),
    LR_df = arrow::int32(),
    LR_p = arrow::float64(),
    main_estimate = arrow::float64(),
    null_main_estimate = arrow::float64(),
    main_std_error = arrow::float64(),
    null_std_error = arrow::float64(),
    main_t_stat = arrow::float64(),
    null_t_stat = arrow::float64(),
    main_p_value = arrow::float64(),
    null_main_p_value = arrow::float64(),
    effect_size = arrow::float64(),
    null_effect = arrow::float64(),
    effect_ci_lower = arrow::float64(),
    null_effect_ci_lower = arrow::float64(),
    effect_ci_upper = arrow::float64(),
    null_effect_ci_upper = arrow::float64(),
    random_intercept_var = arrow::float64(),
    null_random_intercept_var = arrow::float64(),
    random_slope_var = arrow::float64(),
    null_random_slope_var = arrow::float64(),
    random_cong_slope_var = arrow::float64(),
    null_random_cong_slope_var = arrow::float64(),
    random_prev_cong_slope_var = arrow::float64(),
    null_random_prev_cong_slope_var = arrow::float64(),
    random_cse_slope_var = arrow::float64(),
    null_random_cse_slope_var = arrow::float64(),
    residual_var = arrow::float64(),
    null_residual_var = arrow::float64(),
    loglik_full = arrow::float64(),
    loglik_null = arrow::float64(),
    npar_full = arrow::int32(),
    npar_null = arrow::int32(),
    fallback_level = arrow::string(),
    fallback_formula = arrow::string(),
    fallback_p_value = arrow::float64(),
    inference_source = arrow::string(),
    error = arrow::bool(),
    error_message = arrow::string(),
    stage_completed = arrow::string(),
    timestamp = arrow::timestamp("us")
  )
}

#' Extract standardized results from fitted model
#'
#' Converts model objects (lmerMod or aov) to standardized result tibble
#' Extracts fixed effects, random effects variance, model fit metrics
#'
#' @param model_result List from fit_model() containing fitted models and stats
#' @param branch_spec Single row tibble with branch specification
#' @param branch_idx Numeric index of branch in specification list
#'
#' @return Single-row tibble matching RESULTS_SCHEMA with all extracted values
#'
extract_results <- function(model_result, branch_spec, branch_idx) {
  # Handle model fitting errors
  if (!is.null(model_result$error) && model_result$error) {
    return(create_error_result(
      model_result = model_result,
      branch_spec = branch_spec,
      branch_idx = branch_idx
    ))
  }

  # Route to appropriate extraction method
  result <- switch(model_result$type,
    "lmm" = extract_lmm_results(model_result, branch_spec, branch_idx),
    "rmanova" = extract_rmanova_results(model_result, branch_spec, branch_idx),
    create_error_result(
      model_result = list(error = TRUE, message = glue::glue("Unknown model type: {model_result$type}")),
      branch_spec = branch_spec,
      branch_idx = branch_idx
    )
  )

  # Ensure column order matches schema
  result <- result %>%
    dplyr::select(dplyr::all_of(RESULTS_COLUMNS))

  result
}

#' Extract results from linear mixed model (lme4::lmer)
#'
#' @param model_result List with full_model, null_model, coefficients, etc
#' @param branch_spec Branch specification row
#' @param branch_idx Branch index
#'
#' @return Single-row tibble with all extracted values
#'
extract_lmm_results <- function(model_result, branch_spec, branch_idx) {
  full_model <- model_result$full_model
  null_model <- model_result$null_model
  comparison <- model_result$comparison

  # Extract fixed effects (main effect of interest)
  # For interaction model: rt ~ cong * prev_cong + (...)
  # Main effect = interaction term "cong:prev_cong"

  coefs <- model_result$coefficients
  null_coefs <- model_result$null_coefficients

  # Find the intended CSE interaction explicitly; do not select unrelated ':' terms.
  if (!exists("select_cse_coefficient_row", mode = "function")) {
    cse_path <- file.path("functions", "cse_term_extraction.R")
    if (!file.exists(cse_path)) cse_path <- file.path("R", "functions", "cse_term_extraction.R")
    source(cse_path)
  }
  interaction_rows <- select_cse_coefficient_row(coefs)
  null_interaction_rows <- select_cse_coefficient_row(null_coefs)


  main_est <- interaction_rows$estimate[1] %||% {
    log_branch(logger::DEBUG, "No interaction term found for {branch_spec$branch_id}", branch_spec$branch_id)
    NA_real_
  }
  main_se <- interaction_rows$std.error[1] %||% NA_real_
  main_tstat <- interaction_rows$statistic[1] %||% NA_real_
  main_p <- interaction_rows$p.value[1] %||% NA_real_

  null_main_est <- null_interaction_rows$estimate[1] %||% {
    log_branch(logger::DEBUG, "No interaction term in null model for {branch_spec$branch_id}", branch_spec$branch_id)
    0
  }
  null_main_se <- null_interaction_rows$std.error[1] %||% 0
  null_tstat <- null_interaction_rows$statistic[1] %||% 0
  null_main_p <- null_interaction_rows$p.value[1] %||% 0

  # Confidence interval (already in tidy output if conf.int = TRUE)
  ci_lower <- interaction_rows$conf.low[1] %||% NA_real_
  ci_upper <- interaction_rows$conf.high[1] %||% NA_real_

  null_ci_lower <- null_interaction_rows$conf.low[1] %||% NA_real_
  null_ci_upper <- null_interaction_rows$conf.high[1] %||% NA_real_


  # Extract random effects and singularity diagnostics
  ranef_tib <- model_result$random_effects
  null_ranef_tib <- model_result$null_random_effects
  if (!exists("safe_lmm_singularity", mode = "function")) {
    diag_path <- file.path("functions", "lmm_diagnostics.R")
    if (!file.exists(diag_path)) diag_path <- file.path("R", "functions", "lmm_diagnostics.R")
    source(diag_path)
  }
  full_is_singular <- safe_lmm_singularity(full_model)
  null_is_singular <- safe_lmm_singularity(null_model)
  full_repca_min_sd <- safe_repca_min_sd(full_model)
  null_repca_min_sd <- safe_repca_min_sd(null_model)
  random_int_var <- NA_real_
  random_cong_slope_var <- NA_real_
  random_prev_cong_slope_var <- NA_real_
  random_cse_slope_var <- NA_real_
  random_slope_var <- NA_real_
  null_random_int_var <- NA_real_
  null_random_cong_slope_var <- NA_real_
  null_random_prev_cong_slope_var <- NA_real_
  null_random_cse_slope_var <- NA_real_
  null_random_slope_var <- NA_real_

  vc <- lme4::VarCorr(full_model)
  null_vc <- lme4::VarCorr(null_model)
  if (!exists("extract_varcorr_variance", mode = "function")) {
    re_path <- file.path("functions", "random_effect_extraction.R")
    if (!file.exists(re_path)) re_path <- file.path("R", "functions", "random_effect_extraction.R")
    source(re_path)
  }
  random_int_var <- extract_varcorr_variance(vc, term = "(Intercept)")
  null_random_int_var <- extract_varcorr_variance(null_vc, term = "(Intercept)")
  random_cong_slope_var <- extract_varcorr_variance(vc, term = "cong")
  null_random_cong_slope_var <- extract_varcorr_variance(null_vc, term = "cong")
  random_prev_cong_slope_var <- extract_varcorr_variance(vc, term = "prev_cong")
  null_random_prev_cong_slope_var <- extract_varcorr_variance(null_vc, term = "prev_cong")
  random_cse_slope_var <- extract_varcorr_variance(vc, term = "cong:prev_cong")
  null_random_cse_slope_var <- extract_varcorr_variance(null_vc, term = "cong:prev_cong")
  random_slope_var <- random_cse_slope_var
  null_random_slope_var <- null_random_cse_slope_var

  # Model fit metrics from lme4::VarCorr
  resid_var <- attr(vc, "sc")^2 # Residual variance (sigma)
  null_resid_var <- attr(null_vc, "sc")^2

  # Using residual SD as denominator (partial Cohen's d approximation)
  effect_size <- ifelse(
    !is.na(main_est) && !is.na(resid_var) && resid_var > 0,
    main_est / sqrt(resid_var),
    NA_real_
  )
  null_effect <- ifelse(
    (null_main_est != 0) && !is.na(null_resid_var) && null_resid_var > 0,
    null_main_est / sqrt(null_resid_var),
    NA_real_
  )

  # Log-likelihood
  ll_full_obj <- stats::logLik(model_result$refit_full)
  ll_null_obj <- stats::logLik(model_result$refit_null)

  # Number of parameters
  npar_full <- attr(ll_full_obj, "df") %>% as.numeric()
  npar_null <- attr(ll_null_obj, "df") %>% as.numeric()

  ll_full <- as.numeric(ll_full_obj)
  ll_null <- as.numeric(ll_null_obj)

  performance <- model_result$performance

  # Build result tibble
  tibble::tibble(
    # Branch identification
    branch_id = branch_spec$branch_id,
    branch_idx = branch_idx,
    sample_size = branch_spec$sample_size,
    subsample_id = as.integer(branch_spec$subsample_id),
    transformation = branch_spec$transformation,
    outlier = branch_spec$outlier,
    model = branch_spec$model,
    effect_condition = branch_spec$effect_condition,
    strip_method = branch_spec$strip_method,

    # Data characteristics
    n_obs = stats::nobs(full_model) %>% as.integer(),
    null_n_obs = stats::nobs(null_model) %>% as.integer(),
    n_participants = model_result$n_participants %>% as.integer(),

    # Convergence
    full_converged = model_result$full_converged,
    null_converged = model_result$null_converged,
    full_is_singular = full_is_singular,
    null_is_singular = null_is_singular,
    full_repca_min_sd = full_repca_min_sd,
    null_repca_min_sd = null_repca_min_sd,
    AIC_diff = (performance$full_AIC - performance$null_AIC) %>% as.numeric(),
    BIC_diff = (performance$full_BIC - performance$null_BIC) %>% as.numeric(),
    LR_stat = performance$LR_stat,
    LR_df = performance$LR_df,
    LR_p = performance$LR_p,

    # Fixed effect (main effect of interest)
    main_estimate = main_est,
    null_main_estimate = null_main_est,
    main_std_error = main_se,
    null_std_error = null_main_se,
    main_t_stat = main_tstat,
    null_t_stat = null_tstat,
    main_p_value = main_p,
    null_main_p_value = null_main_p,

    # Effect size
    effect_size = effect_size,
    null_effect = null_effect,
    effect_ci_lower = ci_lower,
    null_effect_ci_lower = null_ci_lower,
    effect_ci_upper = ci_upper,
    null_effect_ci_upper = null_ci_upper,

    # Random effects variance
    random_intercept_var = random_int_var,
    null_random_intercept_var = null_random_int_var,
    random_slope_var = random_slope_var,
    null_random_slope_var = null_random_slope_var,
    random_cong_slope_var = random_cong_slope_var,
    null_random_cong_slope_var = null_random_cong_slope_var,
    random_prev_cong_slope_var = random_prev_cong_slope_var,
    null_random_prev_cong_slope_var = null_random_prev_cong_slope_var,
    random_cse_slope_var = random_cse_slope_var,
    null_random_cse_slope_var = null_random_cse_slope_var,
    residual_var = resid_var %>% as.numeric(),
    null_residual_var = null_resid_var %>% as.numeric(),

    # Model fit
    loglik_full = ll_full,
    loglik_null = ll_null,
    npar_full = npar_full,
    npar_null = npar_null,

    # Sensitivity-only fallback inference; primary fields above remain maximal-model results.
    fallback_level = model_result$fallback_level %||% NA_character_,
    fallback_formula = model_result$fallback_formula %||% NA_character_,
    fallback_p_value = model_result$fallback_p_value %||% NA_real_,
    inference_source = dplyr::if_else(
      is.na(model_result$fallback_level %||% NA_character_),
      "primary_maximal",
      "primary_maximal_invalid_fallback_sensitivity_available"
    ),

    # Error tracking
    error = FALSE,
    error_message = NA_character_,

    # Metadata
    stage_completed = "modeling",
    timestamp = Sys.time()
  )
}

#' Extract results from repeated measures ANOVA (afex::aov_car)
#'
#' @param model_result List with full_model and full_stats
#' @param branch_spec Branch specification row
#' @param branch_idx Branch index
#'
#' @return Single-row tibble with extracted values
#'
extract_rmanova_results <- function(model_result, branch_spec, branch_idx) {
  full_stats <- model_result$full_stats

  # For RMANOVA, extract the named CSE interaction instead of relying on row order.
  if (!exists("extract_rmanova_cse_stats", mode = "function")) {
    cse_path <- file.path("functions", "cse_term_extraction.R")
    if (!file.exists(cse_path)) cse_path <- file.path("R", "functions", "cse_term_extraction.R")
    source(cse_path)
  }
  cse_stats <- extract_rmanova_cse_stats(full_stats)
  interaction_row <- cse_stats$row

  # Extract statistics robustly across afex versions/correction settings.
  f_stat <- cse_stats$f_stat
  p_value <- cse_stats$p_value
  num_df <- cse_stats$num_df

  # Convert F-statistic to approximate LR statistic.
  lr_stat <- ifelse(!is.na(f_stat), f_stat * (num_df %||% 1L), NA_real_)

  # Effect size.
  eta_sq <- cse_stats$effect_size

  # Build result tibble
  tibble::tibble(
    # Branch identification
    branch_id = branch_spec$branch_id,
    branch_idx = branch_idx,
    sample_size = branch_spec$sample_size,
    subsample_id = as.integer(branch_spec$subsample_id),
    transformation = branch_spec$transformation,
    outlier = branch_spec$outlier,
    model = branch_spec$model,
    effect_condition = branch_spec$effect_condition,
    strip_method = branch_spec$strip_method,

    # Data characteristics
    n_obs = model_result$n_obs %>% as.integer(),
    null_n_obs = model_result$n_obs %>% as.integer(),
    n_participants = model_result$n_participants %>% as.integer(),

    # Convergence (RMANOVA doesn't have optimizer convergence/singularity)
    full_converged = TRUE,
    null_converged = TRUE,
    full_is_singular = FALSE,
    null_is_singular = FALSE,
    full_repca_min_sd = NA_real_,
    null_repca_min_sd = NA_real_,

    # Model comparison (RMANOVA doesn't have formal model comparison)
    AIC_diff = NA_real_,
    BIC_diff = NA_real_,
    LR_stat = lr_stat,
    LR_df = num_df %>% as.integer(),
    LR_p = p_value,

    # Fixed effect (main effect)
    main_estimate = NA_real_,
    null_main_estimate = NA_real_,
    main_std_error = NA_real_,
    null_std_error = NA_real_,
    main_t_stat = f_stat, # F-stat as proxy
    null_t_stat = f_stat,
    main_p_value = p_value,
    null_main_p_value = p_value,

    # Effect size
    effect_size = eta_sq,
    null_effect = eta_sq,
    effect_ci_lower = NA_real_,
    null_effect_ci_lower = NA_real_,
    effect_ci_upper = NA_real_,
    null_effect_ci_upper = NA_real_,

    # Random effects (RMANOVA: within-subject variance)
    random_intercept_var = NA_real_,
    null_random_intercept_var = NA_real_,
    random_slope_var = NA_real_,
    null_random_slope_var = NA_real_,
    random_cong_slope_var = NA_real_,
    null_random_cong_slope_var = NA_real_,
    random_prev_cong_slope_var = NA_real_,
    null_random_prev_cong_slope_var = NA_real_,
    random_cse_slope_var = NA_real_,
    null_random_cse_slope_var = NA_real_,
    residual_var = NA_real_,
    null_residual_var = NA_real_,

    # Model fit (not applicable for RMANOVA)
    loglik_full = NA_real_,
    loglik_null = NA_real_,
    npar_full = NA_integer_,
    npar_null = NA_integer_,

    # Fallback inference (not applicable for RMANOVA)
    fallback_level = NA_character_,
    fallback_formula = NA_character_,
    fallback_p_value = NA_real_,
    inference_source = "primary_rmanova",

    # Error tracking
    error = FALSE,
    error_message = NA_character_,

    # Metadata
    stage_completed = "modeling",
    timestamp = Sys.time()
  )
}

#' Create error result row
#'
#' @param model_result List with error flag and message
#' @param branch_spec Branch specification
#' @param branch_idx Branch index
#'
#' @return Single-row error tibble
#'
create_error_result <- function(model_result, branch_spec, branch_idx) {
  tibble::tibble(
    branch_id = branch_spec$branch_id,
    branch_idx = branch_idx,
    sample_size = branch_spec$sample_size,
    subsample_id = as.integer(branch_spec$subsample_id),
    transformation = branch_spec$transformation,
    outlier = branch_spec$outlier,
    model = branch_spec$model,
    effect_condition = branch_spec$effect_condition,
    strip_method = branch_spec$strip_method,
    n_obs = NA_integer_,
    null_n_obs = NA_integer_,
    n_participants = NA_integer_,
    full_converged = NA,
    null_converged = NA,
    full_is_singular = NA,
    null_is_singular = NA,
    full_repca_min_sd = NA_real_,
    null_repca_min_sd = NA_real_,
    AIC_diff = NA_real_,
    BIC_diff = NA_real_,
    LR_stat = NA_real_,
    LR_df = NA_integer_,
    LR_p = NA_real_,
    main_estimate = NA_real_,
    null_main_estimate = NA_real_,
    main_std_error = NA_real_,
    null_std_error = NA_real_,
    main_t_stat = NA_real_,
    null_t_stat = NA_real_,
    main_p_value = NA_real_,
    null_main_p_value = NA_real_,
    effect_size = NA_real_,
    null_effect = NA_real_,
    effect_ci_lower = NA_real_,
    null_effect_ci_lower = NA_real_,
    effect_ci_upper = NA_real_,
    null_effect_ci_upper = NA_real_,
    random_intercept_var = NA_real_,
    null_random_intercept_var = NA_real_,
    random_slope_var = NA_real_,
    null_random_slope_var = NA_real_,
    random_cong_slope_var = NA_real_,
    null_random_cong_slope_var = NA_real_,
    random_prev_cong_slope_var = NA_real_,
    null_random_prev_cong_slope_var = NA_real_,
    random_cse_slope_var = NA_real_,
    null_random_cse_slope_var = NA_real_,
    residual_var = NA_real_,
    null_residual_var = NA_real_,
    loglik_full = NA_real_,
    loglik_null = NA_real_,
    npar_full = NA_integer_,
    npar_null = NA_integer_,
    fallback_level = NA_character_,
    fallback_formula = NA_character_,
    fallback_p_value = NA_real_,
    inference_source = "error",
    error = TRUE,
    error_message = model_result$message %||% "Unknown error",
    stage_completed = "failed",
    timestamp = Sys.time()
  )
}


#' Save branch results safely (parallel-safe)
#'
#' @param results_tbl Tibble from extract_model_results()
#' @param paths Project paths object
#' @param branch_id Unique branch identifier
#'
#' @return Invisibly returns results_tbl

result_path_for_branch <- function(paths, branch_id) {
  file.path(get_results_path(paths), glue::glue("results_{branch_id}.parquet"))
}

result_path_for_chunk <- function(paths, chunk_id) {
  file.path(get_results_path(paths), sprintf("results_chunk_%05d.parquet", as.integer(chunk_id)))
}

write_results_parquet_atomic <- function(results_tbl, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(pattern = paste0(basename(path), ".tmp_"), tmpdir = dirname(path), fileext = ".parquet")
  on.exit(unlink(tmp), add = TRUE)
  arrow::write_parquet(results_tbl, tmp)
  if (!file.rename(tmp, path)) {
    file.copy(tmp, path, overwrite = TRUE)
    unlink(tmp)
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

append_results <- function(results_tbl, paths, branch_id) {
  branch_file <- result_path_for_branch(paths, branch_id)

  tryCatch(
    {
      out <- write_results_parquet_atomic(results_tbl, branch_file)
      log_branch(logger::INFO, "Saved {nrow(results_tbl)} result row(s) to {branch_file}", branch_id)
      return(out)
    },
    error = function(e) {
      log_branch(logger::ERROR, "Failed to save branch {branch_id}: {e$message}", branch_id)
      stop(e)
    }
  )
}


#' Save processing metadata (branch specifications, status, timing)
#'
#' @param metadata_tbl Metadata tibble
#' @param metadata_path Path to metadata file
#'
#' @return Invisibly TRUE
#'
save_processing_metadata <- function(metadata_tbl, metadata_path) {
  log_pipeline(logger::DEBUG, "Saving metadata: {nrow(metadata_tbl)} entries")

  dir <- dirname(metadata_path)
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }

  tryCatch(
    {
      if (file.exists(metadata_path)) {
        existing <- arrow::read_parquet(metadata_path)
        combined <- dplyr::bind_rows(existing, metadata_tbl)
      } else {
        combined <- metadata_tbl
      }

      arrow::write_parquet(combined, metadata_path)


      invisible(TRUE)
    },
    error = function(e) {
      log_pipeline(logger::ERROR, "Failed to save metadata: {e$message}")
      stop(e)
    }
  )
}

#' Load all branch results in parallel-efficient way
#'
#' @param paths Project paths object
#' @param required_cols Optional vector of required columns
#' @return Tibble with all results aggregated
load_results <- function(paths, validate = TRUE) {
  results_dir <- get_results_path(paths)

  if (!dir.exists(results_dir)) {
    log_pipeline(logger::WARN, "Results directory does not exist: {results_dir}")
    return(tibble::tibble())
  }

  # Count result files explicitly. Do not open the directory wholesale because
  # hidden schema anchors and stale branch-level files can otherwise be mixed in.
  chunk_files <- list.files(
    results_dir,
    pattern = "^results_chunk_[0-9]+\\.parquet$",
    full.names = TRUE
  )
  branch_files <- list.files(
    results_dir,
    pattern = "^results_[^/]+\\.parquet$",
    full.names = TRUE
  )
  branch_files <- branch_files[!grepl("^results_chunk_", basename(branch_files))]

  # Prefer chunk files if present. This prevents duplicate aggregation when a
  # repository contains stale branch-level outputs from the old tar_map design.
  result_files <- if (length(chunk_files) > 0L) chunk_files else branch_files
  result_files <- result_files[basename(result_files) != ".schema.parquet"]

  if (length(result_files) == 0) {
    log_pipeline(logger::WARN, "No result files found in {results_dir}")
    return(tibble::tibble())
  }

  tryCatch(
    {
      ds <- arrow::open_dataset(result_files, format = "parquet")
      results <- ds %>% dplyr::collect()

      log_pipeline(logger::INFO, "Aggregated {nrow(results)} rows from {length(result_files)} result file(s)")

      if (validate) {
        missing <- setdiff(RESULTS_COLUMNS, names(results))
        if (length(missing) > 0) {
          log_pipeline(logger::ERROR, "Missing columns: {paste(missing, collapse = ', ')}")
          stop("Schema validation failed")
        }
      }

      results
    },
    error = function(e) {
      log_pipeline(logger::ERROR, "Failed to aggregate results: {e$message}")
      stop(e)
    }
  )
}

#' Initialize results schema validation
#'
#' Creates empty parquet file with correct schema for validation
#'
#' @param results_dir Output directory
#'
#' @return Invisibly TRUE
#'
initialize_results_schema <- function(results_dir, schema = NULL, schema_filename = ".schema.parquet") {
  if (is.null(schema)) schema <- get_results_schema()
  if (!dir.exists(results_dir)) {
    dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Build an explicitly typed empty tibble that matches the schema.
  empty_df <- empty_tibble_from_schema(schema)

  # Create an Arrow table with the exact schema.
  empty_table <- arrow::as_arrow_table(empty_df, schema = schema)

  # Write a hidden schema file to anchor the directory’s schema.
  schema_file <- file.path(results_dir, schema_filename)
  arrow::write_parquet(empty_table, schema_file)
  invisible(TRUE)
}

empty_tibble_from_schema <- function(schema) {
  stopifnot(inherits(schema, "Schema"))

  fields <- schema$fields

  make_vec <- function(field) {
    # Map Arrow types to zero-length R vectors of the appropriate storage mode.
    tstr <- field$type$ToString()

    if (tstr == "string") {
      return(character(0))
    }

    if (tstr == "bool") {
      return(logical(0))
    }

    if (tstr %in% c("int8", "int16", "int32", "uint8", "uint16", "uint32")) {
      return(integer(0))
    }

    if (tstr %in% c("int64", "uint64")) {
      # R has no 64-bit integer; use double as a safe container when needed.
      return(double(0))
    }

    if (tstr %in% c("float32", "float64", "double")) {
      return(double(0))
    }

    if (startsWith(tstr, "timestamp")) {
      # Example ToString(): "timestamp[us]" or "timestamp[ms, tz=UTC]"
      # Represent as POSIXct; Arrow will map POSIXct <-> timestamp[*].
      return(as.POSIXct(numeric(0), origin = "1970-01-01", tz = "UTC"))
    }

    stop(sprintf("Unsupported Arrow type in schema: %s (field: %s)", tstr, field$name))
  }

  cols <- lapply(fields, make_vec)
  names(cols) <- vapply(fields, function(f) f$name, character(1))

  tibble::as_tibble(cols)
}

#' Fit model for a single branch and save results atomically
#'
#' @param idx Branch index
#' @param branch_specs All branch specifications
#' @param paths Project paths
#' @param config Configuration
#'
#' @return Result tibble (1 row)
#'
branch_spec_from_row <- function(b) {
  list(
    idx = b$idx[[1]],
    branch_id = b$branch_id[[1]],
    sample_size = b$sample_size[[1]],
    subsample_id = b$subsample_id[[1]],
    transformation = b$transformation[[1]],
    outlier = b$outlier[[1]],
    model = b$model[[1]],
    effect_condition = b$effect_condition[[1]],
    strip_method = b$strip_method[[1]]
  )
}

fit_branch_result_from_data <- function(data, branch_spec, config) {
  idx <- branch_spec$idx
  branch_id <- branch_spec$branch_id

  tryCatch(
    {
      model_spec <- get_model_spec(config, branch_spec$model)
      model_result <- fit_model(
        data = data,
        model_spec = model_spec,
        model_name = branch_spec$model,
        branch_id = branch_id
      )

      results_tbl <- extract_results(model_result, branch_spec, idx)

      log_branch(
        logger::INFO,
        "DONE branch {idx}: p={results_tbl$main_p_value}, converged={results_tbl$full_converged}",
        branch_id
      )

      results_tbl
    },
    error = function(e) {
      log_branch(logger::ERROR, "FAILED branch {idx}: {e$message}", branch_id)
      create_error_result(list(error = TRUE, message = e$message), branch_spec, idx)
    }
  )
}

fit_branch_result <- function(
    idx,
    branch_id, sample_size, subsample_id,
    transformation, outlier, model, effect_condition,
    strip_method, paths, config) {
  setup_logging(log_level = config$log_level, log_dir = paths$logs)
  log_branch(logger::INFO, "START branch {idx}: {branch_id}", branch_id)

  branch_spec <- list(
    idx = idx,
    branch_id = branch_id,
    sample_size = sample_size,
    subsample_id = subsample_id,
    transformation = transformation,
    outlier = outlier,
    model = model,
    effect_condition = effect_condition,
    strip_method = strip_method
  )

  tryCatch(
    {
      data_id <- data_id_from_branch_id(branch_id)
      data_file <- get_processed_data_path(paths, data_id)

      if (!file.exists(data_file)) {
        log_branch(logger::ERROR, "Processed data not found: {data_file}", branch_id)
        stop("Missing processed data")
      }

      data <- safe_read_parquet(data_file)
      log_branch(logger::DEBUG, "Loaded {nrow(data)} observations", branch_id)
      fit_branch_result_from_data(data, branch_spec, config)
    },
    error = function(e) {
      log_branch(logger::ERROR, "FAILED branch {idx}: {e$message}", branch_id)
      create_error_result(list(error = TRUE, message = e$message), branch_spec, idx)
    }
  )
}

fit_and_save_branch <- function(
    idx,
    branch_id, sample_size, subsample_id,
    transformation, outlier, model, effect_condition,
    strip_method, paths, config) {
  output_path <- result_path_for_branch(paths, branch_id)
  if (file.exists(output_path) && !isTRUE(config$overwrite_results)) {
    log_branch(logger::INFO, "SKIP existing branch result: {output_path}", branch_id)
    return(normalizePath(output_path, winslash = "/", mustWork = TRUE))
  }

  results_tbl <- fit_branch_result(
    idx = idx,
    branch_id = branch_id,
    sample_size = sample_size,
    subsample_id = subsample_id,
    transformation = transformation,
    outlier = outlier,
    model = model,
    effect_condition = effect_condition,
    strip_method = strip_method,
    paths = paths,
    config = config
  )

  append_results(results_tbl, paths, branch_id)
}

fit_and_save_branch_chunk <- function(branch_chunk, paths, config) {
  if (!is.data.frame(branch_chunk) || nrow(branch_chunk) == 0L) {
    stop("branch_chunk must be a non-empty data frame")
  }

  chunk_id <- unique(branch_chunk$chunk_id)
  if (length(chunk_id) != 1L || is.na(chunk_id)) {
    stop("branch_chunk must contain exactly one non-missing chunk_id")
  }
  chunk_id <- as.integer(chunk_id)
  output_path <- result_path_for_chunk(paths, chunk_id)

  if (file.exists(output_path) && !isTRUE(config$overwrite_results)) {
    log_pipeline(logger::INFO, "Skipping existing model chunk {chunk_id}: {output_path}")
    return(normalizePath(output_path, winslash = "/", mustWork = TRUE))
  }

  setup_logging(log_level = config$log_level, log_dir = paths$logs)
  log_pipeline(logger::INFO, "START model chunk {chunk_id}: {nrow(branch_chunk)} branches")

  # Minimize filesystem pressure: a chunk commonly contains multiple model
  # branches for the same processed data_id. Read each parquet once, then fit
  # all branch/model variants that share it.
  if (!"data_id" %in% names(branch_chunk)) {
    branch_chunk$data_id <- vapply(branch_chunk$branch_id, data_id_from_branch_id, character(1))
  }

  rows <- list()
  data_groups <- split(branch_chunk, branch_chunk$data_id)
  for (data_id in names(data_groups)) {
    data_file <- get_processed_data_path(paths, data_id)
    if (!file.exists(data_file)) {
      log_pipeline(logger::ERROR, "Processed data not found for chunk {chunk_id}: {data_file}")
      missing_rows <- lapply(seq_len(nrow(data_groups[[data_id]])), function(i) {
        b <- data_groups[[data_id]][i, , drop = FALSE]
        spec <- branch_spec_from_row(b)
        create_error_result(list(error = TRUE, message = "Missing processed data"), spec, spec$idx)
      })
      rows <- c(rows, missing_rows)
      next
    }

    data <- safe_read_parquet(data_file)
    log_pipeline(
      logger::DEBUG,
      "Chunk {chunk_id}: loaded {nrow(data)} observations for data_id={data_id} ({nrow(data_groups[[data_id]])} model branch(es))"
    )

    group_rows <- lapply(seq_len(nrow(data_groups[[data_id]])), function(i) {
      b <- data_groups[[data_id]][i, , drop = FALSE]
      spec <- branch_spec_from_row(b)
      log_branch(logger::INFO, "START branch {spec$idx}: {spec$branch_id}", spec$branch_id)
      fit_branch_result_from_data(data, spec, config)
    })
    rows <- c(rows, group_rows)
  }

  results_tbl <- dplyr::bind_rows(rows) %>%
    dplyr::select(dplyr::all_of(RESULTS_COLUMNS))

  out <- write_results_parquet_atomic(results_tbl, output_path)
  log_pipeline(logger::INFO, "DONE model chunk {chunk_id}: wrote {nrow(results_tbl)} rows to {out}")
  out
}
