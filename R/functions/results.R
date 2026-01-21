# R/functions/results.R
# Result extraction, storage, and metadata tracking

RESULTS_COLUMNS <- c(
  "branch_id", "branch_idx", "sample_size", "transformation", "outlier",
  "model", "effect_condition", "strip_method", "n_obs", "null_n_obs", "n_participants",
  "full_converged", "null_converged", "AIC_diff", "BIC_diff", "LR_stat",
  "LR_df", "LR_p", "main_estimate", "null_main_estimate", "main_std_error", "null_std_error", "main_t_stat",
  "null_t_stat",
  "main_p_value", "null_main_p_value", "effect_size", "null_effect_size", "effect_ci_lower", "null_effect_ci_lower",
  "effect_ci_upper", "null_effect_ci_upper",
  "random_intercept_var", "null_random_intercept_var", "random_slope_var", "null_random_slope_var", "residual_var",
  "null_residual_var",
  "loglik_full", "loglik_null", "npar_full", "npar_null",
  "error", "error_message", "stage_completed", "timestamp"
)

# Function to create Arrow schema when needed (not at load time)
get_results_schema <- function() {
  arrow::schema(
    branch_id = arrow::string(),
    branch_idx = arrow::int32(),
    sample_size = arrow::float64(),
    transformation = arrow::string(),
    outlier = arrow::string(),
    model = arrow::string(),
    effect_condition = arrow::string(),
    strip_method = arrow::string(),
    n_obs = arrow::int32(),
    null_n_obs = arrow::int32,
    n_participants = arrow::int32(),
    full_converged = arrow::bool(),
    null_converged = arrow::bool(),
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
    residual_var = arrow::float64(),
    null_residual_var = arrow::float64(),
    loglik_full = arrow::float64(),
    loglik_null = arrow::float64(),
    npar_full = arrow::int32(),
    npar_null = arrow::int32(),
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
      model_result = list(error = TRUE, error_message = glue::glue("Unknown model type: {model_result$type}")),
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

  # Find main effect (interaction term if present, else main cong effect)

  interaction_rows <- coefs %>%
    dplyr::filter(
      stringr::str_detect(term, ":")
    )
  null_interaction_rows <- null_coefs %>%
    dplyr::filter(
      dplyr::str_detect(term, ":")
    )

  main_est <- interaction_rows$estimate[1] %||% NA_real_
  main_se <- interaction_rows$std.error[1] %||% NA_real_
  main_tstat <- interaction_rows$statistic[1] %||% NA_real_
  main_p <- interaction_rows$p.value[1] %||% NA_real_

  null_main_est <- null_interaction_rows$estimate[1] %||% NA_real_
  null_main_se <- null_interaction_rows$std.error[1] %||% NA_real_
  null_tstat <- null_interaction_rows$statistic[1] %||% NA_real_
  null_main_p <- null_interaction_rows$p.value[1] %||% NA_real_ # Effect size (Cohen's d approximation from t-statistic)
  null_effect_size <- ifelse(!is.na(null_main_tstat), null_main_tstat / sqrt(nobs(null_model)), NA_real_)

  # Confidence interval (already in tidy output if conf.int = TRUE)
  ci_lower <- interaction_rows$conf.low[1] %||% NA_real_
  ci_upper <- interaction_rows$conf.high[1] %||% NA_real_

  null_ci_lower <- null_interaction_rows$conf.low[1] %||% NA_real_
  null_ci_upper <- null_interaction_rows$conf.high[1] %||% NA_real_


  # Extract random effects variance
  ranef_tib <- model_result$random_effects
  null_ranef_tib <- model_result$null_random_efects
  random_int_var <- NA_real_
  random_slope_var <- NA_real_
  null_random_int_var <- NA_real_
  null_random_slope_var <- NA_real_

  vc <- lme4::VarCorr(full_model)
  null_vc <- lme4::VarCorr(null_model)
  random_int_var <- as.numeric(attr(vc$participant_id, "stddev")["(Intercept)"]^2)
  null_random_int_var <- as.numeric(attr(null_vc$participant_id, "stddev")["(Intercept)"]^2)

  # Model fit metrics from lme4::VarCorr
  resid_var <- attr(vc, "sc")^2 # Residual variance (sigma)
  null_resid_var <- attr(null_vc, "sc")^2

  # Using residual SD as denominator (partial Cohen's d approximation)
  effect_size <- ifelse(
    !is.na(main_est) && !is.na(resid_var) && resid_var > 0,
    main_est / sqrt(resid_var),
    NA_real_
  )
  null_effect_size <- ifelse(
    !is.na(null_main_est) && !is.na(null_resid_var) && null_resid_var > 0,
    null_main_est / sqrt(null_resid_var),
    NA_real_
  )

  # Log-likelihood
  ll_full <- logLik(full_model) %>% as.numeric()
  ll_null <- logLik(null_model) %>% as.numeric()

  # Number of parameters
  npar_full <- attr(logLik(full_model), "df") %>% as.numeric()
  npar_null <- attr(logLik(null_model), "df") %>% as.numeric()

  performance <- model_result$performance

  # Build result tibble
  tibble::tibble(
    # Branch identification
    branch_id = branch_spec$branch_id,
    branch_idx = branch_idx,
    sample_size = branch_spec$sample_size,
    transformation = branch_spec$transformation,
    outlier = branch_spec$outlier,
    model = branch_spec$model,
    effect_condition = branch_spec$effect_condition,
    strip_method = branch_spec$strip_method,

    # Data characteristics
    n_obs = nobs(full_model) %>% as.integer(),
    null_n_obs = nobs(null_model) %>% as.integer(),
    n_participants = model_result$n_participants %>% as.integer(),

    # Convergence
    full_converged = model_result$full_converged,
    null_converged = model_result$null_converged,
    AIC_diff = (performance$full_AIC - performance$null_AIC) %>% as.numeric(),
    BIC_diff = (performance$full_BIC - performance$null_bic) %>% as.numeric(),
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
    null_effect_size = null_effect_size,
    effect_ci_lower = ci_lower,
    null_effect_ci_lower = null_ci_lower,
    effect_ci_upper = ci_upper,
    null_effect_ci_upper = null_ci_upper,

    # Random effects variance
    random_intercept_var = random_int_var,
    null_random_intercept_var = null_random_int_var,
    random_slope_var = random_slope_var,
    null_random_slope_var = null_random_slope_var,
    residual_var = resid_var %>% as.numeric(),
    null_residual_var = null_resid_var %>% as.numeric(),

    # Model fit
    loglik_full = ll_full,
    loglik_null = ll_null,
    npar_full = npar_full,
    npar_null = npar_null,

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
  full_stats <- model_result$full_stats %>%
    dplyr::filter(term != "Residuals")

  # For RMANOVA, extract main interaction effect
  # Priority: "cong:prev_cong" > "cong" > first non-error effect

  interaction_row <- full_stats %>%
    dplyr::filter(grepl(":", term)) %>%
    dplyr::slice(1)

  if (nrow(interaction_row) == 0) {
    interaction_row <- full_stats %>%
      dplyr::filter(grepl("cong", term, ignore.case = TRUE)) %>%
      dplyr::slice(1)
  }

  if (nrow(interaction_row) == 0) {
    interaction_row <- full_stats %>% dplyr::slice(1)
  }

  # Extract statistics
  f_stat <- interaction_row$statistic[1] %||% NA_real_
  p_value <- interaction_row$p.value[1] %||% NA_real_
  num_df <- interaction_row$df[1] %||% NA_integer_

  # Convert F-statistic to approximate LR statistic
  # F = LR / df_full, so LR ≈ F * df
  lr_stat <- ifelse(!is.na(f_stat), f_stat * (num_df %||% 1), NA_real_)

  # Effect size NA
  eta_sq <- interaction_row$ges[1] %||% NA_real_

  # Build result tibble
  tibble::tibble(
    # Branch identification
    branch_id = branch_spec$branch_id,
    branch_idx = branch_idx,
    sample_size = branch_spec$sample_size,
    transformation = branch_spec$transformation,
    outlier = branch_spec$outlier,
    model = branch_spec$model,
    effect_condition = branch_spec$effect_condition,
    strip_method = branch_spec$strip_method,

    # Data characteristics
    n_obs = model_result$n_obs %>% as.integer(),
    null_n_obs = model_result$n_obs %>% as.integer(),
    n_participants = model_result$n_participants %>% as.integer(),

    # Convergence (RMANOVA doesn't have convergence issues)
    full_converged = TRUE,
    null_converged = TRUE,

    # Model comparison (RMANOVA doesn't have formal model comparison)
    AIC_diff = NA_real_,
    BIC_diff = NA_real_,
    LR_stat = lr_stat,
    LR_df = num_df %>% as.integer(),
    LR_p = p_value,

    # Fixed effect (main effect)
    main_estimate = f_stat,
    null_main_estimate = f_stat,
    main_std_error = NA_real_,
    null_std_error = NA_real_,
    main_t_stat = f_stat, # F-stat as proxy
    nul_t_stat = f_stat,
    main_p_value = p_value,
    null_main_p_value = p_value,

    # Effect size
    effect_size = eta_sq,
    null_effect_size = eta_sq,
    effect_ci_lower = NA_real_,
    null_effect_ci_lower = NA_real_,
    effect_ci_upper = NA_real_,
    null_effect_ci_upper = NA_real_,

    # Random effects (RMANOVA: within-subject variance)
    random_intercept_var = NA_real_,
    null_random_intercept_var = NA_real_,
    random_slope_var = NA_real_,
    null_random_slope_var = NA_real_,
    residual_var = NA_real_,
    null_residual_var = NA_real_,

    # Model fit (not applicable for RMANOVA)
    loglik_full = NA_real_,
    loglik_null = NA_real_,
    npar_full = NA_integer_,
    npar_null = NA_integer_,

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
    null_effect_size = NA_real_,
    effect_ci_lower = NA_real_,
    null_effect_ci_lower = NA_real_,
    effect_ci_upper = NA_real_,
    null_effect_ci_upper = NA_real_,
    random_intercept_var = NA_real_,
    null_random_intercept_var = NA_real_,
    random_slope_var = NA_real_,
    null_random_slope_var = NA_real_,
    residual_var = NA_real_,
    null_residual_var = NA_real_,
    loglik_full = NA_real_,
    loglik_null = NA_real_,
    npar_full = NA_integer_,
    npar_null = NA_integer_,
    error = TRUE,
    error_message = model_result$error_message %||% "Unknown error",
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

append_results <- function(results_tbl, paths, branch_id) {
  results_dir <- get_results_path(paths)
  if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

  # Branch-specific file
  branch_file <- file.path(results_dir, glue::glue("results_{branch_id}.parquet"))

  tryCatch(
    {
      arrow::write_parquet(results_tbl, branch_file)
      logger::log_info("Saved {nrow(results_tbl)} results to {branch_file}")
      return(normalizePath(results_dir))
    },
    error = function(e) {
      logger::log_error("Failed to save branch {branch_id}: {e$message}")
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
  logger::log_debug("Saving metadata: {nrow(metadata_tbl)} entries")

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

      logger::log_info("Saved metadata to {metadata_path}")

      invisible(TRUE)
    },
    error = function(e) {
      logger::log_error("Failed to save metadata: {e$message}")
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
    logger::log_warn("Results directory does not exist: {results_dir}")
    return(tibble::tibble())
  }

  # Count branch files
  branch_files <- list.files(results_dir, pattern = "^results_.*\\.parquet$")

  if (length(branch_files) == 0) {
    logger::log_warn("No result files found in {results_dir}")
    return(tibble::tibble())
  }

  tryCatch(
    {
      ds <- arrow::open_dataset(results_dir, format = "parquet")
      results <- ds %>% dplyr::collect()

      logger::log_info("Aggregated {nrow(results)} rows from {length(list.files(results_dir))} branch files")

      if (validate) {
        missing <- setdiff(RESULTS_COLUMNS, names(results))
        if (length(missing) > 0) {
          logger::log_error("Missing columns: {paste(missing, collapse = ', ')}")
          stop("Schema validation failed")
        }
      }

      results
    },
    error = function(e) {
      logger::log_error("Failed to aggregate results: {e$message}")
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


  if (exists("logger") && is.function(get("log_debug", asNamespace("logger"), inherits = TRUE))) {
    logger::log_debug("Results schema validated and stored at {schema_file}")
  }

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
fit_and_save_branch <- function(idx, branch_id, sample_size, transformation, outlier, model, effect_condition, strip_method, paths, config) {
  logger::log_info("Processing branch {idx}: {branch_id}")

  model_result <- NULL
  results_tbl <- NULL
  # Reconstruct branch_spec as a list for backwards compatibility
  branch_spec <- list(
    idx = idx,
    branch_id = branch_id,
    sample_size = sample_size,
    transformation = transformation,
    outlier = outlier,
    model = model,
    effect_condition = effect_condition,
    strip_method = strip_method
  )
  tryCatch(
    {
      # Load processed data
      data_file <- get_processed_data_path(paths, branch_id)

      if (!file.exists(data_file)) {
        logger::log_error("Processed data not found: {data_file}")
        stop("Missing processed data")
      }

      data <- safe_read_parquet(data_file)
      logger::log_debug("Loaded {nrow(data)} observations")

      # Get model specification
      model_spec <- get_model_spec(config, model)

      # Fit model
      model_result <- fit_model(
        data = data,
        model_spec = model_spec,
        model_name = model,
        branch_id = branch_id
      )

      # Extract results
      results_tbl <- extract_results(model_result, branch_spec, idx)

      # Save atomically to parquet
      output_path <- append_results(
        results_tbl,
        paths,
        branch_id
      )

      logger::log_info(
        "Branch complete: p={results_tbl$LR_p}, converged={results_tbl$full_converged}"
      )

      return(output_path)
    },
    error = function(e) {
      logger::log_error("Branch {idx} failed: {e$message}")

      # Reconstruct branch_spec for error handling
      branch_spec <- list(
        idx = idx,
        branch_id = branch_id,
        sample_size = sample_size,
        transformation = transformation,
        outlier = outlier,
        model = model,
        effect_condition = effect_condition,
        strip_method = strip_method
      )

      # Create error record and save
      error_tbl <- extract_results(model_result, branch_spec, idx)

      output_path <- append_results(error_tbl, paths, branch_id)

      return(output_path)
    }
  )
}
