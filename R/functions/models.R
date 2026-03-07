# R/models.R - Model fitting functions

#' Fit statistical model to data
#'
#' Handles RMANOVA, LMM with various random effect structures
#'
#' @param data Processed data frame with rt, cong, prev_cong, participant_id
#' @param model_spec List with type, formulas, and control parameters
#' @param model_name Name for logging
#' @param branch_id Branch identifier for tracking
#'
#' @return List with results: fitted models, comparisons, diagnostics
fit_model <- function(data, model_spec, model_name, branch_id) {
  log_branch(logger::INFO, "Fitting {model_name} model for branch: {branch_id}", branch_id)

  # Convert to data.frame if needed
  if (!is.data.frame(data)) {
    data <- as.data.frame(data)
  }

  # Avoid anomalies
  data <- data %>%
    dplyr::filter(rt > 0) %>%
    dplyr::filter(is.finite(rt) & !is.na(rt)) %>%
    dplyr::mutate(
      cong = dplyr::if_else(cong == 1, 0.5, -0.5),
      prev_cong = dplyr::if_else(prev_cong == 1, 0.5, -0.5)
    )

  tryCatch(
    {
      result <- switch(model_spec$type,
        "rmanova" = fit_rmanova(data, model_spec),
        "lmm" = fit_lmm(data, model_spec),
        {
          log_branch(logger::ERROR, "Unknown model type: {model_spec$type}", branch_id)
          list(error = TRUE, message = "Unknown model type")
        }
      )

      result$branch_id <- branch_id
      result$model_name <- model_name
      result$n_obs <- nrow(data)
      result$n_participants <- dplyr::n_distinct(data$participant_id)

      log_branch(
        logger::INFO,
        "Model fitting complete for {model_name}: n_obs={result$n_obs}, n_subs={result$n_participants}", branch_id
      )

      result
    },
    error = function(e) {
      log_branch(logger::ERROR, "Model fitting failed for {branch_id}: {e$message}", branch_id)
      list(
        error = TRUE,
        message = e$message,
        branch_id = branch_id,
        model_name = model_name
      )
    }
  )
}

#' Fit repeated measures ANOVA
fit_rmanova <- function(data, spec) {
  log_pipeline(logger::DEBUG, "Fitting RMANOVA with afex::aov_car()")

  # Parse formula for RMANOVA (within/between structure)
  full_model <- afex::aov_car(
    as.formula(spec$formula_full),
    data = data,
    type = 3,
    fun_aggregate = mean
  )

  full_stats <- as_tibble(anova(full_model))

  list(
    type = "rmanova",
    full_model = full_model,
    full_stats = full_stats
  )
}

#' Fit linear mixed model
fit_lmm <- function(data, spec) {
  log_pipeline(logger::DEBUG, "Fitting LMM with lme4 package")
  log_pipeline(logger::INFO, "LMM formulas: full='{spec$formula_full}' null='{spec$formula_null}'")

  # Fit both full and null models
  full_model <- lmerTest::lmer(
    as.formula(spec$formula_full),
    data = data,
    REML = TRUE,
    control = do.call(lme4::lmerControl, spec$control %||% list())
  )

  null_model <- lmerTest::lmer(
    as.formula(spec$formula_null),
    data = data,
    REML = TRUE,
    control = do.call(lme4::lmerControl, spec$control %||% list())
  )

  # Check convergence
  full_converged <- length(full_model@optinfo$conv$lme4$messages) == 0 &&
    full_model@optinfo$conv$opt == 0
  null_converged <- length(null_model@optinfo$conv$lme4$messages) == 0 &&
    null_model@optinfo$conv$opt == 0

  log_pipeline(
    logger::DEBUG,
    "LMM convergence: full={full_converged}, null={null_converged}"
  )

  # ML models
  #
  refit_full <- lme4::refitML(full_model)
  refit_null <- lme4::refitML(null_model)
  # Model comparison
  comparison <- anova(refit_full, refit_null, test = "Chisq")

  # Extract coefficients and confidence intervals
  coefs <- broom.mixed::tidy(full_model, effects = "fixed", conf.int = TRUE)
  null_coefs <- broom.mixed::tidy(null_model, effects = "fixed", conf.int = TRUE)
  random_effects <- broom.mixed::tidy(full_model, effects = "ran_pars")
  null_random_effects <- broom.mixed::tidy(null_model, effects = "ran_pars")

  assumptions <- check_model_assumptions(full_model)


  # Performance metrics
  performance <- list(
    full_AIC = AIC(refit_full),
    full_BIC = BIC(refit_full),
    null_AIC = AIC(refit_null),
    null_BIC = BIC(refit_null),
    LR_stat = comparison$Chisq[2],
    LR_df = comparison$`Df`[2],
    LR_p = comparison$`Pr(>Chisq)`[2]
  )

  list(
    type = "lmm",
    full_model = full_model,
    null_model = null_model,
    refit_full = refit_full,
    refit_null = refit_null,
    full_converged = full_converged,
    null_converged = null_converged,
    coefficients = coefs,
    null_coefficients = null_coefs,
    random_effects = random_effects,
    null_random_effects = null_random_effects,
    comparison = comparison,
    performance = performance,
    assumptions = assumptions
  )
}

#' Model diagnostics and validation
check_model_assumptions <- function(model) {
  if (inherits(model, "lmerMod")) {
    residuals <- residuals(model)

    # Homogeneity of variance (Levene's test via anova residuals)
    levene_approx <- var(residuals) # Simplified; use car::leveneTest for proper test

    return(list(
      variance = levene_approx
    ))
  }

  NULL
}
