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
  logger::log_info("Fitting {model_name} model for branch: {branch_id}")

  # Convert to data.frame if needed
  if (!is.data.frame(data)) {
    data <- as.data.frame(data)
  }

  tryCatch(
    {
      result <- switch(model_spec$type,
        "rmanova" = fit_rmanova(data, model_spec),
        "lmm" = fit_lmm(data, model_spec),
        {
          logger::log_error("Unknown model type: {model_spec$type}")
          list(error = TRUE, message = "Unknown model type")
        }
      )

      result$branch_id <- branch_id
      result$model_name <- model_name
      result$n_obs <- nrow(data)
      result$n_participants <- dplyr::n_distinct(data$participant_id)

      logger::log_info(
        "Model fitting complete for {model_name}: ",
        "n_obs={result$n_obs}, n_subs={result$n_participants}"
      )

      result
    },
    error = function(e) {
      logger::log_error("Model fitting failed for {branch_id}: {e$message}")
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
  logger::log_debug("Fitting RMANOVA with afex::aov_car()")

  # Parse formula for RMANOVA (within/between structure)
  full_model <- afex::aov_car(
    as.formula(spec$formula_full),
    data = data,
    type = 3
  )

  # Extract statistics
  full_stats <- broom::tidy(full_model)

  list(
    type = "rmanova",
    full_model = full_model,
    full_stats = full_stats
  )
}

#' Fit linear mixed model
fit_lmm <- function(data, spec) {
  logger::log_debug("Fitting LMM with lme4 package")

  # Fit both full and null models
  full_model <- lmerTest::lmer(
    as.formula(spec$formula_full),
    data = data,
    REML = FALSE,
    control = do.call(lme4::lmerControl, spec$control %||% list())
  )

  null_model <- lmerTest::lmer(
    as.formula(spec$formula_null),
    data = data,
    REML = FALSE,
    control = do.call(lme4::lmerControl, spec$control %||% list())
  )

  # Check convergence
  full_converged <- length(full_model@optinfo$conv$lme4$messages) == 0 &&
    full_model@optinfo$conv$opt == 0
  null_converged <- length(null_model@optinfo$conv$lme4$messages) == 0 &&
    null_model@optinfo$conv$opt == 0

  logger::log_info(
    "LMM convergence: full={full_converged}, null={null_converged}"
  )

  # Model comparison
  comparison <- anova(full_model, null_model, test = "Chisq")

  # Extract coefficients and confidence intervals
  coefs <- broom.mixed::tidy(full_model, effects = "fixed", conf.int = TRUE)
  null_coefs <- broom.mixed::tidy(null_model, effects = "fixed", conf.int = TRUE)
  random_effects <- broom.mixed::tidy(full_model, effects = "ran_pars")
  null_random_effects <- broom.mixed::tidy(null_model, effects = "ran_pars")

  assumptions <- check_model_assumptions(full_model)


  # Performance metrics
  performance <- list(
    full_AIC = AIC(full_model),
    full_BIC = BIC(full_model),
    null_AIC = AIC(null_model),
    null_BIC = BIC(null_model),
    LR_stat = comparison$Chisq[2],
    LR_df = comparison$`Df`[2],
    LR_p = comparison$`Pr(>Chisq)`[2]
  )

  list(
    type = "lmm",
    full_model = full_model,
    null_model = null_model,
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
