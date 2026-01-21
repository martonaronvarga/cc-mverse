# Utility to compare original model fit ("truth") with the extracted result (parquet) for a branch, for auditing

library(tidyverse)
library(broom.mixed)
library(arrow)

#' Compare a saved model object to its extracted results row
#'
#' @param model_rds_path Path to saved model fit (e.g. .rds with full_model, as returned by fit_model())
#' @param extracted_results_path Path to extracted parquet file (e.g. results_<branch_id>.parquet)
#' @param target_effect Pattern or function for extracting the effect (default: interaction with colon)
#'
#' @return Invisibly returns result; prints side-by-side comparison and stops if mismatch
compare_extracted_result <- function(
    model_rds,
    extracted_results_path,
    target_effect = function(coefs) {
      # Default: return all interaction terms (colon)
      coefs %>% filter(str_detect(term, ":"))
    }) {
  # Load model object (assumed to contain $full_model)
  model_obj <- readRDS(model_rds_path)
  full_model <- if (!is.null(model_obj$full_model)) model_obj$full_model else model_obj

  # Load extracted branch result
  extr <- arrow::read_parquet(extracted_results_path)

  # Tidy all fixed effects
  coefs <- broom.mixed::tidy(full_model, effects = "fixed", conf.int = TRUE)
  print("Fixed effects in model:")
  print(coefs)

  # Use target_effect selector
  target_rows <- target_effect(coefs)
  if (nrow(target_rows) == 0) {
    stop("No target interaction term found in model coefficients.")
  }

  # If multiple, use mean; or select by your rule.
  model_main_est <- mean(target_rows$estimate)
  model_main_se <- mean(target_rows$std.error)
  model_main_p <- mean(target_rows$p.value)
  model_main_ci <- c(mean(target_rows$conf.low), mean(target_rows$conf.high))

  cat("\n=== Model fit effect summary ===\n")
  print(list(
    estimate = model_main_est,
    std.error = model_main_se,
    p.value = model_main_p,
    conf.low = model_main_ci[1],
    conf.high = model_main_ci[2]
  ))

  cat("\n=== Extracted result (from parquet) ===\n")
  print(extr %>% select(main_estimate, main_std_error, main_p_value, effect_ci_lower, effect_ci_upper))

  # Check for numeric mismatch
  mismatch <- any(
    abs(model_main_est - extr$main_estimate) > .Machine$double.eps * 100,
    abs(model_main_se - extr$main_std_error) > .Machine$double.eps * 100,
    abs(model_main_p - extr$main_p_value) > .Machine$double.eps * 100,
    abs(model_main_ci[1] - extr$effect_ci_lower) > .Machine$double.eps * 100,
    abs(model_main_ci[2] - extr$effect_ci_upper) > .Machine$double.eps * 100
  )
  if (mismatch) {
    stop("Mismatch between extracted effect and model effect! CHECK COEFFICIENT EXTRACTION LOGIC.")
  } else {
    cat("\n>> Extracted result matches the model fit. ✅\n")
  }
  invisible(list(model = full_model, extracted = extr))
}

# Example usage:
# compare_extracted_result("fits/fit__branch_12.rds", "outputs/results/results_branch_12.parquet")
