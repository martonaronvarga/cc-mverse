#!/usr/bin/env Rscript

function_path <- file.path("functions", "random_effect_extraction.R")
if (!file.exists(function_path)) {
  function_path <- file.path("R", "functions", "random_effect_extraction.R")
}
source(function_path)

check <- function(label, value) {
  if (!isTRUE(value)) stop("FAILED: ", label)
  cat("ok -", label, "\n")
}

participant_matrix <- matrix(0, nrow = 3, ncol = 3)
attr(participant_matrix, "stddev") <- c(
  "(Intercept)" = 2,
  cong = 0.5,
  prev_cong = 0.25,
  "cong:prev_cong" = 0.1
)
vc <- list(participant_id = participant_matrix)

check("extracts intercept variance", identical(extract_varcorr_variance(vc, term = "(Intercept)"), 4))
check("extracts cong random slope variance", abs(extract_varcorr_variance(vc, term = "cong") - 0.25) < 1e-12)
check("extracts prev_cong random slope variance", abs(extract_varcorr_variance(vc, term = "prev_cong") - 0.0625) < 1e-12)
check("extracts CSE random slope variance", abs(extract_varcorr_variance(vc, term = "cong:prev_cong") - 0.01) < 1e-12)
check("missing term returns NA", is.na(extract_varcorr_variance(vc, term = "missing")))
check("missing group returns NA", is.na(extract_varcorr_variance(vc, group = "subject", term = "(Intercept)")))

bad_matrix <- matrix(0, nrow = 1, ncol = 1)
attr(bad_matrix, "stddev") <- c("(Intercept)" = Inf)
bad_vc <- list(participant_id = bad_matrix)
check("nonfinite stddev returns NA", is.na(extract_varcorr_variance(bad_vc, term = "(Intercept)")))

cat("All random-effect extraction checks passed.\n")
