#!/usr/bin/env Rscript

function_path <- file.path("functions", "cse_term_extraction.R")
if (!file.exists(function_path)) {
  function_path <- file.path("R", "functions", "cse_term_extraction.R")
}
source(function_path)

check <- function(label, value) {
  if (!isTRUE(value)) stop("FAILED: ", label)
  cat("ok -", label, "\n")
}

coefs <- data.frame(
  term = c("(Intercept)", "cong", "prev_cong", "cong:prev_cong", "trial_index:cong"),
  estimate = c(0, 1, 2, 3, 99),
  stringsAsFactors = FALSE
)
check("selects exact cong:prev_cong and ignores other interactions", select_cse_coefficient_row(coefs)$estimate == 3)

coefs_reversed <- data.frame(
  term = c("prev_cong:cong", "cong:trial_index"),
  estimate = c(4, 100),
  stringsAsFactors = FALSE
)
check("accepts reversed interaction order", select_cse_coefficient_row(coefs_reversed)$estimate == 4)

coefs_factor <- data.frame(
  term = c("cong1:prev_cong1", "congpositive:prev_congpositive", "congruency:prev_cong", "cong:previous_cong"),
  estimate = c(5, 6, 200, 300),
  stringsAsFactors = FALSE
)
check("accepts numeric factor-expanded CSE term names only", select_cse_coefficient_row(coefs_factor[1, ])$estimate == 5)
check("accepts level factor-expanded CSE term names only", select_cse_coefficient_row(coefs_factor[2, ])$estimate == 6)
check("rejects unrelated prefix matches", nrow(select_cse_coefficient_row(coefs_factor[3:4, ])) == 0L)

coefs_absent <- data.frame(term = c("cong", "prev_cong", "cong:trial_index"), estimate = 1:3)
check("returns zero rows when CSE interaction absent", nrow(select_cse_coefficient_row(coefs_absent)) == 0L)

coefs_duplicate <- data.frame(term = c("cong:prev_cong", "prev_cong:cong"), estimate = c(1, 2))
duplicate_error <- inherits(try(select_cse_coefficient_row(coefs_duplicate), silent = TRUE), "try-error")
check("errors on duplicate CSE rows", duplicate_error)

rma <- data.frame(
  Effect = c("cong", "prev_cong", "cong:prev_cong"),
  F = c(1, 2, 3),
  check.names = FALSE
)
check("selects RMANOVA CSE by Effect name", select_rmanova_cse_row(rma)$F == 3)

rma_reordered <- data.frame(
  Effect = c("cong:prev_cong", "cong", "prev_cong"),
  F = c(9, 1, 2),
  check.names = FALSE
)
check("does not rely on RMANOVA row order", select_rmanova_cse_row(rma_reordered)$F == 9)

cat("All CSE term extraction checks passed.\n")
