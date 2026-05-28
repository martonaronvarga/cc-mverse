#!/usr/bin/env Rscript

scripts <- c(
  "R/bin/test_cse_term_extraction.R",
  "R/bin/test_random_effect_extraction.R",
  "R/bin/test_lmm_diagnostics.R",
  "R/bin/test_branch_validation.R",
  "R/bin/test_cse_definition_comparison.R",
  "R/bin/test_data_validation.R",
  "R/bin/test_id_path_helpers.R",
  "R/bin/test_processed_cache_signature.R",
  "R/bin/test_pipeline_readiness.R",
  "R/bin/test_shuffle_adversarial_diagnostics.R",
  "R/bin/test_nullification_diagnostics.R",
  "R/bin/test_nullification_diagnostics_runner.R"
)

run_script <- function(script) {
  if (!file.exists(script)) stop("Smoke script not found: ", script)
  cat("\n==>", script, "\n")
  status <- system2("Rscript", script)
  if (!identical(status, 0L)) stop("Smoke script failed: ", script)
  invisible(TRUE)
}

for (script in scripts) run_script(script)
cat("\nAll local smoke checks passed.\n")
