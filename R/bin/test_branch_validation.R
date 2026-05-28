#!/usr/bin/env Rscript

source_path <- function(path) {
  if (file.exists(path)) return(path)
  file.path("R", path)
}
source(source_path(file.path("functions", "paths.R")))
source(source_path(file.path("functions", "config.R")))
source(source_path(file.path("functions", "branch_validation.R")))
if (!exists("log_pipeline", mode = "function")) {
  log_pipeline <- function(...) invisible(NULL)
}

config <- list(
  sample_sizes = c(0.5, 1.0),
  n_subsamples = 3L,
  transformations = c("log_rt", "no_log_rt"),
  outlier_methods = c("none", "sd_2"),
  effect_conditions = c("present", "null_interaction"),
  strip_methods = c("shuffle", "additive_qmap"),
  models = list(
    rmanova = list(type = "rmanova"),
    lmm_intercept = list(type = "lmm")
  )
)

branches <- generate_all_branches(config)
summary <- validate_branch_table(branches, config)

expected_subsample_rows <- 3L + 1L
expected_data <- expected_subsample_rows * 2L * 2L * 3L
expected_rows <- expected_data * 2L
stopifnot(summary$n_branches == expected_rows)
stopifnot(summary$n_data_branches == expected_data)
stopifnot(!any(branches$effect_condition == "present" & branches$strip_method != "none"))
stopifnot(!any(branches$effect_condition == "null_both"))
stopifnot(!any(branches$effect_condition == "null_interaction" & !branches$strip_method %in% c("shuffle", "additive_qmap")))

bad <- branches
bad$strip_method[bad$effect_condition == "present"][1] <- "shuffle"
stopifnot(inherits(try(validate_branch_table(bad, config), silent = TRUE), "try-error"))

cat("Branch validation checks passed: ", summary$n_branches, " branches, ", summary$n_data_branches, " data branches.\n", sep = "")
