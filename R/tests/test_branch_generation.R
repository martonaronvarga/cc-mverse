#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
})

root <- normalizePath("R", mustWork = TRUE)
log_pipeline <- function(...) invisible(NULL)
source(file.path(root, "functions", "paths.R"))
source(file.path(root, "functions", "config.R"))

assert_true <- function(x, label) {
  if (!isTRUE(x)) stop(label, call. = FALSE)
}
assert_equal <- function(actual, expected, label) {
  if (!identical(actual, expected)) {
    stop(label, ": expected ", expected, ", got ", actual, call. = FALSE)
  }
}

cfg <- list(
  sample_sizes = c(0.5, 1.0),
  n_subsamples = 2L,
  transformations = c("log_rt", "no_log_rt"),
  outlier_methods = c("none", "sd_2"),
  strip_methods = c("shuffle", "additive_qmap", "additive_qmap_trial_bin", "local_mean_residual", "local_median_residual"),
  models = list(
    rmanova = list(),
    lmm_full_slope = list()
  )
)

branches <- generate_all_branches(cfg)

# Subsample grid: 0.5 has 2 draws; 1.0 has 1 deterministic draw.
# Per subsample/transform/outlier/model there are present/none plus one row per null_interaction strip method.
expected_model_branches <- 3L * 2L * 2L * 2L * (1L + length(cfg$strip_methods))
expected_data_branches <- 3L * 2L * 2L * (1L + length(cfg$strip_methods))

assert_equal(nrow(branches), expected_model_branches, "model branch count")
assert_equal(length(unique(branches$branch_id)), nrow(branches), "unique branch_id count")
assert_equal(length(unique(branches$data_id)), expected_data_branches, "unique data_id count")

assert_true(all(branches$effect_condition != "present" | branches$strip_method == "none"), "present strip method constraint")
assert_true(!any(branches$effect_condition == "null_both"), "null_both removed")
assert_true(all(branches$effect_condition != "null_interaction" | branches$strip_method %in% cfg$strip_methods), "null_interaction strip method constraint")

one_full_sample <- branches |>
  filter(sample_size == 1.0) |>
  pull(subsample_id) |>
  unique()
assert_equal(one_full_sample, 1L, "full sample has one deterministic subsample")

branch_id <- compose_branch_id(0.5, 2L, "log_rt", "none", "lmm_full_slope", "null_interaction", "additive_qmap")
assert_equal(
  branch_id,
  "0.5__2__log_rt__none__lmm_full_slope__null_interaction__additive_qmap",
  "compose_branch_id format"
)
assert_equal(
  data_id_from_branch_id(branch_id),
  "0.5__2__log_rt__none__null_interaction__additive_qmap",
  "data_id drops model segment"
)

cat("PASS branch generation tests\n")
