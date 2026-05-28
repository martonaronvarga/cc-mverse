#!/usr/bin/env Rscript

root <- normalizePath("R", mustWork = TRUE)
source(file.path(root, "functions", "paths.R"))

assert_identical <- function(actual, expected, label) {
  if (!identical(actual, expected)) {
    stop(label, ": expected ", expected, ", got ", actual, call. = FALSE)
  }
}

branch_id <- "0.5__2__log_rt__none__lmm_intercept__null_interaction__additive_qmap"
data_id <- "0.5__2__log_rt__none__null_interaction__additive_qmap"
paths <- list(data_processed = tempfile("processed_"))
dir.create(paths$data_processed, recursive = TRUE)

assert_identical(data_id_from_branch_id(branch_id), data_id, "branch_id to data_id")

processed_file <- get_processed_data_path(paths, data_id)
file.create(processed_file)

if (!processed_data_exists(paths, data_id)) {
  stop("processed_data_exists should accept a data_id string", call. = FALSE)
}
missing_data_id <- sub("additive_qmap", "shuffle", data_id, fixed = TRUE)
if (processed_data_exists(paths, missing_data_id)) {
  stop("processed_data_exists should return FALSE for missing files", call. = FALSE)
}

cat("PASS path helper tests\n")
