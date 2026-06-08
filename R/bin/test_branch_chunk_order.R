#!/usr/bin/env Rscript

source_path <- function(path) {
  if (file.exists(path)) return(path)
  file.path("R", path)
}
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}
if (!exists("log_pipeline", mode = "function")) {
  log_pipeline <- function(...) invisible(NULL)
}
suppressPackageStartupMessages(library(dplyr))
source(source_path(file.path("functions", "branch_chunks.R")))

check <- function(label, value) {
  if (!isTRUE(value)) stop("FAILED: ", label)
  cat("ok -", label, "\n")
}

branches <- data.frame(
  idx = 1:8,
  model = c(
    "lmm_full_slope", "lmm_intercept", "rmanova", "lmm_cong_slope",
    "lmm_full_slope", "rmanova", "lmm_intercept", "lmm_cong_slope"
  ),
  branch_id = paste0("b", 1:8),
  data_id = paste0("d", 1:8),
  stringsAsFactors = FALSE
)
chunks <- split_branch_specs(branches, list(model_chunk_size = 2L))
ordered_models <- unlist(lapply(chunks, function(x) x$model), use.names = FALSE)
priorities <- model_runtime_priority(ordered_models)

check("runtime priorities are nondecreasing across chunks", all(diff(priorities) >= 0))
check("fast models are first", identical(ordered_models[1:2], c("rmanova", "rmanova")))
check("full-slope models are last", identical(tail(ordered_models, 2), c("lmm_full_slope", "lmm_full_slope")))
check("chunk ids are consecutive", identical(sort(unique(unlist(lapply(chunks, function(x) unique(x$chunk_id))))), 1:4))

cat("Branch chunk ordering checks passed.\n")
