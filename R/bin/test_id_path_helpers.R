#!/usr/bin/env Rscript

source_path <- function(path) {
  if (file.exists(path)) return(path)
  file.path("R", path)
}
source(source_path(file.path("functions", "paths.R")))
source(source_path(file.path("functions", "config.R")))
if (!exists("log_pipeline", mode = "function")) {
  log_pipeline <- function(...) invisible(NULL)
}

check <- function(label, value) {
  if (!isTRUE(value)) stop("FAILED: ", label)
  cat("ok -", label, "\n")
}

bid <- compose_branch_id(
  sample_size = 0.05,
  subsample_id = 12,
  transformation = "log_rt",
  outlier = "sd_2.5",
  model = "lmm-maximal:model",
  effect_condition = "null_interaction",
  strip_method = "qmap_5"
)
expected_bid <- "0.05__12__log_rt__sd_2.5__lmm-maximal:model__null_interaction__additive_qmap"
check("compose_branch_id formats sample and normalizes legacy strip names", bid == expected_bid)

expected_data_id <- "0.05__12__log_rt__sd_2.5__null_interaction__additive_qmap"
check("data_id_from_branch_id drops model segment only", data_id_from_branch_id(bid) == expected_data_id)

unsafe_model_error <- inherits(try(compose_branch_id(
  0.05, 12, "log_rt", "sd_2.5", "lmm__maximal", "null_interaction", "qmap_5"
), silent = TRUE), "try-error")
check("model names with '__' are rejected", unsafe_model_error)

bad_id_error <- inherits(try(data_id_from_branch_id("too__few__segments"), silent = TRUE), "try-error")
check("bad branch_id segment count fails", bad_id_error)

tmp_root <- tempfile("paths_test_")
dir.create(tmp_root)
paths <- init_project_paths(tmp_root)
check("init_project_paths creates processed dir", dir.exists(paths$data_processed))
check("get_processed_data_path uses data_id", get_processed_data_path(paths, expected_data_id) == file.path(paths$data_processed, paste0("processed__", expected_data_id, ".parquet")))
check("get_results_path creates output results dir", dir.exists(get_results_path(paths)))
analysis_path <- get_analysis_path(paths, "smoke")
check("get_analysis_path uses requested prefix and parquet suffix", grepl("smoke_.*\\.parquet$", basename(analysis_path)))

unlink(tmp_root, recursive = TRUE, force = TRUE)
cat("All ID/path helper checks passed.\n")
