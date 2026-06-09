#!/usr/bin/env Rscript

source_path <- function(path) {
  if (file.exists(path)) return(path)
  file.path("R", path)
}
source(source_path(file.path("functions", "logging.R")))
source(source_path(file.path("functions", "paths.R")))
source(source_path(file.path("functions", "config.R")))
source(source_path(file.path("functions", "rust_interop.R")))
source(source_path(file.path("functions", "cache_signature.R")))

check <- function(label, value) {
  if (!isTRUE(value)) stop("FAILED: ", label)
  cat("ok -", label, "\n")
}

tmp_root <- tempfile("cache_sig_")
dir.create(tmp_root)
old_wd <- getwd()
on.exit({ setwd(old_wd); unlink(tmp_root, recursive = TRUE, force = TRUE) }, add = TRUE)

paths <- init_project_paths(tmp_root)
dir.create(file.path(tmp_root, "functions"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(tmp_root, "rust", "src"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(tmp_root, "rust", "target", "release"), recursive = TRUE, showWarnings = FALSE)
writeLines("data:\n  raw_csv: raw.csv\n", file.path(tmp_root, "pipeline.yaml"))
writeLines("fn <- function() TRUE", file.path(tmp_root, "functions", "config.R"))
writeLines("fn <- function() TRUE", file.path(tmp_root, "functions", "paths.R"))
writeLines("fn <- function() TRUE", file.path(tmp_root, "functions", "rust_interop.R"))
writeLines("fn main() {}", file.path(tmp_root, "rust", "src", "main.rs"))
writeLines("binary-v1", file.path(tmp_root, "rust", "target", "release", "process"))
writeLines("[package]\nname = 'process'\nversion = '0.1.0'\nedition = '2021'\n", file.path(tmp_root, "rust", "Cargo.toml"))
writeLines("participant_id,cong,prev_cong,rt\np1,1,-1,500\n", file.path(tmp_root, "raw.csv"))

config <- list(
  raw_csv = "raw.csv",
  random_seed = 42,
  sample_sizes = 1,
  n_subsamples = 1L,
  transformations = "no_log_rt",
  outlier_methods = "none",
  effect_conditions = c("present", "null_interaction"),
  strip_methods = "local_median_residual",
  rust_release = TRUE,
  rust_threads = 1L,
  writer_threads = 1L,
  save_metadata = FALSE,
  log_level = "info"
)
branch_specs <- data.frame(
  data_id = c(
    "1__1__no_log_rt__none__present__none",
    "1__1__no_log_rt__none__null_interaction__local_median_residual"
  ),
  stringsAsFactors = FALSE
)
input_csv <- file.path(tmp_root, "raw.csv")

sig1 <- build_processed_cache_signature(config, paths, branch_specs, input_csv)
sig2 <- build_processed_cache_signature(config, paths, branch_specs, input_csv)
check("signature is deterministic", identical(sig1$signature, sig2$signature))
check("signature counts expected data IDs", identical(sig1$expected_file_count, 2L))

sig_path <- write_processed_cache_signature(config, paths, branch_specs, input_csv)
check("signature file is written", file.exists(sig_path))
check("matching signature validates", !inherits(try(validate_processed_cache_signature(config, paths, branch_specs, input_csv, current = sig1), silent = TRUE), "try-error"))

writeLines("fn main() { println!(\"changed\"); }", file.path(tmp_root, "rust", "src", "main.rs"))
check("changed Rust source does not invalidate processed cache", !inherits(try(validate_processed_cache_signature(config, paths, branch_specs, input_csv), silent = TRUE), "try-error"))

writeLines("binary-v2", file.path(tmp_root, "rust", "target", "release", "process"))
stale <- inherits(try(validate_processed_cache_signature(config, paths, branch_specs, input_csv), silent = TRUE), "try-error")
check("changed Rust binary invalidates signature", stale)

cat("All processed cache signature checks passed.\n")
