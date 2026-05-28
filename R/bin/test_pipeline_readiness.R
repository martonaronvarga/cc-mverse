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
source(source_path(file.path("functions", "dependency_report.R")))
source(source_path(file.path("functions", "pipeline_readiness.R")))

check <- function(label, value) {
  if (!isTRUE(value)) stop("FAILED: ", label)
  cat("ok -", label, "\n")
}

tmp <- tempfile("readiness_")
dir.create(tmp)
old_wd <- getwd()
on.exit({ setwd(old_wd); unlink(tmp, recursive = TRUE, force = TRUE) }, add = TRUE)

paths <- init_project_paths(tmp)
writeLines("participant_id,cong,prev_cong,rt\np1,1,-1,500\n", file.path(tmp, "raw.csv"))
writeLines("# parseable targets placeholder", file.path(tmp, "_targets.R"))
dir.create(file.path(tmp, "functions"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(tmp, "rust", "src"), recursive = TRUE, showWarnings = FALSE)
writeLines("data:\n  raw_csv: raw.csv\n", file.path(tmp, "pipeline.yaml"))
writeLines("fn main() {}", file.path(tmp, "rust", "src", "main.rs"))
writeLines("[package]\nname = 'process'\nversion = '0.1.0'\nedition = '2021'\n", file.path(tmp, "rust", "Cargo.toml"))
writeLines("", file.path(tmp, "functions", "config.R"))
writeLines("", file.path(tmp, "functions", "paths.R"))
writeLines("", file.path(tmp, "functions", "rust_interop.R"))

config <- list(
  mode = "focused",
  is_hpc = FALSE,
  raw_csv = "raw.csv",
  random_seed = 42,
  sample_sizes = 1,
  n_subsamples = 1L,
  transformations = "no_log_rt",
  outlier_methods = "none",
  effect_conditions = c("present", "null_interaction"),
  strip_methods = "shuffle",
  rust_release = TRUE,
  rust_threads = 1L,
  writer_threads = 1L,
  save_metadata = FALSE,
  log_level = "info"
)
branches <- data.frame(
  branch_id = c(
    "1__1__no_log_rt__none__rmanova__present__none",
    "1__1__no_log_rt__none__rmanova__null_interaction__shuffle"
  ),
  data_id = c(
    "1__1__no_log_rt__none__present__none",
    "1__1__no_log_rt__none__null_interaction__shuffle"
  ),
  effect_condition = c("present", "null_interaction"),
  strip_method = c("none", "shuffle"),
  stringsAsFactors = FALSE
)
write_processed_cache_signature(config, paths, branches, file.path(tmp, "raw.csv"))

report <- build_pipeline_readiness_report(config, paths, branches, mode = "focused", check_dependencies = FALSE)
check("report includes raw/config/targets/cache checks", all(c("raw_csv_exists", "branch_contract", "targets_script_parse", "processed_cache_signature") %in% report$check))
check("raw CSV passes", report$status[report$check == "raw_csv_exists"] == "pass")
check("missing processed files are warning not fatal", report$status[report$check == "processed_files"] == "warn")
check("cache signature is current", report$status[report$check == "processed_cache_signature"] == "pass")

out <- file.path(tmp, "outputs", "analysis", "readiness.csv")
path <- write_pipeline_readiness_report(config, paths, branches, out, mode = "focused", check_dependencies = FALSE)
check("readiness CSV is written", file.exists(path))

cat("All pipeline readiness checks passed.\n")
