#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
mode <- if (length(args) >= 1L && nzchar(args[[1]])) args[[1]] else "hpc"
config_path <- if (length(args) >= 2L && nzchar(args[[2]])) args[[2]] else "pipeline.yaml"

source_path <- function(path) {
  if (file.exists(path)) return(path)
  file.path("R", path)
}
source(source_path("functions/logging.R"))
source(source_path("functions/paths.R"))
source(source_path("functions/config.R"))
source(source_path("functions/rust_interop.R"))
source(source_path("functions/cache_signature.R"))

project_root <- if (dir.exists("functions")) "." else "R"
config <- load_config(mode, config_path = config_path)
paths <- init_project_paths(project_root)
branch_specs <- generate_all_branches(config)
input_csv <- if (grepl("^/", config$raw_csv)) config$raw_csv else file.path(paths$root, config$raw_csv)

ok <- tryCatch(
  {
    validate_processed_cache_signature(config, paths, branch_specs, input_csv)
    TRUE
  },
  error = function(e) {
    message(conditionMessage(e))
    FALSE
  }
)

if (!ok) quit(status = 1L)
cat("Processed cache signature matches current raw/config/Rust inputs.\n")
