#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
mode <- if (length(args) >= 1L && nzchar(args[[1]])) args[[1]] else "focused"
config_path <- if (length(args) >= 2L && nzchar(args[[2]])) args[[2]] else file.path("R", "pipeline.yaml")
output_csv <- if (length(args) >= 3L && nzchar(args[[3]])) args[[3]] else file.path("R", "outputs", "analysis", "pipeline_readiness_report.csv")
check_dependencies <- !any(args %in% "--no-deps")

source_path <- function(path) {
  if (file.exists(path)) return(path)
  file.path("R", path)
}
for (fn in list.files(dirname(source_path(file.path("functions", "logging.R"))), pattern = "\\.R$", full.names = TRUE)) {
  source(fn)
}

project_root <- if (dir.exists("functions")) "." else "R"
config <- load_config(mode, config_path = config_path)
paths <- init_project_paths(project_root)
branches <- generate_all_branches(config)
path <- write_pipeline_readiness_report(
  config = config,
  paths = paths,
  branch_specs = branches,
  output_csv = output_csv,
  mode = mode,
  check_dependencies = check_dependencies
)
report <- readr::read_csv(path, show_col_types = FALSE)
print(report)
if (any(report$status == "fail")) {
  quit(status = 1L)
}
cat("Pipeline readiness report written: ", path, "\n", sep = "")
