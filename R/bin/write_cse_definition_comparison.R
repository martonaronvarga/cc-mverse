#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
diagnostics_csv <- if (length(args) >= 1L) args[[1]] else file.path("R", "outputs", "analysis", "nullification_diagnostics.csv")
output_dir <- if (length(args) >= 2L) args[[2]] else file.path("R", "outputs", "analysis")

source_path <- function(path) {
  if (file.exists(path)) return(path)
  file.path("R", path)
}
source(source_path(file.path("functions", "cse_definition_comparison.R")))

paths <- write_cse_definition_comparison(diagnostics_csv, output_dir)
cat("Wrote CSE definition comparison:\n")
cat("- ", paths$summary, "\n", sep = "")
cat("- ", paths$long, "\n", sep = "")
