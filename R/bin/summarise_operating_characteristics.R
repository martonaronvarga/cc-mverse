#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
results_path <- if (length(args) >= 1) args[[1]] else file.path("outputs", "analysis", "smoke_operating_characteristics.csv")
output_dir <- if (length(args) >= 2) args[[2]] else dirname(results_path)
output_name <- if (length(args) >= 3) args[[3]] else "smoke_operating_characteristics_summary.csv"

function_path <- file.path("functions", "operating_characteristics_summary.R")
if (!file.exists(function_path)) {
  function_path <- file.path("R", "functions", "operating_characteristics_summary.R")
}
source(function_path)

path <- write_operating_characteristics_summary(results_path, output_dir, output_name)
cat("Wrote operating-characteristics summary:", path, "\n")
