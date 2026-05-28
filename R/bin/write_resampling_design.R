#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
output_path <- if (length(args) >= 1) args[[1]] else file.path("R", "outputs", "analysis", "resampling_design_table.csv")

function_path <- file.path("functions", "resampling_design.R")
if (!file.exists(function_path)) {
  function_path <- file.path("R", "functions", "resampling_design.R")
}
source(function_path)

path <- write_resampling_design(output_path)
cat("Wrote resampling design table:", path, "\n")
