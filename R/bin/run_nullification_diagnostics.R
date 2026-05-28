#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
input_dir <- if (length(args) >= 1) args[[1]] else file.path("data", "processed")
output_path <- if (length(args) >= 2) args[[2]] else file.path("outputs", "analysis", "nullification_diagnostics.csv")

function_path <- file.path("functions", "nullification_diagnostics.R")
if (!file.exists(function_path)) {
  function_path <- file.path("R", "functions", "nullification_diagnostics.R")
}
source(function_path)

path <- write_nullification_diagnostics_for_dir(input_dir, output_path)
cat("Wrote nullification diagnostics:", path, "\n")
