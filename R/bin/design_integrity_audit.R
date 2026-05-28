#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
input_path <- if (length(args) >= 1) args[[1]] else "all_indexed.csv"
output_dir <- if (length(args) >= 2) args[[2]] else file.path("outputs", "analysis")

function_path <- file.path("functions", "design_integrity.R")
if (!file.exists(function_path)) {
  function_path <- file.path("R", "functions", "design_integrity.R")
}
source(function_path)
paths <- write_design_integrity_audit(input_path, output_dir)

cat("Wrote design inventory:", paths$design_inventory, "\n")
cat("Wrote previous-trial audit:", paths$previous_trial_column_audit, "\n")
