#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
input_path <- if (length(args) >= 1) args[[1]] else "all_indexed.csv"
output_dir <- if (length(args) >= 2) args[[2]] else file.path("outputs", "analysis")

function_path <- file.path("functions", "data_inventory.R")
if (!file.exists(function_path)) {
  function_path <- file.path("R", "functions", "data_inventory.R")
}
source(function_path)

paths <- write_data_inventory(input_path, output_dir)
cat("Wrote column inventory:", paths$column_inventory, "\n")
cat("Wrote confound risk table:", paths$confound_risk_table, "\n")
