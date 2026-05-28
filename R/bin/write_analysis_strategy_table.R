#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
output_path <- if (length(args) >= 1) args[[1]] else file.path("R", "outputs", "analysis", "analysis_strategy_table.csv")

function_path <- file.path("functions", "analysis_strategy_table.R")
if (!file.exists(function_path)) {
  function_path <- file.path("R", "functions", "analysis_strategy_table.R")
}
source(function_path)

path <- write_analysis_strategy_table(output_path)
cat("Wrote analysis strategy table:", path, "\n")
