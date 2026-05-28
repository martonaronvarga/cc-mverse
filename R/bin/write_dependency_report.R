#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args) >= 1) args[[1]] else file.path("R", "outputs", "analysis")

function_path <- file.path("functions", "dependency_report.R")
if (!file.exists(function_path)) {
  function_path <- file.path("R", "functions", "dependency_report.R")
}
source(function_path)

paths <- write_dependency_report(output_dir)
cat("Wrote R package dependency report:", paths$r_packages, "\n")
cat("Wrote tool dependency report:", paths$tools, "\n")
cat("Wrote Rust dependency report:", paths$rust, "\n")
