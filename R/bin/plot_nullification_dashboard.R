#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  stop("Usage: Rscript R/bin/plot_nullification_dashboard.R <nullification_diagnostics.csv> [output_dir]")
}

diagnostics_csv <- args[[1]]
output_dir <- if (length(args) >= 2) args[[2]] else "R/outputs/analysis/figures"

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readr)
})
source("R/functions/nullification_dashboard.R")
paths <- write_nullification_reliability_dashboard(diagnostics_csv, output_dir)
cat("Wrote nullification dashboard files:\n")
cat(paste0("- ", unname(paths), collapse = "\n"), "\n", sep = "")
