#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3) {
  stop("Usage: Rscript R/bin/write_nullification_operating_characteristics.R <results.csv> <nullification_diagnostics.csv> <output_dir> [alpha]")
}

results_csv <- args[[1]]
diagnostics_csv <- args[[2]]
output_dir <- args[[3]]
alpha <- if (length(args) >= 4) as.numeric(args[[4]]) else 0.05

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})
source("R/functions/nullification_operating_characteristics.R")
paths <- write_nullification_operating_characteristics(results_csv, diagnostics_csv, output_dir, alpha = alpha)
cat("Wrote nullification operating-characteristic tables:\n")
cat(paste0("- ", unname(paths), collapse = "\n"), "\n", sep = "")
