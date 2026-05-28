#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
input_csv <- if (length(args) >= 1) args[[1]] else "R/all_indexed.csv"
output_csv <- if (length(args) >= 2) args[[2]] else "R/outputs/analysis/shuffle_exchangeability_diagnostics.csv"

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})
source("R/functions/shuffle_exchangeability_diagnostics.R")
out <- write_shuffle_exchangeability_diagnostics(input_csv, output_csv)
cat("Wrote shuffle exchangeability diagnostics: ", output_csv, "\n", sep = "")
print(out)
