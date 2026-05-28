#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

source("R/functions/logging.R")
source("R/functions/paths.R")
source("R/functions/config.R")

args <- commandArgs(trailingOnly = TRUE)
output <- if (length(args) >= 1) args[[1]] else "R/outputs/analysis/resampling_pilot_branch_manifest.csv"

config <- load_config("resampling_pilot", config_path = "R/pipeline.yaml")
branches <- generate_all_branches(config)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
readr::write_csv(branches, output)

cat("Wrote resampling pilot branch manifest: ", output, "\n", sep = "")
cat("rows=", nrow(branches), " unique_data=", length(unique(branches$data_id)), "\n", sep = "")
cat("sample_sizes=", paste(sort(unique(branches$sample_size)), collapse = ","), "\n", sep = "")
cat("subsamples_per_size=", paste(tapply(branches$subsample_id, branches$sample_size, function(x) length(unique(x))), collapse = ","), "\n", sep = "")
