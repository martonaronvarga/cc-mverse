#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

source("R/functions/logging.R")
source("R/functions/paths.R")
source("R/functions/config.R")

args <- commandArgs(trailingOnly = TRUE)
output <- if (length(args) >= 1) args[[1]] else "R/outputs/analysis/focused_branch_manifest.csv"

config <- load_config("focused", config_path = "R/pipeline.yaml")
branches <- generate_all_branches(config)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
readr::write_csv(branches, output)

cat("Wrote focused branch manifest: ", output, "\n", sep = "")
cat("rows=", nrow(branches), " unique_data=", length(unique(branches$data_id)), "\n", sep = "")
cat("strip_methods=", paste(sort(unique(branches$strip_method)), collapse = ","), "\n", sep = "")
