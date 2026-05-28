#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
input_path <- if (length(args) >= 1) args[[1]] else "all_indexed.csv"
output_dir <- if (length(args) >= 2) args[[2]] else file.path("outputs", "analysis")
studies <- if (length(args) >= 3) strsplit(args[[3]], ",", fixed = TRUE)[[1]] else c("flanker")
n_replicates <- if (length(args) >= 4) as.integer(args[[4]]) else 5L
max_participants <- if (length(args) >= 5) as.integer(args[[5]]) else 12L
alternative_interactions <- if (length(args) >= 6) {
  as.numeric(strsplit(args[[6]], ",", fixed = TRUE)[[1]])
} else {
  c(0.04)
}

function_path <- file.path("functions", "location_operating_characteristics.R")
if (!file.exists(function_path)) {
  function_path <- file.path("R", "functions", "location_operating_characteristics.R")
}
source(function_path)

path <- run_location_smoke_simulation(
  input_path = input_path,
  output_dir = output_dir,
  studies = studies,
  max_participants_per_study = max_participants,
  n_replicates = n_replicates,
  alternative_interaction_log = alternative_interactions
)
cat("Wrote smoke operating characteristics:", path, "\n")
