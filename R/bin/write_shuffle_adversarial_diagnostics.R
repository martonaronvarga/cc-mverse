#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
input_dir <- if (length(args) >= 1L) args[[1]] else file.path("R", "data", "processed")
output_csv <- if (length(args) >= 2L) args[[2]] else file.path("R", "outputs", "analysis", "shuffle_adversarial_diagnostics.csv")

source_path <- function(path) {
  if (file.exists(path)) return(path)
  file.path("R", path)
}
source(source_path(file.path("functions", "nullification_diagnostics.R")))
source(source_path(file.path("functions", "shuffle_adversarial_diagnostics.R")))

path <- write_shuffle_adversarial_diagnostics_for_dir(input_dir, output_csv)
cat("Wrote shuffle adversarial diagnostics: ", path, "\n", sep = "")
