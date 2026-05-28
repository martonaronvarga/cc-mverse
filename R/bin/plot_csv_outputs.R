#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
input_dir <- if (length(args) >= 1L) args[[1]] else file.path("R", "outputs", "analysis")
output_dir <- if (length(args) >= 2L) args[[2]] else file.path(input_dir, "figures", "csv_outputs")

source_path <- function(path) {
  if (file.exists(path)) return(path)
  file.path("R", path)
}
source(source_path(file.path("functions", "csv_output_plots.R")))
manifest <- write_csv_output_plots(input_dir, output_dir)
manifest_path <- file.path(output_dir, "csv_output_plot_manifest.csv")
readr::write_csv(manifest, manifest_path)
cat("CSV output plots written: ", sum(manifest$plotted), "/", nrow(manifest), "\n", sep = "")
cat("Manifest: ", manifest_path, "\n", sep = "")
if (any(!manifest$plotted)) quit(status = 1L)
