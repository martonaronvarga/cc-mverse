#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
has_flag <- function(flag) flag %in% args

print_usage <- function() {
  cat(
    "Usage: Rscript bin/run_post_model_targets.R [options]\n",
    "\n",
    "Runs the post-model targets declared in _targets.R as one coherent targets-backed step.\n",
    "Existing model chunks are reused; modeling is only submitted if expected chunk outputs are missing.\n",
    "\n",
    "Options:\n",
    "  --skip-diagnostics  Assume diagnostic targets are already up to date; run downstream targets only.\n",
    "  --no-dashboard      Skip the dashboard plot target.\n",
    "  --no-csv-gallery    Skip exhaustive CSV plot gallery.\n",
    "  --no-archive        Do not tar outputs/analysis/figures after targets complete.\n",
    "  --dry-run           Print selected targets and environment defaults only.\n",
    "  --help            Show this help.\n",
    sep = ""
  )
}

if (has_flag("--help") || has_flag("-h")) {
  print_usage()
  quit(status = 0L)
}

if (!file.exists("_targets.R") && file.exists(file.path("R", "_targets.R"))) {
  setwd("R")
}
if (!file.exists("_targets.R")) {
  stop("Run this from the R project directory, or from the repository root containing R/_targets.R")
}

if (!nzchar(Sys.getenv("SKIP_RUST", unset = ""))) Sys.setenv(SKIP_RUST = "true")
if (!nzchar(Sys.getenv("DIAGNOSTICS_MODE", unset = ""))) Sys.setenv(DIAGNOSTICS_MODE = "cached")
if (!has_flag("--no-csv-gallery") && !nzchar(Sys.getenv("PLOT_ALL_CSV_OUTPUTS", unset = ""))) {
  Sys.setenv(PLOT_ALL_CSV_OUTPUTS = "true")
}
if (has_flag("--no-csv-gallery")) Sys.setenv(PLOT_ALL_CSV_OUTPUTS = "false")
skip_diagnostics <- has_flag("--skip-diagnostics")

post_model_targets <- if (skip_diagnostics) {
  c(
    "cse_definition_comparison_files",
    "analysis",
    "nullification_operating_characteristics_files"
  )
} else {
  c(
    "nullification_diagnostics_file",
    "cse_definition_comparison_files",
    "shuffle_adversarial_diagnostics_file",
    "analysis",
    "nullification_operating_characteristics_files"
  )
}
if (!has_flag("--no-dashboard")) post_model_targets <- c(post_model_targets, "plots")
if (!has_flag("--no-csv-gallery")) post_model_targets <- c(post_model_targets, "csv_output_plot_files")

cat("Post-model targets:\n")
cat(paste0("  - ", post_model_targets, collapse = "\n"), "\n", sep = "")
cat("Environment defaults:\n")
cat("  SKIP_RUST=", Sys.getenv("SKIP_RUST"), "\n", sep = "")
cat("  DIAGNOSTICS_MODE=", Sys.getenv("DIAGNOSTICS_MODE"), "\n", sep = "")
cat("  PLOT_ALL_CSV_OUTPUTS=", Sys.getenv("PLOT_ALL_CSV_OUTPUTS", unset = "<unset>"), "\n", sep = "")
cat("  skip diagnostics=", skip_diagnostics, "\n", sep = "")
cat("  targets shortcut=", skip_diagnostics, "\n", sep = "")

if (has_flag("--dry-run")) quit(status = 0L)

if (!requireNamespace("targets", quietly = TRUE)) stop("Package 'targets' is required")
if (!requireNamespace("tidyselect", quietly = TRUE)) stop("Package 'tidyselect' is required")

targets::tar_make(
  names = tidyselect::all_of(post_model_targets),
  shortcut = skip_diagnostics,
  script = "./run_post_model_targets.R"
)

archive_path <- NA_character_
if (!has_flag("--no-archive") && dir.exists(file.path("outputs", "analysis", "figures"))) {
  archive_path <- file.path(
    "outputs",
    "analysis",
    sprintf("figures_%s.tar.gz", format(Sys.time(), "%Y%m%d_%H%M%S"))
  )
  status <- system2(
    "tar",
    c("-czf", archive_path, "-C", file.path("outputs", "analysis"), "figures")
  )
  if (!identical(as.integer(status), 0L)) stop("tar failed while archiving figures")
  cat("Figure archive: ", archive_path, "\n", sep = "")
}

cat("Post-model targets complete\n")
