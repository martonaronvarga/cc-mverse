#!/usr/bin/env Rscript

quiet_load <- function(expr) invisible(capture.output(suppressMessages(suppressWarnings(force(expr)))))
quiet_load({
  invisible(lapply(list.files("functions", pattern = "\\.R$", full.names = TRUE), source))
  load_all_packages()
})

args <- commandArgs(trailingOnly = TRUE)
has_flag <- function(flag) flag %in% args

state <- if (dir.exists("_config")) load_pipeline_state("_config") else NULL
config <- if (!is.null(state)) state$config else load_config("hpc")
paths <- if (!is.null(state)) state$paths else init_project_paths(".")
setup_logging(log_level = config$log_level %||% "info", log_dir = paths$logs)
initialize_results_schema(paths$outputs_results)

Sys.setenv(DIAGNOSTICS_MODE = Sys.getenv("DIAGNOSTICS_MODE", unset = "cached"))

log_pipeline(logger::INFO, "Fast post-model analysis starting")
results <- load_results(paths, validate = TRUE)
log_pipeline(logger::INFO, "Loaded {nrow(results)} model result row(s)")

null_diag <- file.path(paths$outputs_analysis, "nullification_diagnostics.csv")
write_nullification_diagnostics_for_dir(paths$data_processed, null_diag)
log_pipeline(logger::INFO, "Wrote fast nullification diagnostics: {null_diag}")

shuffle_diag <- file.path(paths$outputs_analysis, "shuffle_adversarial_diagnostics.csv")
write_shuffle_adversarial_diagnostics_for_dir(paths$data_processed, shuffle_diag)
log_pipeline(logger::INFO, "Wrote fast shuffle diagnostics: {shuffle_diag}")

write_cse_definition_comparison(null_diag, output_dir = paths$outputs_analysis)
analysis <- analyze_and_save(results, paths, diagnostics_csv = null_diag, alpha = config$alpha)

nullification_tables <- build_nullification_operating_characteristics(
  results,
  readr::read_csv(null_diag, show_col_types = FALSE),
  alpha = config$alpha
)
nullification_dir <- file.path(paths$outputs_analysis, "nullification_operating_characteristics")
dir.create(nullification_dir, recursive = TRUE, showWarnings = FALSE)
readr::write_csv(nullification_tables$fpr_coarse, file.path(nullification_dir, "nullification_fpr_coarse.csv"))
readr::write_csv(nullification_tables$fpr_by_sample, file.path(nullification_dir, "nullification_fpr_by_sample.csv"))
readr::write_csv(nullification_tables$fpr_by_outlier, file.path(nullification_dir, "nullification_fpr_by_outlier.csv"))
readr::write_csv(nullification_tables$failure_aware, file.path(nullification_dir, "nullification_failure_aware_rates.csv"))

if (!has_flag("--skip-plots")) {
  source("functions/analysis_plots.R")
  fig_dir <- file.path(paths$outputs_analysis, "figures")
  generate_multiverse_dashboard(analysis, output_dir = fig_dir, save_individual = TRUE)
  if (tolower(Sys.getenv("PLOT_ALL_CSV_OUTPUTS", unset = "false")) %in% c("1", "true", "yes")) {
    output_dir <- file.path(paths$outputs_analysis, "figures", "csv_outputs")
    manifest <- write_csv_output_plots(paths$outputs_analysis, output_dir)
    readr::write_csv(manifest, file.path(output_dir, "csv_output_plot_manifest.csv"))
  }
}

log_pipeline(logger::INFO, "Fast post-model analysis complete")
cat("Fast post-model analysis complete\n")
