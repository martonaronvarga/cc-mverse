# R/functions/resampling_design.R - requested empirical resampling schedule

build_resampling_design <- function(
    fractions = c(0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.45, 0.50, 0.65, 0.75, 0.90, 1.00),
    n_resamples = 100L) {
  do.call(rbind, lapply(fractions, function(fraction) {
    n_for_fraction <- if (abs(fraction - 1.0) < 1e-6) 1L else n_resamples
    data.frame(
      sample_size = fraction,
      subsample_id = seq_len(n_for_fraction),
      stringsAsFactors = FALSE
    )
  })) |>
    transform(
      resampling_interpretation = "dependent_empirical_multiverse_path",
      rate_label = "empirical multiverse detection proportion",
      uncertainty_policy = "cluster_or_participant_bootstrap_not_independent_replications"
    )
}

write_resampling_design <- function(output_path = file.path("outputs", "analysis", "resampling_design_table.csv")) {
  dir <- dirname(output_path)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  table <- build_resampling_design()
  write.csv(table, output_path, row.names = FALSE)
  invisible(output_path)
}
