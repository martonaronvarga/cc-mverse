# R/functions/resampling_design.R - requested empirical resampling schedule

build_resampling_design <- function(fractions = c(0.05, 0.10, 0.20), n_resamples = 50L) {
  expand.grid(
    sample_size = fractions,
    subsample_id = seq_len(n_resamples),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  ) |>
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
