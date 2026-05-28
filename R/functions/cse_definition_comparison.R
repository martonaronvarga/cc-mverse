# R/functions/cse_definition_comparison.R
# Compare location-only CSE with distributional and time-local diagnostics.

location_distribution_metric_map <- data.frame(
  metric = c(
    "mean_cse", "median_cse", "q010_cse", "q025_cse", "q050_cse",
    "q075_cse", "q090_cse", "max_abs_timebin_q050_cse"
  ),
  definition_family = c(
    "location", "distributional", "distributional", "distributional", "distributional",
    "distributional", "distributional", "time_local_distributional"
  ),
  description = c(
    "Participant-level 2x2 cell-mean location interaction averaged over participants.",
    "Participant-level median of participant cell-mean interactions.",
    "Pooled 10th-percentile 2x2 quantile interaction.",
    "Pooled 25th-percentile 2x2 quantile interaction.",
    "Pooled median 2x2 quantile interaction.",
    "Pooled 75th-percentile 2x2 quantile interaction.",
    "Pooled 90th-percentile 2x2 quantile interaction.",
    "Maximum absolute pooled median quantile interaction across trial-index bins."
  ),
  stringsAsFactors = FALSE
)

available_cse_metrics <- function(diagnostics_df) {
  location_distribution_metric_map$metric[location_distribution_metric_map$metric %in% names(diagnostics_df)]
}

build_cse_definition_long <- function(diagnostics_df) {
  stopifnot(is.data.frame(diagnostics_df))
  id_cols <- c("data_id", "effect_condition", "strip_method", "sample_size", "outlier", "transformation", "nullification_verdict", "preservation_pass")
  id_cols <- id_cols[id_cols %in% names(diagnostics_df)]
  metric_cols <- available_cse_metrics(diagnostics_df)
  if (length(metric_cols) == 0L) {
    stop("Diagnostics table does not contain CSE definition metrics.")
  }

  diagnostics_df |>
    dplyr::select(dplyr::all_of(c(id_cols, metric_cols))) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(metric_cols),
      names_to = "metric",
      values_to = "cse_value"
    ) |>
    dplyr::left_join(location_distribution_metric_map, by = "metric") |>
    dplyr::mutate(abs_cse_value = abs(.data$cse_value))
}

summarise_cse_definition_comparison <- function(diagnostics_df) {
  long <- build_cse_definition_long(diagnostics_df)
  location <- long |>
    dplyr::filter(.data$metric == "mean_cse") |>
    dplyr::select(dplyr::any_of(c("data_id", "effect_condition", "strip_method", "sample_size", "outlier", "transformation", "cse_value"))) |>
    dplyr::rename(location_mean_cse = "cse_value")

  join_cols <- intersect(
    c("data_id", "effect_condition", "strip_method", "sample_size", "outlier", "transformation"),
    names(location)
  )

  long |>
    dplyr::left_join(location, by = join_cols) |>
    dplyr::group_by(
      dplyr::across(dplyr::any_of(c(
        "effect_condition", "strip_method", "sample_size", "outlier", "transformation",
        "nullification_verdict", "preservation_pass"
      )))
    ) |>
    dplyr::summarise(
      n_branches = dplyr::n_distinct(.data$data_id),
      location_mean_cse = mean(.data$location_mean_cse, na.rm = TRUE),
      max_abs_location_cse = max(abs(.data$location_mean_cse), na.rm = TRUE),
      max_abs_distributional_cse = max(.data$abs_cse_value[.data$definition_family == "distributional"], na.rm = TRUE),
      max_abs_time_local_distributional_cse = max(.data$abs_cse_value[.data$definition_family == "time_local_distributional"], na.rm = TRUE),
      distribution_minus_location_abs = .data$max_abs_distributional_cse - .data$max_abs_location_cse,
      time_local_minus_location_abs = .data$max_abs_time_local_distributional_cse - .data$max_abs_location_cse,
      .groups = "drop"
    ) |>
    dplyr::mutate(
      max_abs_distributional_cse = dplyr::if_else(is.infinite(.data$max_abs_distributional_cse), NA_real_, .data$max_abs_distributional_cse),
      max_abs_time_local_distributional_cse = dplyr::if_else(is.infinite(.data$max_abs_time_local_distributional_cse), NA_real_, .data$max_abs_time_local_distributional_cse),
      distributional_flag = dplyr::case_when(
        is.na(.data$max_abs_distributional_cse) ~ "missing_distributional_metrics",
        .data$max_abs_distributional_cse > 10 & abs(.data$location_mean_cse) <= 5 ~ "distributional_residual_without_location_cse",
        .data$max_abs_distributional_cse <= 10 & abs(.data$location_mean_cse) <= 5 ~ "location_and_distributional_near_zero",
        TRUE ~ "location_cse_present"
      )
    )
}

write_cse_definition_comparison <- function(diagnostics_csv, output_dir = file.path("outputs", "analysis")) {
  if (!file.exists(diagnostics_csv)) stop("Diagnostics CSV not found: ", diagnostics_csv)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  diagnostics <- readr::read_csv(diagnostics_csv, show_col_types = FALSE)
  long <- build_cse_definition_long(diagnostics)
  summary <- summarise_cse_definition_comparison(diagnostics)

  long_path <- file.path(output_dir, "cse_definition_metrics_long.csv")
  summary_path <- file.path(output_dir, "cse_definition_comparison.csv")
  readr::write_csv(long, long_path)
  readr::write_csv(summary, summary_path)
  invisible(list(long = long_path, summary = summary_path))
}
