#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (!file.exists("functions/analysis_plots.R") && file.exists(file.path("R", "functions", "analysis_plots.R"))) {
  setwd("R")
}
if (!file.exists("functions/analysis_plots.R")) {
  stop("Run from the R project directory, or repository root containing R/functions/analysis_plots.R")
}

input_dir <- if (length(args) >= 1L) args[[1]] else file.path("outputs", "analysis", "latest")
output_dir <- if (length(args) >= 2L) args[[2]] else file.path(input_dir, "figures", "reporting_filtered")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

source(file.path("functions", "analysis_plots.R"))

read_csv <- function(name) {
  path <- file.path(input_dir, name)
  if (!file.exists(path)) stop("Missing required analysis file: ", path)
  readr::read_csv(path, show_col_types = FALSE)
}

main_models <- c("rmANOVA", "LMM (random intercept)", "LMM (random congruency slope)")
primary_nullifiers <- c(
  "null_interaction:local_mean_residual",
  "null_interaction:local_median_residual",
  "null_interaction:additive_qmap",
  "null_interaction:additive_qmap_trial_bin"
)
location_nullifier <- "null_interaction:local_mean_residual"
shape_sensitive_nullifiers <- c(
  "null_interaction:additive_qmap",
  "null_interaction:additive_qmap_trial_bin",
  "null_interaction:local_median_residual"
)

fpr_coarse <- read_csv("fpr_coarse.csv")
fpr_by_sample_size <- read_csv("fpr_by_sample_size.csv")
fpr_by_outlier <- read_csv("fpr_by_outlier.csv")
branch_health <- read_csv("branch_health.csv")
summary_table <- read_csv("summary_table.csv")

filter_main <- function(df, require_interpretable = TRUE) {
  df |>
    filter_primary_fpr_rows(
      null_types = primary_nullifiers,
      models = main_models,
      require_interpretable = require_interpretable
    )
}

save_plot <- function(name, plot, width = 12, height = 8) {
  plot_save_fallback(file.path(output_dir, name), plot, width = width, height = height)
}

main_fpr_coarse <- filter_main(fpr_coarse)
main_fpr_sample <- filter_main(fpr_by_sample_size)
main_fpr_outlier <- filter_main(fpr_by_outlier)
location_fpr_sample <- main_fpr_sample |>
  dplyr::filter(.data$null_type == location_nullifier)
location_fpr_outlier <- main_fpr_outlier |>
  dplyr::filter(.data$null_type == location_nullifier)
shape_fpr_coarse <- main_fpr_coarse |>
  dplyr::filter(.data$null_type %in% shape_sensitive_nullifiers)
shuffle_fpr_sample <- fpr_by_sample_size |>
  dplyr::filter(
    .data$model_type %in% main_models,
    .data$null_type == "null_interaction:shuffle"
  )
full_lmm_health <- branch_health |>
  dplyr::filter(.data$model_type == "LMM (full)")

save_plot(
  "primary_no_full_no_shuffle_fpr_by_nullifier",
  plot_fpr_by_null_type(main_fpr_coarse) +
    ggplot2::labs(
      title = "Primary FPR by Model and Nullification Choice",
      subtitle = "Diagnostics-passing rows only; full LMM and shuffle removed; dashed line = nominal 5%"
    ),
  width = 12,
  height = 10
)

save_plot(
  "primary_location_null_fpr_by_sample_size",
  plot_fpr_by_sample_size(location_fpr_sample, null_types = location_nullifier) +
    ggplot2::labs(
      title = "FPR When the Correct Null Is a Location-CSE Removal",
      subtitle = "Diagnostics-passing local mean residual rows only; full LMM and shuffle removed"
    ),
  width = 12,
  height = 7
)

save_plot(
  "primary_location_null_fpr_by_outlier",
  plot_fpr_outlier_heatmap(location_fpr_outlier, null_types = location_nullifier) +
    ggplot2::labs(
      title = "Location-Null FPR by Outlier Rule and Model",
      subtitle = "Local mean residual nullifier only; cells pool across sample sizes"
    ),
  width = 12,
  height = 7
)

save_plot(
  "shape_sensitive_nullifier_fpr_by_model",
  plot_fpr_by_null_type(shape_fpr_coarse) +
    ggplot2::labs(
      title = "FPR Under Shape-Sensitive Nullification Choices",
      subtitle = "Diagnostics-passing qmap, trial-bin qmap, and median residual rows; full LMM and shuffle removed"
    ),
  width = 12,
  height = 9
)

save_plot(
  "primary_exceedance_no_full_no_shuffle",
  plot_fpr_exceedance_summary(main_fpr_outlier, null_types = primary_nullifiers) +
    ggplot2::labs(
      title = "Where FPR Inflation Comes From in the Main Reporting Set",
      subtitle = "Full LMM and shuffle removed; all non-shuffle nullifiers included"
    ),
  width = 11,
  height = 9
)

save_plot(
  "primary_extreme_combinations_no_full_no_shuffle",
  plot_fpr_extreme_combinations(main_fpr_outlier, null_types = primary_nullifiers, top_n = 18) +
    ggplot2::labs(
      title = "Most Inflated Main-Set Combinations",
      subtitle = "Full LMM and shuffle removed; point size = null branches"
    ),
  width = 12,
  height = 8
)

save_plot(
  "sensitivity_shuffle_only_no_full_lmm",
  plot_fpr_by_sample_size(
    shuffle_fpr_sample,
    null_types = "null_interaction:shuffle",
    require_interpretable = FALSE
  ) +
    ggplot2::labs(
      title = "Shuffle Sensitivity Only",
      subtitle = "Shuffle fails preservation diagnostics; full LMM removed"
    ),
  width = 12,
  height = 7
)

save_plot(
  "sensitivity_full_lmm_health",
  plot_branch_health(full_lmm_health) +
    ggplot2::labs(
      title = "Full LMM Health, Presented Separately",
      subtitle = "Full LMM is excluded from primary FPR plots because usable branches collapse"
    ),
  width = 12,
  height = 7
)

story <- main_fpr_coarse |>
  dplyr::mutate(
    nullifier_family = dplyr::case_when(
      .data$null_type == location_nullifier ~ "location_null_local_mean_residual",
      .data$null_type %in% shape_sensitive_nullifiers ~ "shape_sensitive_or_distributional_nullifier",
      TRUE ~ "other"
    )
  ) |>
  dplyr::group_by(.data$nullifier_family, .data$null_type, .data$model_type, .data$transformation) |>
  dplyr::summarise(
    n = sum(.data$n, na.rm = TRUE),
    false_positives = sum(.data$false_positives, na.rm = TRUE),
    FPR = dplyr::if_else(.data$n > 0, .data$false_positives / .data$n, NA_real_),
    .groups = "drop"
  ) |>
  dplyr::arrange(.data$nullifier_family, dplyr::desc(.data$FPR))
readr::write_csv(story, file.path(output_dir, "reporting_fpr_story_summary.csv"))

writeLines(
  c(
    "Filtered reporting figures generated.",
    paste("Input:", normalizePath(input_dir, mustWork = FALSE)),
    paste("Output:", normalizePath(output_dir, mustWork = FALSE)),
    "Primary plots exclude LMM (full) and shuffle.",
    "Shuffle and full LMM are presented separately as sensitivity/diagnostic figures."
  ),
  file.path(output_dir, "README.txt")
)

cat("Filtered reporting figures written to ", output_dir, "\n", sep = "")
