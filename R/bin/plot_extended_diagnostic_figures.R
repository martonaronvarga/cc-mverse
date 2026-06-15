#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (!file.exists("functions/analysis_plots.R") && file.exists(file.path("R", "functions", "analysis_plots.R"))) {
  setwd("R")
}
if (!file.exists("functions/analysis_plots.R")) {
  stop("Run from the R project directory, or repository root containing R/functions/analysis_plots.R")
}

analysis_root <- if (length(args) >= 1L) args[[1]] else file.path("outputs", "analysis")
run_dir <- if (length(args) >= 2L) args[[2]] else file.path(analysis_root, "latest")
output_dir <- if (length(args) >= 3L) args[[3]] else file.path(run_dir, "figures", "extended_diagnostics")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

source(file.path("functions", "analysis_plots.R"))

main_models <- c("rmANOVA", "LMM (random intercept)", "LMM (random congruency slope)")
main_nullifiers <- c(
  "additive_qmap",
  "additive_qmap_trial_bin",
  "local_mean_residual",
  "local_median_residual"
)

path_first <- function(...) {
  candidates <- c(...)
  found <- candidates[file.exists(candidates)]
  if (length(found) == 0L) stop("None of these files exist: ", paste(candidates, collapse = ", "))
  found[[1L]]
}

read_selected <- function(path, cols = NULL) {
  if (is.null(cols)) {
    return(readr::read_csv(path, show_col_types = FALSE))
  }
  readr::read_csv(path, col_select = tidyselect::any_of(cols), show_col_types = FALSE)
}

save_plot <- function(name, plot, width = 12, height = 8) {
  plot_save_fallback(file.path(output_dir, name), plot, width = width, height = height)
}

sample_label <- function(x) scales::percent(as.numeric(x), accuracy = 1)

plot_cse_definition_flags <- function(cse) {
  d <- cse |>
    dplyr::filter(.data$effect_condition == "null_interaction") |>
    dplyr::group_by(.data$strip_method, .data$transformation, .data$distributional_flag) |>
    dplyr::summarise(n_branches = sum(.data$n_branches, na.rm = TRUE), .groups = "drop") |>
    dplyr::group_by(.data$strip_method, .data$transformation) |>
    dplyr::mutate(rate = .data$n_branches / sum(.data$n_branches, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      strip_method_label = pretty_strip_method(.data$strip_method),
      transformation_label = dplyr::recode(.data$transformation, log_rt = "log(RT)", no_log_rt = "Raw RT"),
      flag_label = stringr::str_replace_all(.data$distributional_flag, "_", " ") |>
        stringr::str_wrap(width = 24)
    )

  ggplot2::ggplot(d, ggplot2::aes(x = strip_method_label, y = rate, fill = flag_label)) +
    ggplot2::geom_col(width = 0.72) +
    ggplot2::facet_wrap(ggplot2::vars(transformation_label), scales = "free") +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::scale_fill_brewer(palette = "Set2", name = "CSE definition flag") +
    ggplot2::labs(
      title = "CSE Definition Outcomes by Nullifier",
      subtitle = "How often stripped branches look location-only, distributional/shape-residual, or still location-CSE-positive",
      x = "Nullifier",
      y = "Share of grouped branches"
    ) +
    theme_multiverse(base_size = 10) +
    legend_bottom_rows(1) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 25, hjust = 1, vjust = 1))
}

plot_cse_location_vs_shape <- function(cse) {
  d <- cse |>
    dplyr::filter(.data$effect_condition == "null_interaction") |>
    dplyr::mutate(
      strip_method_label = pretty_strip_method(.data$strip_method),
      transformation_label = dplyr::recode(.data$transformation, log_rt = "log(RT)", no_log_rt = "Raw RT"),
      verdict_label = stringr::str_replace_all(.data$nullification_verdict, "_", " "),
      flag_label = stringr::str_replace_all(.data$distributional_flag, "_", " ")
    )

  ggplot2::ggplot(d, ggplot2::aes(
    x = .data$max_abs_location_cse,
    y = .data$max_abs_distributional_cse,
    color = .data$strip_method_label,
    shape = .data$preservation_pass,
    size = .data$n_branches
  )) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "grey45") +
    ggplot2::geom_vline(xintercept = 5, linetype = "dashed", color = "#C62828", alpha = 0.7) +
    ggplot2::geom_hline(yintercept = 5, linetype = "dashed", color = "#C62828", alpha = 0.7) +
    ggplot2::geom_point(alpha = 0.72) +
    ggplot2::scale_x_continuous(trans = "sqrt") +
    ggplot2::scale_y_continuous(trans = "sqrt") +
    ggplot2::scale_size_continuous(range = c(1.2, 5), name = "Branches") +
    ggplot2::facet_wrap(ggplot2::vars(transformation_label), scales = "free") +
    ggplot2::labs(
      title = "Location CSE Versus Distributional/Shape CSE",
      subtitle = "Dashed lines mark the 5 ms location-style threshold; points above horizontal line retain shape CSE",
      x = "Max absolute location CSE",
      y = "Max absolute distributional CSE",
      color = "Nullifier",
      shape = "Preservation pass"
    ) +
    theme_multiverse(base_size = 10) +
    legend_bottom_rows(1)
}

plot_cse_metrics_long_summary <- function(metrics) {
  d <- metrics |>
    dplyr::filter(
      .data$effect_condition == "null_interaction",
      .data$strip_method %in% main_nullifiers,
      is.finite(.data$abs_cse_value)
    ) |>
    dplyr::group_by(.data$definition_family, .data$metric, .data$strip_method, .data$transformation) |>
    dplyr::summarise(
      median_abs_cse = stats::median(.data$abs_cse_value, na.rm = TRUE),
      p95_abs_cse = stats::quantile(.data$abs_cse_value, probs = 0.95, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::group_by(.data$metric) |>
    dplyr::mutate(max_p95 = max(.data$p95_abs_cse, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::slice_max(.data$max_p95, n = 24, with_ties = FALSE) |>
    dplyr::mutate(
      metric_label = stringr::str_wrap(.data$metric, 24),
      strip_method_label = pretty_strip_method(.data$strip_method),
      transformation_label = dplyr::recode(.data$transformation, log_rt = "log(RT)", no_log_rt = "Raw RT"),
      family_label = stringr::str_replace_all(.data$definition_family, "_", " ")
    )

  ggplot2::ggplot(d, ggplot2::aes(x = strip_method_label, y = metric_label, fill = p95_abs_cse)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.35) +
    ggplot2::geom_text(ggplot2::aes(label = scales::number(median_abs_cse, accuracy = 0.01)), size = 2.4) +
    ggplot2::facet_grid(ggplot2::vars(family_label), ggplot2::vars(transformation_label), scales = "free", space = "free") +
    ggplot2::scale_fill_viridis_c(option = "magma", trans = "sqrt", name = "95th pct |CSE|") +
    ggplot2::labs(
      title = "Residual CSE Metric Profile",
      subtitle = "Fill is 95th percentile |CSE|; text is median |CSE|. Shows the worst retained CSE definitions.",
      x = "Nullifier",
      y = "CSE metric"
    ) +
    theme_multiverse(base_size = 9) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 25, hjust = 1, vjust = 1))
}

plot_nullification_verdicts <- function(diag) {
  d <- diag |>
    dplyr::filter(.data$effect_condition == "null_interaction") |>
    dplyr::group_by(.data$strip_method, .data$transformation, .data$sample_size, .data$nullification_verdict) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    dplyr::group_by(.data$strip_method, .data$transformation, .data$sample_size) |>
    dplyr::mutate(rate = .data$n / sum(.data$n)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      strip_method_label = pretty_strip_method(.data$strip_method),
      transformation_label = dplyr::recode(.data$transformation, log_rt = "log(RT)", no_log_rt = "Raw RT"),
      sample_label = sample_label(.data$sample_size),
      verdict_label = stringr::str_replace_all(.data$nullification_verdict, "_", " ")
    )

  ggplot2::ggplot(d, ggplot2::aes(x = sample_label, y = strip_method_label, fill = rate)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.3) +
    ggplot2::facet_grid(ggplot2::vars(verdict_label), ggplot2::vars(transformation_label), scales = "free") +
    ggplot2::scale_fill_viridis_c(labels = scales::percent, name = "Share") +
    ggplot2::labs(
      title = "Nullification Verdicts Across the Design",
      subtitle = "This is the diagnostic denominator behind interpretable versus sensitivity-only FPR rows",
      x = "Sample fraction",
      y = "Nullifier"
    ) +
    theme_multiverse(base_size = 9) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1, vjust = 1))
}

plot_preservation_warning_breakdown <- function(diag) {
  d <- diag |>
    dplyr::filter(.data$effect_condition == "null_interaction") |>
    dplyr::mutate(
      preservation_warnings = dplyr::na_if(.data$preservation_warnings, ""),
      preservation_warnings = dplyr::coalesce(.data$preservation_warnings, "passes_all_gates")
    ) |>
    tidyr::separate_rows(preservation_warnings, sep = ";") |>
    dplyr::count(.data$strip_method, .data$transformation, warning = .data$preservation_warnings, name = "n") |>
    dplyr::group_by(.data$strip_method, .data$transformation) |>
    dplyr::mutate(rate = .data$n / sum(.data$n)) |>
    dplyr::ungroup() |>
    dplyr::group_by(.data$warning) |>
    dplyr::mutate(total = sum(.data$n)) |>
    dplyr::ungroup() |>
    dplyr::filter(.data$warning == "passes_all_gates" | .data$total >= 100) |>
    dplyr::mutate(
      warning_label = stringr::str_replace_all(.data$warning, "_", " ") |> stringr::str_wrap(26),
      strip_method_label = pretty_strip_method(.data$strip_method),
      transformation_label = dplyr::recode(.data$transformation, log_rt = "log(RT)", no_log_rt = "Raw RT")
    )

  ggplot2::ggplot(d, ggplot2::aes(x = strip_method_label, y = rate, fill = warning_label)) +
    ggplot2::geom_col(width = 0.72) +
    ggplot2::facet_wrap(ggplot2::vars(transformation_label), scales = "free") +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::labs(
      title = "Which Preservation Gates Fail?",
      subtitle = "Warnings split on semicolons; rare warnings are omitted to keep the figure legible",
      x = "Nullifier",
      y = "Share of nullification rows",
      fill = "Gate/warning"
    ) +
    theme_multiverse(base_size = 9) +
    legend_bottom_rows(1) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 25, hjust = 1, vjust = 1))
}

plot_residual_cse_deltas <- function(diag) {
  metrics <- c(
    "mean_cse_delta_from_present",
    "q010_cse_delta_from_present",
    "q025_cse_delta_from_present",
    "q050_cse_delta_from_present",
    "q075_cse_delta_from_present",
    "q090_cse_delta_from_present",
    "max_abs_timebin_q050_cse_delta_from_present"
  )
  d <- diag |>
    dplyr::filter(.data$effect_condition == "null_interaction", .data$strip_method %in% main_nullifiers) |>
    tidyr::pivot_longer(tidyselect::any_of(metrics), names_to = "metric", values_to = "delta_ms") |>
    dplyr::filter(is.finite(.data$delta_ms)) |>
    dplyr::group_by(.data$strip_method, .data$transformation, .data$metric) |>
    dplyr::summarise(
      median_abs_delta = stats::median(abs(.data$delta_ms), na.rm = TRUE),
      p95_abs_delta = stats::quantile(abs(.data$delta_ms), probs = 0.95, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      metric_label = stringr::str_remove(.data$metric, "_delta_from_present$") |>
        stringr::str_replace_all("_", " ") |>
        stringr::str_wrap(20),
      strip_method_label = pretty_strip_method(.data$strip_method),
      transformation_label = dplyr::recode(.data$transformation, log_rt = "log(RT)", no_log_rt = "Raw RT")
    )

  ggplot2::ggplot(d, ggplot2::aes(x = strip_method_label, y = metric_label, fill = p95_abs_delta)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.35) +
    ggplot2::geom_text(ggplot2::aes(label = scales::number(median_abs_delta, accuracy = 0.01)), size = 2.5) +
    ggplot2::facet_wrap(ggplot2::vars(transformation_label), scales = "free") +
    ggplot2::scale_fill_viridis_c(option = "magma", trans = "sqrt", name = "95th pct |delta|") +
    ggplot2::labs(
      title = "Residual Diagnostics Relative to Present Branches",
      subtitle = "Fill is 95th percentile absolute delta; text is median absolute delta",
      x = "Nullifier",
      y = "Diagnostic"
    ) +
    theme_multiverse(base_size = 9) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 25, hjust = 1, vjust = 1))
}

plot_shuffle_preservation <- function(shuf) {
  d <- shuf |>
    dplyr::mutate(
      parts = strsplit(.data$data_id, "__", fixed = TRUE),
      sample_size = suppressWarnings(as.numeric(vapply(.data$parts, `[`, character(1), 1))),
      transformation = vapply(.data$parts, `[`, character(1), 3),
      outlier = vapply(.data$parts, `[`, character(1), 4)
    ) |>
    dplyr::select(-.data$parts) |>
    dplyr::group_by(.data$sample_size, .data$transformation, .data$outlier) |>
    dplyr::summarise(
      row_count_match = mean(.data$row_count_match %in% TRUE, na.rm = TRUE),
      multiset_preserved = mean(.data$multiset_preserved %in% TRUE, na.rm = TRUE),
      adversarial_pass = mean(.data$shuffle_adversarial_pass %in% TRUE, na.rm = TRUE),
      median_abs_shuffle_slope = stats::median(.data$abs_shuffle_prev_cong_rt_slope, na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::pivot_longer(
      c("row_count_match", "multiset_preserved", "adversarial_pass", "median_abs_shuffle_slope"),
      names_to = "metric",
      values_to = "value"
    ) |>
    dplyr::mutate(
      sample_label = sample_label(.data$sample_size),
      transformation_label = dplyr::recode(.data$transformation, log_rt = "log(RT)", no_log_rt = "Raw RT"),
      metric_label = stringr::str_replace_all(.data$metric, "_", " ") |> stringr::str_wrap(18),
      outlier_label = pretty_outlier(.data$outlier)
    )

  ggplot2::ggplot(d, ggplot2::aes(x = sample_label, y = outlier_label, fill = value)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.3) +
    ggplot2::facet_grid(ggplot2::vars(metric_label), ggplot2::vars(transformation_label), scales = "free") +
    ggplot2::scale_fill_viridis_c(name = "Value") +
    ggplot2::labs(
      title = "Shuffle Adversarial Diagnostics",
      subtitle = "Shuffle is shown separately because preservation/multiset checks define whether it is a valid null or only a stress test",
      x = "Sample fraction",
      y = "Outlier rule"
    ) +
    theme_multiverse(base_size = 8) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1, vjust = 1))
}

plot_shuffle_slope_scatter <- function(shuf) {
  d <- shuf |>
    dplyr::mutate(
      parts = strsplit(.data$data_id, "__", fixed = TRUE),
      transformation = vapply(.data$parts, `[`, character(1), 3),
      outlier = vapply(.data$parts, `[`, character(1), 4),
      transformation_label = dplyr::recode(.data$transformation, log_rt = "log(RT)", no_log_rt = "Raw RT"),
      outlier_label = pretty_outlier(.data$outlier)
    ) |>
    dplyr::filter(is.finite(.data$present_prev_cong_rt_slope), is.finite(.data$shuffle_prev_cong_rt_slope))

  ggplot2::ggplot(d, ggplot2::aes(x = present_prev_cong_rt_slope, y = shuffle_prev_cong_rt_slope, color = outlier_label)) +
    ggplot2::geom_hline(yintercept = 0, color = "grey45", linewidth = 0.3) +
    ggplot2::geom_vline(xintercept = 0, color = "grey45", linewidth = 0.3) +
    ggplot2::geom_point(alpha = 0.25, size = 0.7) +
    ggplot2::facet_wrap(ggplot2::vars(transformation_label), scales = "free") +
    ggplot2::labs(
      title = "Does Shuffle Remove Previous-Congruency RT Slope?",
      subtitle = "A good stress-test shuffle should pull points toward y = 0, but preservation gates still matter",
      x = "Present branch previous-congruency RT slope",
      y = "Shuffle branch previous-congruency RT slope",
      color = "Outlier rule"
    ) +
    theme_multiverse(base_size = 9) +
    legend_bottom_rows(1)
}

plot_failure_aware_composition <- function(fail) {
  d <- fail |>
    dplyr::filter(.data$model_type %in% main_models, .data$strip_method %in% main_nullifiers) |>
    dplyr::group_by(.data$model_type, .data$strip_method, .data$transformation) |>
    dplyr::summarise(
      significant = sum(.data$significant_primary, na.rm = TRUE),
      usable_nonsignificant = sum(.data$usable_nonsignificant, na.rm = TRUE),
      singular = sum(.data$singular, na.rm = TRUE),
      non_converged = sum(.data$non_converged, na.rm = TRUE),
      extraction_or_preprocessing_error = sum(.data$extraction_or_preprocessing_error, na.rm = TRUE),
      other_invalid = sum(.data$other_invalid, na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::pivot_longer(
      c("significant", "usable_nonsignificant", "singular", "non_converged", "extraction_or_preprocessing_error", "other_invalid"),
      names_to = "status",
      values_to = "n"
    ) |>
    dplyr::group_by(.data$model_type, .data$strip_method, .data$transformation) |>
    dplyr::mutate(rate = .data$n / sum(.data$n, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      model_label = pretty_model(.data$model_type),
      strip_method_label = pretty_strip_method(.data$strip_method),
      transformation_label = dplyr::recode(.data$transformation, log_rt = "log(RT)", no_log_rt = "Raw RT"),
      status_label = stringr::str_replace_all(.data$status, "_", " ") |> stringr::str_wrap(16)
    )

  ggplot2::ggplot(d, ggplot2::aes(x = model_label, y = rate, fill = status_label)) +
    ggplot2::geom_col(width = 0.72) +
    ggplot2::facet_grid(ggplot2::vars(strip_method_label), ggplot2::vars(transformation_label), scales = "free") +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::labs(
      title = "Failure-Aware Nullification Operating Characteristics",
      subtitle = "Denominator is planned branches, not only usable fits; full LMM and shuffle are excluded here",
      x = "Model",
      y = "Share of planned branches",
      fill = "Outcome"
    ) +
    theme_multiverse(base_size = 9) +
    legend_bottom_rows(1) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 25, hjust = 1, vjust = 1))
}

plot_conditional_vs_unconditional <- function(fail) {
  d <- fail |>
    dplyr::filter(.data$model_type %in% main_models, .data$strip_method %in% main_nullifiers) |>
    dplyr::group_by(.data$model_type, .data$strip_method, .data$transformation, .data$sample_size) |>
    dplyr::summarise(
      n_planned = sum(.data$n_planned, na.rm = TRUE),
      significant_primary = sum(.data$significant_primary, na.rm = TRUE),
      conditional_n_usable = sum(.data$conditional_n_usable, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      unconditional_rate = .data$significant_primary / .data$n_planned,
      conditional_rate = dplyr::if_else(.data$conditional_n_usable > 0, .data$significant_primary / .data$conditional_n_usable, NA_real_),
      invalid_rate = 1 - .data$conditional_n_usable / .data$n_planned,
      model_label = pretty_model(.data$model_type),
      strip_method_label = pretty_strip_method(.data$strip_method),
      transformation_label = dplyr::recode(.data$transformation, log_rt = "log(RT)", no_log_rt = "Raw RT")
    )

  ggplot2::ggplot(d, ggplot2::aes(x = unconditional_rate, y = conditional_rate, color = model_label, size = invalid_rate)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "grey45") +
    ggplot2::geom_vline(xintercept = 0.05, linetype = "dashed", color = "#C62828", alpha = 0.7) +
    ggplot2::geom_hline(yintercept = 0.05, linetype = "dashed", color = "#C62828", alpha = 0.7) +
    ggplot2::geom_point(alpha = 0.75) +
    ggplot2::scale_x_continuous(labels = scales::percent) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::scale_size_continuous(labels = scales::percent, name = "Invalid branch share") +
    ggplot2::facet_grid(ggplot2::vars(strip_method_label), ggplot2::vars(transformation_label), scales = "free") +
    ggplot2::labs(
      title = "Conditional Versus Unconditional Nullification Rates",
      subtitle = "Shows whether high/low FPR is driven by significance among usable fits or by fit failures changing the denominator",
      x = "Unconditional rate among planned branches",
      y = "Conditional rate among usable fits",
      color = "Model"
    ) +
    theme_multiverse(base_size = 9) +
    legend_bottom_rows(1)
}

plot_interpretable_vs_all_fpr <- function(fpr) {
  d <- fpr |>
    dplyr::filter(.data$model_type %in% main_models, .data$strip_method %in% main_nullifiers) |>
    dplyr::mutate(source = dplyr::if_else(.data$interpretable_fpr_source, "Diagnostics-passing only", "All usable nullified fits")) |>
    dplyr::group_by(.data$model_type, .data$strip_method, .data$transformation, .data$source) |>
    dplyr::summarise(n = sum(.data$n, na.rm = TRUE), false_positives = sum(.data$false_positives, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(
      FPR = .data$false_positives / .data$n,
      model_label = pretty_model(.data$model_type),
      strip_method_label = pretty_strip_method(.data$strip_method),
      transformation_label = dplyr::recode(.data$transformation, log_rt = "log(RT)", no_log_rt = "Raw RT")
    )

  ggplot2::ggplot(d, ggplot2::aes(x = model_label, y = FPR, fill = source)) +
    ggplot2::geom_hline(yintercept = 0.05, linetype = "dashed", color = "#C62828") +
    ggplot2::geom_col(position = "dodge", width = 0.72) +
    ggplot2::facet_grid(ggplot2::vars(strip_method_label), ggplot2::vars(transformation_label), scales = "free") +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::labs(
      title = "Do Preservation Gates Change the FPR Story?",
      subtitle = "Compares all usable nullified fits with diagnostics-passing/interpretable rows",
      x = "Model",
      y = "Nullification-based FPR",
      fill = "Rate denominator"
    ) +
    theme_multiverse(base_size = 9) +
    legend_bottom_rows(1) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 25, hjust = 1, vjust = 1))
}

# Top-level diagnostic outputs.
cse <- read_selected(path_first(file.path(analysis_root, "cse_definition_comparison.csv")))
metrics <- read_selected(
  path_first(file.path(analysis_root, "cse_definition_metrics_long.csv")),
  c("effect_condition", "strip_method", "sample_size", "outlier", "transformation", "nullification_verdict", "preservation_pass", "metric", "definition_family", "abs_cse_value")
)
diag <- read_selected(
  path_first(file.path(analysis_root, "nullification_diagnostics.csv")),
  c("effect_condition", "strip_method", "sample_size", "outlier", "transformation", "preservation_pass", "preservation_warnings", "nullification_verdict", "mean_cse_delta_from_present", "q010_cse_delta_from_present", "q025_cse_delta_from_present", "q050_cse_delta_from_present", "q075_cse_delta_from_present", "q090_cse_delta_from_present", "max_abs_timebin_q050_cse_delta_from_present")
)
shuf <- read_selected(path_first(file.path(analysis_root, "shuffle_adversarial_diagnostics.csv")))

# Run-level model/nullification operating-characteristic outputs.
failure_aware <- read_selected(path_first(
  file.path(run_dir, "nullification_failure_aware_rates.csv"),
  file.path(run_dir, "nullification_operating_characteristics", "nullification_failure_aware_rates.csv")
))
fpr_coarse <- read_selected(path_first(
  file.path(run_dir, "fpr_coarse.csv"),
  file.path(run_dir, "nullification_operating_characteristics", "nullification_fpr_coarse.csv")
))

save_plot("cse_definition_flags", plot_cse_definition_flags(cse), width = 12, height = 7)
save_plot("cse_location_vs_distributional_shape", plot_cse_location_vs_shape(cse), width = 12, height = 8)
save_plot("cse_metrics_long_residual_profile", plot_cse_metrics_long_summary(metrics), width = 14, height = 11)
save_plot("nullification_verdict_matrix", plot_nullification_verdicts(diag), width = 14, height = 10)
save_plot("nullification_preservation_warning_breakdown", plot_preservation_warning_breakdown(diag), width = 13, height = 9)
save_plot("nullification_residual_cse_delta_profile", plot_residual_cse_deltas(diag), width = 13, height = 8)
save_plot("shuffle_adversarial_preservation", plot_shuffle_preservation(shuf), width = 14, height = 11)
save_plot("shuffle_prev_congruency_slope_scatter", plot_shuffle_slope_scatter(shuf), width = 11, height = 8)
save_plot("operating_failure_aware_composition", plot_failure_aware_composition(failure_aware), width = 14, height = 10)
save_plot("operating_conditional_vs_unconditional", plot_conditional_vs_unconditional(failure_aware), width = 14, height = 10)
save_plot("operating_interpretable_vs_all_fpr", plot_interpretable_vs_all_fpr(fpr_coarse), width = 14, height = 10)

writeLines(
  c(
    "Extended diagnostic figures generated.",
    paste("analysis_root:", normalizePath(analysis_root, mustWork = FALSE)),
    paste("run_dir:", normalizePath(run_dir, mustWork = FALSE)),
    paste("output_dir:", normalizePath(output_dir, mustWork = FALSE)),
    "These plots cover CSE definition comparison, CSE metric profiles, nullification diagnostics, shuffle adversarial diagnostics, and nullification operating characteristics."
  ),
  file.path(output_dir, "README.txt")
)

cat("Extended diagnostic plotting code wrote figures to ", output_dir, "\n", sep = "")
