# R/functions/nullification_dashboard.R
# Reliability plots for empirical CSE nullification diagnostics.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

nd_wrap <- function(x, width = 22) stringr::str_wrap(as.character(x), width = width)

nd_theme <- function(base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.box = "vertical",
      panel.grid.minor = ggplot2::element_blank(),
      panel.spacing = grid::unit(1.1, "lines"),
      strip.text = ggplot2::element_text(face = "bold", lineheight = 0.95, margin = ggplot2::margin(4, 4, 4, 4)),
      plot.title = ggplot2::element_text(face = "bold", size = base_size + 2, lineheight = 0.95),
      plot.subtitle = ggplot2::element_text(color = "grey40", lineheight = 0.95),
      plot.margin = ggplot2::margin(10, 14, 10, 10),
      axis.title = ggplot2::element_text(face = "bold")
    )
}

nd_save <- function(path, plot, width, height, dpi = 150) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ext <- tolower(tools::file_ext(path))
  if (identical(ext, "png") && requireNamespace("ragg", quietly = TRUE)) {
    ggplot2::ggsave(path, plot, device = ragg::agg_png, width = width, height = height, dpi = dpi, limitsize = FALSE, bg = "white")
  } else if (identical(ext, "svg") && requireNamespace("svglite", quietly = TRUE)) {
    ggplot2::ggsave(path, plot, device = svglite::svglite, width = width, height = height, dpi = dpi, limitsize = FALSE, bg = "white")
  } else {
    fallback <- if (identical(ext, "pdf")) path else paste0(tools::file_path_sans_ext(path), ".pdf")
    ggplot2::ggsave(fallback, plot, device = grDevices::pdf, width = width, height = height, dpi = dpi, limitsize = FALSE, bg = "white")
    path <- fallback
  }
  path
}

nd_prepare <- function(diagnostics_df) {
  diagnostics_df %>%
    dplyr::mutate(
      strip_method_label = nd_wrap(.data$strip_method, 18),
      transformation_label = dplyr::case_when(
        "transformation" %in% names(.) & .data$transformation == "log_rt" ~ "log(RT)",
        "transformation" %in% names(.) & .data$transformation %in% c("no_log_rt", "raw_rt") ~ "Raw RT",
        "transformation" %in% names(.) ~ nd_wrap(.data$transformation, 16),
        TRUE ~ "unknown"
      ),
      outlier_label = if ("outlier" %in% names(.)) nd_wrap(dplyr::coalesce(.data$outlier, "none"), 14) else "unknown",
      sample_label = if ("sample_size" %in% names(.)) scales::percent(.data$sample_size, accuracy = 1) else "sample n/a",
      preservation_status = dplyr::case_when(
        isTRUE(.data$preservation_pass) ~ "pass",
        isFALSE(.data$preservation_pass) ~ "fail",
        TRUE ~ "unknown"
      ),
      nullification_verdict = dplyr::coalesce(.data$nullification_verdict, "diagnostics_missing")
    )
}

plot_nullification_reliability <- function(diagnostics_df) {
  stopifnot(is.data.frame(diagnostics_df))
  required <- c("strip_method", "effect_condition", "mean_cse", "q050_cse", "max_abs_timebin_q050_cse", "preservation_pass")
  missing <- setdiff(required, names(diagnostics_df))
  if (length(missing) > 0) stop("Missing diagnostics columns: ", paste(missing, collapse = ", "))

  d <- diagnostics_df %>%
    dplyr::filter(.data$effect_condition == "null_interaction") %>%
    nd_prepare()

  residual <- d %>%
    dplyr::select(strip_method_label, transformation_label, mean_cse, q050_cse, max_abs_timebin_q050_cse, preservation_status) %>%
    tidyr::pivot_longer(
      cols = c(mean_cse, q050_cse, max_abs_timebin_q050_cse),
      names_to = "metric",
      values_to = "value"
    ) %>%
    dplyr::mutate(metric = dplyr::recode(.data$metric,
      mean_cse = "Mean CSE",
      q050_cse = "Median quantile CSE",
      max_abs_timebin_q050_cse = "Max time-bin median CSE"
    )) %>%
    ggplot2::ggplot(ggplot2::aes(x = strip_method_label, y = value, fill = preservation_status)) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.3, color = "grey45") +
    ggplot2::geom_hline(yintercept = c(-5, 5), linewidth = 0.25, linetype = "dashed", color = "grey60") +
    ggplot2::geom_col(width = 0.68) +
    ggplot2::facet_grid(rows = ggplot2::vars(metric), cols = ggplot2::vars(transformation_label), scales = "free_y") +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::scale_fill_manual(values = c(pass = "#2E7D32", fail = "#C62828", unknown = "#78909C"), name = "Preservation") +
    ggplot2::labs(
      title = "Residual CSE After Nullification",
      subtitle = "Dashed guides mark +/-5 ms residual thresholds where applicable",
      x = "Strip method",
      y = "CSE diagnostic (ms)"
    ) +
    nd_theme()

  delta_cols <- grep("_delta_from_present$", names(d), value = TRUE)
  deltas <- if (length(delta_cols) > 0) {
    d %>%
      dplyr::select(strip_method_label, transformation_label, dplyr::all_of(delta_cols)) %>%
      tidyr::pivot_longer(-c(strip_method_label, transformation_label), names_to = "metric", values_to = "delta") %>%
      dplyr::mutate(metric = nd_wrap(gsub("_delta_from_present$", "", .data$metric), 18)) %>%
      ggplot2::ggplot(ggplot2::aes(x = strip_method_label, y = delta)) +
      ggplot2::geom_hline(yintercept = 0, linewidth = 0.3, color = "grey45") +
      ggplot2::geom_point(size = 2, color = "#455A64") +
      ggplot2::facet_grid(rows = ggplot2::vars(metric), cols = ggplot2::vars(transformation_label), scales = "free_y") +
      ggplot2::coord_flip(clip = "off") +
      ggplot2::labs(
        title = "Preservation Deltas From Present Branch",
        subtitle = "Each panel shows how much nullification changed a non-target diagnostic",
        x = "Strip method",
        y = "Delta from present"
      ) +
      nd_theme(base_size = 10)
  } else {
    ggplot2::ggplot() + ggplot2::labs(title = "No preservation delta columns available") + ggplot2::theme_void()
  }

  quantile_cols <- intersect(c("q010_cse", "q025_cse", "q050_cse", "q075_cse", "q090_cse"), names(d))
  quantile_profile <- if (length(quantile_cols) > 0) {
    d %>%
      dplyr::select(strip_method_label, transformation_label, dplyr::all_of(quantile_cols), nullification_verdict) %>%
      tidyr::pivot_longer(dplyr::all_of(quantile_cols), names_to = "quantile", values_to = "cse") %>%
      dplyr::mutate(quantile = dplyr::recode(.data$quantile, q010_cse = "10%", q025_cse = "25%", q050_cse = "50%", q075_cse = "75%", q090_cse = "90%")) %>%
      ggplot2::ggplot(ggplot2::aes(x = quantile, y = cse, group = strip_method_label, color = strip_method_label)) +
      ggplot2::geom_hline(yintercept = 0, linewidth = 0.3, color = "grey45") +
      ggplot2::geom_line(linewidth = 0.8, alpha = 0.85) +
      ggplot2::geom_point(size = 2) +
      ggplot2::facet_wrap(ggplot2::vars(transformation_label)) +
      ggplot2::labs(
        title = "Distributional CSE Profile",
        subtitle = "Quantile curves reveal residual distributional CSE that mean-only summaries can miss",
        x = "RT quantile",
        y = "Quantile CSE (ms)",
        color = "Strip method"
      ) +
      nd_theme()
  } else {
    ggplot2::ggplot() + ggplot2::labs(title = "No quantile CSE columns available") + ggplot2::theme_void()
  }

  verdict_matrix <- d %>%
    dplyr::group_by(strip_method_label, transformation_label, outlier_label, sample_label, nullification_verdict) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::slice_max(.data$n, n = 1, with_ties = FALSE) %>%
    ggplot2::ggplot(ggplot2::aes(x = sample_label, y = outlier_label, fill = nullification_verdict)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.45) +
    ggplot2::facet_grid(rows = ggplot2::vars(strip_method_label), cols = ggplot2::vars(transformation_label), scales = "free_y", space = "free_y") +
    ggplot2::scale_fill_manual(
      values = c(interpretable_nullifier = "#2E7D32", fails_preservation_gates = "#C62828", unpaired = "#6D4C41", diagnostics_missing = "#78909C"),
      name = "Verdict",
      labels = function(x) nd_wrap(x, 24)
    ) +
    ggplot2::labs(
      title = "Nullification Verdict Matrix",
      subtitle = "Use this panel to decide which branches are interpretable for nullification-based FPR",
      x = "Sample fraction",
      y = "Outlier rule"
    ) +
    nd_theme(base_size = 10) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1, vjust = 1))

  list(
    residual_cse = residual,
    preservation_deltas = deltas,
    quantile_profile = quantile_profile,
    verdict_matrix = verdict_matrix
  )
}

write_nullification_reliability_dashboard <- function(diagnostics_csv, output_dir) {
  if (!file.exists(diagnostics_csv)) stop("Diagnostics CSV not found: ", diagnostics_csv)
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  diagnostics <- readr::read_csv(diagnostics_csv, show_col_types = FALSE)
  plots <- plot_nullification_reliability(diagnostics)
  paths <- c(
    residual_cse = file.path(output_dir, "nullification_residual_cse.png"),
    preservation_deltas = file.path(output_dir, "nullification_preservation_deltas.png"),
    quantile_profile = file.path(output_dir, "nullification_quantile_profile.png"),
    verdict_matrix = file.path(output_dir, "nullification_verdict_matrix.png")
  )
  paths[["residual_cse"]] <- nd_save(paths[["residual_cse"]], plots$residual_cse, width = 12, height = 8)
  paths[["preservation_deltas"]] <- nd_save(paths[["preservation_deltas"]], plots$preservation_deltas, width = 14, height = 10)
  paths[["quantile_profile"]] <- nd_save(paths[["quantile_profile"]], plots$quantile_profile, width = 12, height = 7)
  paths[["verdict_matrix"]] <- nd_save(paths[["verdict_matrix"]], plots$verdict_matrix, width = 14, height = 10)
  invisible(paths)
}
