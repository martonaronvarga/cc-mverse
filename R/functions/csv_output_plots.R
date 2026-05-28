# R/functions/csv_output_plots.R
# Plot every analysis CSV with specialized views where possible and a safe fallback.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readr)
})

csvp_wrap <- function(x, width = 24) stringr::str_wrap(as.character(x), width = width)

csvp_theme <- function(base_size = 10) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.box = "vertical",
      panel.grid.minor = ggplot2::element_blank(),
      panel.spacing = grid::unit(1.0, "lines"),
      strip.text = ggplot2::element_text(face = "bold", lineheight = 0.95),
      plot.title = ggplot2::element_text(face = "bold", lineheight = 0.95),
      plot.subtitle = ggplot2::element_text(color = "grey40", lineheight = 0.95),
      plot.margin = ggplot2::margin(10, 14, 10, 10),
      axis.title = ggplot2::element_text(face = "bold")
    )
}

csvp_save <- function(plot, path, width = 11, height = 7) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ext <- tolower(tools::file_ext(path))
  if (identical(ext, "png") && requireNamespace("ragg", quietly = TRUE)) {
    ggplot2::ggsave(path, plot, device = ragg::agg_png, width = width, height = height, dpi = 150, limitsize = FALSE, bg = "white")
  } else if (identical(ext, "svg") && requireNamespace("svglite", quietly = TRUE)) {
    ggplot2::ggsave(path, plot, device = svglite::svglite, width = width, height = height, dpi = 150, limitsize = FALSE, bg = "white")
  } else if (identical(ext, "pdf")) {
    ggplot2::ggsave(path, plot, device = grDevices::pdf, width = width, height = height, dpi = 150, limitsize = FALSE, bg = "white")
  } else if (requireNamespace("ragg", quietly = TRUE)) {
    path <- paste0(tools::file_path_sans_ext(path), ".png")
    ggplot2::ggsave(path, plot, device = ragg::agg_png, width = width, height = height, dpi = 150, limitsize = FALSE, bg = "white")
  } else {
    path <- paste0(tools::file_path_sans_ext(path), ".pdf")
    ggplot2::ggsave(path, plot, device = grDevices::pdf, width = width, height = height, dpi = 150, limitsize = FALSE, bg = "white")
  }
  path
}

csvp_labels <- function(df) {
  out <- df %>%
    dplyr::mutate(
      dplyr::across(dplyr::any_of(c("model_type", "null_type", "outlier", "transformation", "strip_method", "effect_condition", "study", "condition", "model")), as.character),
      model_label = if ("model_type" %in% names(.)) csvp_wrap(.data$model_type, 20) else if ("model" %in% names(.)) csvp_wrap(.data$model, 20) else NA_character_,
      null_label = if ("null_type" %in% names(.)) csvp_wrap(dplyr::coalesce(.data$null_type, "effect present"), 28) else if ("condition" %in% names(.)) csvp_wrap(.data$condition, 22) else NA_character_,
      outlier_label = if ("outlier" %in% names(.)) csvp_wrap(dplyr::coalesce(.data$outlier, "none"), 14) else NA_character_
    )
  if ("transformation" %in% names(out)) {
    out$transformation_label <- dplyr::case_when(
      out$transformation == "log_rt" ~ "log(RT)",
      out$transformation %in% c("no_log_rt", "raw_rt") ~ "Raw RT",
      TRUE ~ csvp_wrap(out$transformation, 16)
    )
  } else {
    out$transformation_label <- NA_character_
  }
  out
}

plot_saturated_tpr_context <- function(df, title = "TPR Saturated: Read FPR And Precision Instead") {
  required <- c("model_type", "transformation", "sample_size", "null_type", "FPR", "TPR")
  if (!all(required %in% names(df))) return(NULL)
  d <- df %>%
    csvp_labels() %>%
    dplyr::mutate(
      tpr_gap = pmax(0, 1 - .data$TPR),
      utility_gap = .data$FPR + .data$tpr_gap,
      sample_label = scales::percent(.data$sample_size, accuracy = 1)
    )

  ggplot2::ggplot(d, ggplot2::aes(x = sample_label, y = FPR, color = model_label, group = interaction(model_label, null_type))) +
    ggplot2::geom_hline(yintercept = 0.05, linetype = "dashed", color = "#C62828") +
    ggplot2::geom_line(linewidth = 0.75, alpha = 0.85) +
    ggplot2::geom_point(ggplot2::aes(size = utility_gap), alpha = 0.9) +
    ggplot2::facet_grid(rows = ggplot2::vars(null_label), cols = ggplot2::vars(transformation_label)) +
    ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0, NA), expand = ggplot2::expansion(mult = c(0.02, 0.12))) +
    ggplot2::scale_size_continuous(name = "FPR + TPR gap", range = c(2, 6)) +
    ggplot2::labs(
      title = title,
      subtitle = "When all TPR values are 100%, the useful distinction is FPR, branch health, and how far each branch is from the nominal error line.",
      x = "Sample fraction",
      y = "False positive rate",
      color = "Model"
    ) +
    csvp_theme()
}

plot_roc_table <- function(df, title) {
  if (!all(c("FPR", "TPR", "model_type", "transformation", "sample_size", "null_type") %in% names(df))) return(NULL)
  if (all(abs(df$TPR - 1) < 1e-12, na.rm = TRUE)) return(plot_saturated_tpr_context(df, paste(title, "- saturated TPR view")))
  d <- df %>% csvp_labels()
  ggplot2::ggplot(d, ggplot2::aes(x = FPR, y = TPR, color = model_label, size = sample_size)) +
    ggplot2::geom_vline(xintercept = 0.05, linetype = "dashed", color = "#C62828") +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "grey60") +
    ggplot2::geom_point(alpha = 0.85) +
    ggplot2::facet_grid(rows = ggplot2::vars(null_label), cols = ggplot2::vars(transformation_label)) +
    ggplot2::scale_x_continuous(labels = scales::percent, limits = c(0, NA)) +
    ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    ggplot2::scale_size_continuous(labels = scales::percent, name = "Sample") +
    ggplot2::labs(title = title, x = "FPR", y = "TPR", color = "Model") +
    csvp_theme()
}

plot_branch_result_map <- function(df, title) {
  required <- c("sample_size", "outlier", "significant", "estimate")
  if (!all(required %in% names(df))) return(NULL)
  d <- df %>%
    csvp_labels() %>%
    dplyr::mutate(
      significance = dplyr::case_when(
        isTRUE(.data$significant) ~ "significant",
        isFALSE(.data$significant) ~ "not significant",
        TRUE ~ "missing"
      ),
      sample_label = scales::percent(.data$sample_size, accuracy = 1),
      estimate_abs = abs(.data$estimate)
    )
  facet_vars <- intersect(c("null_label", "transformation_label"), names(d))
  p <- ggplot2::ggplot(d, ggplot2::aes(x = sample_label, y = outlier_label, fill = significance)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.35) +
    ggplot2::geom_point(ggplot2::aes(size = estimate_abs), shape = 21, color = "grey20", alpha = 0.8) +
    ggplot2::scale_fill_manual(values = c("significant" = "#C62828", "not significant" = "#2E7D32", "missing" = "#78909C"), name = "Result") +
    ggplot2::scale_size_continuous(name = "|estimate|", range = c(1.3, 4.5)) +
    ggplot2::labs(
      title = paste(title, "Branch Decision Map"),
      subtitle = "Tiles show significance; point size shows effect-estimate magnitude.",
      x = "Sample fraction",
      y = "Outlier rule"
    ) +
    csvp_theme(base_size = 9) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1, vjust = 1))
  if (all(c("null_label", "transformation_label") %in% names(d)) && !all(is.na(d$null_label))) {
    p + ggplot2::facet_grid(rows = ggplot2::vars(null_label), cols = ggplot2::vars(transformation_label), scales = "free_y")
  } else if ("transformation_label" %in% names(d)) {
    p + ggplot2::facet_wrap(ggplot2::vars(transformation_label), scales = "free_y")
  } else {
    p
  }
}

plot_spec_curve_table <- function(df, title) {
  required <- c("estimate", "significant", "numerically_usable")
  if (!all(required %in% names(df))) return(NULL)
  d <- df %>%
    csvp_labels() %>%
    dplyr::filter(.data$numerically_usable) %>%
    dplyr::arrange(.data$estimate) %>%
    dplyr::mutate(rank = dplyr::row_number())
  if (nrow(d) == 0) return(NULL)
  ggplot2::ggplot(d, ggplot2::aes(x = rank, y = estimate, color = significant)) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.3, color = "grey35") +
    ggplot2::geom_point(size = 1.2, alpha = 0.75) +
    ggplot2::facet_wrap(ggplot2::vars(transformation_label), scales = "free_y") +
    ggplot2::scale_color_manual(values = c("TRUE" = "#C62828", "FALSE" = "#78909C"), name = "Significant") +
    ggplot2::labs(
      title = paste(title, "Ranked Specification Curve"),
      subtitle = "Ranks preserve the multiverse spread even when rate summaries saturate.",
      x = "Specification rank",
      y = "Interaction estimate"
    ) +
    csvp_theme()
}

plot_dependency_status <- function(df, title) {
  if (!all(c("installed", "package") %in% names(df))) return(NULL)
  d <- df %>% dplyr::mutate(package = csvp_wrap(.data$package, 18), installed = as.logical(.data$installed))
  ggplot2::ggplot(d, ggplot2::aes(x = package, y = 1, fill = installed)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = c("TRUE" = "#2E7D32", "FALSE" = "#C62828"), name = "Installed") +
    ggplot2::labs(title = title, x = NULL, y = NULL) +
    csvp_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank())
}

plot_rate_table <- function(df, title) {
  rate_col <- intersect(c("FPR", "power", "pct_significant", "unconditional_rate", "conditional_rate"), names(df))[1]
  if (is.na(rate_col)) return(NULL)
  d <- df %>% csvp_labels()
  x_col <- dplyr::case_when(
    "sample_size" %in% names(d) ~ "sample_size",
    "outlier" %in% names(d) ~ "outlier_label",
    "model_type" %in% names(d) ~ "model_label",
    TRUE ~ names(d)[[1]]
  )
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[[x_col]], y = .data[[rate_col]], color = model_label, group = model_label)) +
    ggplot2::geom_hline(yintercept = 0.05, linetype = "dashed", color = "#C62828", alpha = 0.55) +
    ggplot2::geom_point(size = 2.4, alpha = 0.85) +
    ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0, NA), expand = ggplot2::expansion(mult = c(0.02, 0.12))) +
    ggplot2::labs(title = title, x = NULL, y = rate_col, color = "Model") +
    csvp_theme()
  if (x_col == "sample_size") p <- p + ggplot2::geom_line(linewidth = 0.75) + ggplot2::scale_x_continuous(labels = scales::percent)
  if ("transformation_label" %in% names(d)) p <- p + ggplot2::facet_wrap(ggplot2::vars(transformation_label))
  p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1, vjust = 1))
}

plot_health_table <- function(df, title) {
  cols <- intersect(c("pct_usable", "pct_error", "pct_singular", "pct_converged"), names(df))
  if (length(cols) == 0) return(NULL)
  d <- df %>%
    csvp_labels() %>%
    tidyr::pivot_longer(dplyr::all_of(cols), names_to = "metric", values_to = "value") %>%
    dplyr::mutate(metric = csvp_wrap(gsub("^pct_", "", .data$metric), 12))
  ggplot2::ggplot(d, ggplot2::aes(x = model_label, y = value / 100, fill = metric)) +
    ggplot2::geom_col(position = "dodge", width = 0.72) +
    ggplot2::facet_wrap(ggplot2::vars(transformation_label)) +
    ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    ggplot2::labs(title = title, x = NULL, y = "Branch proportion", fill = "Metric") +
    csvp_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 25, hjust = 1, vjust = 1))
}

plot_numeric_profile <- function(df, title) {
  numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  numeric_cols <- setdiff(numeric_cols, c("idx", "branch_idx", "subsample_id"))
  if (length(numeric_cols) == 0) return(NULL)
  id_col <- intersect(c("study", "model_type", "model", "condition", "effect_condition", "package", "tool", "key", "column", "confound", "strategy_id"), names(df))[1]
  if (is.na(id_col)) id_col <- names(df)[[1]]
  d <- df %>%
    dplyr::mutate(row_label = csvp_wrap(.data[[id_col]], 28)) %>%
    tidyr::pivot_longer(dplyr::all_of(numeric_cols), names_to = "metric", values_to = "value") %>%
    dplyr::filter(is.finite(.data$value)) %>%
    dplyr::mutate(metric = csvp_wrap(.data$metric, 18))
  if (nrow(d) == 0) return(NULL)
  ggplot2::ggplot(d, ggplot2::aes(x = row_label, y = value)) +
    ggplot2::geom_col(fill = "#546E7A", width = 0.72) +
    ggplot2::facet_wrap(ggplot2::vars(metric), scales = "free_y") +
    ggplot2::labs(title = title, x = NULL, y = "Value") +
    csvp_theme(base_size = 9) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1, vjust = 1))
}

plot_csv_output <- function(csv_path) {
  df <- readr::read_csv(csv_path, show_col_types = FALSE)
  base <- tools::file_path_sans_ext(basename(csv_path))
  title <- gsub("_20[0-9]{6}_[0-9]{6}$", "", base)
  title <- stringr::str_to_title(gsub("_", " ", title))

  plot <- if (grepl("^roc_metrics", base)) {
    plot_roc_table(df, title)
  } else if (grepl("branch_health", base)) {
    plot_health_table(df, title)
  } else if (grepl("spec_curve", base)) {
    plot_spec_curve_table(df, title)
  } else if (grepl("_per_branch|results_with_diag", base)) {
    plot_branch_result_map(df, title)
  } else if (grepl("dependency_report_r_packages", base)) {
    plot_dependency_status(df, title)
  } else if (any(c("FPR", "power", "pct_significant", "unconditional_rate", "conditional_rate") %in% names(df))) {
    plot_rate_table(df, title)
  } else {
    plot_numeric_profile(df, title)
  }

  if (is.null(plot)) {
    plot <- ggplot2::ggplot() +
      ggplot2::theme_void() +
      ggplot2::labs(title = title, subtitle = "No numeric columns available for automatic plotting")
  }
  plot
}

write_csv_plot_index <- function(manifest, output_dir) {
  ok <- manifest %>% dplyr::filter(.data$plotted)
  rel_plot <- function(path) basename(path)
  lines <- c(
    "<!doctype html>",
    "<html><head><meta charset='utf-8'><title>CSV Output Plot Gallery</title>",
    "<style>body{font-family: sans-serif; margin: 24px; color:#263238;} .grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(360px,1fr));gap:20px;} figure{margin:0;border:1px solid #d8dee3;border-radius:12px;padding:12px;background:#fff;} img{width:100%;height:auto;} figcaption{font-size:13px;line-height:1.35;word-break:break-word;} .missing{color:#b71c1c;}</style>",
    "</head><body>",
    paste0("<h1>CSV Output Plot Gallery</h1><p>", sum(manifest$plotted), " / ", nrow(manifest), " CSV files plotted.</p>"),
    "<div class='grid'>"
  )
  for (i in seq_len(nrow(ok))) {
    lines <- c(lines, "<figure>", paste0("<img src='", rel_plot(ok$plot[[i]]), "' alt='plot'>"), paste0("<figcaption>", basename(ok$csv[[i]]), "</figcaption>"), "</figure>")
  }
  lines <- c(lines, "</div>")
  missing <- manifest %>% dplyr::filter(!.data$plotted)
  if (nrow(missing) > 0) {
    lines <- c(lines, "<h2 class='missing'>Unplotted CSVs</h2><ul>", paste0("<li>", missing$csv, "</li>"), "</ul>")
  }
  lines <- c(lines, "</body></html>")
  index_path <- file.path(output_dir, "index.html")
  writeLines(lines, index_path)
  index_path
}

write_csv_output_plots <- function(input_dir = file.path("R", "outputs", "analysis"), output_dir = file.path(input_dir, "figures", "csv_outputs")) {
  csvs <- list.files(input_dir, pattern = "[.]csv$", full.names = TRUE, recursive = TRUE)
  csvs <- csvs[!grepl("/figures/", csvs)]
  if (length(csvs) == 0L) stop("No CSV files found in ", input_dir)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  rows <- lapply(csvs, function(path) {
    rel <- gsub(paste0("^", normalizePath(input_dir, winslash = "/", mustWork = FALSE), "/?"), "", normalizePath(path, winslash = "/", mustWork = FALSE))
    safe <- gsub("[^A-Za-z0-9_.-]+", "_", tools::file_path_sans_ext(rel))
    out <- file.path(output_dir, paste0(safe, ".png"))
    ok <- tryCatch({
      p <- plot_csv_output(path)
      csvp_save(p, out)
      TRUE
    }, error = function(e) {
      message("CSV plot failed for ", path, ": ", conditionMessage(e))
      FALSE
    })
    data.frame(csv = path, plot = if (ok) out else NA_character_, plotted = ok, stringsAsFactors = FALSE)
  })
  manifest <- dplyr::bind_rows(rows)
  index_path <- write_csv_plot_index(manifest, output_dir)
  manifest$index <- index_path
  manifest
}
