# R/functions/analysis_plots.R
# Multiverse Visualization
#
# Consumed tables (from analyze_multiverse_results):
#   fpr_coarse, fpr_by_sample_size, fpr_by_outlier, branch_health,
#   model_diagnostics, results_with_diag,
#   by_outlier, by_sample_size, by_model,
#   or_*  (odds-ratio tables from model_multiverse),
#   coef_* (coefficient tables).
# ROC/TPR helpers remain available but are omitted from the generated dashboard.

# library(ggplot2)
# library(dplyr)
# library(tidyr)
# library(patchwork)

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(patchwork)
})

# ==============================================================================
# SAVE HELPER
# ==============================================================================

#' Save plot with headless-safe devices.
#' @param filepath Base path WITHOUT extension
#' @param plot ggplot object
#' @param width Width in inches
#' @param height Height in inches
#' @param dpi Resolution for raster fallback
plot_save_fallback <- function(filepath, plot, width = 10, height = 7, dpi = 180) {
  filepath <- tools::file_path_sans_ext(filepath)

  devices <- list()
  if (requireNamespace("ragg", quietly = TRUE)) {
    devices$png <- function(filename) ggplot2::ggsave(filename, plot = plot, device = ragg::agg_png, width = width, height = height, dpi = dpi, limitsize = FALSE, bg = "white")
  }
  devices$pdf <- function(filename) ggplot2::ggsave(filename, plot = plot, device = grDevices::pdf, width = width, height = height, dpi = dpi, limitsize = FALSE, bg = "white")
  if (requireNamespace("svglite", quietly = TRUE)) {
    devices$svg <- function(filename) ggplot2::ggsave(filename, plot = plot, device = svglite::svglite, width = width, height = height, dpi = dpi, limitsize = FALSE, bg = "white")
  }

  saved <- character()
  for (ext in names(devices)) {
    ok <- tryCatch(
      {
        devices[[ext]](paste0(filepath, ".", ext))
        TRUE
      },
      error = function(e) FALSE
    )
    if (ok) {
      saved <- c(saved, paste0(filepath, ".", ext))
      plot_log(logger::INFO, "Saved plot: {filepath}.{ext}")
      if (ext == "png") next
      return(invisible(saved))
    }
  }
  if (length(saved) > 0) {
    return(invisible(saved))
  }
  plot_log(logger::WARN, "Could not save plot in any headless-safe format: {filepath}")
  invisible(filepath)
}

# ==============================================================================
# COLOR PALETTES — must match derive_model() output exactly
# ==============================================================================

get_model_palette <- function() {
  c(
    "rmANOVA"                          = "#CC6677",
    "LMM (random intercept)"           = "#332288",
    "LMM (random congruency slope)"    = "#88CCEE",
    "LMM (full)"                       = "#117733"
  )
}

get_effect_palette <- function() {
  c(
    "present"          = "#2E7D32",
    "null_interaction" = "#C62828"
  )
}

get_null_type_palette <- function() {
  c(
    "null_interaction:shuffle" = "#1976D2",
    "null_interaction:additive_qmap" = "#F57C00",
    "null_interaction:additive_qmap_trial_bin" = "#00897B",
    "null_interaction:local_mean_residual" = "#8D6E63",
    "null_interaction:local_median_residual" = "#546E7A"
  )
}

get_transformation_labels <- function() {
  c("log_rt" = "log(RT)", "no_log_rt" = "Raw RT")
}

# ==============================================================================
# THEME
# ==============================================================================

wrap_label <- function(x, width = 22) {
  stringr::str_wrap(as.character(x), width = width)
}

wrap_discrete_labels <- function(width = 22) {
  function(x) wrap_label(x, width = width)
}

plot_log <- function(level, message) {
  formatted_msg <- tryCatch(
    as.character(glue::glue(message, .envir = parent.frame())),
    error = function(glue_err) {
      paste0(
        message,
        " [glue error: ",
        glue_err$message,
        "]"
      )
    }
  )
  if (requireNamespace("logger", quietly = TRUE)) {
    logger::log_level(level, logger::skip_formatter(formatted_msg))
  } else {
    message(formatted_msg)
  }

  invisible(NULL)
}

primary_null_type <- function() "null_interaction:local_mean_residual"

primary_reporting_models <- function() {
  c("rmANOVA", "LMM (random intercept)", "LMM (random congruency slope)")
}

primary_reporting_null_types <- function() {
  c(
    "null_interaction:local_mean_residual",
    "null_interaction:local_median_residual",
    "null_interaction:additive_qmap",
    "null_interaction:additive_qmap_trial_bin"
  )
}

filter_primary_fpr_rows <- function(
    df,
    null_types = primary_reporting_null_types(),
    models = primary_reporting_models(),
    require_interpretable = TRUE) {
  if (is.null(df) || !is.data.frame(df)) {
    return(df)
  }

  out <- df
  if ("model_type" %in% names(out)) {
    out <- dplyr::filter(out, .data$model_type %in% models)
  }
  if ("null_type" %in% names(out)) {
    out <- dplyr::filter(out, .data$null_type %in% null_types)
  }
  if (isTRUE(require_interpretable) && "interpretable_fpr_source" %in% names(out)) {
    out <- dplyr::filter(out, .data$interpretable_fpr_source %in% TRUE)
  }
  out
}

filter_stable_rate_rows <- function(
    df,
    min_n = 25,
    n_col = "n",
    drop_full_sample = TRUE) {
  if (is.null(df) || !is.data.frame(df)) {
    return(df)
  }

  out <- df

  if (isTRUE(drop_full_sample) && "sample_size" %in% names(out)) {
    out <- dplyr::filter(out, is.na(.data$sample_size) | .data$sample_size < 1)
  }

  if (!is.null(min_n) && min_n > 1 && n_col %in% names(out)) {
    out <- dplyr::filter(out, is.na(.data[[n_col]]) | .data[[n_col]] >= min_n)
  }

  out
}

stable_rate_note <- function(min_n, drop_full_sample = TRUE) {
  paste0(
    if (!is.null(min_n) && min_n > 1) {
      paste0("; cells with n < ", min_n, " omitted")
    } else {
      ""
    },
    if (isTRUE(drop_full_sample)) {
      "; full-sample cells omitted"
    } else {
      ""
    }
  )
}

pretty_model <- function(x) {
  dplyr::case_when(
    x == "rmANOVA" ~ "ANOVA",
    x == "LMM (random intercept)" ~ "RI LMM",
    x == "LMM (random congruency slope)" ~ "Cong-slope LMM",
    x == "LMM (full)" ~ "Full LMM",
    TRUE ~ wrap_label(x, 20)
  )
}

pretty_null_type <- function(x) {
  dplyr::case_when(
    x == "null_interaction:local_mean_residual" ~ "Mean residual",
    x == "null_interaction:local_median_residual" ~ "Median residual",
    x == "null_interaction:additive_qmap" ~ "Additive Q-map",
    x == "null_interaction:additive_qmap_trial_bin" ~ "Trial-bin Q-map",
    x == "null_interaction:shuffle" ~ "Shuffle",
    is.na(x) | x == "present" ~ "Present",
    TRUE ~ wrap_label(x, 24)
  )
}

pretty_strip_method <- function(x) {
  dplyr::case_when(
    x == "local_mean_residual" ~ "Mean residual",
    x == "local_median_residual" ~ "Median residual",
    x == "additive_qmap" ~ "Additive Q-map",
    x == "additive_qmap_trial_bin" ~ "Trial-bin Q-map",
    x == "shuffle" ~ "Shuffle",
    is.na(x) ~ "Present",
    TRUE ~ wrap_label(x, 24)
  )
}

pretty_outlier <- function(x) {
  dplyr::case_when(
    x == "none" ~ "None",
    grepl("^sd_", x) ~ paste0("SD ", sub("^sd_", "", x)),
    grepl("^mad_", x) ~ paste0("MAD ", sub("^mad_", "", x)),
    grepl("^range_", x) ~ paste0("Range ", sub("^range_", "", x)),
    is.na(x) ~ "None",
    TRUE ~ wrap_label(x, 16)
  )
}

prepare_plot_labels <- function(df) {
  if (is.null(df) || !is.data.frame(df)) {
    return(df)
  }
  df %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(c("model_type", "null_type", "outlier", "transformation", "strip_method")),
        as.character
      ),
      null_type_label = if ("null_type" %in% names(.)) pretty_null_type(.data$null_type) else NA_character_,
      model_type_label = if ("model_type" %in% names(.)) pretty_model(.data$model_type) else NA_character_,
      transformation_label = dplyr::case_when(
        "transformation" %in% names(.) & .data$transformation == "log_rt" ~ "log(RT)",
        "transformation" %in% names(.) & .data$transformation %in% c("no_log_rt", "raw_rt") ~ "Raw RT",
        "transformation" %in% names(.) ~ wrap_label(.data$transformation, 18),
        TRUE ~ NA_character_
      ),
      outlier_label = if ("outlier" %in% names(.)) pretty_outlier(dplyr::coalesce(.data$outlier, "none")) else NA_character_,
      strip_method_label = if ("strip_method" %in% names(.)) pretty_strip_method(.data$strip_method) else NA_character_
    )
}

theme_multiverse <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.direction = "horizontal",
      legend.title = element_text(face = "bold", size = base_size - 1),
      legend.text = element_text(size = base_size - 2),
      panel.grid.minor = element_blank(),
      panel.spacing = grid::unit(1.1, "lines"),
      strip.text = element_text(face = "bold", lineheight = 0.95, margin = margin(4, 4, 4, 4)),
      plot.title = element_text(face = "bold", size = base_size + 2, lineheight = 0.95, margin = margin(b = 5)),
      plot.subtitle = element_text(color = "grey40", lineheight = 0.95, margin = margin(b = 8)),
      plot.caption = element_text(color = "grey45", size = base_size - 2, hjust = 0),
      plot.margin = margin(10, 14, 10, 10),
      axis.title = element_text(face = "bold"),
      axis.text.x = element_text(margin = margin(t = 3)),
      axis.text.y = element_text(margin = margin(r = 3))
    )
}

legend_bottom_rows <- function(nrow = 1) {
  list(
    guides(
      colour = guide_legend(nrow = nrow, byrow = TRUE),
      color = guide_legend(nrow = nrow, byrow = TRUE),
      fill = guide_legend(nrow = nrow, byrow = TRUE),
      shape = guide_legend(nrow = nrow, byrow = TRUE),
      size = guide_legend(nrow = nrow, byrow = TRUE),
      alpha = guide_legend(nrow = nrow, byrow = TRUE),
      linetype = guide_legend(nrow = nrow, byrow = TRUE)
    ),
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.direction = "horizontal",
      legend.box.just = "center"
    )
  )
}

# ==============================================================================
# PLOT 1: ROC CURVES BY MODEL
# ==============================================================================

#' ROC curves — TPR vs FPR, lines per model, points sized by sample_size
#' Consumes: roc_metrics (from compute_roc_metrics)
#' Required columns: FPR, TPR, model_type, sample_size, null_type, transformation
plot_roc_by_model <- function(roc_metrics, palette = get_model_palette()) {
  df <- roc_metrics %>%
    dplyr::filter(!is.na(TPR), !is.na(FPR)) %>%
    prepare_plot_labels()

  ggplot(df, aes(x = FPR, y = TPR, color = model_type, group = model_type)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "grey50") +
    geom_vline(xintercept = 0.05, linetype = "dashed", color = "red", alpha = 0.5) +
    geom_path(linewidth = 0.8) +
    geom_point(aes(size = sample_size), alpha = 0.8) +
    geom_text(
      aes(label = scales::percent(sample_size, accuracy = 1)),
      size = 2.5, vjust = -0.7,
      show.legend = FALSE, check_overlap = TRUE
    ) +
    scale_color_manual(values = palette, name = "Model", drop = FALSE) +
    scale_size_continuous(range = c(2, 5), guide = "none") +
    scale_x_continuous(
      limits = c(0, 0.25),
      breaks = seq(0, 0.25, 0.05),
      labels = scales::percent
    ) +
    scale_y_continuous(
      limits = c(0, 1), breaks = seq(0, 1, 0.2), labels = scales::percent
    ) +
    facet_grid(
      rows = vars(null_type_label),
      cols = vars(transformation_label)
    ) +
    labs(
      title = "ROC Curves by Model Type",
      subtitle = "Points sized by sample fraction; dashed line = nominal α = 0.05",
      x = "False Positive Rate",
      y = "True Positive Rate"
    ) +
    theme_multiverse() +
    legend_bottom_rows(1)
}

# ==============================================================================
# PLOT 2: ROC CURVES BY OUTLIER METHOD
# ==============================================================================

#' ROC curves colored by outlier method, shaped by model
#' Consumes: roc_metrics
plot_roc_by_outlier <- function(roc_metrics) {
  df <- roc_metrics %>%
    dplyr::filter(!is.na(TPR), !is.na(FPR)) %>%
    prepare_plot_labels()

  # Too many outlier levels for a clean legend — group into families
  df <- df %>%
    dplyr::mutate(
      outlier_family = dplyr::case_when(
        grepl("^sd_", outlier) ~ "SD-based",
        grepl("^mad_", outlier) ~ "MAD-based",
        grepl("^range_", outlier) ~ "Range-based",
        TRUE ~ "None"
      )
    )

  ggplot(df, aes(x = FPR, y = TPR, color = outlier_family)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "grey50") +
    geom_vline(xintercept = 0.05, linetype = "dashed", color = "red", alpha = 0.5) +
    geom_point(aes(shape = model_type), alpha = 0.7, size = 2.5) +
    scale_color_brewer(palette = "Set1", name = "Outlier Family") +
    scale_shape_manual(
      values = c(16, 17, 15, 3),
      name = "Model"
    ) +
    scale_x_continuous(
      limits = c(0, 0.25), breaks = seq(0, 0.25, 0.05), labels = scales::percent
    ) +
    scale_y_continuous(
      limits = c(0, 1), breaks = seq(0, 1, 0.2), labels = scales::percent
    ) +
    facet_grid(
      rows = vars(null_type_label),
      cols = vars(transformation_label)
    ) +
    labs(
      title = "ROC Curves by Outlier Strategy",
      subtitle = "Shape = model type; dashed line = nominal α = 0.05",
      x = "False Positive Rate",
      y = "True Positive Rate"
    ) +
    theme_multiverse() +
    legend_bottom_rows(1)
}

# ==============================================================================
# PLOT 3: FPR BY NULL TYPE (replaces broken strip-method plot)
# ==============================================================================

#' FPR comparison across null types and models
#' Consumes: fpr_coarse
#' Columns: null_type, model_type, transformation, n, false_positives, FPR
plot_fpr_by_null_type <- function(
    fpr_coarse,
    palette = get_null_type_palette(),
    null_types = primary_reporting_null_types(),
    models = primary_reporting_models(),
    require_interpretable = TRUE) {
  df <- fpr_coarse %>%
    filter_primary_fpr_rows(
      null_types = null_types,
      models = models,
      require_interpretable = require_interpretable
    ) %>%
    prepare_plot_labels()
  ggplot(df, aes(x = FPR, y = model_type_label, color = null_type_label)) +
    geom_vline(xintercept = 0.05, linetype = "dashed", color = "#C62828", alpha = 0.7) +
    geom_point(size = 2.8, alpha = 0.9) +
    geom_text(aes(label = scales::percent(FPR, accuracy = 0.1)), hjust = -0.15, size = 2.4, color = "grey20") +
    scale_color_brewer(palette = "Set2", name = "Nullifier") +
    scale_x_continuous(labels = scales::percent, limits = c(0, NA), expand = expansion(mult = c(0.02, 0.22))) +
    facet_grid(rows = vars(null_type_label), cols = vars(transformation_label), scales = "free", space = "free") +
    labs(
      title = "FPR by Model and Nullifier",
      subtitle = "Dashed line = nominal 5%; primary claim uses diagnostics-passing mean residual rows",
      x = "False positive rate",
      y = NULL
    ) +
    theme_multiverse(base_size = 10) +
    theme(
      legend.position = "none",
      panel.spacing = grid::unit(1.8, "lines")
    )
}

# ==============================================================================
# PLOT 4: FPR BY SAMPLE SIZE (new — exploits fpr_by_sample_size table)
# ==============================================================================

#' FPR × sample size curves per model, faceted by null_type and transformation
#' Consumes: fpr_by_sample_size
plot_fpr_by_sample_size <- function(
    fpr_by_ss,
    palette = get_model_palette(),
    null_types = primary_reporting_null_types(),
    models = primary_reporting_models(),
    require_interpretable = TRUE,
    drop_full_sample = TRUE,
    min_n = 25) {
  df <- fpr_by_ss %>%
    filter_primary_fpr_rows(
      null_types = null_types,
      models = models,
      require_interpretable = require_interpretable
    ) %>%
    filter_stable_rate_rows(min_n = min_n, drop_full_sample = drop_full_sample) %>%
    prepare_plot_labels() %>%
    arrange(.data$null_type, .data$transformation, .data$model_type, .data$sample_size)
  subtitle <- if (isTRUE(require_interpretable)) {
    paste0("Diagnostics-passing nullified rows only; dashed line = 5%", stable_rate_note(min_n, drop_full_sample))
  } else if ("null_interaction:shuffle" %in% df$null_type) {
    paste0("Diagnostic/sensitivity rows; shuffle shown separately; dashed line = 5%", stable_rate_note(min_n, drop_full_sample))
  } else {
    paste0("Diagnostic/sensitivity rows; dashed line = 5%", stable_rate_note(min_n, drop_full_sample))
  }

  ggplot(df, aes(
    x = sample_size, y = FPR, color = model_type, group = interaction(model_type, null_type, transformation, interpretable_fpr_source, drop = TRUE)
  )) +
    geom_hline(yintercept = 0.05, linetype = "dashed", color = "#C62828", alpha = 0.7) +
    geom_line(linewidth = 0.75, alpha = 0.9) +
    geom_point(size = 2.1) +
    scale_color_manual(values = palette, name = "Model", drop = FALSE, labels = pretty_model) +
    scale_x_continuous(labels = scales::percent, breaks = sort(unique(df$sample_size))) +
    scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0.02, 0.15))) +
    facet_grid(rows = vars(null_type_label), cols = vars(transformation_label), scales = "free") +
    labs(
      title = "FPR Across Sample Sizes",
      subtitle = subtitle,
      x = "Sample fraction",
      y = "FPR"
    ) +
    theme_multiverse(base_size = 10) +
    legend_bottom_rows(1) +
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),
      panel.spacing = grid::unit(1.8, "lines")
    )
}

# ==============================================================================
# PLOT 5: POWER BY SAMPLE SIZE
# ==============================================================================

#' Power curves (TPR when effect present) by sample size and model
#' Consumes: power_by_sample_size
plot_power_by_sample_size <- function(power_by_ss, palette = get_model_palette()) {
  df <- prepare_plot_labels(power_by_ss)
  ggplot(df, aes(
    x = sample_size, y = power, color = model_type, group = model_type
  )) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 3) +
    scale_color_manual(values = palette, name = "Model", drop = FALSE) +
    scale_x_continuous(labels = scales::percent, breaks = seq(0.1, 1, 0.1)) +
    scale_y_continuous(
      limits = c(0, 1), labels = scales::percent, breaks = seq(0, 1, 0.2)
    ) +
    facet_wrap(vars(transformation_label), scales = "free") +
    labs(
      title = "Statistical Power by Sample Size",
      subtitle = "True positive rate when interaction effect is present",
      x = "Sample Fraction",
      y = "Power (TPR)"
    ) +
    theme_multiverse() +
    legend_bottom_rows(1)
}

# ==============================================================================
# PLOT 6: SPECIFICATION CURVE (fixed alignment)
# ==============================================================================

#' Specification curve: effect estimates ranked, with specification indicators
#' Consumes: spec_curve (from specification_curve_data)
plot_specification_curve <- function(spec_curve_df, alpha = 0.05) {
  # Filter to usable branches only (avoids NAs in estimate)
  df <- spec_curve_df %>%
    dplyr::filter(numerically_usable) %>%
    dplyr::arrange(estimate) %>%
    dplyr::mutate(spec_rank = dplyr::row_number())

  # Top panel: effect estimates colored by significance
  p_top <- ggplot(df, aes(x = spec_rank, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    geom_point(
      aes(color = significant),
      size = 0.6, alpha = 0.7
    ) +
    scale_color_manual(
      values = c("TRUE" = "#2E7D32", "FALSE" = "#BDBDBD"),
      labels = c("TRUE" = paste0("p < ", alpha), "FALSE" = paste0("p ≥ ", alpha)),
      name = NULL, na.value = "grey80"
    ) +
    labs(y = "Effect Estimate", x = NULL) +
    theme_multiverse(base_size = 10) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "top",
      panel.grid.major.x = element_blank()
    )

  # Bottom panel: specification indicators (all usable rows, aligned)
  spec_long <- df %>%
    dplyr::select(
      spec_rank, model_type, transformation, sample_size, outlier,
      effect_condition
    ) %>%
    dplyr::mutate(sample_size = as.character(sample_size)) %>%
    tidyr::pivot_longer(
      cols = -spec_rank,
      names_to = "dimension",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      dimension = factor(dimension, levels = c(
        "effect_condition", "model_type", "transformation",
        "sample_size", "outlier"
      ))
    )

  p_bottom <- ggplot(spec_long, aes(x = spec_rank, y = dimension, fill = value)) +
    geom_tile(height = 0.8) +
    scale_fill_viridis_d(option = "turbo", guide = "none") +
    labs(x = "Specification (ranked by effect size)", y = NULL) +
    theme_multiverse(base_size = 10) +
    theme(panel.grid = element_blank(), axis.text.y = element_text(hjust = 1))

  p_top / p_bottom + plot_layout(heights = c(3, 2))
}

# ==============================================================================
# PLOT 7: EFFECT SIZE DISTRIBUTIONS
# ==============================================================================

#' Density plots of effect estimates by condition, faceted by model × transform
#' Consumes: results_with_diag (prepared_df with numerically_usable)
plot_effect_distributions <- function(prepared_df, palette = get_effect_palette()) {
  df <- prepared_df %>%
    dplyr::filter(numerically_usable)

  ggplot(df, aes(x = main_estimate, fill = effect_condition)) +
    geom_density(alpha = 0.5, color = NA) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
    scale_fill_manual(
      values = palette,
      labels = c(
        "present" = "Effect present",
        "null_interaction" = "Null (interaction stripped)"
      ),
      name = "Condition"
    ) +
    facet_grid(
      rows = vars(model_type),
      cols = vars(transformation),
      labeller = labeller(transformation = get_transformation_labels()),
      scales = "free"
    ) +
    labs(
      title = "Effect Size Distributions",
      subtitle = "Separation indicates discriminability between true and null effects",
      x = "Interaction Effect Estimate",
      y = "Density"
    ) +
    theme_multiverse()
}

# ==============================================================================
# PLOT 8: P-VALUE DISTRIBUTIONS
# ==============================================================================

#' P-value histograms by condition and model
#' Under the null, p-values should be uniform.
#' Under the alternative, they should be left-skewed.
#' Consumes: results_with_diag
plot_pvalue_distributions <- function(prepared_df, palette = get_effect_palette()) {
  df <- prepared_df %>%
    dplyr::filter(numerically_usable) %>%
    dplyr::mutate(null_type_label = pretty_null_type(null_type))

  ggplot(df, aes(x = main_p_value, fill = effect_condition)) +
    geom_histogram(bins = 20, alpha = 0.7, position = "identity", color = "white", linewidth = 0.1) +
    geom_vline(xintercept = 0.05, linetype = "dashed", color = "red") +
    scale_fill_manual(values = palette, name = "Condition") +
    facet_grid(
      rows = vars(null_type_label),
      cols = vars(model_type),
      scales = "free"
    ) +
    labs(
      title = "P-value Distributions",
      subtitle = "Null conditions should be uniform; present should be left-skewed",
      x = "p-value",
      y = "Count"
    ) +
    theme_multiverse(base_size = 10) +
    theme(strip.text = element_text(size = 8))
}

# ==============================================================================
# PLOT 9: STRIPPING ROBUSTNESS — shuffle vs qmap FPR
# ==============================================================================

#' Compare FPR between stripping methods for null_interaction branches
#' Consumes: fpr_by_sample_size (has null_type, model_type, transformation, sample_size)
plot_stripping_robustness <- function(
    fpr_by_ss,
    null_types = primary_reporting_null_types(),
    models = primary_reporting_models(),
    require_interpretable = TRUE) {
  # Extract strip method from null_type for null_interaction only after primary FPR filtering.
  df <- fpr_by_ss %>%
    filter_primary_fpr_rows(
      null_types = null_types,
      models = models,
      require_interpretable = require_interpretable
    ) %>%
    dplyr::filter(grepl("^null_interaction:", null_type)) %>%
    dplyr::mutate(
      strip_method = sub("^null_interaction:", "", null_type),
      strip_method_label = pretty_strip_method(strip_method),
      transformation_label = dplyr::case_when(
        transformation == "log_rt" ~ "log(RT)",
        transformation %in% c("no_log_rt", "raw_rt") ~ "Raw RT",
        TRUE ~ wrap_label(transformation, 18)
      )
    )

  # Aggregate after dropping metadata columns so pivot_wider has one row per cell.
  wide <- df %>%
    dplyr::group_by(model_type, sample_size, transformation, strip_method) %>%
    dplyr::summarise(
      FPR = stats::weighted.mean(FPR, w = pmax(n, 0), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(FPR = dplyr::if_else(is.nan(FPR), NA_real_, FPR)) %>%
    tidyr::pivot_wider(names_from = strip_method, values_from = FPR)

  # If both methods present, scatter them
  if (all(c("shuffle", "additive_qmap") %in% names(wide))) {
    p <- ggplot(wide, aes(x = additive_qmap, y = shuffle)) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
      geom_point(
        aes(color = model_type, size = sample_size),
        alpha = 0.7
      ) +
      geom_vline(xintercept = 0.05, linetype = "dotted", color = "red", alpha = 0.5) +
      geom_hline(yintercept = 0.05, linetype = "dotted", color = "red", alpha = 0.5) +
      scale_color_manual(values = get_model_palette(), name = "Model", drop = FALSE) +
      scale_size_continuous(range = c(2, 5), name = "Sample Fraction") +
      scale_x_continuous(labels = scales::percent) +
      scale_y_continuous(labels = scales::percent) +
      coord_equal() +
      facet_wrap(
        vars(transformation),
        labeller = labeller(transformation = get_transformation_labels())
      ) +
      labs(
        title = "Nullifier Pair Check",
        subtitle = "Points on diagonal mean the two nullifiers have similar FPR",
        x = "FPR (Additive quantile map)",
        y = "FPR (Within-cell shuffle)"
      ) +
      theme_multiverse()
  } else {
    # Fallback: compact bar chart for any available nullifiers.
    p <- ggplot(df, aes(x = strip_method_label, y = FPR, fill = model_type)) +
      geom_col(position = "dodge", width = 0.72) +
      geom_hline(yintercept = 0.05, linetype = "dashed", color = "#C62828") +
      scale_fill_manual(values = get_model_palette(), name = "Model", drop = FALSE) +
      scale_y_continuous(labels = scales::percent) +
      facet_wrap(vars(transformation_label), scales = "free") +
      labs(title = "FPR by Nullifier", x = "Nullifier", y = "FPR") +
      theme_multiverse(base_size = 10) +
      theme(axis.text.x = element_text(angle = 25, hjust = 1, vjust = 1))
  }

  p
}

# ==============================================================================
# FPR DIAGNOSTICS: OUTLIER, TRANSFORM, AND COMBINATION VIEWS
# ==============================================================================

plot_fpr_outlier_heatmap <- function(
    fpr_by_outlier,
    null_types = primary_reporting_null_types(),
    models = primary_reporting_models(),
    require_interpretable = TRUE,
    drop_full_sample = TRUE,
    min_n = 25) {
  df <- fpr_by_outlier %>%
    filter_primary_fpr_rows(
      null_types = null_types,
      models = models,
      require_interpretable = require_interpretable
    ) %>%
    filter_stable_rate_rows(min_n = min_n, drop_full_sample = drop_full_sample) %>%
    prepare_plot_labels()
  df <- df %>%
    dplyr::group_by(null_type_label, model_type_label, transformation_label, outlier_label) %>%
    dplyr::summarise(
      n = sum(n, na.rm = TRUE),
      false_positives = sum(false_positives, na.rm = TRUE),
      FPR = dplyr::if_else(n > 0, false_positives / n, NA_real_),
      .groups = "drop"
    )

  ggplot(df, aes(x = model_type_label, y = outlier_label, fill = FPR)) +
    geom_tile(color = "white", linewidth = 0.45) +
    geom_text(aes(label = scales::percent(FPR, accuracy = 1)), size = 2.3, color = "grey12") +
    scale_fill_gradient2(
      low = "#2E7D32",
      mid = "#FFF8E1",
      high = "#C62828",
      midpoint = 0.05,
      labels = scales::percent,
      name = "FPR",
      guide = ggplot2::guide_colorbar(
        direction = "horizontal",
        title.position = "top",
        barwidth = grid::unit(8, "lines"),
        barheight = grid::unit(0.6, "lines")
      )
    ) +
    facet_grid(rows = vars(null_type_label), cols = vars(transformation_label), scales = "free", space = "free") +
    labs(
      title = "FPR by Outlier Rule, Model, and Nullifier",
      subtitle = paste0("Cells pool across sample sizes after dropping sparse cells; green is below nominal alpha, red is inflated", stable_rate_note(min_n)),
      x = "Model",
      y = "Outlier rule"
    ) +
    theme_multiverse(base_size = 9) +
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),
      panel.spacing = grid::unit(1.0, "lines")
    )
}

plot_fpr_transform_delta <- function(
    fpr_by_ss,
    null_types = primary_reporting_null_types(),
    models = primary_reporting_models(),
    require_interpretable = TRUE) {
  df <- fpr_by_ss %>%
    filter_primary_fpr_rows(
      null_types = null_types,
      models = models,
      require_interpretable = require_interpretable
    ) %>%
    dplyr::group_by(null_type, model_type, sample_size, transformation) %>%
    dplyr::summarise(
      n = sum(n, na.rm = TRUE),
      false_positives = sum(false_positives, na.rm = TRUE),
      FPR = dplyr::if_else(n > 0, false_positives / n, NA_real_),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(names_from = transformation, values_from = c(n, false_positives, FPR)) %>%
    dplyr::filter(!is.na(FPR_log_rt), !is.na(FPR_no_log_rt)) %>%
    dplyr::mutate(
      delta_raw_minus_log = FPR_no_log_rt - FPR_log_rt,
      null_type_label = pretty_null_type(null_type),
      model_type_label = pretty_model(model_type)
    )

  ggplot(df, aes(x = sample_size, y = delta_raw_minus_log, color = model_type)) +
    geom_hline(yintercept = 0, linewidth = 0.35, color = "grey35") +
    geom_line(aes(group = model_type), linewidth = 0.75) +
    geom_point(size = 2.1) +
    scale_color_manual(values = get_model_palette(), name = "Model", drop = FALSE) +
    scale_x_continuous(labels = scales::percent, breaks = sort(unique(df$sample_size))) +
    scale_y_continuous(labels = scales::percent) +
    facet_wrap(vars(null_type_label), scales = "free") +
    labs(
      title = "Transformation Sensitivity of FPR",
      subtitle = "Positive values mean Raw RT has higher FPR than log(RT)",
      x = "Sample fraction",
      y = "Raw RT FPR - log(RT) FPR"
    ) +
    theme_multiverse(base_size = 10) +
    legend_bottom_rows(1) +
    theme(axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1))
}

plot_fpr_extreme_combinations <- function(
    fpr_by_outlier,
    top_n = 18,
    null_types = primary_reporting_null_types(),
    models = primary_reporting_models(),
    require_interpretable = TRUE,
    min_n = 25,
    include_sample = TRUE,
    drop_full_sample = TRUE) {
  df <- fpr_by_outlier %>%
    filter_primary_fpr_rows(
      null_types = null_types,
      models = models,
      require_interpretable = require_interpretable
    ) %>%
    filter_stable_rate_rows(min_n = min_n, drop_full_sample = drop_full_sample) %>%
    prepare_plot_labels() %>%
    dplyr::filter(is.finite(FPR), n > 0)

  group_cols <- c("null_type_label", "model_type_label", "transformation_label", "outlier_label")
  if (isTRUE(include_sample)) group_cols <- c(group_cols, "sample_size")

  df <- df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarise(
      n = sum(.data$n, na.rm = TRUE),
      false_positives = sum(.data$false_positives, na.rm = TRUE),
      FPR = dplyr::if_else(.data$n > 0, .data$false_positives / .data$n, NA_real_),
      .groups = "drop"
    ) %>%
    dplyr::filter(is.finite(.data$FPR), .data$n >= min_n) %>%
    dplyr::mutate(
      excess = .data$FPR - 0.05,
      sample_label = if ("sample_size" %in% names(.)) scales::percent(.data$sample_size, accuracy = 1) else NULL,
      combo = if (isTRUE(include_sample)) {
        paste(.data$model_type_label, .data$transformation_label, .data$outlier_label, .data$sample_label, sep = " | ")
      } else {
        paste(.data$model_type_label, .data$transformation_label, .data$outlier_label, sep = " | ")
      }
    ) %>%
    dplyr::arrange(dplyr::desc(.data$excess), dplyr::desc(.data$n)) %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::mutate(combo = forcats::fct_reorder(.data$combo, .data$FPR))

  ggplot(df, aes(x = FPR, y = combo, color = null_type_label, size = n)) +
    geom_vline(xintercept = 0.05, linetype = "dashed", color = "#C62828") +
    geom_point(alpha = 0.82) +
    scale_x_continuous(labels = scales::percent, expand = expansion(mult = c(0.02, 0.12))) +
    scale_size_continuous(range = c(1.8, 5.0), name = "Null branches") +
    labs(
      title = "Most Inflated FPR Combinations",
      subtitle = paste0("Top stable combinations by FPR above nominal alpha; label = model | transform | outlier", if (isTRUE(include_sample)) " | sample" else "", stable_rate_note(min_n, drop_full_sample = drop_full_sample)),
      x = "FPR",
      y = NULL,
      color = "Nullifier"
    ) +
    theme_multiverse(base_size = 9) +
    legend_bottom_rows(1)
}

plot_fpr_exceedance_summary <- function(
    fpr_by_outlier,
    null_types = primary_reporting_null_types(),
    models = primary_reporting_models(),
    require_interpretable = TRUE,
    min_n = 25,
    drop_full_sample = TRUE) {
  base <- fpr_by_outlier %>%
    filter_primary_fpr_rows(
      null_types = null_types,
      models = models,
      require_interpretable = require_interpretable
    ) %>%
    filter_stable_rate_rows(min_n = min_n, drop_full_sample = drop_full_sample) %>%
    prepare_plot_labels()
  factor_tables <- list(
    Nullifier = base %>% dplyr::transmute(factor = null_type_label, n, false_positives),
    Model = base %>% dplyr::transmute(factor = model_type_label, n, false_positives),
    Transform = base %>% dplyr::transmute(factor = transformation_label, n, false_positives),
    Outlier = base %>% dplyr::transmute(factor = outlier_label, n, false_positives),
    Sample = base %>% dplyr::transmute(factor = scales::percent(sample_size, accuracy = 1), n, false_positives)
  )

  df <- purrr::imap_dfr(factor_tables, function(tbl, axis) {
    tbl %>%
      dplyr::group_by(factor) %>%
      dplyr::summarise(
        n = sum(n, na.rm = TRUE),
        false_positives = sum(false_positives, na.rm = TRUE),
        FPR = dplyr::if_else(n > 0, false_positives / n, NA_real_),
        .groups = "drop"
      ) %>%
      dplyr::mutate(axis = axis)
  }) %>%
    dplyr::filter(is.finite(FPR)) %>%
    dplyr::group_by(axis) %>%
    dplyr::mutate(factor = forcats::fct_reorder(factor, FPR)) %>%
    dplyr::ungroup()

  ggplot(df, aes(x = FPR, y = factor, fill = FPR)) +
    geom_vline(xintercept = 0.05, linetype = "dashed", color = "#C62828") +
    geom_col(width = 0.72) +
    geom_text(aes(label = scales::percent(FPR, accuracy = 0.1)), hjust = -0.12, size = 2.7) +
    scale_x_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.22))) +
    scale_fill_gradient2(
      low = "#2E7D32", mid = "#FFF8E1", high = "#C62828",
      midpoint = 0.05, labels = scales::percent, guide = "none"
    ) +
    facet_wrap(vars(axis), scales = "free", ncol = 2) +
    labs(
      title = "Where FPR Inflation Comes From",
      subtitle = paste0("Each panel marginalizes over all other axes after dropping sparse cells; red dashed line = nominal alpha = 0.05", stable_rate_note(min_n, drop_full_sample = drop_full_sample)),
      x = "Marginal FPR",
      y = NULL
    ) +
    theme_multiverse(base_size = 10)
}

# ==============================================================================
# PLOT 10: SENSITIVITY HEATMAP (fixed to use correct data)
# ==============================================================================

#' Power heatmap: model × outlier, faceted by sample_size × transformation
#' Consumes: power_by_outlier (from compute_power_tables$by_outlier)
#' Columns: model_type, transformation, sample_size, outlier, significant, p_value, estimate
plot_sensitivity_heatmap <- function(power_by_outlier) {
  # Aggregate per (model, transformation, sample_size, outlier)
  heat_df <- power_by_outlier %>%
    prepare_plot_labels() %>%
    dplyr::group_by(model_type_label, transformation_label, sample_size, outlier_label) %>%
    dplyr::summarise(
      n = dplyr::n(),
      power = mean(significant, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      ss_label = scales::percent(sample_size, accuracy = 1)
    )

  ggplot(heat_df, aes(x = outlier_label, y = model_type_label, fill = power)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(
      aes(label = scales::percent(power, accuracy = 1)),
      size = 2, color = "white"
    ) +
    scale_fill_viridis_c(
      option = "magma", limits = c(0, 1),
      labels = scales::percent, name = "Power"
    ) +
    facet_grid(
      rows = vars(ss_label),
      cols = vars(transformation_label)
    ) +
    labs(
      title = "Power Sensitivity to Analytic Choices",
      subtitle = "Rows = sample size; columns = transformation",
      x = "Outlier Method",
      y = "Model"
    ) +
    theme_multiverse(base_size = 10) +
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),
      panel.grid = element_blank()
    )
}

# ==============================================================================
# PLOT 11: BRANCH HEALTH (new)
# ==============================================================================

#' Stacked bar chart of branch outcomes: usable / singular / non-converged / error
#' Consumes: branch_health
plot_branch_health <- function(branch_health_df) {
  df <- branch_health_df %>%
    prepare_plot_labels() %>%
    dplyr::mutate(
      n_singular_only = n_singular,
      n_nonconv_only = n_converged - n_usable + n_singular, # non-converged but not singular
      n_nonconv_only = pmax(n_no_error - n_converged, 0),
      label = paste(model_type_label, transformation_label, sep = "\n")
    ) %>%
    dplyr::select(
      label, effect_condition, null_type,
      n_usable, n_singular, n_nonconv_only, n_error
    ) %>%
    tidyr::pivot_longer(
      cols = c(n_usable, n_singular, n_nonconv_only, n_error),
      names_to = "status",
      values_to = "count"
    ) %>%
    dplyr::mutate(
      status = factor(
        status,
        levels = c("n_usable", "n_singular", "n_nonconv_only", "n_error"),
        labels = c("Usable", "Singular", "Non-converged", "Error")
      ),
      null_type_label = dplyr::if_else(is.na(null_type), effect_condition, pretty_null_type(null_type))
    )

  ggplot(df, aes(x = label, y = count, fill = status)) +
    geom_col(position = "stack", width = 0.7) +
    scale_fill_manual(
      values = c(
        "Usable"        = "#4CAF50",
        "Singular"      = "#FFC107",
        "Non-converged" = "#FF9800",
        "Error"         = "#F44336"
      ),
      name = "Status"
    ) +
    facet_wrap(vars(null_type_label), scales = "free_x") +
    labs(
      title = "Branch Health Across the Multiverse",
      subtitle = "How many branches survived to usable analysis?",
      x = NULL,
      y = "Number of Branches"
    ) +
    theme_multiverse() +
    legend_bottom_rows(1) +
    theme(axis.text.x = element_text(angle = 25, hjust = 1, vjust = 1, size = 8))
}

# ==============================================================================
# PLOT 12: MODEL DIAGNOSTICS — d' and Youden's J (new)
# ==============================================================================

#' Signal detection metrics from ROC: d' and Youden's J
#' Consumes: roc_metrics
plot_signal_detection <- function(roc_metrics, palette = get_model_palette()) {
  df <- roc_metrics %>%
    dplyr::filter(!is.na(d_prime), is.finite(d_prime)) %>%
    prepare_plot_labels()

  p_dprime <- ggplot(df, aes(x = sample_size, y = d_prime, color = model_type)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    scale_color_manual(values = palette, name = "Model", drop = FALSE) +
    scale_x_continuous(labels = scales::percent) +
    facet_grid(
      rows = vars(null_type_label),
      cols = vars(transformation_label)
    ) +
    labs(
      title = "Discriminability (d')",
      subtitle = "Higher = better separation between true and null effects",
      x = "Sample Fraction",
      y = "d'"
    ) +
    theme_multiverse() +
    legend_bottom_rows(1)

  p_youden <- ggplot(df, aes(x = sample_size, y = youden_j, color = model_type)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    scale_color_manual(values = palette, name = "Model", drop = FALSE) +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(limits = c(-0.1, 1)) +
    facet_grid(
      rows = vars(null_type_label),
      cols = vars(transformation_label)
    ) +
    labs(
      title = "Youden's J Index (TPR − FPR)",
      x = "Sample Fraction",
      y = "J"
    ) +
    theme_multiverse() +
    legend_bottom_rows(1)

  p_dprime / p_youden +
    plot_annotation(
      title = "Signal Detection Metrics Across the Multiverse"
    )
}

# ==============================================================================
# PLOT 13: ODDS RATIOS FROM MULTIVERSE LOGISTIC MODELS (new)
# ==============================================================================

#' Forest plot of odds ratios from model_multiverse logistic regressions
#' Consumes: or_* tables (odds ratio tables from model_multiverse)
plot_odds_ratios <- function(analysis_list) {
  # Collect all or_* tables
  or_names <- grep("^or_", names(analysis_list), value = TRUE)
  if (length(or_names) == 0) {
    plot_log(logger::WARN, "No odds ratio tables found for plotting")
    return(ggplot() +
      theme_void() +
      labs(title = "No OR tables available"))
  }

  or_all <- purrr::map_dfr(or_names, function(nm) {
    analysis_list[[nm]] %>%
      dplyr::mutate(model_label = sub("^or_", "", nm))
  }) %>%
    dplyr::filter(!has_separation) # Exclude terms with quasi-separation

  if (nrow(or_all) == 0) {
    return(ggplot() +
      theme_void() +
      labs(title = "All OR terms had separation"))
  }

  ggplot(or_all, aes(
    x = odds_ratio, y = term, xmin = or_ci_lower, xmax = or_ci_upper,
    color = model_label
  )) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
    geom_pointrange(position = position_dodge(width = 0.6), size = 0.4) +
    scale_x_log10() +
    scale_color_brewer(palette = "Set2", name = "Outcome Model") +
    labs(
      title = "Odds Ratios from Multiverse Logistic Models",
      subtitle = "OR > 1 = higher odds of significance/failure; terms with separation excluded",
      x = "Odds Ratio (log scale)",
      y = NULL
    ) +
    theme_multiverse() +
    theme(axis.text.y = element_text(size = 8))
}

# ==============================================================================
# PLOT 14: SPECIFICATION INCONSISTENCIES (new)
# ==============================================================================

#' Highlight specs where outlier/subsample choice flips significance
#' Consumes: spec_inconsistencies
plot_spec_inconsistencies <- function(spec_incon_df) {
  df <- spec_incon_df %>%
    dplyr::filter(n_usable >= 2) %>%
    dplyr::mutate(
      label = paste(model_type, effect_condition, sep = " | "),
      ss_label = scales::percent(sample_size, accuracy = 1)
    )

  ggplot(df, aes(x = ss_label, y = pct_significant, color = is_inconsistent)) +
    geom_jitter(aes(size = n_usable), alpha = 0.6, width = 0.15) +
    scale_color_manual(
      values = c("TRUE" = "#E53935", "FALSE" = "#78909C"),
      labels = c("TRUE" = "Mixed (some sig, some not)", "FALSE" = "Consistent"),
      name = "Consistency"
    ) +
    scale_size_continuous(range = c(1, 5), name = "N usable") +
    facet_grid(
      rows = vars(model_type),
      cols = vars(transformation),
      labeller = labeller(transformation = get_transformation_labels()),
      scales = "free"
    ) +
    labs(
      title = "Specification Inconsistencies",
      subtitle = "Red = same spec with different outlier/subsample choices gives opposite conclusions",
      x = "Sample Size",
      y = "% Significant (within spec group)"
    ) +
    theme_multiverse() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# ==============================================================================
# PLOT 15: POWER-FPR FRONTIER (new)
# ==============================================================================

plot_power_fpr_frontier <- function(analysis_list) {
  power <- analysis_list$power_by_sample_size
  fpr <- analysis_list$fpr_by_sample_size
  if (is.null(power) || is.null(fpr) || !is.data.frame(power) || !is.data.frame(fpr)) {
    stop("power_by_sample_size and fpr_by_sample_size are required")
  }

  join_cols <- intersect(c("model_type", "transformation", "sample_size"), intersect(names(power), names(fpr)))
  df <- dplyr::left_join(
    power %>% dplyr::select(dplyr::all_of(join_cols), power),
    fpr %>% dplyr::select(dplyr::all_of(c(join_cols, "null_type", "FPR"))),
    by = join_cols
  ) %>%
    dplyr::filter(is.finite(.data$power), is.finite(.data$FPR)) %>%
    prepare_plot_labels() %>%
    dplyr::mutate(sample_label = scales::percent(.data$sample_size, accuracy = 1))

  ggplot(df, aes(x = FPR, y = power, color = model_type, size = sample_size)) +
    geom_vline(xintercept = 0.05, linetype = "dashed", color = "#C62828", alpha = 0.65) +
    geom_hline(yintercept = 0.8, linetype = "dotted", color = "grey45") +
    geom_path(aes(group = interaction(model_type, null_type)), linewidth = 0.75, alpha = 0.75) +
    geom_point(alpha = 0.9) +
    geom_text(aes(label = sample_label), size = 2.3, vjust = -0.75, check_overlap = TRUE, show.legend = FALSE) +
    scale_color_manual(values = get_model_palette(), name = "Model", drop = FALSE, labels = wrap_discrete_labels(22)) +
    scale_size_continuous(range = c(2.2, 5.5), labels = scales::percent, name = "Sample") +
    scale_x_continuous(labels = scales::percent, limits = c(0, NA), expand = expansion(mult = c(0.02, 0.12))) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1), expand = expansion(mult = c(0.02, 0.08))) +
    facet_grid(rows = vars(null_type_label), cols = vars(transformation_label), scales = "free") +
    labs(
      title = "Operating-Characteristic Frontier",
      subtitle = "Best branches move upward (power) without crossing the 5% FPR guide",
      x = "False positive rate under nullified empirical data",
      y = "Detection rate under present empirical data"
    ) +
    theme_multiverse(base_size = 10) +
    legend_bottom_rows(1)
}

plot_empirical_attenuation <- function(prepared_df) {
  required <- c("model_type", "transformation", "sample_size", "outlier", "subsample_id", "effect_condition", "strip_method", "main_estimate")
  missing <- setdiff(required, names(prepared_df))
  if (length(missing) > 0) stop("Missing columns for attenuation plot: ", paste(missing, collapse = ", "))

  usable <- prepared_df %>%
    dplyr::filter(.data$numerically_usable %||% TRUE) %>%
    dplyr::mutate(null_type = dplyr::coalesce(.data$null_type, paste0(.data$effect_condition, ":", .data$strip_method)))

  keys <- c("model_type", "transformation", "sample_size", "outlier", "subsample_id")
  present <- usable %>%
    dplyr::filter(.data$effect_condition == "present") %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::summarise(present_estimate = mean(.data$main_estimate, na.rm = TRUE), .groups = "drop")
  nullified <- usable %>%
    dplyr::filter(.data$effect_condition == "null_interaction") %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(keys, "null_type", "strip_method")))) %>%
    dplyr::summarise(nullified_estimate = mean(.data$main_estimate, na.rm = TRUE), .groups = "drop")

  df <- dplyr::left_join(nullified, present, by = keys) %>%
    dplyr::filter(is.finite(.data$present_estimate), is.finite(.data$nullified_estimate)) %>%
    dplyr::mutate(
      attenuation = abs(.data$nullified_estimate) / pmax(abs(.data$present_estimate), .Machine$double.eps),
      attenuation = pmin(.data$attenuation, 2)
    ) %>%
    prepare_plot_labels()

  ggplot(df, aes(x = present_estimate, y = nullified_estimate, color = null_type, size = sample_size)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "grey60") +
    geom_hline(yintercept = 0, linewidth = 0.35, color = "grey35") +
    geom_vline(xintercept = 0, linewidth = 0.35, color = "grey35") +
    geom_point(alpha = 0.72) +
    scale_color_manual(values = get_null_type_palette(), name = "Nullifier", drop = FALSE, labels = wrap_discrete_labels(28)) +
    scale_size_continuous(range = c(1.6, 4.8), labels = scales::percent, name = "Sample") +
    facet_grid(rows = vars(model_type_label), cols = vars(transformation_label), scales = "free") +
    labs(
      title = "Empirical Interaction Attenuation",
      subtitle = "Successful stripping pulls estimates toward zero while preserving the present-branch reference on x",
      x = "Present-branch interaction estimate",
      y = "Nullified-branch interaction estimate"
    ) +
    theme_multiverse(base_size = 10) +
    legend_bottom_rows(1)
}

# ==============================================================================
# PLOT 17: NULLIFIER VERDICT MATRIX (new)
# ==============================================================================

plot_nullifier_verdict_matrix <- function(prepared_df) {
  required <- c("strip_method", "transformation", "outlier", "sample_size", "effect_condition")
  missing <- setdiff(required, names(prepared_df))
  if (length(missing) > 0) stop("Missing columns for nullifier verdict matrix: ", paste(missing, collapse = ", "))

  df <- prepared_df %>%
    dplyr::filter(.data$effect_condition == "null_interaction") %>%
    dplyr::mutate(
      nullification_verdict = dplyr::coalesce(.data$nullification_verdict, "diagnostics_missing")
    ) %>%
    prepare_plot_labels() %>%
    dplyr::group_by(.data$strip_method_label, .data$transformation_label, .data$outlier_label, .data$sample_size, .data$nullification_verdict) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::group_by(.data$strip_method_label, .data$transformation_label, .data$outlier_label, .data$sample_size) %>%
    dplyr::slice_max(.data$n, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(sample_label = scales::percent(.data$sample_size, accuracy = 1))

  ggplot(df, aes(x = sample_label, y = outlier_label, fill = nullification_verdict)) +
    geom_tile(color = "white", linewidth = 0.5) +
    facet_grid(rows = vars(strip_method_label), cols = vars(transformation_label), scales = "free", space = "free") +
    scale_fill_manual(
      values = c(
        interpretable_nullifier = "#2E7D32",
        fails_preservation_gates = "#C62828",
        unpaired = "#6D4C41",
        diagnostics_missing = "#78909C"
      ),
      breaks = c("interpretable_nullifier", "fails_preservation_gates", "unpaired", "diagnostics_missing"),
      labels = wrap_discrete_labels(24),
      name = "Verdict"
    ) +
    labs(
      title = "Nullifier Interpretability Matrix",
      subtitle = "Where residual CSE and preservation gates allow FPR interpretation",
      x = "Sample Fraction",
      y = "Outlier Rule"
    ) +
    theme_multiverse(base_size = 10) +
    legend_bottom_rows(1) +
    theme(axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1))
}

plot_failure_composition_lollipop <- function(analysis_list) {
  rates <- analysis_list$nullification_failure_aware_rates
  if (is.null(rates) || !is.data.frame(rates) || nrow(rates) == 0L) {
    stop("nullification_failure_aware_rates is missing or empty")
  }
  count_cols <- c(
    significant_primary = "Significant",
    usable_nonsignificant = "Usable non-significant",
    singular = "Singular",
    non_converged = "Non-converged",
    extraction_or_preprocessing_error = "Extraction/preprocessing error",
    other_invalid = "Other invalid"
  )
  count_cols <- count_cols[names(count_cols) %in% names(rates)]
  if (length(count_cols) == 0L) stop("No failure composition count columns found")

  rates %>%
    prepare_plot_labels() %>%
    tidyr::pivot_longer(dplyr::all_of(names(count_cols)), names_to = "component", values_to = "count") %>%
    dplyr::mutate(
      component = dplyr::recode(.data$component, !!!as.list(count_cols)),
      component = factor(.data$component, levels = unname(count_cols))
    ) %>%
    dplyr::group_by(.data$model_type_label, .data$transformation_label, .data$component) %>%
    dplyr::summarise(
      count = sum(.data$count, na.rm = TRUE),
      n_planned = sum(.data$n_planned, na.rm = TRUE),
      proportion = dplyr::if_else(.data$n_planned > 0, .data$count / .data$n_planned, NA_real_),
      .groups = "drop"
    ) %>%
    ggplot(aes(x = proportion, y = component, color = component)) +
    geom_segment(aes(x = 0, xend = proportion, yend = component), linewidth = 0.9, alpha = 0.8) +
    geom_point(size = 2.8) +
    geom_text(aes(label = scales::percent(proportion, accuracy = 1)), hjust = -0.15, size = 2.6, color = "grey20") +
    facet_grid(rows = vars(model_type_label), cols = vars(transformation_label), scales = "free") +
    scale_x_continuous(labels = scales::percent, limits = c(0, 1), expand = expansion(mult = c(0, 0.10))) +
    scale_color_manual(values = c(
      "Significant" = "#C62828",
      "Usable non-significant" = "#2E7D32",
      "Singular" = "#F9A825",
      "Non-converged" = "#EF6C00",
      "Extraction/preprocessing error" = "#6A1B9A",
      "Other invalid" = "#78909C"
    ), guide = "none") +
    labs(
      title = "Failure-Aware Nullification Composition",
      subtitle = "FPR is only one component; singular and error rates remain visible",
      x = "Proportion of planned branches",
      y = NULL
    ) +
    theme_multiverse(base_size = 10)
}

plot_tpr_saturation_summary <- function(power_by_ss) {
  df <- power_by_ss %>%
    prepare_plot_labels() %>%
    dplyr::mutate(tpr_gap = pmax(0, 1 - .data$power))

  ggplot(df, aes(x = sample_size, y = power, color = model_type, group = model_type)) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "#2E7D32", alpha = 0.7) +
    geom_line(linewidth = 0.75, alpha = 0.85) +
    geom_point(size = 2.4, alpha = 0.9) +
    scale_color_manual(values = get_model_palette(), name = "Model", drop = FALSE) +
    scale_x_continuous(labels = scales::percent, breaks = sort(unique(df$sample_size))) +
    scale_y_continuous(labels = scales::percent, limits = c(0.9, 1), breaks = seq(0.9, 1, 0.025)) +
    facet_wrap(vars(transformation_label), scales = "free") +
    labs(
      title = "TPR Is Saturated",
      subtitle = "Present-branch detection is essentially 100%; interpretation should focus on FPR and failures",
      x = "Sample fraction",
      y = "TPR"
    ) +
    theme_multiverse(base_size = 10) +
    legend_bottom_rows(1) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))
}

# ==============================================================================
# COMBINED DASHBOARD
# ==============================================================================

#' Generate full multiverse dashboard from precomputed analysis tables
#'
#' @param analysis_list Named list from analyze_multiverse_results()
#' @param output_dir Directory to save plots
#' @param save_individual Save individual plots in addition to dashboards
#' @return Invisible list of ggplot objects
generate_multiverse_dashboard <- function(
    analysis_list,
    output_dir = "outputs/figures",
    save_individual = TRUE) {
  plot_log(logger::INFO, "Generating multiverse dashboard...")

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  # Extract tables used by the FPR-focused reporting layer.
  fpr_coarse <- analysis_list$fpr_coarse
  fpr_by_ss <- analysis_list$fpr_by_sample_size
  fpr_by_outlier <- analysis_list$fpr_by_outlier
  branch_health <- analysis_list$branch_health
  prepared_df <- analysis_list$results_with_diag

  # Build all plots, catching errors so one failure doesn't kill the dashboard
  safe_plot <- function(expr, name) {
    tryCatch(
      expr,
      error = function(e) {
        plot_log(logger::WARN, "Plot '{name}' failed: {e$message}")
        ggplot() +
          theme_void() +
          labs(title = paste("Error:", name), subtitle = e$message)
      }
    )
  }

  plot_specs <- list(
    fpr_by_null_type = function() plot_fpr_by_null_type(fpr_coarse),
    fpr_by_sample_size = function() plot_fpr_by_sample_size(fpr_by_ss),
    fpr_by_outlier = function() plot_fpr_outlier_heatmap(fpr_by_outlier),
    fpr_exceedance = function() plot_fpr_exceedance_summary(fpr_by_outlier),
    fpr_transform_delta = function() plot_fpr_transform_delta(fpr_by_ss),
    fpr_extreme_combos = function() plot_fpr_extreme_combinations(fpr_by_outlier),
    strip_robust = function() plot_stripping_robustness(fpr_by_ss),
    nullifier_matrix = function() plot_nullifier_verdict_matrix(prepared_df),
    failure_lollipop = function() plot_failure_composition_lollipop(analysis_list),
    branch_health = function() plot_branch_health(branch_health),
    tpr_saturated = function() plot_tpr_saturation_summary(analysis_list$power_by_sample_size)
  )
  plots <- lapply(names(plot_specs), function(name) {
    plot_log(logger::INFO, "Building plot: {name}")
    safe_plot(plot_specs[[name]](), name)
  })
  names(plots) <- names(plot_specs)

  # Save individual plots
  if (save_individual) {
    for (name in names(plots)) {
      filepath <- file.path(output_dir, name)
      width <- dplyr::case_when(
        name %in% c("fpr_by_outlier", "fpr_extreme_combos", "nullifier_matrix") ~ 16,
        name %in% c("fpr_by_sample_size", "fpr_by_null_type", "failure_lollipop", "fpr_exceedance", "tpr_saturated") ~ 14,
        TRUE ~ 11
      )
      height <- dplyr::case_when(
        name %in% c("fpr_by_outlier") ~ 10,
        name %in% c("fpr_extreme_combos", "nullifier_matrix") ~ 9,
        name %in% c("fpr_by_sample_size", "fpr_by_null_type", "fpr_exceedance") ~ 8,
        TRUE ~ 7
      )
      plot_save_fallback(filepath, plots[[name]], width = width, height = height)
    }
  }

  # Summary dashboard (key plots)
  dashboard <- (plots$fpr_by_sample_size + plots$fpr_by_null_type) /
    (plots$fpr_exceedance + plots$fpr_extreme_combos) /
    (plots$failure_lollipop + plots$tpr_saturated) +
    plot_annotation(
      title = "False-Positive-Rate Summary",
      subtitle = sprintf(
        "%d usable branches; one compact TPR panel included only to show saturation",
        sum(prepared_df$numerically_usable, na.rm = TRUE)
      ),
      theme = theme(
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12, color = "grey40")
      )
    )

  plot_save_fallback(
    file.path(output_dir, "dashboard_summary"),
    dashboard,
    width = 20, height = 22
  )

  # Robustness dashboard
  robustness <- (plots$fpr_by_outlier + plots$fpr_transform_delta) /
    (plots$nullifier_matrix + plots$branch_health) +
    plot_annotation(
      title = "FPR Robustness and Nullifier Health",
      subtitle = "Outlier, transform, preservation, and convergence context for FPR"
    )

  plot_save_fallback(
    file.path(output_dir, "dashboard_robustness"),
    robustness,
    width = 18, height = 14
  )

  # Diagnostics dashboard
  diagnostics <- (plots$branch_health + plots$failure_lollipop) +
    plot_annotation(
      title = "Pipeline Diagnostics",
      subtitle = "Convergence and failure composition across planned branches"
    )

  plot_save_fallback(
    file.path(output_dir, "dashboard_diagnostics"),
    diagnostics,
    width = 16, height = 8
  )

  plots$dashboard_summary <- dashboard
  plots$dashboard_robustness <- robustness
  plots$dashboard_diagnostics <- diagnostics

  plot_log(
    logger::INFO,
    "Dashboard generation complete: {length(plots)} plots created"
  )

  invisible(plots)
}
