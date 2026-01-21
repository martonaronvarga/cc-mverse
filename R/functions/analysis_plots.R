# R/functions/analysis_plots.R
# Comprehensive Multiverse Visualization Suite

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

plot_save_fallback <- function(filename, plot, width = 10, height = 7, dpi = 300) {
  tryCatch(
    {
      ggsave(filename = paste0(filename, ".svg"), plot = plot, device = "svg", width = width, height = height, dpi = dpi)
      message("Saved as SVG: ", filename, ".svg")
    },
    error = function(e) {
      tryCatch(
        {
          ggsave(filename = paste0(filename, ".pdf"), plot = plot, device = "pdf", width = width, height = height, dpi = dpi)
          message("Saved as PDF: ", filename, ".pdf")
        },
        error = function(e2) {
          stop("Cannot save plot in any available graphics format.")
        }
      )
    }
  )
}

# ==============================================================================
# COLOR PALETTES
# ==============================================================================

get_cb_palette <- function() {
  c(
    "LMM (intercept)" = "#332288",
    "LMM (cong slope)" = "#88CCEE",
    "LMM (full)" = "#117733",
    "rmANOVA" = "#CC6677"
  )
}

get_effect_palette <- function() {
  c(
    "present" = "#2E7D32",
    "null_interaction" = "#C62828",
    "null_both" = "#6A1B9A"
  )
}

get_strip_palette <- function() {
  c(
    "No stripping" = "#424242",
    "Shuffle" = "#1976D2",
    "QMap" = "#F57C00"
  )
}

# ==============================================================================
# DATA PREPARATION
# ==============================================================================

#' Prepare results with proper diagnostics
#'
#' Ensures all derived columns exist before analysis
#'
rename_for_plots <- function(df) {
  df %>%
    dplyr::mutate(
      # Model type grouping
      model_type = dplyr::case_when(
        grepl("rmanova", model) ~ "rmANOVA",
        grepl("full_slope", model) ~ "LMM (full)",
        grepl("cong_slope", model) ~ "LMM (cong slope)",
        grepl("intercept", model) ~ "LMM (intercept)",
        TRUE ~ model
      ),

      # Clean labels
      sample_size = paste0(sample_size * 100, "%"),
      transformation_label = dplyr::if_else(
        transformation == "log_rt", "log(RT)", "Raw RT"
      ),
      strip_label = dplyr::case_when(
        strip_method == "none" ~ "No stripping",
        strip_method == "none" & effect_condition == "null_both" ~ "ERROR!",
        strip_method == "shuffle" ~ "Shuffle",
        strip_method == "qmap_5" ~ "QMap",
        TRUE ~ strip_method
      )
    )
}


# ==============================================================================
# INDIVIDUAL PLOTS
# ==============================================================================

#' Plot 1: ROC curves by model and sample size
#'
plot_roc_by_model <- function(
    roc_metrics,
    facet_by = "transformation",
    cb_palette = get_cb_palette()) {
  p <- ggplot(roc_metrics, aes(x = FPR, y = TPR, color = model_type, group = model_type)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "grey50") +
    geom_vline(xintercept = 0.05, linetype = "dashed", color = "red", alpha = 0.5) +
    geom_path(linewidth = 0.8) +
    geom_point(aes(size = sample_size), alpha = 0.8) +
    ggrepel::geom_text_repel(
      aes(label = scales::percent(sample_size, accuracy = 1)),
      size = 2.5,
      max.overlaps = 10,
      show.legend = FALSE,
      segment.alpha = 0.3
    ) +
    scale_color_manual(values = cb_palette, name = "Model") +
    scale_size_continuous(range = c(2, 5), guide = "none") +
    scale_x_continuous(
      limits = c(0, 0.25),
      breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25),
      labels = scales::percent
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, 0.2),
      labels = scales::percent
    ) +
    facet_wrap(vars(!!sym(facet_by)), ncol = 2) +
    labs(
      title = "ROC Curves by Model Type",
      subtitle = "Points sized by sample size; dashed line = nominal alpha = 0.05",
      x = "False Positive Rate",
      y = "True Positive Rate"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold")
    )

  p
}


plot_roc_by_outlier <- function(
    roc_metrics,
    facet_by = "transformation",
    cb_palette = get_cb_palette()) {
  p <- ggplot(roc_metrics, aes(x = FPR, y = TPR, color = outlier, group = outlier)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "grey50") +
    geom_vline(xintercept = 0.05, linetype = "dashed", color = "red", alpha = 0.5) +
    geom_path(linewidth = 0.8) +
    geom_point(aes(shape = model_type), alpha = 0.8) +
    ggrepel::geom_text_repel(
      aes(label = model_type),
      size = 2.5,
      max.overlaps = 10,
      show.legend = FALSE,
      segment.alpha = 0.3
    ) +
    scale_color_manual(values = cb_palette, name = "Filtering Method") +
    scale_size_continuous(range = c(2, 5), guide = "none") +
    scale_x_continuous(
      limits = c(0, 0.25),
      breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25),
      labels = scales::percent
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, 0.2),
      labels = scales::percent
    ) +
    facet_wrap(vars(!!sym(facet_by)), ncol = 2) +
    labs(
      title = "ROC Curves by Filtering Strategies",
      subtitle = "Points shaped by model type; dashed line = nominal alpha = 0.05",
      x = "False Positive Rate",
      y = "True Positive Rate"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold")
    )

  p
}
#' Plot 2: FPR comparison across stripping methods

#'
plot_fdr_by_stripping <- function(fpr_by_null, cb_palette = get_strip_palette()) {
  p <- ggplot(fpr_by_null, aes(x = strip_method, y = FPR, fill = strip_method)) +
    geom_col(position = "dodge", alpha = 0.8) +
    geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
    scale_fill_manual(values = cb_palette, guide = "none") +
    scale_y_continuous(
      limits = c(0, NA),
      labels = scales::percent,
      expand = expansion(mult = c(0, 0.1))
    ) +
    facet_grid(
      cols = vars(model_type),
      labeller = labeller(
        effect_condition = c(
          "null_interaction" = "Null: Interaction only",
        )
      )
    ) +
    labs(
      title = "False Positive Rate by Stripping Method",
      subtitle = "Red dashed line = nominal alpha = 0.05",
      x = "Stripping Method",
      y = "False Positive Rate"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major.x = element_blank(),
      strip.text = element_text(face = "bold")
    )

  p
}

#' Plot 3: TPR (Power) by sample size
#'
plot_power_by_sample_size <- function(power_metrics, cb_palette = get_cb_palette()) {
  p <- ggplot(power_metrics, aes(
    x = factor(sample_size), y = power,
    color = model_type, group = model_type
  )) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 3) +
    scale_color_manual(values = cb_palette, name = "Model") +
    scale_y_continuous(
      limits = c(0, 1),
      labels = scales::percent,
      breaks = seq(0, 1, 0.2)
    ) +
    facet_wrap(vars(transformation), ncol = 2) +
    labs(
      title = "Statistical Power by Sample Size",
      subtitle = "True Positive Rate when effect is present",
      x = "Sample Size (proportion of full data)",
      y = "Power (TPR)"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )

  p
}

#' Plot 4: Specification curve
#'
plot_specification_curve_detailed <- function(results_df, alpha = 0.05) {
  prepared <- results_df %>%
    dplyr::arrange(main_estimate) %>%
    dplyr::mutate(spec_rank = dplyr::row_number())

  # Top panel: effect sizes
  p_effects <- ggplot(prepared, aes(x = spec_rank, y = main_estimate)) +
    geom_point(
      aes(color = is_significant),
      size = 0.8,
      alpha = 0.7
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    scale_color_manual(
      values = c("TRUE" = "#2E7D32", "FALSE" = "#9E9E9E"),
      labels = c("TRUE" = "p < 0.05", "FALSE" = "p >= 0.05"),
      name = NULL
    ) +
    labs(y = "Effect Estimate", x = NULL) +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "top",
      panel.grid.major.x = element_blank()
    )

  # Bottom panel: specification indicators
  spec_indicators <- prepared %>%
    dplyr::filter(effect_condition == "present" & strip_method == "none") %>%
    dplyr::mutate(sample_size = as.factor(sample_size)) %>%
    dplyr::select(
      spec_rank, model_type, transformation, sample_size,
      outlier
    ) %>%
    tidyr::pivot_longer(
      cols = -spec_rank,
      names_to = "dimension",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      dimension = factor(dimension, levels = c(
        "model_type", "transformation",
        "sample_size", "outlier"
      ))
    )

  p_specs <- ggplot(spec_indicators, aes(x = spec_rank, y = dimension, fill = as.factor(value))) +
    geom_tile(height = 0.8) +
    scale_fill_viridis_d(option = "turbo", guide = "none") +
    labs(x = "Specification (ranked by effect size)", y = NULL) +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid = element_blank(),
      axis.text.y = element_text(hjust = 1)
    )

  # Combine
  p_effects / p_specs + plot_layout(heights = c(2, 1))
}

#' Plot 5: Effect size distributions by condition
#'
plot_effect_distributions <- function(results_df, cb_palette = get_effect_palette()) {
  p <- ggplot(results_df, aes(x = main_estimate, fill = effect_condition)) +
    geom_density(alpha = 0.6, color = NA) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
    scale_fill_manual(
      values = cb_palette,
      labels = c(
        "present" = "Effect present",
        "null_interaction" = "Null interaction"
      ),
      name = "Condition"
    ) +
    facet_grid(rows = vars(model_type), cols = vars(transformation)) +
    labs(
      title = "Effect Size Distributions",
      subtitle = "Separation indicates discriminability between true and null effects",
      x = "Effect Size",
      y = "Density"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "bottom",
      strip.text = element_text(face = "bold")
    )

  p
}

#' Plot 6: P-value distributions (diagnostic)
#'
plot_pvalue_distributions <- function(results_df, cb_palette = get_effect_palette()) {
  p <- ggplot(results_df, aes(x = main_p_value, fill = effect_condition)) +
    geom_histogram(bins = 20, alpha = 0.7, position = "identity") +
    geom_vline(xintercept = 0.05, linetype = "dashed", color = "red") +
    scale_fill_manual(values = cb_palette, name = "Condition") +
    facet_grid(
      rows = vars(strip_method),
      cols = vars(model_type),
      scales = "free_y"
    ) +
    labs(
      title = "P-value Distributions",
      subtitle = "Null condition should be uniform; present condition should be left-skewed",
      x = "P-value (Satterthwaite)",
      y = "Count"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "bottom",
      strip.text = element_text(size = 8)
    )

  p
}

#' Plot 7: Stripping robustness - FPR should NOT depend on stripping
#'
plot_stripping_robustness <- function(fpr_df) {
  # Compare none vs shuffle vs qmap
  fpr_wide <- fpr_df %>%
    dplyr::select(
      model_type, sample_size, transformation, effect_condition,
      strip_method, FPR
    ) %>%
    tidyr::pivot_wider(
      names_from = strip_method,
      values_from = FPR,
      names_prefix = "FPR_"
    )

  # Plot: FPR with stripping vs without
  p1 <- ggplot(fpr_wide, aes(x = FPR_qmap_5, y = FPR_shuffle)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
    geom_point(aes(color = model_type, shape = effect_condition), size = 3, alpha = 0.7) +
    geom_vline(xintercept = 0.05, linetype = "dotted", color = "red", alpha = 0.5) +
    geom_hline(yintercept = 0.05, linetype = "dotted", color = "red", alpha = 0.5) +
    scale_x_continuous(limits = c(0, 0.3), labels = scales::percent) +
    scale_y_continuous(limits = c(0, 0.3), labels = scales::percent) +
    labs(
      title = "Shuffle vs Quantile Mapping",
      x = "FPR (qmap_5)",
      y = "FPR (shuffle)"
    ) +
    theme_minimal(base_size = 10) +
    theme(legend.position = "right")

  p1 +
    plot_annotation(
      title = "Stripping Robustness Check",
      subtitle = "Points on diagonal = stripping method does not influence FPR"
    )
}

#' Plot 8: Sensitivity heatmap
#'
plot_sensitivity_heatmap <- function(sensitivity_df) {
  p <- ggplot(sensitivity_df, aes(x = outlier, y = model_type, fill = power)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = scales::percent(power, accuracy = 1)), size = 2.5) +
    scale_fill_viridis_c(
      option = "magma",
      limits = c(0, 1),
      labels = scales::percent,
      name = "Power"
    ) +
    facet_grid(rows = vars(sample_size), cols = vars(transformation)) +
    labs(
      title = "Power Sensitivity to Analytic Choices",
      x = "Outlier Method",
      y = "Model"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank(),
      strip.text = element_text(face = "bold")
    )

  p
}

# ==============================================================================
# COMBINED DASHBOARD
# ==============================================================================

#' Generate full multiverse analysis dashboard
#'
#' @param results_df Results tibble from pipeline
#' @param output_dir Directory to save plots
#' @param save_individual Save individual plots as well as combined
#'
#' @return List of ggplot objects
#'
generate_multiverse_dashboard <- function(analysis_list,
                                          output_dir = "outputs/figures",
                                          save_individual = TRUE) {
  logger::log_info("Generating multiverse dashboard from precomputed tables...")

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)


  roc_metrics <- analysis_list$roc_metrics
  fpr_by_null <- analysis_list$fdr_by_null_type
  power_metrics <- analysis_list$power_analysis
  sensitivity_df <- analysis_list$spec_sensitivity
  results_with_diag <- analysis_list$results_with_diag

  plots <- list(
    roc_by_outlier = plot_roc_by_outlier(roc_metrics),
    roc_by_model = plot_roc_by_model(roc_metrics),
    fdr_by_stripping = plot_fdr_by_stripping(fpr_by_null),
    power_by_sample = plot_power_by_sample_size(power_metrics),
    spec_curve = plot_specification_curve_detailed(results_with_diag),
    effect_dist = plot_effect_distributions(results_with_diag),
    pvalue_dist = plot_pvalue_distributions(results_with_diag),
    strip_robust = plot_stripping_robustness(fpr_by_null),
    sensitivity = plot_sensitivity_heatmap(sensitivity_df)
  )

  # Save individual plots
  if (save_individual) {
    for (name in names(plots)) {
      filepath <- file.path(output_dir, paste0(name, ".png"))

      # Determine size based on plot type
      width <- if (name %in% c("spec_curve", "pvalue_dist")) 14 else 10
      height <- if (name %in% c("spec_curve", "sensitivity")) 10 else 7

      plot_save_fallback(filepath, plots[[name]], width = width, height = height, dpi = 300)
      logger::log_info("Saved:  {filepath}")
    }
  }

  # Create summary dashboard (3x2 of key plots)
  dashboard <- plots$roc_by_outlier /
    (plots$roc_by_model + plots$power_by_sample) /
    (plots$fdr_by_stripping + plots$strip_robust) +
    plot_annotation(
      title = "Multiverse Analysis Summary",
      subtitle = sprintf("Pre-computed from %d analysis groups", nrow(roc_metrics)),
      theme = theme(
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12)
      )
    )

  dashboard_path <- file.path(output_dir, "dashboard_summary.png")
  plot_save_fallback(dashboard_path, dashboard, width = 16, height = 14, dpi = 300)
  logger::log_info("Saved dashboard:  {dashboard_path}")

  # Create robustness-focused dashboard
  robustness <- (plots$strip_robust) /
    (plots$fdr_by_stripping) +
    plot_annotation(
      title = "Robustness Analysis:  Effect of Stripping Methods",
      subtitle = "Checking if nullification method inflates false discovery rate"
    )

  robust_path <- file.path(output_dir, "dashboard_robustness.png")
  plot_save_fallback(robust_path, robustness, width = 14, height = 12, dpi = 300)
  logger::log_info("Saved robustness dashboard: {robust_path}")

  plots$dashboard <- dashboard
  plots$robustness <- robustness

  logger::log_info("Dashboard generation complete:  {length(plots)} plots created")

  invisible(plots)
}
