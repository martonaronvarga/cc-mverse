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
# DATA PREPARATION (FIXED LOGIC)
# ==============================================================================

#' Prepare results with proper diagnostics
#'
#' Ensures all derived columns exist before analysis
#'
prepare_results <- function(results_df) {
  results_df %>%
    dplyr::filter(!error) %>%
    dplyr::mutate(
      # Convergence
      converged_both = full_converged & dplyr::coalesce(null_converged, TRUE),

      # Effect classification
      is_true_effect = (effect_condition == "present" & strip_method == "none"),
      is_null_effect = (
        (effect_condition == "null_interaction" & strip_method %in% c("shuffle", "qmap_5")) |
          (effect_condition == "null_both" & strip_method == "none")
      ), # Significance (use t test as primary)
      is_significant = main_p_value < 0.05,

      # Model type grouping
      model_type = dplyr::case_when(
        grepl("rmanova", model) ~ "rmANOVA",
        grepl("full_slope", model) ~ "LMM (full)",
        grepl("cong_slope", model) ~ "LMM (cong slope)",
        grepl("intercept", model) ~ "LMM (intercept)",
        TRUE ~ model
      ),

      # Clean labels
      sample_size_pct = paste0(sample_size * 100, "%"),
      transformation_label = dplyr::if_else(
        transformation == "log_rt", "log(RT)", "Raw RT"
      ),
      strip_label = dplyr::case_when(
        strip_method == "none" ~ "No stripping",
        strip_method == "shuffle" ~ "Shuffle",
        strip_method == "qmap_5" ~ "QMap",
        TRUE ~ strip_method
      )
    ) %>%
    dplyr::filter(converged_both) %>%
    dplyr::filter(!(effect_condition == "present" & strip_method != "none"))
}

#' Compute ROC metrics properly
#'
#' TPR from present condition, FPR from null conditions
#' Grouped appropriately to avoid single-row groups
#'
compute_roc_metrics <- function(results_df,
                                group_vars = c(
                                  "model_type", "sample_size",
                                  "transformation", "strip_method"
                                )) {
  prepared <- prepare_results(results_df)

  # TPR:  proportion significant when effect is PRESENT
  tpr_df <- prepared %>%
    dplyr::filter(is_true_effect) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::summarise(
      n_true = dplyr::n(),
      n_detected = sum(is_significant, na.rm = TRUE),
      TPR = ifelse(n_true > 0, n_detected / n_true, NA_real_),
      .groups = "drop"
    )

  # FPR:  proportion significant when effect is NULL
  fpr_df <- prepared %>%
    dplyr::filter(is_null_effect) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::summarise(
      n_null = dplyr::n(),
      n_false_positive = sum(is_significant, na.rm = TRUE),
      FPR = ifelse(n_null > 0, n_false_positive / n_null, NA_real_),
      .groups = "drop"
    )

  # Join TPR and FPR
  roc_df <- tpr_df %>%
    dplyr::left_join(fpr_df, by = group_vars) %>%
    dplyr::mutate(
      # d' (sensitivity index)
      d_prime = qnorm(pmin(TPR, 0.999)) - qnorm(pmax(FPR, 0.001)),
      # Youden's J
      youden_j = TPR - FPR
    )

  roc_df
}

#' Compute FDR by null condition type
#'
#' Separates null_interaction vs null_both to check if stripping works
#'
compute_fdr_by_null_type <- function(results_df,
                                     group_vars = c(
                                       "model_type", "sample_size",
                                       "transformation", "strip_method"
                                     )) {
  prepared <- prepare_results(results_df)

  prepared %>%
    dplyr::filter(is_null_effect) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_vars, "effect_condition")))) %>%
    dplyr::summarise(
      n = dplyr::n(),
      n_significant = sum(is_significant, na.rm = TRUE),
      FDR = ifelse(n > 0, n_significant / n, NA_real_),
      mean_p = mean(main_p_value, na.rm = TRUE),
      median_p = median(main_p_value, na.rm = TRUE),
      .groups = "drop"
    )
}

# ==============================================================================
# INDIVIDUAL PLOTS
# ==============================================================================

#' Plot 1: ROC curves by model and sample size
#'
plot_roc_by_model <- function(results_df,
                              facet_by = "transformation",
                              cb_palette = get_cb_palette()) {
  roc_df <- compute_roc_metrics(
    results_df,
    group_vars = c("model_type", "sample_size", "transformation", "strip_method")
  ) %>%
    dplyr::filter(strip_method == "none") # Baseline without stripping

  p <- ggplot(roc_df, aes(x = FPR, y = TPR, color = model_type, group = model_type)) +
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

#' Plot 2: FDR comparison across stripping methods
#'
#' Key robustness check: does stripping inflate FDR?
#'
plot_fdr_by_stripping <- function(results_df, cb_palette = get_strip_palette()) {
  fdr_df <- compute_fdr_by_null_type(
    results_df,
    group_vars = c("model_type", "sample_size", "transformation", "strip_method")
  )

  # Summarize across sample sizes for cleaner plot
  fdr_summary <- fdr_df %>%
    dplyr::group_by(model_type, transformation, strip_method, effect_condition) %>%
    dplyr::summarise(
      mean_FDR = mean(FDR, na.rm = TRUE),
      se_FDR = sd(FDR, na.rm = TRUE) / sqrt(dplyr::n()),
      .groups = "drop"
    )

  p <- ggplot(fdr_summary, aes(x = strip_method, y = mean_FDR, fill = strip_method)) +
    geom_col(position = "dodge", alpha = 0.8) +
    geom_errorbar(
      aes(ymin = mean_FDR - se_FDR, ymax = mean_FDR + se_FDR),
      width = 0.2,
      position = position_dodge(0.9)
    ) +
    geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
    scale_fill_manual(values = cb_palette, guide = "none") +
    scale_y_continuous(
      limits = c(0, NA),
      labels = scales::percent,
      expand = expansion(mult = c(0, 0.1))
    ) +
    facet_grid(
      rows = vars(effect_condition),
      cols = vars(model_type),
      labeller = labeller(
        effect_condition = c(
          "null_interaction" = "Null: Interaction only",
          "null_both" = "Null: Both effects"
        )
      )
    ) +
    labs(
      title = "False Discovery Rate by Stripping Method",
      subtitle = "Red dashed line = nominal alpha = 0.05",
      x = "Stripping Method",
      y = "False Discovery Rate"
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
plot_power_by_sample_size <- function(results_df, cb_palette = get_cb_palette()) {
  roc_df <- compute_roc_metrics(
    results_df,
    group_vars = c("model_type", "sample_size", "transformation", "strip_method")
  ) %>%
    dplyr::filter(strip_method == "none")

  p <- ggplot(roc_df, aes(
    x = factor(sample_size), y = TPR,
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
  prepared <- prepare_results(results_df) %>%
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
    dplyr::mutate(sample_size = as.factor(sample_size)) %>%
    dplyr::select(
      spec_rank, model_type, transformation, sample_size,
      outlier, strip_method, effect_condition
    ) %>%
    tidyr::pivot_longer(
      cols = -spec_rank,
      names_to = "dimension",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      dimension = factor(dimension, levels = c(
        "effect_condition", "model_type", "transformation",
        "sample_size", "outlier", "strip_method"
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
  prepared <- prepare_results(results_df)

  p <- ggplot(prepared, aes(x = effect_size, fill = effect_condition)) +
    geom_density(alpha = 0.6, color = NA) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
    scale_fill_manual(
      values = cb_palette,
      labels = c(
        "present" = "Effect present",
        "null_interaction" = "Null interaction",
        "null_both" = "Null both"
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
  prepared <- prepare_results(results_df)

  p <- ggplot(prepared, aes(x = main_p_value, fill = effect_condition)) +
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
      subtitle = "Null conditions should be uniform; present condition should be left-skewed",
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

#' Plot 7: Stripping robustness - FDR should NOT depend on stripping
#'
plot_stripping_robustness <- function(results_df) {
  fdr_df <- compute_fdr_by_null_type(
    results_df,
    group_vars = c("model_type", "sample_size", "transformation", "strip_method")
  )

  # Compare none vs shuffle vs qmap
  fdr_wide <- fdr_df %>%
    dplyr::select(
      model_type, sample_size, transformation, effect_condition,
      strip_method, FDR
    ) %>%
    tidyr::pivot_wider(
      names_from = strip_method,
      values_from = FDR,
      names_prefix = "FDR_"
    )

  # Plot:  FDR with stripping vs without
  p1 <- ggplot(fdr_wide, aes(x = FDR_none, y = FDR_shuffle)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
    geom_point(aes(color = model_type, shape = effect_condition), size = 3, alpha = 0.7) +
    geom_vline(xintercept = 0.05, linetype = "dotted", color = "red", alpha = 0.5) +
    geom_hline(yintercept = 0.05, linetype = "dotted", color = "red", alpha = 0.5) +
    scale_x_continuous(limits = c(0, 0.3), labels = scales::percent) +
    scale_y_continuous(limits = c(0, 0.3), labels = scales::percent) +
    labs(
      title = "Shuffle vs No Stripping",
      x = "FDR (no stripping)",
      y = "FDR (shuffle)"
    ) +
    theme_minimal(base_size = 10) +
    theme(legend.position = "none")

  p2 <- ggplot(fdr_wide, aes(x = FDR_none, y = FDR_qmap_5)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
    geom_point(aes(color = model_type, shape = effect_condition), size = 3, alpha = 0.7) +
    geom_vline(xintercept = 0.05, linetype = "dotted", color = "red", alpha = 0.5) +
    geom_hline(yintercept = 0.05, linetype = "dotted", color = "red", alpha = 0.5) +
    scale_x_continuous(limits = c(0, 0.3), labels = scales::percent) +
    scale_y_continuous(limits = c(0, 0.3), labels = scales::percent) +
    labs(
      title = "QMap vs No Stripping",
      x = "FDR (no stripping)",
      y = "FDR (QMap)"
    ) +
    theme_minimal(base_size = 10) +
    theme(legend.position = "right")

  p1 + p2 +
    plot_annotation(
      title = "Stripping Robustness Check",
      subtitle = "Points on diagonal = stripping does not inflate FDR"
    )
}

#' Plot 8: Sensitivity heatmap
#'
plot_sensitivity_heatmap <- function(results_df) {
  sensitivity <- results_df %>%
    prepare_results() %>%
    dplyr::filter(is_true_effect) %>% # Only look at power for true effects
    dplyr::group_by(model_type, outlier, transformation, sample_size) %>%
    dplyr::summarise(
      power = mean(is_significant, na.rm = TRUE),
      .groups = "drop"
    )

  p <- ggplot(sensitivity, aes(x = outlier, y = model_type, fill = power)) +
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
generate_multiverse_dashboard <- function(results_df,
                                          output_dir = "outputs/figures",
                                          save_individual = TRUE) {
  logger::log_info("Generating multiverse dashboard...")

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  # Generate all plots
  plots <- list(
    roc_by_model = plot_roc_by_model(results_df),
    fdr_by_stripping = plot_fdr_by_stripping(results_df),
    power_by_sample = plot_power_by_sample_size(results_df),
    spec_curve = plot_specification_curve_detailed(results_df),
    effect_dist = plot_effect_distributions(results_df),
    pvalue_dist = plot_pvalue_distributions(results_df),
    strip_robust = plot_stripping_robustness(results_df),
    sensitivity = plot_sensitivity_heatmap(results_df)
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

  # Create summary dashboard (2x2 of key plots)
  dashboard <- (plots$roc_by_model + plots$power_by_sample) /
    (plots$fdr_by_stripping + plots$strip_robust) +
    plot_annotation(
      title = "Multiverse Analysis Summary",
      subtitle = sprintf("N = %d specifications", nrow(results_df)),
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

# ==============================================================================
# SUMMARY TABLE
# ==============================================================================

#' Generate summary statistics table
#'
create_summary_table <- function(results_df) {
  prepared <- prepare_results(results_df)

  summary_tbl <- prepared %>%
    dplyr::group_by(model_type, transformation, effect_condition) %>%
    dplyr::summarise(
      n = dplyr::n(),
      n_converged = sum(converged_both, na.rm = TRUE),
      pct_significant = mean(is_significant, na.rm = TRUE) * 100,
      mean_effect = mean(main_estimate, na.rm = TRUE),
      sd_effect = sd(main_estimate, na.rm = TRUE),
      median_p = median(main_p_value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      metric_type = dplyr::case_when(
        effect_condition == "present" ~ "Power (TPR)",
        TRUE ~ "FDR"
      )
    ) %>%
    dplyr::arrange(model_type, transformation, effect_condition)

  summary_tbl
}
