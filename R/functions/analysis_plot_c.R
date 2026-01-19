# R/functions/complete_multiverse_plotting_suite_trans_split.R

library(ggplot2)
library(patchwork)
library(dplyr)
library(forcats)
library(ggridges)
library(viridis)
library(tidyr)
library(plotly)
library(htmlwidgets)

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
ensure_figures2_dir <- function(output_dir) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
}

prep_alt_results <- function(results_df) {
  results_df %>%
    mutate(
      converged_both = full_converged & coalesce(null_converged, TRUE),
      is_true_effect = (effect_condition == "present" & strip_method == "none"),
      is_null_effect = (
        (effect_condition == "null_interaction" & strip_method %in% c("shuffle", "qmap_5")) |
          (effect_condition == "null_both" & strip_method == "none")
      ),
      is_significant = main_p_value < 0.05,
      model_type = case_when(
        grepl("rmanova", model) ~ "rmANOVA",
        grepl("full_slope", model) ~ "LMM (full)",
        grepl("cong_slope", model) ~ "LMM (cong slope)",
        grepl("intercept", model) ~ "LMM (intercept)",
        TRUE ~ model
      ),
      strip_label = case_when(
        strip_method == "none" ~ "No stripping",
        strip_method == "shuffle" ~ "Shuffle",
        strip_method == "qmap_5" ~ "QMap",
        TRUE ~ strip_method
      ),
      outlier_label = ifelse(is.na(outlier), "None", as.character(outlier)),
      transformation_label = case_when(
        transformation == "log_rt" ~ "log(RT)",
        transformation == "raw_rt" ~ "Raw RT",
        TRUE ~ transformation
      )
    ) %>%
    filter(converged_both, !error, !is.na(main_estimate)) %>%
    filter(!(effect_condition == "present" & strip_method != "none"))
}

### What's below: ALL plots ALWAYS faceted by transformation_label ###########

plot_effect_ridges <- function(results) {
  lims <- quantile(results$main_estimate, probs = c(0.01, 0.99), na.rm = TRUE)
  ggplot(results, aes(
    x = pmax(pmin(main_estimate, lims[2]), lims[1]),
    y = interaction(model_type, strip_label, outlier_label, drop = TRUE),
    fill = effect_condition
  )) +
    ggridges::geom_density_ridges(
      scale = 1.2, rel_min_height = 0.01, alpha = 0.85
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray30") +
    scale_fill_viridis_d(option = "plasma", name = "Condition") +
    facet_wrap(~transformation_label, scales = "free_x") +
    labs(
      y = "Specification (Model:Strip:Outlier)",
      x = "Effect Estimate (Clamped 1%-99%)"
    ) +
    theme_ridges(grid = TRUE)
}
plot_effect_scatter <- function(results) {
  ggplot(results, aes(
    x = effect_condition,
    y = main_estimate, color = is_significant
  )) +
    geom_violin(fill = NA, alpha = 0.2, color = "gray60") +
    geom_jitter(width = 0.22, height = 0, size = 1.7, alpha = 0.4) +
    facet_grid(model_type ~ transformation_label, scales = "free_y") +
    scale_color_manual(values = c("TRUE" = "#2E7D32", "FALSE" = "#999999")) +
    labs(
      x = "Ground Truth Condition",
      y = "Effect Estimate",
      color = "Significant (p<0.05)"
    ) +
    theme_minimal()
}
plot_multiverse_scatter <- function(results) {
  results <- results %>%
    arrange(main_estimate) %>%
    mutate(
      spec_idx = row_number(),
      is_extreme = abs(main_estimate) > quantile(abs(main_estimate), 0.99, na.rm = TRUE)
    )
  ggplot(results, aes(x = spec_idx, y = main_estimate, color = is_significant)) +
    geom_point(alpha = 0.6, size = 1.3) +
    geom_point(data = filter(results, is_extreme), aes(x = spec_idx, y = main_estimate), color = "red", shape = 8, size = 2.2) +
    facet_wrap(~transformation_label, scales = "free_y") +
    labs(
      x = "Specification Index (sorted by estimate)",
      y = "Main Estimate",
      color = "Significant (p<0.05)"
    ) +
    theme_minimal()
}
plot_pvalue_density <- function(results) {
  ggplot(results, aes(x = main_p_value, fill = effect_condition)) +
    geom_density(alpha = 0.7) +
    geom_vline(xintercept = 0.05, linetype = "dashed", color = "red") +
    scale_fill_viridis_d() +
    facet_wrap(~transformation_label) +
    labs(
      x = "P-value", y = "Density", fill = "Condition"
    ) +
    theme_minimal()
}
plot_estimate_vs_sample_size <- function(results) {
  ggplot(results, aes(x = sample_size, y = main_estimate, color = strip_label, shape = outlier_label)) +
    geom_point(size = 1.5, alpha = 0.45) +
    geom_smooth(method = "loess", se = FALSE, linewidth = 1) +
    facet_wrap(~transformation_label, scales = "free_y") +
    labs(
      x = "Sample Size Proportion", y = "Effect Estimate",
      color = "Stripping", shape = "Outlier"
    ) +
    theme_minimal()
}
plot_roc_by_samplesize <- function(results) {
  ggplot(results, aes(x = sample_size, y = is_significant, group = model_type, color = model_type)) +
    geom_point(alpha = 0.4, size = 1.1, position = position_jitter(width = 0.01)) +
    geom_smooth(method = "loess", span = 0.8, se = FALSE, linewidth = 1.1) +
    scale_y_continuous(
      name = "Significance Proportion (TPR/FDR)", limits = c(0, 1), labels = scales::percent
    ) +
    scale_x_continuous(name = "Sample Size Proportion") +
    facet_wrap(~transformation_label) +
    labs(color = "Model") +
    theme_minimal()
}
# FDR panels
plot_fdr_by_outlier_transform <- function(results) {
  plot_data <- results %>%
    filter(is_null_effect) %>%
    group_by(
      transformation_label, outlier_label, strip_label, model_type, effect_condition
    ) %>%
    summarise(
      n = n(),
      FDR = mean(is_significant, na.rm = TRUE),
      se = sqrt(FDR * (1 - FDR) / n),
      .groups = "drop"
    )
  ggplot(plot_data, aes(
    x = outlier_label, y = FDR,
    fill = strip_label
  )) +
    geom_col(position = position_dodge(width = 0.7), color = "gray70", width = 0.6, alpha = 0.8) +
    geom_errorbar(
      aes(ymin = FDR - se, ymax = FDR + se),
      position = position_dodge(width = 0.7), width = 0.15
    ) +
    geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
    facet_grid(transformation_label ~ effect_condition + model_type, labeller = label_both) +
    scale_fill_viridis_d(option = "B", name = "Stripping") +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    labs(
      x = "Outlier Strategy",
      y = "FDR",
      title = "FDR by Transformation, Outlier, Stripping, Model"
    ) +
    theme_minimal(base_size = 10) +
    theme(axis.text.x = element_text(angle = 35, hjust = 1))
}

# Patchwork dashboards
dashboard_overview <- function(results) {
  (plot_effect_ridges(results) | plot_multiverse_scatter(results)) /
    (plot_roc_by_samplesize(results) | plot_pvalue_density(results)) +
    plot_annotation(
      title = "Multiverse Overview Dashboard (Split by Transformation)",
      subtitle = "Effect distributions, significance rates, and analytic space",
      theme = theme(plot.title = element_text(face = "bold", size = 16))
    )
}
dashboard_effects <- function(results) {
  (plot_effect_scatter(results) | plot_estimate_vs_sample_size(results)) +
    plot_annotation(
      title = "Effect Distributions Dashboard (Transformation-segregated)",
      subtitle = "Estimate scatter/violins and sample size dependence",
      theme = theme(plot.title = element_text(face = "bold", size = 14))
    )
}
dashboard_robustness <- function(results) {
  plot_fdr_by_outlier_transform(results) +
    plot_annotation(
      title = "False Discovery & Robustness Dashboard (Transformation-segregated)",
      theme = theme(plot.title = element_text(face = "bold", size = 14))
    )
}
dashboard_significance <- function(results) {
  (plot_pvalue_density(results) | plot_roc_by_samplesize(results)) +
    plot_annotation(
      title = "Significance and Power Dashboard (Faceted by Transformation)",
      subtitle = "Distributional and sample size effects",
      theme = theme(plot.title = element_text(face = "bold", size = 13))
    )
}

### Plotly 3D FDR interactive: separate panel for each transformation
compute_fdr_table <- function(results) {
  results %>%
    filter(is_null_effect) %>%
    group_by(
      transformation_label, sample_size, outlier_label, strip_label, model_type, effect_condition
    ) %>%
    summarise(
      n = n(),
      FDR = mean(is_significant, na.rm = TRUE),
      .groups = "drop"
    )
}
plotly_fdr_3d <- function(tbl, transformation_value) {
  subset_tbl <- tbl %>% filter(transformation_label == transformation_value)
  plot_ly(
    subset_tbl,
    x = ~sample_size,
    y = ~outlier_label,
    z = ~FDR,
    color = ~strip_label,
    symbol = ~model_type,
    symbols = levels(factor(subset_tbl$model_type)),
    type = "scatter3d", mode = "markers",
    size = ~FDR, sizes = c(10, 42),
    text = ~ paste0(
      "Sample Size: ", round(sample_size, 2),
      "<br>Outlier: ", outlier_label,
      "<br>Stripping: ", strip_label,
      "<br>Model: ", model_type,
      "<br>Null Condition: ", effect_condition,
      "<br>FDR: ", scales::percent(FDR, accuracy = 0.1)
    ),
    hoverinfo = "text"
  ) %>%
    layout(
      title = paste0("FDR (Null Design Only) - ", transformation_value),
      scene = list(
        xaxis = list(title = "Sample Size Proportion", tickformat = ".0%"),
        yaxis = list(title = "Outlier"),
        zaxis = list(title = "FDR", range = c(0, 1)),
        camera = list(eye = list(x = 2, y = 1.4, z = 1.2))
      ),
      legend = list(title = list(text = "Stripping / Model"))
    )
}

cont_plots <- function(results_df, output_dir) {
  ensure_figures2_dir(output_dir)
  results <- prep_alt_results(results_df)

  # Core dashboards
  plot_save_fallback(file.path(output_dir, "dashboard_overview"), dashboard_overview(results), width = 18, height = 12)
  plot_save_fallback(file.path(output_dir, "dashboard_effects"), dashboard_effects(results), width = 16, height = 8)
  plot_save_fallback(file.path(output_dir, "dashboard_robustness"), dashboard_robustness(results), width = 16, height = 13)
  plot_save_fallback(file.path(output_dir, "dashboard_significance"), dashboard_significance(results), width = 14, height = 8)

  # Plotly 3D FDR: one html per transformation
  fdr_tbl <- compute_fdr_table(results)
  for (tr in unique(fdr_tbl$transformation_label)) {
    fdr_plotly <- plotly_fdr_3d(fdr_tbl, tr)
    html_path <- file.path(output_dir, paste0("fdr_3d_plotly_", gsub("[^A-Za-z0-9]", "_", tr), ".html"))
    htmlwidgets::saveWidget(fdr_plotly, file = html_path, selfcontained = TRUE)
    message("Interactive 3D FDR plot saved as: ", html_path)
  }
}
