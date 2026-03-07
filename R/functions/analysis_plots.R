# R/functions/analysis_plots.R
# Multiverse Visualization
#
# Consumed tables (from analyze_multiverse_results):
#   roc_metrics, power_coarse, power_by_sample_size, power_by_outlier,
#   fpr_coarse, fpr_by_sample_size, fpr_by_outlier, branch_health,
#   estimate_summary, spec_curve, spec_inconsistencies, summary_table,
#   model_diagnostics, results_with_diag,
#   by_outlier, by_sample_size, by_model,
#   or_*  (odds-ratio tables from model_multiverse),
#   coef_* (coefficient tables)

# library(ggplot2)
# library(dplyr)
# library(tidyr)
# library(patchwork)

# ==============================================================================
# SAVE HELPER
# ==============================================================================

#' Save plot with format fallback (SVG → PDF → PNG)
#' @param filepath Base path WITHOUT extension
#' @param plot ggplot object
#' @param width Width in inches
#' @param height Height in inches
#' @param dpi Resolution for raster fallback
plot_save_fallback <- function(filepath, plot, width = 10, height = 7, dpi = 300) {
  # Strip any existing extension so we don't double up
  filepath <- tools::file_path_sans_ext(filepath)

  saved <- FALSE
  for (ext in c("svg", "pdf", "png")) {
    result <- tryCatch(
      {
        ggsave(
          filename = paste0(filepath, ".", ext),
          plot = plot, device = ext,
          width = width, height = height, dpi = dpi
        )
        TRUE
      },
      error = function(e) FALSE
    )
    if (result) {
      log_pipeline(logger::INFO, "Saved plot: {filepath}.{ext}")
      saved <- TRUE
      break
    }
  }
  if (!saved) {
    log_pipeline(logger::WARN, "Could not save plot in any format: {filepath}")
  }
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
    "null_interaction" = "#C62828",
    "null_both"        = "#6A1B9A"
  )
}

get_null_type_palette <- function() {
  c(
    "null_interaction:shuffle" = "#1976D2",
    "null_interaction:qmap_5"  = "#F57C00",
    "null_both"                = "#6A1B9A"
  )
}

get_transformation_labels <- function() {
  c("log_rt" = "log(RT)", "no_log_rt" = "Raw RT")
}

# ==============================================================================
# THEME
# ==============================================================================

theme_multiverse <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold", size = base_size + 2),
      plot.subtitle = element_text(color = "grey40")
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
    dplyr::filter(!is.na(TPR), !is.na(FPR))

  ggplot(df, aes(x = FPR, y = TPR, color = model_type, group = model_type)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "grey50") +
    geom_vline(xintercept = 0.05, linetype = "dashed", color = "red", alpha = 0.5) +
    geom_path(linewidth = 0.8) +
    geom_point(aes(size = sample_size), alpha = 0.8) +
    ggrepel::geom_text_repel(
      aes(label = scales::percent(sample_size, accuracy = 1)),
      size = 2.5, max.overlaps = 10,
      show.legend = FALSE, segment.alpha = 0.3
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
      rows = vars(null_type),
      cols = vars(transformation),
      labeller = labeller(transformation = get_transformation_labels())
    ) +
    labs(
      title = "ROC Curves by Model Type",
      subtitle = "Points sized by sample fraction; dashed line = nominal α = 0.05",
      x = "False Positive Rate",
      y = "True Positive Rate"
    ) +
    theme_multiverse()
}

# ==============================================================================
# PLOT 2: ROC CURVES BY OUTLIER METHOD
# ==============================================================================

#' ROC curves colored by outlier method, shaped by model
#' Consumes: roc_metrics
plot_roc_by_outlier <- function(roc_metrics) {
  df <- roc_metrics %>%
    dplyr::filter(!is.na(TPR), !is.na(FPR))

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
      rows = vars(null_type),
      cols = vars(transformation),
      labeller = labeller(transformation = get_transformation_labels())
    ) +
    labs(
      title = "ROC Curves by Outlier Strategy",
      subtitle = "Shape = model type; dashed line = nominal α = 0.05",
      x = "False Positive Rate",
      y = "True Positive Rate"
    ) +
    theme_multiverse()
}

# ==============================================================================
# PLOT 3: FPR BY NULL TYPE (replaces broken strip-method plot)
# ==============================================================================

#' FPR comparison across null types and models
#' Consumes: fpr_coarse
#' Columns: null_type, model_type, transformation, n, false_positives, FPR
plot_fpr_by_null_type <- function(fpr_coarse, palette = get_null_type_palette()) {
  ggplot(fpr_coarse, aes(x = model_type, y = FPR, fill = null_type)) +
    geom_col(position = position_dodge(0.8), width = 0.7, alpha = 0.85) +
    geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
    geom_text(
      aes(label = scales::percent(FPR, accuracy = 0.1)),
      position = position_dodge(0.8), vjust = -0.5, size = 2.5
    ) +
    scale_fill_manual(values = palette, name = "Null Type") +
    scale_y_continuous(
      limits = c(0, NA), labels = scales::percent,
      expand = expansion(mult = c(0, 0.15))
    ) +
    facet_wrap(
      vars(transformation),
      labeller = labeller(transformation = get_transformation_labels())
    ) +
    labs(
      title = "False Positive Rate by Null Generation Method",
      subtitle = "Red dashed line = nominal α = 0.05; bars grouped by null type",
      x = "Model",
      y = "False Positive Rate"
    ) +
    theme_multiverse() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
}

# ==============================================================================
# PLOT 4: FPR BY SAMPLE SIZE (new — exploits fpr_by_sample_size table)
# ==============================================================================

#' FPR × sample size curves per model, faceted by null_type and transformation
#' Consumes: fpr_by_sample_size
plot_fpr_by_sample_size <- function(fpr_by_ss, palette = get_model_palette()) {
  ggplot(fpr_by_ss, aes(
    x = sample_size, y = FPR, color = model_type, group = model_type
  )) +
    geom_hline(yintercept = 0.05, linetype = "dashed", color = "red", alpha = 0.5) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2.5) +
    scale_color_manual(values = palette, name = "Model", drop = FALSE) +
    scale_x_continuous(labels = scales::percent, breaks = seq(0.1, 1, 0.1)) +
    scale_y_continuous(labels = scales::percent) +
    facet_grid(
      rows = vars(null_type),
      cols = vars(transformation),
      labeller = labeller(transformation = get_transformation_labels())
    ) +
    labs(
      title = "False Positive Rate by Sample Size",
      subtitle = "Red dashed line = nominal α; does FPR inflate at small N?",
      x = "Sample Fraction",
      y = "FPR"
    ) +
    theme_multiverse()
}

# ==============================================================================
# PLOT 5: POWER BY SAMPLE SIZE
# ==============================================================================

#' Power curves (TPR when effect present) by sample size and model
#' Consumes: power_by_sample_size
plot_power_by_sample_size <- function(power_by_ss, palette = get_model_palette()) {
  ggplot(power_by_ss, aes(
    x = sample_size, y = power, color = model_type, group = model_type
  )) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 3) +
    scale_color_manual(values = palette, name = "Model", drop = FALSE) +
    scale_x_continuous(labels = scales::percent, breaks = seq(0.1, 1, 0.1)) +
    scale_y_continuous(
      limits = c(0, 1), labels = scales::percent, breaks = seq(0, 1, 0.2)
    ) +
    facet_wrap(
      vars(transformation),
      labeller = labeller(transformation = get_transformation_labels())
    ) +
    labs(
      title = "Statistical Power by Sample Size",
      subtitle = "True positive rate when interaction effect is present",
      x = "Sample Fraction",
      y = "Power (TPR)"
    ) +
    theme_multiverse()
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
        "null_interaction" = "Null (interaction removed)",
        "null_both" = "Null (all effects removed)"
      ),
      name = "Condition"
    ) +
    facet_grid(
      rows = vars(model_type),
      cols = vars(transformation),
      labeller = labeller(transformation = get_transformation_labels()),
      scales = "free_y"
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
    dplyr::mutate(
      null_type_label = dplyr::coalesce(null_type, "effect present")
    )

  ggplot(df, aes(x = main_p_value, fill = effect_condition)) +
    geom_histogram(bins = 20, alpha = 0.7, position = "identity", color = "white", linewidth = 0.1) +
    geom_vline(xintercept = 0.05, linetype = "dashed", color = "red") +
    scale_fill_manual(values = palette, name = "Condition") +
    facet_grid(
      rows = vars(null_type_label),
      cols = vars(model_type),
      scales = "free_y"
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
plot_stripping_robustness <- function(fpr_by_ss) {
  # Extract strip method from null_type for null_interaction only
  df <- fpr_by_ss %>%
    dplyr::filter(grepl("^null_interaction:", null_type)) %>%
    dplyr::mutate(
      strip_method = sub("^null_interaction:", "", null_type)
    )

  # Pivot to get shuffle vs qmap_5 side by side
  wide <- df %>%
    dplyr::select(model_type, sample_size, transformation, strip_method, FPR) %>%
    tidyr::pivot_wider(names_from = strip_method, values_from = FPR)

  # If both methods present, scatter them
  if (all(c("shuffle", "qmap_5") %in% names(wide))) {
    p <- ggplot(wide, aes(x = qmap_5, y = shuffle)) +
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
        title = "Stripping Method Robustness",
        subtitle = "Points on diagonal → stripping method does not influence FPR",
        x = "FPR (Quantile Mapping, κ=5)",
        y = "FPR (Shuffle)"
      ) +
      theme_multiverse()
  } else {
    # Fallback: bar chart
    p <- ggplot(df, aes(x = strip_method, y = FPR, fill = model_type)) +
      geom_col(position = "dodge") +
      geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
      scale_fill_manual(values = get_model_palette(), name = "Model", drop = FALSE) +
      scale_y_continuous(labels = scales::percent) +
      facet_wrap(vars(transformation)) +
      labs(title = "FPR by Stripping Method", x = "Method", y = "FPR") +
      theme_multiverse()
  }

  p
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
    dplyr::group_by(model_type, transformation, sample_size, outlier) %>%
    dplyr::summarise(
      n = dplyr::n(),
      power = mean(significant, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      ss_label = scales::percent(sample_size, accuracy = 1)
    )

  ggplot(heat_df, aes(x = outlier, y = model_type, fill = power)) +
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
      cols = vars(transformation),
      labeller = labeller(transformation = get_transformation_labels())
    ) +
    labs(
      title = "Power Sensitivity to Analytic Choices",
      subtitle = "Rows = sample size; columns = transformation",
      x = "Outlier Method",
      y = "Model"
    ) +
    theme_multiverse(base_size = 10) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
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
    dplyr::mutate(
      n_singular_only = n_singular,
      n_nonconv_only = n_converged - n_usable + n_singular, # non-converged but not singular
      n_nonconv_only = pmax(n_no_error - n_converged, 0),
      label = paste(model_type, transformation, sep = "\n")
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
      null_type_label = dplyr::coalesce(null_type, effect_condition)
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
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
}

# ==============================================================================
# PLOT 12: MODEL DIAGNOSTICS — d' and Youden's J (new)
# ==============================================================================

#' Signal detection metrics from ROC: d' and Youden's J
#' Consumes: roc_metrics
plot_signal_detection <- function(roc_metrics, palette = get_model_palette()) {
  df <- roc_metrics %>%
    dplyr::filter(!is.na(d_prime), is.finite(d_prime))

  p_dprime <- ggplot(df, aes(x = sample_size, y = d_prime, color = model_type)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    scale_color_manual(values = palette, name = "Model", drop = FALSE) +
    scale_x_continuous(labels = scales::percent) +
    facet_grid(
      rows = vars(null_type),
      cols = vars(transformation),
      labeller = labeller(transformation = get_transformation_labels())
    ) +
    labs(
      title = "Discriminability (d')",
      subtitle = "Higher = better separation between true and null effects",
      x = "Sample Fraction",
      y = "d'"
    ) +
    theme_multiverse()

  p_youden <- ggplot(df, aes(x = sample_size, y = youden_j, color = model_type)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    scale_color_manual(values = palette, name = "Model", drop = FALSE) +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(limits = c(-0.1, 1)) +
    facet_grid(
      rows = vars(null_type),
      cols = vars(transformation),
      labeller = labeller(transformation = get_transformation_labels())
    ) +
    labs(
      title = "Youden's J Index (TPR − FPR)",
      x = "Sample Fraction",
      y = "J"
    ) +
    theme_multiverse()

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
    log_pipeline(logger::WARN, "No odds ratio tables found for plotting")
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
      scales = "free_y"
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
  log_pipeline(logger::INFO, "Generating multiverse dashboard...")

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  # Extract tables
  roc_metrics <- analysis_list$roc_metrics
  roc_by_outlier <- analysis_list$roc_metrics_by_outlier
  fpr_coarse <- analysis_list$fpr_coarse
  fpr_by_ss <- analysis_list$fpr_by_sample_size
  power_by_ss <- analysis_list$power_by_sample_size
  power_by_out <- analysis_list$power_by_outlier
  branch_health <- analysis_list$branch_health
  spec_curve <- analysis_list$spec_curve
  spec_incon <- analysis_list$spec_inconsistencies
  prepared_df <- analysis_list$results_with_diag

  # Build all plots, catching errors so one failure doesn't kill the dashboard
  safe_plot <- function(expr, name) {
    tryCatch(
      expr,
      error = function(e) {
        log_pipeline(logger::WARN, "Plot '{name}' failed: {e$message}")
        ggplot() +
          theme_void() +
          labs(title = paste("Error:", name), subtitle = e$message)
      }
    )
  }

  plots <- list(
    roc_by_model       = safe_plot(plot_roc_by_model(roc_metrics), "roc_by_model"),
    roc_by_outlier     = safe_plot(plot_roc_by_outlier(roc_by_outlier), "roc_by_outlier"),
    fpr_by_null_type   = safe_plot(plot_fpr_by_null_type(fpr_coarse), "fpr_by_null_type"),
    fpr_by_sample_size = safe_plot(plot_fpr_by_sample_size(fpr_by_ss), "fpr_by_sample_size"),
    power_by_sample    = safe_plot(plot_power_by_sample_size(power_by_ss), "power_by_sample"),
    spec_curve         = safe_plot(plot_specification_curve(spec_curve), "spec_curve"),
    effect_dist        = safe_plot(plot_effect_distributions(prepared_df), "effect_dist"),
    pvalue_dist        = safe_plot(plot_pvalue_distributions(prepared_df), "pvalue_dist"),
    strip_robust       = safe_plot(plot_stripping_robustness(fpr_by_ss), "strip_robust"),
    sensitivity        = safe_plot(plot_sensitivity_heatmap(power_by_out), "sensitivity"),
    branch_health      = safe_plot(plot_branch_health(branch_health), "branch_health"),
    signal_detection   = safe_plot(plot_signal_detection(roc_metrics), "signal_detection"),
    odds_ratios        = safe_plot(plot_odds_ratios(analysis_list), "odds_ratios"),
    spec_inconsistency = safe_plot(plot_spec_inconsistencies(spec_incon), "spec_inconsistency")
  )

  # Save individual plots
  if (save_individual) {
    for (name in names(plots)) {
      filepath <- file.path(output_dir, name)
      width <- if (name %in% c("spec_curve", "pvalue_dist", "signal_detection")) 14 else 10
      height <- if (name %in% c("spec_curve", "sensitivity", "signal_detection", "odds_ratios")) 10 else 7
      plot_save_fallback(filepath, plots[[name]], width = width, height = height)
    }
  }

  # Summary dashboard (key plots)
  dashboard <- (plots$roc_by_model + plots$power_by_sample) /
    (plots$fpr_by_null_type + plots$strip_robust) /
    (plots$branch_health + plots$effect_dist) +
    plot_annotation(
      title = "Multiverse Analysis Summary",
      subtitle = sprintf(
        "%d ROC groupings | %d usable branches",
        nrow(roc_metrics),
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
    width = 18, height = 16
  )

  # Robustness dashboard
  robustness <- (plots$strip_robust + plots$fpr_by_sample_size) /
    (plots$spec_inconsistency + plots$signal_detection) +
    plot_annotation(
      title = "Robustness & Sensitivity Analysis",
      subtitle = "Does the conclusion depend on arbitrary analytic choices?"
    )

  plot_save_fallback(
    file.path(output_dir, "dashboard_robustness"),
    robustness,
    width = 16, height = 14
  )

  # Diagnostics dashboard
  diagnostics <- (plots$branch_health + plots$pvalue_dist) /
    plots$odds_ratios +
    plot_annotation(
      title = "Pipeline Diagnostics",
      subtitle = "Convergence, p-value calibration, and multiverse model coefficients"
    )

  plot_save_fallback(
    file.path(output_dir, "dashboard_diagnostics"),
    diagnostics,
    width = 16, height = 14
  )

  plots$dashboard_summary <- dashboard
  plots$dashboard_robustness <- robustness
  plots$dashboard_diagnostics <- diagnostics

  log_pipeline(
    logger::INFO,
    "Dashboard generation complete: {length(plots)} plots created"
  )

  invisible(plots)
}
