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
plot_save_fallback <- function(filepath, plot, width = 10, height = 7, dpi = 300) {
  filepath <- tools::file_path_sans_ext(filepath)

  devices <- list()
  if (requireNamespace("svglite", quietly = TRUE)) {
    devices$svg <- function(filename) ggplot2::ggsave(filename, plot = plot, device = svglite::svglite, width = width, height = height, dpi = dpi, limitsize = FALSE, bg = "white")
  }
  devices$pdf <- function(filename) ggplot2::ggsave(filename, plot = plot, device = grDevices::pdf, width = width, height = height, dpi = dpi, limitsize = FALSE, bg = "white")
  if (requireNamespace("ragg", quietly = TRUE)) {
    devices$png <- function(filename) ggplot2::ggsave(filename, plot = plot, device = ragg::agg_png, width = width, height = height, dpi = dpi, limitsize = FALSE, bg = "white")
  }

  for (ext in names(devices)) {
    ok <- tryCatch({ devices[[ext]](paste0(filepath, ".", ext)); TRUE }, error = function(e) FALSE)
    if (ok) {
      if (exists("log_pipeline", mode = "function")) log_pipeline(logger::INFO, "Saved plot: {filepath}.{ext}")
      return(invisible(filepath))
    }
  }
  if (exists("log_pipeline", mode = "function")) log_pipeline(logger::WARN, "Could not save plot in any headless-safe format: {filepath}")
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
    "null_interaction:additive_qmap"  = "#F57C00",
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

prepare_plot_labels <- function(df) {
  if (is.null(df) || !is.data.frame(df)) return(df)
  df %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(c("model_type", "null_type", "outlier", "transformation", "strip_method")),
        as.character
      ),
      null_type_label = if ("null_type" %in% names(.)) wrap_label(dplyr::coalesce(.data$null_type, "effect present"), 28) else NA_character_,
      model_type_label = if ("model_type" %in% names(.)) wrap_label(.data$model_type, 22) else NA_character_,
      transformation_label = dplyr::case_when(
        "transformation" %in% names(.) & .data$transformation == "log_rt" ~ "log(RT)",
        "transformation" %in% names(.) & .data$transformation %in% c("no_log_rt", "raw_rt") ~ "Raw RT",
        "transformation" %in% names(.) ~ wrap_label(.data$transformation, 18),
        TRUE ~ NA_character_
      ),
      outlier_label = if ("outlier" %in% names(.)) wrap_label(dplyr::coalesce(.data$outlier, "none"), 14) else NA_character_,
      strip_method_label = if ("strip_method" %in% names(.)) wrap_label(.data$strip_method, 20) else NA_character_
    )
}

theme_multiverse <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      legend.position = "bottom",
      legend.box = "vertical",
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

legend_bottom_rows <- function(nrow = 2) {
  guides(
    color = guide_legend(nrow = nrow, byrow = TRUE),
    fill = guide_legend(nrow = nrow, byrow = TRUE),
    shape = guide_legend(nrow = nrow, byrow = TRUE),
    size = guide_legend(nrow = nrow, byrow = TRUE)
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
    legend_bottom_rows(2)
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
    legend_bottom_rows(2)
}

# ==============================================================================
# PLOT 3: FPR BY NULL TYPE (replaces broken strip-method plot)
# ==============================================================================

#' FPR comparison across null types and models
#' Consumes: fpr_coarse
#' Columns: null_type, model_type, transformation, n, false_positives, FPR
plot_fpr_by_null_type <- function(fpr_coarse, palette = get_null_type_palette()) {
  df <- prepare_plot_labels(fpr_coarse)
  ggplot(df, aes(x = model_type_label, y = FPR, fill = null_type)) +
    geom_col(position = position_dodge(0.8), width = 0.7, alpha = 0.85) +
    geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
    geom_text(
      aes(label = scales::percent(FPR, accuracy = 0.1)),
      position = position_dodge(0.8), vjust = -0.5, size = 2.5
    ) +
    scale_fill_manual(values = palette, name = "Null Type", labels = wrap_discrete_labels(26)) +
    scale_y_continuous(
      limits = c(0, NA), labels = scales::percent,
      expand = expansion(mult = c(0, 0.15))
    ) +
    facet_wrap(vars(transformation_label)) +
    labs(
      title = "False Positive Rate by Null Generation Method",
      subtitle = "Red dashed line = nominal α = 0.05; bars grouped by null type",
      x = "Model",
      y = "False Positive Rate"
    ) +
    theme_multiverse() +
    legend_bottom_rows(2) +
    theme(axis.text.x = element_text(angle = 20, hjust = 1, vjust = 1))
}

# ==============================================================================
# PLOT 4: FPR BY SAMPLE SIZE (new — exploits fpr_by_sample_size table)
# ==============================================================================

#' FPR × sample size curves per model, faceted by null_type and transformation
#' Consumes: fpr_by_sample_size
plot_fpr_by_sample_size <- function(fpr_by_ss, palette = get_model_palette()) {
  df <- prepare_plot_labels(fpr_by_ss)
  ggplot(df, aes(
    x = sample_size, y = FPR, color = model_type, group = model_type
  )) +
    geom_hline(yintercept = 0.05, linetype = "dashed", color = "red", alpha = 0.5) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2.5) +
    scale_color_manual(values = palette, name = "Model", drop = FALSE) +
    scale_x_continuous(labels = scales::percent, breaks = seq(0.1, 1, 0.1)) +
    scale_y_continuous(labels = scales::percent) +
    facet_grid(
      rows = vars(null_type_label),
      cols = vars(transformation_label)
    ) +
    labs(
      title = "False Positive Rate by Sample Size",
      subtitle = "Red dashed line = nominal α; does FPR inflate at small N?",
      x = "Sample Fraction",
      y = "FPR"
    ) +
    theme_multiverse() +
    legend_bottom_rows(2)
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
    facet_wrap(vars(transformation_label)) +
    labs(
      title = "Statistical Power by Sample Size",
      subtitle = "True positive rate when interaction effect is present",
      x = "Sample Fraction",
      y = "Power (TPR)"
    ) +
    theme_multiverse() +
    legend_bottom_rows(2)
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

  # Pivot to get shuffle vs additive_qmap side by side
  wide <- df %>%
    dplyr::select(model_type, sample_size, transformation, strip_method, FPR) %>%
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
        title = "Stripping Method Robustness",
        subtitle = "Points on diagonal → stripping method does not influence FPR",
        x = "FPR (Additive qmap)",
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
      null_type_label = wrap_label(dplyr::coalesce(null_type, effect_condition), 28)
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
    legend_bottom_rows(2)

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
    legend_bottom_rows(2)

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
    facet_grid(rows = vars(null_type_label), cols = vars(transformation_label)) +
    labs(
      title = "Operating-Characteristic Frontier",
      subtitle = "Best branches move upward (power) without crossing the 5% FPR guide",
      x = "False positive rate under nullified empirical data",
      y = "Detection rate under present empirical data"
    ) +
    theme_multiverse(base_size = 10) +
    legend_bottom_rows(2)
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
    legend_bottom_rows(2)
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
      nullification_verdict = dplyr::coalesce(.data$nullification_verdict, "diagnostics_missing"),
      strip_method = dplyr::recode(.data$strip_method,
        additive_qmap_trial_bin = "qmap trial-bin",
        local_mean_residual = "local mean",
        local_median_residual = "local median",
        additive_qmap = "qmap",
        .default = .data$strip_method
      )
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
    facet_grid(rows = vars(strip_method_label), cols = vars(transformation_label), scales = "free_y", space = "free_y") +
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
    legend_bottom_rows(2) +
    theme(axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1))
}

plot_failure_composition_lollipop <- function(analysis_list) {
  rates <- analysis_list$nullification_failure_aware_rates
  if (is.null(rates) || !is.data.frame(rates) || nrow(rates) == 0L) {
    stop("nullification_failure_aware_rates is missing or empty")
  }
  metric_cols <- intersect(
    c("significant_prop", "usable_nonsignificant_prop", "singular_prop", "nonconverged_prop", "error_prop"),
    names(rates)
  )
  if (length(metric_cols) == 0L) stop("No failure composition proportion columns found")

  rates %>%
    prepare_plot_labels() %>%
    tidyr::pivot_longer(dplyr::all_of(metric_cols), names_to = "component", values_to = "proportion") %>%
    dplyr::mutate(
      component = dplyr::recode(.data$component,
        significant_prop = "Significant",
        usable_nonsignificant_prop = "Usable non-significant",
        singular_prop = "Singular",
        nonconverged_prop = "Non-converged",
        error_prop = "Error"
      ),
      component = factor(.data$component, levels = c("Significant", "Usable non-significant", "Singular", "Non-converged", "Error"))
    ) %>%
    ggplot(aes(x = proportion, y = component, color = component)) +
    geom_segment(aes(x = 0, xend = proportion, yend = component), linewidth = 0.8, alpha = 0.8) +
    geom_point(size = 2.7) +
    facet_grid(rows = vars(model_type_label), cols = vars(transformation_label)) +
    scale_x_continuous(labels = scales::percent, limits = c(0, 1)) +
    scale_color_manual(values = c(
      "Significant" = "#C62828",
      "Usable non-significant" = "#2E7D32",
      "Singular" = "#F9A825",
      "Non-converged" = "#EF6C00",
      "Error" = "#6A1B9A"
    ), guide = "none") +
    labs(
      title = "Failure-Aware Nullification Composition",
      subtitle = "FPR is only one component; singular and error rates remain visible",
      x = "Proportion of planned branches",
      y = NULL
    ) +
    theme_multiverse(base_size = 10)
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
    spec_inconsistency = safe_plot(plot_spec_inconsistencies(spec_incon), "spec_inconsistency"),
    nullifier_matrix  = safe_plot(plot_nullifier_verdict_matrix(prepared_df), "nullifier_matrix"),
    failure_lollipop  = safe_plot(plot_failure_composition_lollipop(analysis_list), "failure_lollipop"),
    oc_frontier       = safe_plot(plot_power_fpr_frontier(analysis_list), "oc_frontier"),
    attenuation       = safe_plot(plot_empirical_attenuation(prepared_df), "attenuation")
  )

  # Save individual plots
  if (save_individual) {
    for (name in names(plots)) {
      filepath <- file.path(output_dir, name)
      width <- if (name %in% c("spec_curve", "pvalue_dist", "signal_detection", "nullifier_matrix", "failure_lollipop", "oc_frontier", "attenuation")) 14 else 10
      height <- if (name %in% c("spec_curve", "sensitivity", "signal_detection", "odds_ratios", "nullifier_matrix", "oc_frontier", "attenuation")) 10 else 7
      plot_save_fallback(filepath, plots[[name]], width = width, height = height)
    }
  }

  # Summary dashboard (key plots)
  dashboard <- (plots$oc_frontier + plots$attenuation) /
    (plots$fpr_by_null_type + plots$nullifier_matrix) /
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
  robustness <- (plots$oc_frontier + plots$strip_robust) /
    (plots$attenuation + plots$signal_detection) /
    (plots$spec_inconsistency + plots$failure_lollipop) +
    plot_annotation(
      title = "Robustness & Sensitivity Analysis",
      subtitle = "Does the conclusion depend on arbitrary analytic choices?"
    )

  plot_save_fallback(
    file.path(output_dir, "dashboard_robustness"),
    robustness,
    width = 18, height = 18
  )

  # Diagnostics dashboard
  diagnostics <- (plots$branch_health + plots$pvalue_dist) /
    (plots$failure_lollipop + plots$odds_ratios) +
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
