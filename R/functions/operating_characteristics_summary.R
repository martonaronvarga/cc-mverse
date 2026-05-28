# R/functions/operating_characteristics_summary.R - conditional/unconditional/failure-aware rates

classify_truth_condition <- function(condition) {
  ifelse(condition == "null_location", "known_null",
    ifelse(condition == "present_location", "known_alternative", NA_character_)
  )
}

summarise_operating_characteristics <- function(results_path) {
  if (!file.exists(results_path)) {
    stop("Results file not found: ", results_path)
  }
  df <- read.csv(results_path, stringsAsFactors = FALSE)

  required <- c("model", "condition", "replicate_id", "truth_interaction_log", "error", "p_value", "is_significant")
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }

  df$truth_type <- classify_truth_condition(df$condition)
  if (any(is.na(df$truth_type))) {
    stop("Unknown condition values: ", paste(unique(df$condition[is.na(df$truth_type)]), collapse = ", "))
  }

  df$error <- as.logical(df$error)
  df$is_significant <- as.logical(df$is_significant)
  df$usable <- !df$error & !is.na(df$p_value)
  df$significant_usable <- df$usable & df$is_significant
  df$usable_non_significant <- df$usable & !df$is_significant

  groups <- unique(df[c("truth_type", "condition", "truth_interaction_log", "model")])
  groups <- groups[order(groups$truth_type, groups$condition, groups$truth_interaction_log, groups$model), , drop = FALSE]

  rows <- lapply(seq_len(nrow(groups)), function(i) {
    g <- groups[i, , drop = FALSE]
    x <- df[
      df$truth_type == g$truth_type &
        df$condition == g$condition &
        df$truth_interaction_log == g$truth_interaction_log &
        df$model == g$model,
      ,
      drop = FALSE
    ]
    n_planned <- nrow(x)
    n_usable <- sum(x$usable)
    n_significant <- sum(x$significant_usable)
    n_usable_non_significant <- sum(x$usable_non_significant)
    n_error <- sum(x$error)
    n_missing_p <- sum(!x$error & is.na(x$p_value))

    rate_name <- if (g$truth_type == "known_null") "FPR" else "TPR"
    unconditional_rate <- if (n_planned > 0) n_significant / n_planned else NA_real_
    conditional_rate <- if (n_usable > 0) n_significant / n_usable else NA_real_

    data.frame(
      truth_type = g$truth_type,
      condition = g$condition,
      truth_interaction_log = g$truth_interaction_log,
      model = g$model,
      rate_name = rate_name,
      n_planned = n_planned,
      n_usable = n_usable,
      n_significant = n_significant,
      n_usable_non_significant = n_usable_non_significant,
      n_error = n_error,
      n_missing_p = n_missing_p,
      unconditional_rate = unconditional_rate,
      conditional_rate = conditional_rate,
      pct_significant = n_significant / n_planned,
      pct_usable_non_significant = n_usable_non_significant / n_planned,
      pct_error = n_error / n_planned,
      pct_missing_p = n_missing_p / n_planned,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

write_operating_characteristics_summary <- function(
  results_path = file.path("outputs", "analysis", "smoke_operating_characteristics.csv"),
  output_dir = dirname(results_path),
  output_name = "smoke_operating_characteristics_summary.csv"
) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  summary <- summarise_operating_characteristics(results_path)
  output_path <- file.path(output_dir, output_name)
  write.csv(summary, output_path, row.names = FALSE)
  invisible(output_path)
}
