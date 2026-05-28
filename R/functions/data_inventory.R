# R/functions/data_inventory.R - raw/combined data inventory and confound risk

infer_inventory_study <- function(participant_id) {
  sub("^.*_", "", as.character(participant_id))
}

build_column_inventory <- function(input_path = "all_indexed.csv") {
  if (!file.exists(input_path)) stop("Input file not found: ", input_path)
  df <- read.csv(input_path, stringsAsFactors = FALSE, check.names = FALSE)
  names(df)[names(df) == ""] <- "export_row_index"
  if (!"participant_id" %in% names(df)) stop("participant_id column is required")
  df$study <- infer_inventory_study(df$participant_id)

  rows <- lapply(names(df), function(col) {
    x <- df[[col]]
    missing <- is.na(x) | as.character(x) %in% c("", "NA", "NaN", "NULL")
    by_study <- tapply(!missing, df$study, any)
    examples <- unique(as.character(x[!missing]))
    examples <- examples[seq_len(min(length(examples), 5L))]
    data.frame(
      column = col,
      rows = length(x),
      missing_n = sum(missing),
      missing_rate = sum(missing) / length(x),
      n_unique_observed = length(unique(as.character(x[!missing]))),
      studies_present = paste(names(by_study)[by_study], collapse = ";"),
      example_values = paste(examples, collapse = ";"),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

build_confound_risk_table <- function(column_inventory) {
  has_col <- function(cols) any(cols %in% column_inventory$column)
  missing_rate <- function(col) {
    if (!col %in% column_inventory$column) return(NA_real_)
    column_inventory$missing_rate[column_inventory$column == col][[1]]
  }

  data.frame(
    confound = c(
      "previous congruency",
      "previous accuracy/post-error slowing",
      "current accuracy filtering",
      "trial index/fatigue/practice trend",
      "block effects",
      "practice trials",
      "stimulus identity/repetition",
      "response identity/repetition",
      "feature/event-file integration",
      "contingency learning/item-specific proportion congruency",
      "study-specific affect/conflict condition"
    ),
    available_columns = c(
      "prev_cong",
      "prev_correct",
      "correct",
      "trial_index; is_practice",
      "block_index; name; condition",
      "is_practice; name",
      "not retained in all_indexed; only coarse name/congruency in some studies",
      "not retained in all_indexed",
      "not retained in all_indexed",
      "not retained in all_indexed",
      "condition; congruency; name"
    ),
    status = c(
      if (has_col("prev_cong")) "available_but_recompute_required" else "missing",
      if (has_col("prev_correct")) "available_but_recompute_required" else "missing",
      if (has_col("correct")) "available" else "missing",
      if (has_col("trial_index")) "available" else "missing",
      if (has_col("block_index")) "available_study_specific" else "missing",
      if (has_col("is_practice")) "available_study_sparse" else "missing",
      "insufficient",
      "missing",
      "missing",
      "missing",
      "available_study_sparse"
    ),
    risk_for_cse_interpretation = c(
      "core design variable; current column may contain participant-boundary lag leakage",
      "post-error slowing can masquerade as CSE if not conditioned or filtered",
      "accuracy filtering changes cell counts and can induce selection effects",
      "slow drift/fatigue/practice can confound unrestricted nullification",
      "block meanings differ by study and require study-aware handling",
      "practice coding is missing for some studies and must not be pooled blindly",
      "cannot distinguish exact stimulus repetitions from CSE in combined data",
      "cannot distinguish response repetition/alternation from CSE in combined data",
      "cannot identify event-file/feature integration mechanisms in combined data",
      "cannot estimate item-level contingency learning in combined data",
      "condition/congruency/name are sparse and study-specific, not global covariates"
    ),
    missing_rate_key_column = c(
      missing_rate("prev_cong"),
      missing_rate("prev_correct"),
      missing_rate("correct"),
      missing_rate("trial_index"),
      missing_rate("block_index"),
      missing_rate("is_practice"),
      NA_real_, NA_real_, NA_real_, NA_real_, missing_rate("condition")
    ),
    stringsAsFactors = FALSE
  )
}

write_data_inventory <- function(input_path = "all_indexed.csv", output_dir = file.path("outputs", "analysis")) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  inventory <- build_column_inventory(input_path)
  risks <- build_confound_risk_table(inventory)
  inventory_path <- file.path(output_dir, "raw_column_inventory.csv")
  risk_path <- file.path(output_dir, "confound_risk_table.csv")
  write.csv(inventory, inventory_path, row.names = FALSE)
  write.csv(risks, risk_path, row.names = FALSE)
  invisible(list(column_inventory = inventory_path, confound_risk_table = risk_path))
}
