# R/functions/design_integrity.R - study-aware data construction diagnostics

infer_study_from_participant <- function(participant_id) {
  sub("^.*_", "", as.character(participant_id))
}

read_indexed_design <- function(input_path = "all_indexed.csv") {
  if (!file.exists(input_path)) {
    stop("Input file not found: ", input_path)
  }

  df <- read.csv(input_path, stringsAsFactors = FALSE, check.names = FALSE)
  names(df)[names(df) == ""] <- "export_row_index"

  required <- c(
    "trial_index", "block_index", "cong", "participant_id", "rt",
    "correct", "prev_correct", "prev_cong"
  )
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }

  df$study <- infer_study_from_participant(df$participant_id)
  df$source_row <- seq_len(nrow(df))
  df
}

normalize_missing <- function(x) {
  out <- as.character(x)
  out[out %in% c("", "NA", "NaN", "NULL")] <- NA_character_
  out
}

coerce_logicalish <- function(x) {
  x <- normalize_missing(x)
  ifelse(is.na(x), NA, tolower(x) %in% c("true", "t", "1"))
}

mismatch_with_na_equal <- function(observed, expected) {
  out <- observed != expected
  out[is.na(observed) & is.na(expected)] <- FALSE
  out[is.na(out)] <- TRUE
  out
}

canonical_previous_fields <- function(df) {
  order_cols <- c("study", "participant_id", "block_index", "trial_index", "source_row")
  ord <- do.call(order, df[order_cols])
  sorted <- df[ord, , drop = FALSE]

  split_idx <- split(seq_len(nrow(sorted)), sorted$participant_id)
  sorted$canonical_prev_cong <- NA
  sorted$canonical_prev_correct <- NA
  sorted$canonical_is_first_trial <- NA

  source_supplied_prev <- c("flanker", "primeprobe", "yang")
  for (idx in split_idx) {
    study <- unique(sorted$study[idx])[[1]]
    if (study %in% source_supplied_prev) {
      sorted$canonical_prev_cong[idx] <- sorted$prev_cong[idx]
      sorted$canonical_prev_correct[idx] <- sorted$prev_correct[idx]
      if ("is_first_trial" %in% names(sorted)) {
        sorted$canonical_is_first_trial[idx] <- coerce_logicalish(sorted$is_first_trial[idx])
      } else {
        sorted$canonical_is_first_trial[idx] <- c(TRUE, rep(FALSE, length(idx) - 1L))
      }
    } else {
      sorted$canonical_prev_cong[idx] <- c(NA, head(sorted$cong[idx], -1))
      sorted$canonical_prev_correct[idx] <- c(NA, head(sorted$correct[idx], -1))
      sorted$canonical_is_first_trial[idx] <- c(TRUE, rep(FALSE, length(idx) - 1L))
    }
  }

  sorted[order(sorted$source_row), , drop = FALSE]
}

cell_count_summary <- function(df) {
  df2 <- df[!is.na(df$cong) & !is.na(df$prev_cong), , drop = FALSE]
  if (nrow(df2) == 0) {
    return(data.frame(
      study = character(), participant_id = character(), min_cell_n = integer(),
      complete_four_cells = logical(), stringsAsFactors = FALSE
    ))
  }

  tab <- as.data.frame(
    xtabs(~ study + participant_id + cong + prev_cong, data = df2),
    stringsAsFactors = FALSE
  )
  names(tab)[names(tab) == "Freq"] <- "n"

  aggregate(
    n ~ study + participant_id,
    data = tab,
    FUN = function(x) c(min_cell_n = min(x), complete_four_cells = all(x > 0))
  ) |>
    transform(
      min_cell_n = n[, "min_cell_n"],
      complete_four_cells = as.logical(n[, "complete_four_cells"])
    ) |>
    subset(select = -n)
}

build_design_integrity_audit <- function(input_path = "all_indexed.csv") {
  df <- read_indexed_design(input_path)
  df <- canonical_previous_fields(df)

  prev_cong_norm <- normalize_missing(df$prev_cong)
  prev_correct_norm <- normalize_missing(df$prev_correct)
  first_norm <- normalize_missing(if ("is_first_trial" %in% names(df)) df$is_first_trial else NA)

  canonical_prev_cong_norm <- normalize_missing(df$canonical_prev_cong)
  canonical_prev_correct_norm <- normalize_missing(df$canonical_prev_correct)
  canonical_first_norm <- ifelse(df$canonical_is_first_trial, "TRUE", "FALSE")

  df$prev_cong_mismatch <- mismatch_with_na_equal(prev_cong_norm, canonical_prev_cong_norm)
  df$prev_correct_mismatch <- mismatch_with_na_equal(prev_correct_norm, canonical_prev_correct_norm)
  df$is_first_trial_mismatch <- mismatch_with_na_equal(toupper(first_norm), canonical_first_norm)

  df$rt_num <- suppressWarnings(as.numeric(df$rt))
  df$cong_norm <- normalize_missing(df$cong)
  df$prev_cong_norm <- prev_cong_norm

  cell_counts <- cell_count_summary(df)
  participant_summary <- aggregate(
    cbind(
      rows = rep(1, nrow(df)),
      missing_rt = is.na(df$rt_num),
      nonpositive_rt = !is.na(df$rt_num) & df$rt_num <= 0,
      prev_cong_mismatch = df$prev_cong_mismatch,
      prev_correct_mismatch = df$prev_correct_mismatch,
      is_first_trial_mismatch = df$is_first_trial_mismatch,
      missing_prev_cong = is.na(prev_cong_norm)
    ) ~ study + participant_id,
    data = df,
    FUN = sum,
    na.rm = TRUE
  )
  participant_summary <- merge(
    participant_summary,
    cell_counts,
    by = c("study", "participant_id"),
    all.x = TRUE
  )
  participant_summary$min_cell_n[is.na(participant_summary$min_cell_n)] <- 0L
  participant_summary$complete_four_cells[is.na(participant_summary$complete_four_cells)] <- FALSE

  design_inventory <- aggregate(
    cbind(
      rows = rep(1, nrow(df)),
      missing_rt = is.na(df$rt_num),
      nonpositive_rt = !is.na(df$rt_num) & df$rt_num <= 0,
      missing_prev_cong = is.na(prev_cong_norm),
      prev_cong_mismatch = df$prev_cong_mismatch,
      prev_correct_mismatch = df$prev_correct_mismatch,
      is_first_trial_mismatch = df$is_first_trial_mismatch
    ) ~ study,
    data = df,
    FUN = sum,
    na.rm = TRUE
  )

  participants_by_study <- aggregate(participant_id ~ study, data = df, FUN = function(x) length(unique(x)))
  names(participants_by_study)[2] <- "participants"
  design_inventory <- merge(design_inventory, participants_by_study, by = "study", all.x = TRUE)
  design_inventory$missing_rt_rate <- design_inventory$missing_rt / design_inventory$rows
  design_inventory$nonpositive_rt_rate <- design_inventory$nonpositive_rt / design_inventory$rows
  design_inventory$missing_prev_cong_rate <- design_inventory$missing_prev_cong / design_inventory$rows
  design_inventory$prev_cong_mismatch_rate <- design_inventory$prev_cong_mismatch / design_inventory$rows
  design_inventory$prev_correct_mismatch_rate <- design_inventory$prev_correct_mismatch / design_inventory$rows
  design_inventory$is_first_trial_mismatch_rate <- design_inventory$is_first_trial_mismatch / design_inventory$rows

  cell_by_study <- aggregate(
    cbind(min_cell_n, complete_four_cells) ~ study,
    data = participant_summary,
    FUN = function(x) c(min = min(x), median = stats::median(x), mean = mean(x))
  )
  design_inventory$participants_with_complete_four_cells <- vapply(
    design_inventory$study,
    function(study) sum(participant_summary$study == study & participant_summary$complete_four_cells),
    integer(1)
  )
  design_inventory$min_participant_cell_n <- vapply(
    design_inventory$study,
    function(study) min(participant_summary$min_cell_n[participant_summary$study == study]),
    numeric(1)
  )
  design_inventory$median_participant_cell_n <- vapply(
    design_inventory$study,
    function(study) stats::median(participant_summary$min_cell_n[participant_summary$study == study]),
    numeric(1)
  )

  previous_trial_column_audit <- participant_summary
  previous_trial_column_audit$prev_cong_mismatch_rate <-
    previous_trial_column_audit$prev_cong_mismatch / previous_trial_column_audit$rows
  previous_trial_column_audit$prev_correct_mismatch_rate <-
    previous_trial_column_audit$prev_correct_mismatch / previous_trial_column_audit$rows
  previous_trial_column_audit$is_first_trial_mismatch_rate <-
    previous_trial_column_audit$is_first_trial_mismatch / previous_trial_column_audit$rows

  list(
    design_inventory = design_inventory,
    previous_trial_column_audit = previous_trial_column_audit
  )
}

write_design_integrity_audit <- function(
  input_path = "all_indexed.csv",
  output_dir = file.path("outputs", "analysis")
) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  audit <- build_design_integrity_audit(input_path)

  design_path <- file.path(output_dir, "design_inventory.csv")
  prev_path <- file.path(output_dir, "previous_trial_column_audit.csv")

  write.csv(audit$design_inventory, design_path, row.names = FALSE)
  write.csv(audit$previous_trial_column_audit, prev_path, row.names = FALSE)

  invisible(list(
    design_inventory = design_path,
    previous_trial_column_audit = prev_path
  ))
}
