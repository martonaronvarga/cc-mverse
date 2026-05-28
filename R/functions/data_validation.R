# R/functions/data_validation.R - defensive validation for CSE branch data

validate_required_columns <- function(df, required, context = "data") {
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop(context, " missing required columns: ", paste(missing, collapse = ", "))
  }
  invisible(TRUE)
}

validate_binary_pm1 <- function(x, column, context = "data", allow_na = FALSE) {
  values <- suppressWarnings(as.numeric(x))
  bad_na <- is.na(values)
  bad_code <- !bad_na & !values %in% c(-1, 1)
  if ((!allow_na && any(bad_na)) || any(bad_code)) {
    bad <- unique(as.character(x[bad_na | bad_code]))
    stop(context, " has invalid +/-1 coding in ", column, ": ", paste(head(bad, 10), collapse = ", "))
  }
  invisible(TRUE)
}

validate_positive_rt <- function(rt, context = "data", allow_na = FALSE) {
  values <- suppressWarnings(as.numeric(rt))
  bad_na <- is.na(values)
  bad_value <- !bad_na & (!is.finite(values) | values <= 0)
  if ((!allow_na && any(bad_na)) || any(bad_value)) {
    stop(
      context,
      " has invalid RT values: missing=", sum(bad_na),
      ", nonpositive_or_nonfinite=", sum(bad_value)
    )
  }
  invisible(TRUE)
}

validate_trial_order <- function(df, context = "data") {
  if (!all(c("participant_id", "trial_index") %in% names(df))) {
    return(invisible(TRUE))
  }
  bad <- vapply(split(df$trial_index, df$participant_id), function(x) {
    values <- suppressWarnings(as.numeric(x))
    any(is.na(values)) || any(diff(values) < 0)
  }, logical(1))
  if (any(bad)) {
    stop(context, " has missing or decreasing trial_index for participants: ", paste(names(bad)[bad], collapse = ", "))
  }
  invisible(TRUE)
}

validate_transformed_rt <- function(df, context = "data", transformation = NULL) {
  if (is.null(transformation) || transformation %in% c(NA, "", "no_log_rt", "none")) {
    return(invisible(TRUE))
  }
  rt <- suppressWarnings(as.numeric(df$rt))
  if (transformation == "log_rt" && any(!is.finite(log(rt)))) {
    stop(context, " has RT values invalid for log_rt transformation")
  }
  invisible(TRUE)
}

validate_participant_cells <- function(df, context = "data", min_cell_n = 1L) {
  validate_required_columns(df, c("participant_id", "cong", "prev_cong"), context)
  bad <- vapply(split(df, df$participant_id), function(x) {
    tab <- table(as.character(x$cong), as.character(x$prev_cong))
    !all(c("-1", "1") %in% rownames(tab)) ||
      !all(c("-1", "1") %in% colnames(tab)) ||
      min(tab[c("-1", "1"), c("-1", "1")]) < min_cell_n
  }, logical(1))
  if (any(bad)) {
    stop(context, " has incomplete or sparse participant cong x prev_cong cells: ", paste(names(bad)[bad], collapse = ", "))
  }
  invisible(TRUE)
}

validate_cse_branch_data <- function(
  df,
  context = "data",
  required = c("participant_id", "cong", "prev_cong", "rt"),
  min_cell_n = 1L,
  allow_missing_rt = FALSE,
  allow_missing_prev = FALSE,
  transformation = NULL,
  validate_order = TRUE
) {
  validate_required_columns(df, required, context)
  validate_binary_pm1(df$cong, "cong", context, allow_na = FALSE)
  validate_binary_pm1(df$prev_cong, "prev_cong", context, allow_na = allow_missing_prev)
  validate_positive_rt(df$rt, context, allow_na = allow_missing_rt)
  validate_transformed_rt(df, context, transformation = transformation)
  if (validate_order) validate_trial_order(df, context)

  usable <- df
  if (allow_missing_rt) usable <- usable[!is.na(suppressWarnings(as.numeric(usable$rt))), , drop = FALSE]
  if (allow_missing_prev) usable <- usable[!is.na(suppressWarnings(as.numeric(usable$prev_cong))), , drop = FALSE]
  validate_participant_cells(usable, context, min_cell_n = min_cell_n)
  invisible(TRUE)
}
