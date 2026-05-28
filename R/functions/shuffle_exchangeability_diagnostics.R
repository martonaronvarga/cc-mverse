# R/functions/shuffle_exchangeability_diagnostics.R
# Lightweight diagnostics for whether unrestricted within-(participant, cong) shuffle is plausible.

coerce_logicalish <- function(x) {
  if (is.logical(x)) return(x)
  if (is.numeric(x)) return(x != 0)
  x <- tolower(as.character(x))
  dplyr::case_when(
    x %in% c("true", "t", "1", "yes") ~ TRUE,
    x %in% c("false", "f", "0", "no") ~ FALSE,
    TRUE ~ NA
  )
}

participant_lag1 <- function(df) {
  vals <- vapply(split(df, df$participant_id), function(x) {
    x <- x[order(as.numeric(x$trial_index)), , drop = FALSE]
    if (nrow(x) < 3L || stats::sd(x$rt, na.rm = TRUE) == 0) return(NA_real_)
    suppressWarnings(stats::cor(head(x$rt, -1), tail(x$rt, -1), use = "complete.obs"))
  }, numeric(1))
  mean(vals[is.finite(vals)], na.rm = TRUE)
}

participant_trial_slope <- function(df) {
  vals <- vapply(split(df, df$participant_id), function(x) {
    trial <- suppressWarnings(as.numeric(x$trial_index))
    if (length(unique(trial[is.finite(trial)])) < 2L) return(NA_real_)
    coef(stats::lm(rt ~ trial, data = data.frame(rt = x$rt, trial = trial)))[[2]]
  }, numeric(1))
  mean(vals[is.finite(vals)], na.rm = TRUE)
}

post_error_slowing_study <- function(df) {
  if (!"prev_correct" %in% names(df)) return(NA_real_)
  prev <- coerce_logicalish(df$prev_correct)
  err <- df$rt[!prev & !is.na(prev)]
  ok <- df$rt[prev & !is.na(prev)]
  if (length(err) == 0L || length(ok) == 0L) return(NA_real_)
  mean(err, na.rm = TRUE) - mean(ok, na.rm = TRUE)
}

transition_imbalance_study <- function(df) {
  tab <- table(df$cong, df$prev_cong)
  if (!all(c("-1", "1") %in% rownames(tab)) || !all(c("-1", "1") %in% colnames(tab))) return(NA_real_)
  prop <- prop.table(tab[c("-1", "1"), c("-1", "1")])
  max(abs(prop - 0.25))
}

block_mean_sd_study <- function(df) {
  if (!"block_index" %in% names(df)) return(NA_real_)
  block_means <- stats::aggregate(rt ~ study + block_index, df, mean, na.rm = TRUE)
  stats::sd(block_means$rt, na.rm = TRUE)
}

build_shuffle_exchangeability_diagnostics <- function(df) {
  required <- c("study", "participant_id", "trial_index", "rt", "cong", "prev_cong")
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) stop("Missing required columns: ", paste(missing, collapse = ", "))

  df <- df %>%
    dplyr::mutate(
      rt = suppressWarnings(as.numeric(rt)),
      cong = as.character(cong),
      prev_cong = as.character(prev_cong)
    ) %>%
    dplyr::filter(is.finite(rt), rt > 0, cong %in% c("-1", "1"), prev_cong %in% c("-1", "1"))

  dplyr::bind_rows(lapply(split(df, df$study), function(x) {
    data.frame(
      study = unique(x$study)[[1]],
      n_rows_valid = nrow(x),
      n_participants = length(unique(x$participant_id)),
      lag1_autocorr_mean = participant_lag1(x),
      trial_rt_slope_mean = participant_trial_slope(x),
      block_mean_sd = block_mean_sd_study(x),
      transition_imbalance = transition_imbalance_study(x),
      post_error_slowing = post_error_slowing_study(x),
      unrestricted_shuffle_plausible = FALSE,
      recommendation = "do_not_use_as_primary_counterfactual",
      stringsAsFactors = FALSE
    )
  }))
}

write_shuffle_exchangeability_diagnostics <- function(input_csv, output_csv) {
  df <- readr::read_csv(input_csv, show_col_types = FALSE)
  diagnostics <- build_shuffle_exchangeability_diagnostics(df)
  dir.create(dirname(output_csv), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(diagnostics, output_csv)
  invisible(diagnostics)
}
