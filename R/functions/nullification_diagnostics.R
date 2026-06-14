# R/functions/nullification_diagnostics.R - lightweight residual CSE diagnostics

required_nullification_columns <- c("participant_id", "cong", "prev_cong", "rt")

participant_mean_cse <- function(df) {
  split(df, df$participant_id) |>
    lapply(function(x) {
      means <- tapply(x$rt, list(x$cong, x$prev_cong), mean, na.rm = TRUE)
      if (!all(c("-1", "1") %in% rownames(means)) || !all(c("-1", "1") %in% colnames(means))) {
        return(NA_real_)
      }
      (means["1", "1"] - means["1", "-1"]) - (means["-1", "1"] - means["-1", "-1"])
    }) |>
    unlist(use.names = FALSE)
}

pooled_quantile_cse <- function(df, probs = c(0.1, 0.25, 0.5, 0.75, 0.9)) {
  out <- vapply(probs, function(p) {
    q <- tapply(df$rt, list(df$cong, df$prev_cong), stats::quantile, probs = p, na.rm = TRUE, names = FALSE)
    if (!all(c("-1", "1") %in% rownames(q)) || !all(c("-1", "1") %in% colnames(q))) {
      return(NA_real_)
    }
    (q["1", "1"] - q["1", "-1"]) - (q["-1", "1"] - q["-1", "-1"])
  }, numeric(1))
  names(out) <- paste0("q", gsub("\\.", "", sprintf("%.2f", probs)), "_cse")
  out
}

cell_count_minimum <- function(df) {
  tab <- table(df$cong, df$prev_cong)
  if (!all(c("-1", "1") %in% rownames(tab)) || !all(c("-1", "1") %in% colnames(tab))) {
    return(0L)
  }
  as.integer(min(tab[c("-1", "1"), c("-1", "1")]))
}

marginal_effect <- function(df, predictor) {
  if (!predictor %in% names(df)) return(NA_real_)
  x <- as.numeric(as.character(df[[predictor]]))
  pos <- df$rt[x == 1]
  neg <- df$rt[x == -1]
  if (length(pos) == 0L || length(neg) == 0L) return(NA_real_)
  mean(pos, na.rm = TRUE) - mean(neg, na.rm = TRUE)
}

mean_lag1_autocorr <- function(df) {
  if (!all(c("participant_id", "rt") %in% names(df))) return(NA_real_)
  vals <- vapply(split(df, df$participant_id), function(x) {
    if ("trial_index" %in% names(x)) x <- x[order(as.numeric(x$trial_index)), , drop = FALSE]
    if (nrow(x) < 3L || stats::sd(x$rt, na.rm = TRUE) == 0) return(NA_real_)
    suppressWarnings(stats::cor(head(x$rt, -1), tail(x$rt, -1), use = "complete.obs"))
  }, numeric(1))
  mean(vals[is.finite(vals)], na.rm = TRUE)
}

mean_trial_slope <- function(df) {
  if (!all(c("participant_id", "trial_index", "rt") %in% names(df))) return(NA_real_)
  vals <- vapply(split(df, df$participant_id), function(x) {
    trial <- suppressWarnings(as.numeric(x$trial_index))
    if (length(unique(trial[is.finite(trial)])) < 2L) return(NA_real_)
    coef(stats::lm(rt ~ trial, data = data.frame(rt = x$rt, trial = trial)))[[2]]
  }, numeric(1))
  mean(vals[is.finite(vals)], na.rm = TRUE)
}

post_error_slowing <- function(df) {
  if (!all(c("prev_correct", "rt") %in% names(df))) return(NA_real_)
  prev <- as.character(df$prev_correct)
  post_error <- df$rt[prev %in% c("FALSE", "False", "false", "0")]
  post_correct <- df$rt[prev %in% c("TRUE", "True", "true", "1")]
  if (length(post_error) == 0L || length(post_correct) == 0L) return(NA_real_)
  mean(post_error, na.rm = TRUE) - mean(post_correct, na.rm = TRUE)
}

source_row_monotonic <- function(df) {
  if (!"source_row" %in% names(df)) return(NA)
  all(diff(suppressWarnings(as.numeric(df$source_row))) >= 0, na.rm = TRUE)
}

block_mean_sd <- function(df) {
  if (!all(c("participant_id", "block_index", "rt") %in% names(df))) return(NA_real_)
  block_id <- paste(df$participant_id, df$block_index, sep = "__")
  means <- tapply(df$rt, block_id, mean, na.rm = TRUE)
  stats::sd(means[is.finite(means)], na.rm = TRUE)
}

transition_imbalance <- function(df) {
  if (!all(c("cong", "prev_cong") %in% names(df))) return(NA_real_)
  tab <- table(df$cong, df$prev_cong)
  if (sum(tab) == 0L) return(NA_real_)
  props <- as.numeric(tab) / sum(tab)
  max(props, na.rm = TRUE) - min(props, na.rm = TRUE)
}

time_bin_quantile_cse <- function(df, n_bins = 5L) {
  if (!all(c("trial_index", "rt", "cong", "prev_cong") %in% names(df))) return(c(max_abs_timebin_q050_cse = NA_real_))
  trial <- suppressWarnings(as.numeric(df$trial_index))
  ok <- is.finite(trial)
  if (sum(ok) < 4L || length(unique(trial[ok])) < n_bins) return(c(max_abs_timebin_q050_cse = NA_real_))
  bins <- cut(trial, breaks = stats::quantile(trial[ok], probs = seq(0, 1, length.out = n_bins + 1L), na.rm = TRUE), include.lowest = TRUE, labels = FALSE)
  vals <- vapply(sort(unique(bins[is.finite(bins)])), function(b) {
    x <- df[bins == b & !is.na(bins), , drop = FALSE]
    pooled_quantile_cse(x, probs = 0.5)[[1]]
  }, numeric(1))
  finite_vals <- vals[is.finite(vals)]
  c(max_abs_timebin_q050_cse = if (length(finite_vals)) max(abs(finite_vals)) else NA_real_)
}

build_nullification_diagnostic <- function(
  df,
  data_id = NA_character_,
  effect_condition = NA_character_,
  strip_method = NA_character_,
  sample_size = NA_real_,
  outlier = NA_character_,
  transformation = NA_character_,
  reference_df = NULL
) {
  missing <- setdiff(required_nullification_columns, names(df))
  if (length(missing) > 0) {
    stop("Diagnostic data missing columns: ", paste(missing, collapse = ", "))
  }

  df$rt <- suppressWarnings(as.numeric(df$rt))
  df$cong <- as.character(df$cong)
  df$prev_cong <- as.character(df$prev_cong)
  valid <- is.finite(df$rt) & df$rt > 0 & !is.na(df$cong) & !is.na(df$prev_cong)
  d <- df[valid, , drop = FALSE]

  participant_cse <- participant_mean_cse(d)
  finite_participant_cse <- participant_cse[is.finite(participant_cse)]
  quantile_cse <- pooled_quantile_cse(d)
  current_cong_effect <- marginal_effect(d, "cong")
  previous_cong_effect <- marginal_effect(d, "prev_cong")
  lag1_autocorr <- mean_lag1_autocorr(d)
  trial_slope <- mean_trial_slope(d)
  post_error <- post_error_slowing(d)
  block_sd <- block_mean_sd(d)
  trans_imbalance <- transition_imbalance(d)
  timebin_qcse <- time_bin_quantile_cse(d)
  row_order_ok <- source_row_monotonic(df)
  reference_metrics <- NULL
  if (!is.null(reference_df)) {
    reference_metrics <- build_nullification_diagnostic(
      reference_df,
      data_id = NA_character_,
      effect_condition = "reference",
      strip_method = "none",
      sample_size = sample_size,
      outlier = outlier,
      transformation = transformation,
      reference_df = NULL
    )
  }

  warnings <- character()
  if (sum(!valid) > 0) warnings <- c(warnings, paste0("dropped_invalid_rows=", sum(!valid)))
  if (length(participant_cse) == 0L || any(!is.finite(participant_cse))) {
    warnings <- c(warnings, "incomplete_participant_cells")
  }
  if (length(finite_participant_cse) == 0L) warnings <- c(warnings, "no_finite_participant_cse")
  if (cell_count_minimum(d) == 0L) warnings <- c(warnings, "missing_pooled_cell")

  mean_cse <- if (length(finite_participant_cse) == 0L) NA_real_ else mean(finite_participant_cse)
  median_cse <- if (length(finite_participant_cse) == 0L) NA_real_ else stats::median(finite_participant_cse)
  max_abs_participant_cse <- if (length(finite_participant_cse) == 0L) {
    NA_real_
  } else {
    max(abs(finite_participant_cse))
  }

  out <- data.frame(
    data_id = data_id,
    effect_condition = effect_condition,
    strip_method = strip_method,
    sample_size = sample_size,
    outlier = outlier,
    transformation = transformation,
    n_rows = nrow(df),
    n_rows_valid = nrow(d),
    n_participants = length(unique(d$participant_id)),
    n_studies = if ("study" %in% names(d)) length(unique(d$study)) else NA_integer_,
    studies = if ("study" %in% names(d)) paste(sort(unique(d$study)), collapse = ";") else NA_character_,
    cell_min_n = cell_count_minimum(d),
    source_row_monotonic = row_order_ok,
    current_cong_effect = current_cong_effect,
    previous_cong_effect = previous_cong_effect,
    lag1_autocorr_mean = lag1_autocorr,
    trial_rt_slope_mean = trial_slope,
    block_mean_sd = block_sd,
    transition_imbalance = trans_imbalance,
    post_error_slowing = post_error,
    max_abs_timebin_q050_cse = timebin_qcse[[1]],
    mean_cse = mean_cse,
    median_cse = median_cse,
    max_abs_participant_cse = max_abs_participant_cse,
    t(quantile_cse),
    warnings = paste(warnings, collapse = ";"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  out$preservation_pass <- NA
  out$preservation_warnings <- ""
  out$nullification_verdict <- if (identical(effect_condition, "present")) "reference_present" else "unpaired"
  if (!is.null(reference_metrics)) {
    delta_cols <- c(
      "current_cong_effect", "previous_cong_effect", "lag1_autocorr_mean",
      "trial_rt_slope_mean", "block_mean_sd", "transition_imbalance",
      "post_error_slowing", "max_abs_timebin_q050_cse", "mean_cse", names(quantile_cse)
    )
    for (col in delta_cols) {
      out[[paste0(col, "_delta_from_present")]] <- out[[col]] - reference_metrics[[col]]
    }

    preservation_warnings <- character()
    if (is.finite(out$mean_cse) && abs(out$mean_cse) > 5) preservation_warnings <- c(preservation_warnings, "residual_mean_cse_gt_5ms")
    if (is.finite(out$q050_cse) && abs(out$q050_cse) > 5) preservation_warnings <- c(preservation_warnings, "residual_median_quantile_cse_gt_5ms")
    if (is.finite(out$current_cong_effect_delta_from_present) && abs(out$current_cong_effect_delta_from_present) > 5) preservation_warnings <- c(preservation_warnings, "current_cong_effect_delta_gt_5ms")
    if (is.finite(out$previous_cong_effect_delta_from_present) && abs(out$previous_cong_effect_delta_from_present) > 5) preservation_warnings <- c(preservation_warnings, "previous_cong_effect_delta_gt_5ms")
    if (is.finite(out$lag1_autocorr_mean_delta_from_present) && abs(out$lag1_autocorr_mean_delta_from_present) > 0.05) preservation_warnings <- c(preservation_warnings, "lag1_autocorr_delta_gt_0.05")
    if (is.finite(out$trial_rt_slope_mean_delta_from_present) && abs(out$trial_rt_slope_mean_delta_from_present) > 0.05) preservation_warnings <- c(preservation_warnings, "trial_slope_delta_gt_0.05ms")
    if (is.finite(out$block_mean_sd_delta_from_present) && abs(out$block_mean_sd_delta_from_present) > 10) preservation_warnings <- c(preservation_warnings, "block_mean_sd_delta_gt_10ms")
    if (is.finite(out$transition_imbalance_delta_from_present) && abs(out$transition_imbalance_delta_from_present) > 0.01) preservation_warnings <- c(preservation_warnings, "transition_imbalance_delta_gt_0.01")
    if (is.finite(out$max_abs_timebin_q050_cse) && abs(out$max_abs_timebin_q050_cse) > 10) preservation_warnings <- c(preservation_warnings, "timebin_median_quantile_cse_gt_10ms")
    if (is.finite(out$post_error_slowing_delta_from_present) && abs(out$post_error_slowing_delta_from_present) > 10) preservation_warnings <- c(preservation_warnings, "post_error_slowing_delta_gt_10ms")
    out$preservation_pass <- length(preservation_warnings) == 0L
    out$preservation_warnings <- paste(preservation_warnings, collapse = ";")
    out$nullification_verdict <- if (isTRUE(out$preservation_pass)) "interpretable_nullifier" else "fails_preservation_gates"
  }

  out
}

parse_processed_data_id <- function(path) {
  stem <- basename(path)
  stem <- sub("\\.(parquet|csv)$", "", stem, ignore.case = TRUE)
  stem <- sub("^processed__", "", stem)
  parts <- strsplit(stem, "__", fixed = TRUE)[[1]]
  out <- list(
    data_id = stem,
    sample_size = NA_real_,
    subsample_id = NA_integer_,
    transformation = NA_character_,
    outlier = NA_character_,
    effect_condition = NA_character_,
    strip_method = NA_character_
  )
  if (length(parts) == 6L) {
    out$sample_size <- suppressWarnings(as.numeric(parts[[1]]))
    out$subsample_id <- suppressWarnings(as.integer(parts[[2]]))
    out$transformation <- parts[[3]]
    out$outlier <- parts[[4]]
    out$effect_condition <- parts[[5]]
    out$strip_method <- parts[[6]]
  }
  out
}

metadata_only_nullification_diagnostics <- function(paths) {
  rows <- lapply(paths, function(path) {
    meta <- parse_processed_data_id(path)
    data.frame(
      data_id = meta$data_id,
      effect_condition = meta$effect_condition,
      strip_method = meta$strip_method,
      sample_size = meta$sample_size,
      subsample_id = meta$subsample_id,
      outlier = meta$outlier,
      transformation = meta$transformation,
      n_rows = NA_integer_,
      n_rows_valid = NA_integer_,
      n_participants = NA_integer_,
      n_studies = NA_integer_,
      studies = NA_character_,
      cell_min_n = NA_integer_,
      source_row_monotonic = NA,
      current_cong_effect = NA_real_,
      previous_cong_effect = NA_real_,
      lag1_autocorr_mean = NA_real_,
      trial_rt_slope_mean = NA_real_,
      block_mean_sd = NA_real_,
      transition_imbalance = NA_real_,
      post_error_slowing = NA_real_,
      max_abs_timebin_q050_cse = NA_real_,
      mean_cse = NA_real_,
      median_cse = NA_real_,
      max_abs_participant_cse = NA_real_,
      q010_cse = NA_real_,
      q025_cse = NA_real_,
      q050_cse = NA_real_,
      q075_cse = NA_real_,
      q090_cse = NA_real_,
      warnings = "metadata_only_fast_diagnostics",
      preservation_pass = NA,
      preservation_warnings = "metadata_only_fast_diagnostics",
      nullification_verdict = dplyr::case_when(
        meta$effect_condition == "present" ~ "reference_present_metadata_only",
        TRUE ~ "metadata_only_fast_diagnostics"
      ),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  dplyr::bind_rows(rows)
}

diagnostics_mode <- function() {
  fast <- tolower(Sys.getenv("FAST_DIAGNOSTICS", unset = ""))
  if (fast %in% c("1", "true", "yes")) return("metadata")
  mode <- tolower(Sys.getenv("DIAGNOSTICS_MODE", unset = "cached"))
  if (mode %in% c("fast", "metadata", "metadata_only")) return("metadata")
  if (mode %in% c("full", "uncached")) return("full")
  "cached"
}

use_fast_nullification_diagnostics <- function(n_paths = NULL) {
  identical(diagnostics_mode(), "metadata")
}

diagnostics_overwrite <- function() {
  tolower(Sys.getenv("DIAGNOSTICS_OVERWRITE", unset = "false")) %in% c("1", "true", "yes")
}

diagnostics_workers <- function() {
  workers <- suppressWarnings(as.integer(Sys.getenv("DIAGNOSTIC_WORKERS", unset = "1")))
  if (is.na(workers) || workers < 1L) workers <- 1L
  workers
}

diagnostics_progress_every <- function(total, workers = diagnostics_workers()) {
  every <- suppressWarnings(as.integer(Sys.getenv("DIAGNOSTIC_PROGRESS_EVERY", unset = NA_character_)))
  if (is.na(every) || every < 1L) every <- max(100L, workers * 25L)
  min(every, max(1L, total))
}

format_diagnostic_duration <- function(seconds) {
  seconds <- max(0, as.numeric(seconds))
  if (!is.finite(seconds)) return("unknown")
  if (seconds < 60) return(sprintf("%.0fs", seconds))
  if (seconds < 3600) return(sprintf("%.1fmin", seconds / 60))
  sprintf("%.1fh", seconds / 3600)
}

diagnostics_map_with_progress <- function(items, worker, label, workers = diagnostics_workers()) {
  total <- length(items)
  if (!total) return(list())
  every <- diagnostics_progress_every(total, workers)
  groups <- split(seq_len(total), ceiling(seq_len(total) / every))
  started <- Sys.time()
  out <- vector("list", total)
  completed <- 0L

  for (idx in groups) {
    batch <- items[idx]
    batch_out <- if (workers > 1L && .Platform$OS.type != "windows") {
      parallel::mclapply(batch, worker, mc.cores = workers)
    } else {
      lapply(batch, worker)
    }
    out[idx] <- batch_out
    completed <- completed + length(idx)
    elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
    rate <- if (elapsed > 0) completed / elapsed else NA_real_
    remaining <- total - completed
    eta <- if (is.finite(rate) && rate > 0) remaining / rate else NA_real_
    log_pipeline(
      logger::INFO,
      sprintf(
        "%s: %d/%d (%.1f%%) complete; elapsed=%s; rate=%.2f/s; eta=%s",
        label,
        completed,
        total,
        100 * completed / total,
        format_diagnostic_duration(elapsed),
        rate,
        format_diagnostic_duration(eta)
      )
    )
  }
  out
}

read_processed_diagnostic_file <- function(path, columns = NULL) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "csv") {
    df <- read.csv(path, stringsAsFactors = FALSE)
    if (!is.null(columns)) df <- df[intersect(columns, names(df))]
    return(df)
  }
  if (ext == "parquet") {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("Package 'arrow' is required to read parquet diagnostics input: ", path)
    }
    if (!is.null(columns)) {
      selected <- try(
        arrow::read_parquet(path, col_select = tidyselect::any_of(columns)),
        silent = TRUE
      )
      if (!inherits(selected, "try-error")) return(as.data.frame(selected))
    }
    return(as.data.frame(arrow::read_parquet(path)))
  }
  stop("Unsupported diagnostic input extension: ", ext)
}

reference_key <- function(meta) {
  paste(meta$sample_size, meta$subsample_id, meta$transformation, meta$outlier, sep = "__")
}

diagnostic_cache_path <- function(cache_dir, data_id) {
  safe <- gsub("[^A-Za-z0-9_.-]+", "_", data_id)
  file.path(cache_dir, paste0(safe, ".rds"))
}

write_rds_atomic <- function(object, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(pattern = paste0(basename(path), ".tmp_"), tmpdir = dirname(path), fileext = ".rds")
  saveRDS(object, tmp)
  if (!file.rename(tmp, path)) {
    file.copy(tmp, path, overwrite = TRUE)
    unlink(tmp)
  }
  path
}

compute_nullification_diagnostic_for_path <- function(path) {
  meta <- parse_processed_data_id(path)
  df <- read_processed_diagnostic_file(path)
  build_nullification_diagnostic(
    df,
    data_id = meta$data_id,
    effect_condition = meta$effect_condition,
    strip_method = meta$strip_method,
    sample_size = meta$sample_size,
    outlier = meta$outlier,
    transformation = meta$transformation,
    reference_df = NULL
  )
}

compute_nullification_diagnostic_cached <- function(path, cache_dir, overwrite = FALSE) {
  meta <- parse_processed_data_id(path)
  cache_path <- diagnostic_cache_path(cache_dir, meta$data_id)
  if (file.exists(cache_path) && !isTRUE(overwrite)) {
    return(readRDS(cache_path))
  }
  diagnostic <- compute_nullification_diagnostic_for_path(path)
  write_rds_atomic(diagnostic, cache_path)
  diagnostic
}

compute_nullification_diagnostic_cache_path <- function(path, cache_dir, overwrite = FALSE) {
  meta <- parse_processed_data_id(path)
  cache_path <- diagnostic_cache_path(cache_dir, meta$data_id)
  if (file.exists(cache_path) && !isTRUE(overwrite)) return(cache_path)
  diagnostic <- compute_nullification_diagnostic_for_path(path)
  write_rds_atomic(diagnostic, cache_path)
}

diagnostic_chunk_size <- function(default = 500L) {
  size <- suppressWarnings(as.integer(Sys.getenv("DIAGNOSTIC_CHUNK_SIZE", unset = as.character(default))))
  if (is.na(size) || size < 1L) size <- default
  size
}

shuffle_diagnostic_chunk_size <- function(default = 25L) {
  size <- suppressWarnings(as.integer(Sys.getenv("SHUFFLE_DIAGNOSTIC_CHUNK_SIZE", unset = as.character(default))))
  if (is.na(size) || size < 1L) size <- default
  size
}

list_processed_diagnostic_paths <- function(input_dir, pattern = "^processed__.*\\.(parquet|csv)$") {
  paths <- list.files(input_dir, pattern = pattern, full.names = TRUE, ignore.case = TRUE)
  if (!length(paths)) stop("No processed diagnostic files found in ", input_dir)
  sort(paths)
}

split_diagnostic_paths <- function(paths, chunk_size = diagnostic_chunk_size()) {
  paths <- sort(paths)
  if (!length(paths)) return(list())
  split(paths, ceiling(seq_along(paths) / max(1L, as.integer(chunk_size))))
}

compute_nullification_diagnostic_cache_paths <- function(paths, cache_dir, overwrite = FALSE) {
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  worker <- function(path) compute_nullification_diagnostic_cache_path(path, cache_dir, overwrite = overwrite)
  diagnostics_map_with_progress(
    paths,
    worker,
    label = "Nullification diagnostics cache",
    workers = diagnostics_workers()
  ) |>
    unlist(use.names = FALSE)
}

aggregate_nullification_diagnostic_cache <- function(cache_paths, output_path) {
  cache_paths <- sort(unique(unlist(cache_paths, use.names = FALSE)))
  if (!length(cache_paths)) stop("No nullification diagnostic cache files to aggregate")
  rows <- lapply(cache_paths, readRDS)
  diagnostics <- attach_nullification_reference_metrics(dplyr::bind_rows(rows))
  write_nullification_diagnostics(diagnostics, output_path)
}

row_reference_key <- function(df) {
  paste(df$sample_size, df$subsample_id, df$transformation, df$outlier, sep = "__")
}

attach_nullification_reference_metrics <- function(diagnostics) {
  if (!nrow(diagnostics)) return(diagnostics)
  if (!"subsample_id" %in% names(diagnostics)) {
    diagnostics$subsample_id <- vapply(strsplit(diagnostics$data_id, "__", fixed = TRUE), function(x) {
      if (length(x) >= 2L) suppressWarnings(as.integer(x[[2]])) else NA_integer_
    }, integer(1))
  }
  references <- diagnostics[
    diagnostics$effect_condition == "present" & diagnostics$strip_method == "none",
    , drop = FALSE
  ]
  if (!nrow(references)) return(diagnostics)
  ref_keys <- row_reference_key(references)
  ref_index <- stats::setNames(seq_len(nrow(references)), ref_keys)
  delta_cols <- c(
    "current_cong_effect", "previous_cong_effect", "lag1_autocorr_mean",
    "trial_rt_slope_mean", "block_mean_sd", "transition_imbalance",
    "post_error_slowing", "max_abs_timebin_q050_cse", "mean_cse",
    "q010_cse", "q025_cse", "q050_cse", "q075_cse", "q090_cse"
  )
  delta_cols <- intersect(delta_cols, names(diagnostics))
  for (col in delta_cols) diagnostics[[paste0(col, "_delta_from_present")]] <- NA_real_

  null_rows <- which(diagnostics$effect_condition != "present")
  for (i in null_rows) {
    key <- row_reference_key(diagnostics[i, , drop = FALSE])
    if (!key %in% names(ref_index)) next
    ref_i <- ref_index[[key]]
    if (is.null(ref_i) || is.na(ref_i)) next
    for (col in delta_cols) {
      diagnostics[[paste0(col, "_delta_from_present")]][[i]] <- diagnostics[[col]][[i]] - references[[col]][[ref_i]]
    }

    preservation_warnings <- character()
    if (is.finite(diagnostics$mean_cse[[i]]) && abs(diagnostics$mean_cse[[i]]) > 5) preservation_warnings <- c(preservation_warnings, "residual_mean_cse_gt_5ms")
    if (is.finite(diagnostics$q050_cse[[i]]) && abs(diagnostics$q050_cse[[i]]) > 5) preservation_warnings <- c(preservation_warnings, "residual_median_quantile_cse_gt_5ms")
    if ("current_cong_effect_delta_from_present" %in% names(diagnostics) && is.finite(diagnostics$current_cong_effect_delta_from_present[[i]]) && abs(diagnostics$current_cong_effect_delta_from_present[[i]]) > 5) preservation_warnings <- c(preservation_warnings, "current_cong_effect_delta_gt_5ms")
    if ("previous_cong_effect_delta_from_present" %in% names(diagnostics) && is.finite(diagnostics$previous_cong_effect_delta_from_present[[i]]) && abs(diagnostics$previous_cong_effect_delta_from_present[[i]]) > 5) preservation_warnings <- c(preservation_warnings, "previous_cong_effect_delta_gt_5ms")
    if ("lag1_autocorr_mean_delta_from_present" %in% names(diagnostics) && is.finite(diagnostics$lag1_autocorr_mean_delta_from_present[[i]]) && abs(diagnostics$lag1_autocorr_mean_delta_from_present[[i]]) > 0.05) preservation_warnings <- c(preservation_warnings, "lag1_autocorr_delta_gt_0.05")
    if ("trial_rt_slope_mean_delta_from_present" %in% names(diagnostics) && is.finite(diagnostics$trial_rt_slope_mean_delta_from_present[[i]]) && abs(diagnostics$trial_rt_slope_mean_delta_from_present[[i]]) > 0.05) preservation_warnings <- c(preservation_warnings, "trial_slope_delta_gt_0.05ms")
    if ("block_mean_sd_delta_from_present" %in% names(diagnostics) && is.finite(diagnostics$block_mean_sd_delta_from_present[[i]]) && abs(diagnostics$block_mean_sd_delta_from_present[[i]]) > 10) preservation_warnings <- c(preservation_warnings, "block_mean_sd_delta_gt_10ms")
    if ("transition_imbalance_delta_from_present" %in% names(diagnostics) && is.finite(diagnostics$transition_imbalance_delta_from_present[[i]]) && abs(diagnostics$transition_imbalance_delta_from_present[[i]]) > 0.01) preservation_warnings <- c(preservation_warnings, "transition_imbalance_delta_gt_0.01")
    if (is.finite(diagnostics$max_abs_timebin_q050_cse[[i]]) && abs(diagnostics$max_abs_timebin_q050_cse[[i]]) > 10) preservation_warnings <- c(preservation_warnings, "timebin_median_quantile_cse_gt_10ms")
    if ("post_error_slowing_delta_from_present" %in% names(diagnostics) && is.finite(diagnostics$post_error_slowing_delta_from_present[[i]]) && abs(diagnostics$post_error_slowing_delta_from_present[[i]]) > 10) preservation_warnings <- c(preservation_warnings, "post_error_slowing_delta_gt_10ms")

    diagnostics$preservation_pass[[i]] <- length(preservation_warnings) == 0L
    diagnostics$preservation_warnings[[i]] <- paste(preservation_warnings, collapse = ";")
    diagnostics$nullification_verdict[[i]] <- if (isTRUE(diagnostics$preservation_pass[[i]])) "interpretable_nullifier" else "fails_preservation_gates"
  }
  diagnostics
}

build_nullification_diagnostics_for_files <- function(paths) {
  rows <- lapply(paths, compute_nullification_diagnostic_for_path)
  attach_nullification_reference_metrics(dplyr::bind_rows(rows))
}

build_nullification_diagnostics_for_files_cached <- function(paths, cache_dir, overwrite = FALSE) {
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  worker <- function(path) compute_nullification_diagnostic_cached(path, cache_dir, overwrite = overwrite)
  workers <- diagnostics_workers()
  rows <- diagnostics_map_with_progress(
    paths,
    worker,
    label = "Nullification diagnostics",
    workers = workers
  )
  attach_nullification_reference_metrics(dplyr::bind_rows(rows))
}

write_nullification_diagnostics <- function(diagnostics, output_path = file.path("outputs", "analysis", "nullification_diagnostics.csv")) {
  dir <- dirname(output_path)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  write.csv(diagnostics, output_path, row.names = FALSE)
  invisible(output_path)
}

write_nullification_diagnostics_for_dir <- function(
  input_dir = file.path("data", "processed"),
  output_path = file.path("outputs", "analysis", "nullification_diagnostics.csv"),
  pattern = "^processed__.*\\.(parquet|csv)$"
) {
  paths <- list.files(input_dir, pattern = pattern, full.names = TRUE, ignore.case = TRUE)
  if (length(paths) == 0L) {
    stop("No processed diagnostic files found in ", input_dir)
  }
  mode <- diagnostics_mode()
  if (identical(mode, "metadata")) {
    log_pipeline(
      logger::WARN,
      "Using metadata-only nullification diagnostics for {length(paths)} processed files; set DIAGNOSTICS_MODE=cached or full to read parquet data"
    )
    diagnostics <- metadata_only_nullification_diagnostics(paths)
  } else if (identical(mode, "cached")) {
    cache_dir <- file.path(dirname(output_path), "diagnostics_cache", "nullification")
    log_pipeline(
      logger::INFO,
      "Using cached per-file nullification diagnostics for {length(paths)} processed files in {cache_dir}; workers={diagnostics_workers()}"
    )
    diagnostics <- build_nullification_diagnostics_for_files_cached(
      paths,
      cache_dir = cache_dir,
      overwrite = diagnostics_overwrite()
    )
  } else {
    diagnostics <- build_nullification_diagnostics_for_files(paths)
  }
  write_nullification_diagnostics(diagnostics, output_path)
}
