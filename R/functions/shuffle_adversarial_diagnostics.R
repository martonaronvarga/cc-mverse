# R/functions/shuffle_adversarial_diagnostics.R
# Empirical checks for processed shuffle nullification outputs.

shuffle_group_key <- function(df) {
  paste(df$participant_id, df$cong, sep = "\r")
}

rt_multiset_fingerprint <- function(x, digits = 10L) {
  x <- sort(round(as.numeric(x), digits), na.last = TRUE)
  paste(ifelse(is.na(x), "NA", format(x, scientific = FALSE, trim = TRUE)), collapse = "|")
}

conditional_prev_cong_rt_slope <- function(df) {
  required <- c("participant_id", "cong", "prev_cong", "rt")
  if (!all(required %in% names(df))) return(NA_real_)
  d <- df |>
    dplyr::mutate(
      rt = suppressWarnings(as.numeric(.data$rt)),
      prev_cong_num = suppressWarnings(as.numeric(as.character(.data$prev_cong)))
    ) |>
    dplyr::filter(is.finite(.data$rt), .data$prev_cong_num %in% c(-1, 1))
  if (nrow(d) < 4L || length(unique(d$prev_cong_num)) < 2L) return(NA_real_)
  fit <- try(stats::lm(rt ~ prev_cong_num + factor(participant_id):factor(cong), data = d), silent = TRUE)
  if (inherits(fit, "try-error")) return(NA_real_)
  coef(fit)[["prev_cong_num"]] %||% NA_real_
}

compare_shuffle_multisets <- function(present_df, shuffle_df) {
  required <- c("participant_id", "cong", "rt")
  missing <- setdiff(required, intersect(names(present_df), names(shuffle_df)))
  if (length(missing) > 0L) stop("Missing required shuffle multiset columns: ", paste(missing, collapse = ", "))

  p <- present_df |>
    dplyr::mutate(group_key = shuffle_group_key(present_df)) |>
    dplyr::group_by(.data$group_key) |>
    dplyr::summarise(n_present = dplyr::n(), fp_present = rt_multiset_fingerprint(.data$rt), .groups = "drop")
  s <- shuffle_df |>
    dplyr::mutate(group_key = shuffle_group_key(shuffle_df)) |>
    dplyr::group_by(.data$group_key) |>
    dplyr::summarise(n_shuffle = dplyr::n(), fp_shuffle = rt_multiset_fingerprint(.data$rt), .groups = "drop")

  joined <- dplyr::full_join(p, s, by = "group_key") |>
    dplyr::mutate(
      count_match = .data$n_present == .data$n_shuffle,
      multiset_match = .data$count_match & .data$fp_present == .data$fp_shuffle
    )

  data.frame(
    n_groups = nrow(joined),
    n_count_mismatch_groups = sum(!isTRUE(joined$count_match), na.rm = TRUE),
    n_multiset_mismatch_groups = sum(!isTRUE(joined$multiset_match), na.rm = TRUE),
    multiset_preserved = all(joined$multiset_match, na.rm = FALSE),
    stringsAsFactors = FALSE
  )
}

build_shuffle_adversarial_diagnostic <- function(present_df, shuffle_df, data_id = NA_character_, present_data_id = NA_character_) {
  multiset <- compare_shuffle_multisets(present_df, shuffle_df)
  present_slope <- conditional_prev_cong_rt_slope(present_df)
  shuffle_slope <- conditional_prev_cong_rt_slope(shuffle_df)
  data.frame(
    data_id = data_id,
    present_data_id = present_data_id,
    n_rows_present = nrow(present_df),
    n_rows_shuffle = nrow(shuffle_df),
    row_count_match = nrow(present_df) == nrow(shuffle_df),
    multiset,
    present_prev_cong_rt_slope = present_slope,
    shuffle_prev_cong_rt_slope = shuffle_slope,
    abs_shuffle_prev_cong_rt_slope = abs(shuffle_slope),
    slope_reduction = abs(present_slope) - abs(shuffle_slope),
    shuffle_adversarial_pass = isTRUE(multiset$multiset_preserved) && is.finite(shuffle_slope) && abs(shuffle_slope) <= 5,
    stringsAsFactors = FALSE
  )
}

parse_shuffle_processed_meta <- function(path) {
  meta <- parse_processed_data_id(path)
  meta$path <- path
  meta$key <- paste(meta$sample_size, meta$subsample_id, meta$transformation, meta$outlier, sep = "__")
  meta
}

metadata_only_shuffle_adversarial_diagnostics <- function(paths) {
  metas <- lapply(paths, parse_shuffle_processed_meta)
  rows <- lapply(metas, function(meta) {
    if (!identical(meta$effect_condition, "null_interaction") || !identical(meta$strip_method, "shuffle")) {
      return(NULL)
    }
    data.frame(
      data_id = meta$data_id,
      present_data_id = NA_character_,
      n_rows_present = NA_integer_,
      n_rows_shuffle = NA_integer_,
      row_count_match = NA,
      n_groups = NA_integer_,
      n_count_mismatch_groups = NA_integer_,
      n_multiset_mismatch_groups = NA_integer_,
      multiset_preserved = NA,
      present_prev_cong_rt_slope = NA_real_,
      shuffle_prev_cong_rt_slope = NA_real_,
      abs_shuffle_prev_cong_rt_slope = NA_real_,
      slope_reduction = NA_real_,
      shuffle_adversarial_pass = NA,
      warnings = "metadata_only_fast_diagnostics",
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0L) return(data.frame())
  dplyr::bind_rows(rows)
}

compute_shuffle_adversarial_pair <- function(meta, ref) {
  build_shuffle_adversarial_diagnostic(
    present_df = read_processed_diagnostic_file(ref$path),
    shuffle_df = read_processed_diagnostic_file(meta$path),
    data_id = meta$data_id,
    present_data_id = ref$data_id
  )
}

compute_shuffle_adversarial_pair_cached <- function(meta, ref, cache_dir, overwrite = FALSE) {
  cache_path <- diagnostic_cache_path(cache_dir, meta$data_id)
  if (file.exists(cache_path) && !isTRUE(overwrite)) {
    return(readRDS(cache_path))
  }
  diagnostic <- compute_shuffle_adversarial_pair(meta, ref)
  write_rds_atomic(diagnostic, cache_path)
  diagnostic
}

compute_shuffle_adversarial_cache_path <- function(meta, ref, cache_dir, overwrite = FALSE) {
  cache_path <- diagnostic_cache_path(cache_dir, meta$data_id)
  if (file.exists(cache_path) && !isTRUE(overwrite)) return(cache_path)
  diagnostic <- compute_shuffle_adversarial_pair(meta, ref)
  write_rds_atomic(diagnostic, cache_path)
}

build_shuffle_present_lookup <- function(paths) {
  metas <- lapply(paths, parse_shuffle_processed_meta)
  present <- list()
  for (meta in metas) {
    if (identical(meta$effect_condition, "present") && identical(meta$strip_method, "none")) {
      present[[meta$key]] <- meta
    }
  }
  list(metas = metas, present = present)
}

compute_shuffle_adversarial_cache_paths <- function(paths, all_paths, cache_dir, overwrite = FALSE) {
  lookup <- build_shuffle_present_lookup(all_paths)
  metas <- lapply(paths, parse_shuffle_processed_meta)
  shuffle_metas <- Filter(function(meta) {
    identical(meta$effect_condition, "null_interaction") && identical(meta$strip_method, "shuffle")
  }, metas)
  if (!length(shuffle_metas)) return(character())

  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  worker <- function(meta) {
    ref <- lookup$present[[meta$key]]
    if (is.null(ref)) stop("No matching present branch for shuffle data_id: ", meta$data_id)
    compute_shuffle_adversarial_cache_path(meta, ref, cache_dir, overwrite = overwrite)
  }
  diagnostics_map_with_progress(
    shuffle_metas,
    worker,
    label = "Shuffle adversarial diagnostics cache",
    workers = diagnostics_workers()
  ) |>
    unlist(use.names = FALSE)
}

aggregate_shuffle_adversarial_diagnostic_cache <- function(cache_paths, output_csv) {
  cache_paths <- sort(unique(unlist(cache_paths, use.names = FALSE)))
  diagnostics <- if (length(cache_paths)) dplyr::bind_rows(lapply(cache_paths, readRDS)) else data.frame()
  dir.create(dirname(output_csv), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(diagnostics, output_csv)
  invisible(output_csv)
}

build_shuffle_adversarial_diagnostics_for_files <- function(paths) {
  metas <- lapply(paths, parse_shuffle_processed_meta)
  present <- list()
  for (i in seq_along(paths)) {
    meta <- metas[[i]]
    if (identical(meta$effect_condition, "present") && identical(meta$strip_method, "none")) {
      present[[meta$key]] <- meta
    }
  }

  rows <- lapply(seq_along(paths), function(i) {
    meta <- metas[[i]]
    if (!identical(meta$effect_condition, "null_interaction") || !identical(meta$strip_method, "shuffle")) {
      return(NULL)
    }
    ref <- present[[meta$key]]
    if (is.null(ref)) {
      stop("No matching present branch for shuffle data_id: ", meta$data_id)
    }
    compute_shuffle_adversarial_pair(meta, ref)
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0L) {
    return(data.frame())
  }
  dplyr::bind_rows(rows)
}

build_shuffle_adversarial_diagnostics_for_files_cached <- function(paths, cache_dir, overwrite = FALSE) {
  metas <- lapply(paths, parse_shuffle_processed_meta)
  present <- list()
  for (meta in metas) {
    if (identical(meta$effect_condition, "present") && identical(meta$strip_method, "none")) {
      present[[meta$key]] <- meta
    }
  }
  shuffle_metas <- Filter(function(meta) {
    identical(meta$effect_condition, "null_interaction") && identical(meta$strip_method, "shuffle")
  }, metas)
  if (!length(shuffle_metas)) return(data.frame())

  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  worker <- function(meta) {
    ref <- present[[meta$key]]
    if (is.null(ref)) stop("No matching present branch for shuffle data_id: ", meta$data_id)
    compute_shuffle_adversarial_pair_cached(meta, ref, cache_dir, overwrite = overwrite)
  }
  workers <- diagnostics_workers()
  rows <- diagnostics_map_with_progress(
    shuffle_metas,
    worker,
    label = "Shuffle adversarial diagnostics",
    workers = workers
  )
  dplyr::bind_rows(rows)
}

write_shuffle_adversarial_diagnostics_for_dir <- function(
  input_dir = file.path("data", "processed"),
  output_csv = file.path("outputs", "analysis", "shuffle_adversarial_diagnostics.csv"),
  pattern = "^processed__.*\\.(parquet|csv)$"
) {
  paths <- list.files(input_dir, pattern = pattern, full.names = TRUE, ignore.case = TRUE)
  if (length(paths) == 0L) stop("No processed files found in ", input_dir)
  mode <- if (exists("diagnostics_mode", mode = "function")) diagnostics_mode() else "cached"
  if (identical(mode, "metadata")) {
    log_pipeline(
      logger::WARN,
      "Using metadata-only shuffle adversarial diagnostics for {length(paths)} processed files; set DIAGNOSTICS_MODE=cached or full to read parquet data"
    )
    diagnostics <- metadata_only_shuffle_adversarial_diagnostics(paths)
  } else if (identical(mode, "cached")) {
    cache_dir <- file.path(dirname(output_csv), "diagnostics_cache", "shuffle_adversarial")
    log_pipeline(
      logger::INFO,
      "Using cached per-pair shuffle adversarial diagnostics in {cache_dir}; workers={diagnostics_workers()}"
    )
    diagnostics <- build_shuffle_adversarial_diagnostics_for_files_cached(
      paths,
      cache_dir = cache_dir,
      overwrite = diagnostics_overwrite()
    )
  } else {
    diagnostics <- build_shuffle_adversarial_diagnostics_for_files(paths)
  }
  dir.create(dirname(output_csv), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(diagnostics, output_csv)
  invisible(output_csv)
}
