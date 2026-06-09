# R/functions/branch_chunks.R - coarse-grained model-fitting shards
#
# Keep the high-cardinality multiverse grid as data, not as target graph
# structure. targets sees O(number of chunks), while each worker executes a
# sequential chunk of branch fits and writes one parquet file.

model_chunk_size <- function(config) {
  size <- config$model_chunk_size %||% 50L
  size <- as.integer(size)
  if (is.na(size) || size < 1L) {
    stop("model_chunk_size must be a positive integer")
  }
  size
}

model_runtime_priority <- function(model) {
  dplyr::case_when(
    grepl("rmanova", model) ~ 1L,
    grepl("intercept", model) ~ 2L,
    grepl("cong_slope", model) ~ 3L,
    grepl("full_slope", model) ~ 4L,
    TRUE ~ 5L
  )
}

split_branch_specs <- function(branch_specs, config) {
  if (!is.data.frame(branch_specs) || nrow(branch_specs) == 0L) {
    stop("branch_specs must be a non-empty data frame")
  }

  size <- model_chunk_size(config)
  out <- branch_specs %>%
    dplyr::mutate(runtime_priority = model_runtime_priority(.data$model)) %>%
    dplyr::arrange(.data$runtime_priority, .data$idx) %>%
    dplyr::mutate(chunk_id = as.integer((dplyr::row_number() - 1L) %/% size + 1L)) %>%
    split(.$chunk_id)

  log_pipeline(
    logger::INFO,
    "Split {nrow(branch_specs)} model branches into {length(out)} chunk(s) of up to {size} branch(es)"
  )

  unname(out)
}

filter_missing_branch_chunks <- function(branch_chunks, paths, overwrite = FALSE) {
  if (isTRUE(overwrite)) return(branch_chunks)
  missing <- Filter(function(chunk) {
    chunk_id <- as.integer(unique(chunk$chunk_id))
    !file.exists(result_path_for_chunk(paths, chunk_id))
  }, branch_chunks)
  log_pipeline(
    logger::INFO,
    "Model chunks pending: {length(missing)} missing / {length(branch_chunks)} total"
  )
  unname(missing)
}

write_branch_chunk_manifest <- function(branch_chunks, paths) {
  manifest <- purrr::map_dfr(branch_chunks, function(chunk) {
    tibble::tibble(
      chunk_id = as.integer(unique(chunk$chunk_id)),
      n_branches = nrow(chunk),
      first_idx = min(chunk$idx),
      last_idx = max(chunk$idx),
      result_path = result_path_for_chunk(paths, unique(chunk$chunk_id))
    )
  })

  out <- file.path(paths$outputs, "model_branch_chunks.csv")
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(manifest, out)
  normalizePath(out, winslash = "/", mustWork = TRUE)
}
