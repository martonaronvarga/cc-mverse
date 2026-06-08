# R/functions/cache_signature.R - processed parquet cache signatures

processed_cache_signature_path <- function(paths) {
  file.path(paths$data_metadata, "processed_cache_signature.rds")
}

normalize_existing_files <- function(files) {
  files <- unique(as.character(files))
  files <- files[nzchar(files) & file.exists(files)]
  normalizePath(files, winslash = "/", mustWork = TRUE)
}

relative_to_root <- function(files, root) {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  files <- normalizePath(files, winslash = "/", mustWork = TRUE)
  prefix <- paste0(root, "/")
  ifelse(startsWith(files, prefix), substring(files, nchar(prefix) + 1L), files)
}

file_manifest <- function(files, root) {
  files <- normalize_existing_files(files)
  if (length(files) == 0L) {
    return(data.frame(
      path = character(),
      size = numeric(),
      md5 = character(),
      stringsAsFactors = FALSE
    ))
  }

  info <- file.info(files)
  manifest <- data.frame(
    path = relative_to_root(files, root),
    size = unname(info$size),
    md5 = unname(tools::md5sum(files)),
    stringsAsFactors = FALSE
  )
  manifest[order(manifest$path), , drop = FALSE]
}

list_processed_cache_dependency_files <- function(paths) {
  root <- paths$root
  rust_src <- file.path(root, "rust", "src")
  rust_files <- if (dir.exists(rust_src)) {
    list.files(rust_src, pattern = "[.]rs$", recursive = TRUE, full.names = TRUE)
  } else {
    character()
  }

  # Only include implementation files. Runtime resource changes in pipeline.yaml
  # or .pipeline.resolved.yaml (worker counts, memory, time limits, logging) must
  # not invalidate already materialized processed parquet data. Semantic analysis
  # axes are captured separately in build_processed_cache_signature().
  normalize_existing_files(c(
    file.path(root, "functions", "config.R"),
    file.path(root, "functions", "paths.R"),
    file.path(root, "functions", "rust_interop.R"),
    file.path(root, "rust", "Cargo.toml"),
    file.path(root, "rust", "Cargo.lock"),
    rust_files
  ))
}

hash_object_md5 <- function(x) {
  tmp <- tempfile("cache_signature_", fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(x, tmp, version = 2)
  unname(tools::md5sum(tmp))
}

build_processed_cache_signature <- function(config, paths, branch_specs, input_csv) {
  input_csv <- normalizePath(input_csv, winslash = "/", mustWork = TRUE)
  expected_data_ids <- sort(unique(as.character(branch_specs$data_id)))

  payload <- list(
    schema_version = 2L,
    processing_config = list(
      raw_csv = config$raw_csv,
      random_seed = config$random_seed,
      sample_sizes = as.numeric(config$sample_sizes),
      n_subsamples = as.integer(config$n_subsamples),
      transformations = as.character(config$transformations),
      outlier_methods = as.character(config$outlier_methods),
      effect_conditions = as.character(config$effect_conditions),
      strip_methods = as.character(config$strip_methods)
    ),
    expected_data_ids = expected_data_ids,
    raw_input = file_manifest(input_csv, paths$root),
    dependency_files = file_manifest(list_processed_cache_dependency_files(paths), paths$root)
  )

  list(
    signature = hash_object_md5(payload),
    expected_file_count = length(expected_data_ids),
    expected_data_ids = expected_data_ids,
    payload = payload
  )
}

write_processed_cache_signature <- function(config, paths, branch_specs, input_csv, path = processed_cache_signature_path(paths)) {
  sig <- build_processed_cache_signature(config, paths, branch_specs, input_csv)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(sig, path)
  log_pipeline(logger::INFO, "Wrote processed cache signature: {path}")
  invisible(path)
}

validate_processed_cache_signature <- function(config, paths, branch_specs, input_csv, current = NULL, path = processed_cache_signature_path(paths)) {
  if (is.null(current)) {
    current <- build_processed_cache_signature(config, paths, branch_specs, input_csv)
  }
  if (!file.exists(path)) {
    stop(
      "Processed cache signature is missing: ", path,
      "\nRegenerate processed parquet files with the current Rust/config inputs."
    )
  }

  stored <- readRDS(path)
  if (!identical(stored$signature, current$signature)) {
    stop(
      "Processed parquet cache is stale for the current Rust/config inputs.",
      "\nStored signature:  ", stored$signature,
      "\nCurrent signature: ", current$signature,
      "\nRegenerate processed parquet files before fitting models."
    )
  }

  invisible(current)
}
