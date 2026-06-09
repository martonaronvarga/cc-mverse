# R/functions/cache_signature.R - processed parquet cache signatures

processed_cache_signature_path <- function(paths) {
  file.path(paths$data_metadata, "processed_cache_signature.rds")
}

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x
}

normalize_existing_files <- function(files) {
  files <- unique(as.character(files))
  files <- files[!is.na(files) & nzchar(files)]
  files <- files[file.exists(files)]
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

rust_binary_for_signature <- function(paths) {
  candidates <- c(
    file.path(paths$rust_target, "process"),
    file.path(paths$root, "rust", "target", "release", "process"),
    Sys.which("process")
  )
  candidates <- candidates[nzchar(candidates)]
  existing <- candidates[file.exists(candidates)]
  if (length(existing) > 0L) existing[[1]] else character()
}

hash_object_md5 <- function(x) {
  tmp <- tempfile("cache_signature_", fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(x, tmp, version = 2)
  unname(tools::md5sum(tmp))
}

processed_cache_payload <- function(config, paths, branch_specs, input_csv) {
  input_csv <- normalizePath(input_csv, winslash = "/", mustWork = TRUE)
  expected_data_ids <- sort(unique(as.character(branch_specs$data_id)))

  list(
    schema_version = 3L,
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
    rust_processor = file_manifest(rust_binary_for_signature(paths), paths$root)
  )
}

build_processed_cache_signature <- function(config, paths, branch_specs, input_csv) {
  payload <- processed_cache_payload(config, paths, branch_specs, input_csv)
  list(
    signature = hash_object_md5(payload),
    expected_file_count = length(payload$expected_data_ids),
    expected_data_ids = payload$expected_data_ids,
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

cache_payload_diff_summary <- function(stored, current) {
  if (is.null(stored$payload) || is.null(current$payload)) {
    return("stored or current signature lacks payload")
  }

  fields <- union(names(stored$payload), names(current$payload))
  changed <- fields[vapply(fields, function(field) {
    !identical(stored$payload[[field]], current$payload[[field]])
  }, logical(1))]

  if (length(changed) == 0L) return("payloads differ only by signature metadata")

  details <- vapply(changed, function(field) {
    if (field == "expected_data_ids") {
      stored_ids <- stored$payload$expected_data_ids %||% character()
      current_ids <- current$payload$expected_data_ids %||% character()
      return(sprintf(
        "expected_data_ids: stored=%d current=%d added=%d removed=%d",
        length(stored_ids), length(current_ids),
        length(setdiff(current_ids, stored_ids)),
        length(setdiff(stored_ids, current_ids))
      ))
    }
    if (field %in% c("raw_input", "rust_processor")) {
      stored_manifest <- stored$payload[[field]]
      current_manifest <- current$payload[[field]]
      stored_paths <- stored_manifest$path %||% character()
      current_paths <- current_manifest$path %||% character()
      common <- intersect(stored_paths, current_paths)
      stored_idx <- match(common, stored_manifest$path)
      current_idx <- match(common, current_manifest$path)
      changed_paths <- common[stored_manifest$md5[stored_idx] != current_manifest$md5[current_idx]]
      return(sprintf(
        "%s: stored_files=%d current_files=%d changed=%s",
        field, length(stored_paths), length(current_paths),
        paste(head(changed_paths, 5), collapse = ", ")
      ))
    }
    field
  }, character(1))

  paste(details, collapse = "\n")
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
      "Processed parquet cache is stale for the current Rust binary/config inputs.",
      "\nStored signature:  ", stored$signature,
      "\nCurrent signature: ", current$signature,
      "\nChanged payload fields:\n", cache_payload_diff_summary(stored, current),
      "\nRegenerate processed parquet files or rewrite the signature only if the files were produced by this exact Rust binary/config."
    )
  }

  invisible(current)
}
