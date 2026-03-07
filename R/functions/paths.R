# R/functions/paths.R - Path Management
# Robust path utilities with validation

#' Initialize project directory structure
#'
#' @param project_root Root directory (default: current directory)
#'
#' @return List of validated project paths
#'
init_project_paths <- function(project_root = ".") {
  log_pipeline(logger::INFO, "Initializing project paths at: {project_root}")

  paths <- list(
    root = normalizePath(project_root),
    config_objects = file.path(project_root, "_config"),

    # Data directories
    data_raw = file.path(project_root, "data", "raw"),
    data_processed = file.path(project_root, "data", "processed"),
    data_metadata = file.path(project_root, "data", "metadata"),

    # Output directories
    outputs = file.path(project_root, "outputs"),
    outputs_results = file.path(project_root, "outputs", "results"),
    outputs_analysis = file.path(project_root, "outputs", "analysis"),

    # Logging
    logs = file.path(project_root, "logs"),

    # Rust compilation
    rust_src = file.path(project_root, "rust", "src"),
    rust_target = file.path(project_root, "rust", "target", "release")
  )

  # Create all directories
  purrr::walk(paths, ~ {
    if (!dir.exists(.x)) {
      dir.create(.x, recursive = TRUE, showWarnings = FALSE)
      log_pipeline(logger::DEBUG, "Created directory: {.x}")
    }
  })

  # Validate all paths exist
  all_exist <- purrr::map_lgl(paths, dir.exists)
  if (!all(all_exist)) {
    failed <- names(paths)[!all_exist]
    log_pipeline(logger::ERROR, "Failed to create directories: {paste(failed, collapse=', ')}")
    stop("Path initialization failed")
  }

  structure(
    paths,
    class = c("project_paths", "list"),
    initialized_at = Sys.time()
  )
}

#' Save pipeline state for _targets.R
#'
#' Only saves what _targets.R cannot reconstruct from pipeline.yaml alone:
#' the expanded branch table and the run identity.
save_pipeline_state <- function(config, paths, branches, logging) {
  dir <- paths$config_objects
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  qs2::qs_save(config, file.path(dir, "config.qs2"))
  qs2::qs_save(paths, file.path(dir, "paths.qs2"))
  qs2::qs_save(branches, file.path(dir, "branches.qs2"))
  qs2::qs_save(logging, file.path(dir, "logging.qs2"))

  log_pipeline(logger::INFO, "Pipeline state saved to {dir}")
  invisible(TRUE)
}

#' Load pipeline state for _targets.R
load_pipeline_state <- function(config_dir = "_config") {
  required <- c("config.qs2", "paths.qs2", "branches.qs2", "logging.qs2")
  missing <- required[!file.exists(file.path(config_dir, required))]

  if (length(missing) > 0) {
    stop(
      "Missing state files in ", config_dir, ": ", paste(missing, collapse = ", "),
      "\nRun `Rscript run.R` first."
    )
  }

  list(
    config   = qs2::qs_read(file.path(config_dir, "config.qs2")),
    paths    = qs2::qs_read(file.path(config_dir, "paths.qs2")),
    branches = qs2::qs_read(file.path(config_dir, "branches.qs2")),
    logging  = qs2::qs_read(file.path(config_dir, "logging.qs2"))
  )
}

#' Get path to processed data file for a branch
#'
#' @param paths Project paths object
#' @param branch_spec Branch specification tibble row
#'
#' @return Path to processed data file
#'
get_processed_data_path <- function(paths, data_id) {
  file.path(
    paths$data_processed,
    paste0("processed__", data_id, ".parquet")
  )
}

#' Derive data_id from branch_id
#'
#' branch_id format (7 segments):
#'   sample_size__subsample_id__transformation__outlier__model__effect_condition__strip_method
#' data_id format (6 segments, model dropped):
#'   sample_size__subsample_id__transformation__outlier__effect_condition__strip_method
data_id_from_branch_id <- function(branch_id) {
  vapply(branch_id, function(bid) {
    bid <- trimws(bid)
    parts <- trimws(strsplit(bid, "__", fixed = TRUE)[[1]])
    if (length(parts) != 7L) {
      stop(
        "branch_id must have exactly 7 '__'-separated segments, got ",
        length(parts), ": ", bid
      )
    }
    # Drop segment 5 (model) — keep 1,2,3,4,6,7
    paste(parts[c(1L, 2L, 3L, 4L, 6L, 7L)], collapse = "__")
  }, character(1), USE.NAMES = FALSE)
}

#' Check if processed data exists for a branch
#'
#' @param paths Project paths
#' @param branch_spec Branch specification
#'
#' @return Logical
#'
processed_data_exists <- function(paths, branch_spec) {
  filepath <- get_processed_data_path(paths, branch_spec)
  file.exists(filepath)
}

#' Get path to results file
#'
#' @param paths Project paths
#' @param filepaths where results are located
#'
#' @return Path to results file
#'

get_results_path <- function(paths) {
  dir <- paths$outputs_results
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}

#' Get path to analysis output
#'
#' @param paths Project paths
#' @param analysis_name Name of analysis
#'
#' @return Path to analysis file
#'
get_analysis_path <- function(paths, analysis_name) {
  filename <- glue::glue("{analysis_name}_{format(Sys.time(), '%Y%m%d_%H%M%S')}.parquet")

  file.path(paths$outputs_analysis, filename)
}

#' Get path to metadata file
#'
#' @param paths Project paths
#'
#' @return Path to metadata file
#'
get_metadata_path <- function(paths) {
  file.path(paths$outputs, "processing_metadata.parquet")
}

#' Safe read of parquet file with validation
#'
#' @param filepath Path to parquet file
#' @param required_cols Optional vector of required column names
#'
#' @return Data frame or tibble
#'
safe_read_parquet <- function(filepath, required_cols = NULL) {
  if (!file.exists(filepath)) {
    log_pipeline(logger::ERROR, "File not found: {filepath}")
    stop(glue::glue("File not found: {filepath}"))
  }

  log_pipeline(logger::DEBUG, "Reading parquet: {filepath}")

  tryCatch(
    {
      df <- arrow::read_parquet(filepath)

      if (!is.null(required_cols)) {
        missing <- setdiff(required_cols, names(df))
        if (length(missing) > 0) {
          log_pipeline(logger::ERROR, "Missing columns in {filepath}: {paste(missing, collapse=', ')}")
          stop(glue::glue("Missing columns: {paste(missing, collapse=', ')}"))
        }
      }

      df
    },
    error = function(e) {
      log_pipeline(logger::ERROR, "Failed to read {filepath}: {e$message}")
      stop(glue::glue("Failed to read {filepath}: {e$message}"))
    }
  )
}

#' Safe write of parquet file (atomic)
#'
#' @param data Data frame to write
#' @param filepath Path to write to
#' @param append If TRUE, append to existing file; if FALSE, overwrite
#'
#' @return Invisibly returns filepath
#'
safe_write_parquet <- function(data, filepath, append = FALSE) {
  log_pipeline(logger::DEBUG, "Writing parquet: {filepath}")

  # Create directory if needed
  dir <- dirname(filepath)
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }

  tryCatch(
    {
      if (append && file.exists(filepath)) {
        log_pipeline(logger::DEBUG, "Appending to existing file: {filepath}")
        existing <- arrow::read_parquet(filepath)
        combined <- dplyr::bind_rows(existing, data)
        arrow::write_parquet(combined, filepath)
      } else {
        arrow::write_parquet(data, filepath)
      }

      log_pipeline(logger::DEBUG, "Wrote {nrow(data)} rows to {filepath}")
      invisible(filepath)
    },
    error = function(e) {
      log_pipeline(logger::ERROR, "Failed to write {filepath}: {e$message}")
      stop(glue::glue("Failed to write {filepath}: {e$message}"))
    }
  )
}

#' Print project paths summary
#'
#' @param paths Project paths object
#'
print.project_paths <- function(x, ...) {
  cat("Project Paths:\n")
  cat("  Root:", x$root, "\n")
  cat("  Data (raw):", x$data_raw, "\n")
  cat("  Data (processed):", x$data_processed, "\n")
  cat("  Outputs:", x$outputs, "\n")
  cat("  Logs:", x$logs, "\n")

  invisible(x)
}
