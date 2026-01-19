# R/functions/paths.R - Path Management
# Robust path utilities with validation

#' Initialize project directory structure
#'
#' @param project_root Root directory (default: current directory)
#'
#' @return List of validated project paths
#'
init_project_paths <- function(project_root = ".") {
  logger::log_info("Initializing project paths at: {project_root}")

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
      logger::log_debug("Created directory: {.x}")
    }
  })

  # Validate all paths exist
  all_exist <- purrr::map_lgl(paths, dir.exists)
  if (!all(all_exist)) {
    failed <- names(paths)[!all_exist]
    logger::log_error("Failed to create directories: {paste(failed, collapse=', ')}")
    stop("Path initialization failed")
  }

  structure(
    paths,
    class = c("project_paths", "list"),
    initialized_at = Sys.time()
  )
}

#' Save pipeline configuration/state objects for `_targets.R`
#'
#' @param config Pipeline configuration list
#' @param paths Project paths object
#' @param branches Tibble of branch specifications
#' @param targets_conf Targets configuration object
#' @param logging Logging configuration
#'
#' @return Invisibly returns TRUE on success
#'
save_pipeline_state <- function(config, paths, branches, targets_conf, logging) {
  # Ensure directory exists
  if (!dir.exists(paths$config_objects)) {
    dir.create(paths$config_objects, recursive = TRUE, showWarnings = FALSE)
  }

  logger::log_info("Saving pipeline state to {paths$config_objects}")

  tryCatch(
    {
      qs2::qs_save(config, file.path(paths$config_objects, "config.qs2"))
      qs2::qs_save(paths, file.path(paths$config_objects, "paths.qs2"))
      qs2::qs_save(branches, file.path(paths$config_objects, "branches.qs2"))
      qs2::qs_save(targets_conf, file.path(paths$config_objects, "targets_conf.qs2"))
      qs2::qs_save(logging, file.path(paths$config_objects, "logging.qs2"))

      logger::log_debug("Pipeline state saved: config, paths, branches, targets_conf, logging")
      invisible(TRUE)
    },
    error = function(e) {
      logger::log_error("Failed to save pipeline state: {e$message}")
      stop(e)
    }
  )
}

#' Load pipeline configuration/state objects for targets
#'
#' Loads the 5 objects saved by `save_pipeline_state()`
#'
#' @param config_dir Path to config storage directory (default: "_config")
#'
#' @return Named list: config, paths, branches, targets_conf, logging
#'
load_pipeline_state <- function(config_dir = "_config") {
  logger::log_info("Loading pipeline state from {config_dir}")

  required <- c(
    "config.qs2",
    "paths.qs2",
    "branches.qs2",
    "targets_conf.qs2",
    "logging.qs2"
  )

  full_paths <- file.path(config_dir, required)

  # Check existence
  missing <- !file.exists(full_paths)
  if (any(missing)) {
    missing_files <- required[missing]
    logger::log_error("Missing pipeline state files: {paste(missing_files, collapse=', ')}")
    stop("Cannot load pipeline state: missing files")
  }

  tryCatch(
    {
      list(
        config        = qs2::qs_read(file.path(config_dir, "config.qs2")),
        paths         = qs2::qs_read(file.path(config_dir, "paths.qs2")),
        branches      = qs2::qs_read(file.path(config_dir, "branches.qs2")),
        targets_conf  = qs2::qs_read(file.path(config_dir, "targets_conf.qs2")),
        logging       = qs2::qs_read(file.path(config_dir, "logging.qs2"))
      )
    },
    error = function(e) {
      logger::log_error("Failed to load pipeline state: {e$message}")
      stop(e)
    }
  )
}

#' Get path to processed data file for a branch
#'
#' @param paths Project paths object
#' @param branch_spec Branch specification tibble row
#'
#' @return Path to processed data file
#'
get_processed_data_path <- function(paths, branch_id) {
  # filename <- glue::glue(
  #   "processed__{branch_spec$sample_size}__{branch_spec$transformation}__{branch_spec$outlier}__{branch_spec$effect_condition}__{branch_spec$strip_method}.parquet"
  # )
  # Parse branch_id to extract components (without model)
  # branch_id format: <sample_size>__<transformation>__<outlier>__<model>__<effect_condition>__<strip_method>

  parts <- strsplit(branch_id, "__", fixed = TRUE)[[1]]

  if (length(parts) != 6) {
    stop("Invalid branch_id format:  ", branch_id, " (expected 6 parts)")
  }

  # Extract components
  sample_size <- parts[1]
  transformation <- parts[2]
  outlier <- parts[3]
  # parts[4] is model - SKIP IT
  effect_condition <- parts[5]
  strip_method <- parts[6]

  # Construct processed file ID WITHOUT model
  processed_id <- paste(sample_size, transformation, outlier, effect_condition, strip_method, sep = "__")

  # Construct full path
  file.path(
    paths$data_processed,
    paste0("processed__", processed_id, ".parquet")
  )
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
    logger::log_error("File not found: {filepath}")
    stop(glue::glue("File not found: {filepath}"))
  }

  logger::log_debug("Reading parquet: {filepath}")

  tryCatch(
    {
      df <- arrow::read_parquet(filepath)

      if (!is.null(required_cols)) {
        missing <- setdiff(required_cols, names(df))
        if (length(missing) > 0) {
          logger::log_error("Missing columns in {filepath}: {paste(missing, collapse=', ')}")
          stop(glue::glue("Missing columns: {paste(missing, collapse=', ')}"))
        }
      }

      df
    },
    error = function(e) {
      logger::log_error("Failed to read {filepath}: {e$message}")
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
  logger::log_debug("Writing parquet: {filepath}")

  # Create directory if needed
  dir <- dirname(filepath)
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }

  tryCatch(
    {
      if (append && file.exists(filepath)) {
        logger::log_debug("Appending to existing file: {filepath}")
        existing <- arrow::read_parquet(filepath)
        combined <- dplyr::bind_rows(existing, data)
        arrow::write_parquet(combined, filepath)
      } else {
        arrow::write_parquet(data, filepath)
      }

      logger::log_info("Wrote {nrow(data)} rows to {filepath}")
      invisible(filepath)
    },
    error = function(e) {
      logger::log_error("Failed to write {filepath}: {e$message}")
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
