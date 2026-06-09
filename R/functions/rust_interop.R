# R/functions/rust_interop.R
# Rust processor integration and communication

#' Build Rust processor command
#'
#' @param input_file Path to input CSV
#' @param output_dir Path to output directory
#' @param config Configuration list
#'
#' @return String containing full command
#'
build_rust_args <- function(config, paths, input_csv) {
  subsample_specs <- vapply(config$sample_sizes, function(ss) {
    n_sub <- if (abs(ss - 1.0) < 1e-6) 1L else config$n_subsamples
    paste0(ss, ":", n_sub)
  }, character(1))

  # Sanitize paths
  input_file <- normalizePath(input_csv)
  output_dir <- normalizePath(paths$data_processed)

  # Build parameter lists
  sample_sizes <- paste(config$sample_sizes, collapse = ",")
  subsamples_specs <- paste(subsample_specs, collapse = ",")
  transformations <- paste(config$transformations, collapse = ",")
  outliers <- paste(config$outlier_methods, collapse = ",")
  effect_conditions <- paste(config$effect_conditions, collapse = ",")
  strip_methods <- paste(config$strip_methods, collapse = ",")
  seed <- as.character(config$random_seed)
  threads <- as.character(config$rust_threads)
  writer_threads <- as.character(config$writer_threads)
  metadata <- if (config$save_metadata) "--save-metadata" else ""

  # Build command
  args <- c(
    "--input", input_file,
    "--output-dir", output_dir,
    "--sample-sizes", sample_sizes,
    "--subsamples-per-size", subsamples_specs,
    "--transformations", transformations,
    "--outliers", outliers,
    "--effect-conditions", effect_conditions,
    "--strip-methods", strip_methods,
    "--seed", seed,
    "--threads", threads,
    "--writer-threads", writer_threads,
    "--overwrite",
    "--log-level", tolower(config$log_level)
  )

  if (nzchar(metadata)) {
    args <- c(args, metadata)
  }

  args
}

#' Write Rust CLI arguments to a file for standalone execution.
#'
#' This allows the Rust processor to run as a separate SLURM job
#' with its own CPU allocation, rather than inside the controller.
#'
#' @param config Pipeline configuration
#' @param paths Project paths
#' @param input_csv Path to raw CSV
#' @return Path to the generated arguments file
write_rust_args_file <- function(config, paths, input_csv) {
  args <- build_rust_args(config, paths, input_csv)
  args_file <- file.path(paths$config_objects, "rust_args.txt")

  # Write one argument per line (easy to consume with xargs or read)
  writeLines(args, args_file)
  if (exists("write_processed_cache_signature", mode = "function")) {
    branch_specs <- generate_all_branches(config)
    write_processed_cache_signature(config, paths, branch_specs, input_csv)
  }

  log_pipeline(logger::INFO, "Wrote Rust arguments to {args_file}")
  args_file
}

find_rust_binary <- function(paths) {
  candidates <- c(
    file.path(paths$rust_target, "process"),
    file.path(paths$root, "rust", "target", "release", "process"),
    Sys.which("process")
  )
  for (p in candidates) {
    if (nzchar(p) && file.exists(p)) {
      return(normalizePath(p))
    }
  }
  stop("Rust binary 'process' not found. Run: cargo build --release")
}

#' Generate the standalone Rust SLURM script from config.
#'
#' @param config Pipeline configuration
#' @param paths Project paths
#' @param input_csv Path to raw CSV
#' @param n_cpus Number of CPUs to request for Rust processing
#' @param mem_gb Memory in GB
#' @param time_min Time limit in minutes
#'
#' @return Path to the generated SLURM script
generate_rust_slurm_script <- function(
    config, paths, input_csv,
    n_cpus = 50, mem_gb = 32) {
  args <- build_rust_args(config, paths, input_csv)
  args_str <- paste(shQuote(args), collapse = " \\\n    ")

  binary <- find_rust_binary(paths)

  lines <- c(
    "#!/bin/bash",
    paste0("#SBATCH --job-name=mv_rust_process"),
    paste0("#SBATCH --cpus-per-task=", n_cpus),
    paste0("#SBATCH --mem=", mem_gb, "G"),
    paste0("#SBATCH --partition=", config$slurm_partition),
    paste0("#SBATCH --output=", paths$logs, "/rust_processing_%j.out"),
    paste0("#SBATCH --error=", paths$logs, "/rust_processing_%j.err"),
    "",
    "set -euo pipefail",
    "",
    'echo "[$(date)] Rust processor starting on $(hostname)"',
    'echo "  CPUs allocated: ${SLURM_CPUS_PER_TASK}"',
    "",
    "# Let rayon use all allocated cores",
    'export RAYON_NUM_THREADS="${SLURM_CPUS_PER_TASK}"',
    "",
    paste0(binary, " \\"),
    paste0("    ", args_str),
    "",
    paste0("N_FILES=$(find ", paths$data_processed, ' -name "processed__*.parquet" | wc -l)'),
    'echo "[$(date)] Complete: ${N_FILES} parquet files"'
  )

  script_path <- file.path(paths$root, "slurm_rust_generated.sh")
  writeLines(lines, script_path)
  Sys.chmod(script_path, "0755")

  log_pipeline(logger::INFO, "Generated Rust SLURM script: {script_path} ({n_cpus} CPUs)")
  script_path
}

#' Call Rust data processor
#'
#' Handles compilation, execution, and error management for Rust processor
#'
#' @param input_csv Input data frame
#' @param paths Project paths object
#' @param config Configuration list
#'
#' @return List of processed file paths
#' @deprecated
call_rust_processor <- function(input_csv, paths, config) {
  output_dir <- normalizePath(paths$data_processed, mustWork = FALSE)

  # Check if processing is needed
  if (!config$overwrite_results && dir.exists(output_dir)) {
    existing <- list.files(output_dir, pattern = "^processed__.*\\.parquet$")
    expected_data_branches <- length(unique(
      generate_all_branches(config)$data_id
    ))
    if (length(existing) >= expected_data_branches) {
      log_pipeline(
        logger::INFO,
        "Rust processing skipped: {length(existing)} files already exist"
      )
      return(output_dir)
    }
  }

  binary <- find_rust_binary(paths)
  args <- build_rust_args(config, paths, input_csv)
  if (exists("write_processed_cache_signature", mode = "function")) {
    branch_specs <- generate_all_branches(config)
    write_processed_cache_signature(config, paths, branch_specs, input_csv)
  }

  log_pipeline(logger::INFO, "Calling Rust processor: {binary}")
  log_pipeline(logger::DEBUG, "Args: {paste(args, collapse = ' ')}")

  result <- processx::run(
    command = binary,
    args = args,
    echo_cmd = TRUE,
    echo = TRUE,
    error_on_status = TRUE
  )

  log_pipeline(logger::INFO, "Rust processor completed (status {result$status})")
  output_dir
}


#' Generate simulated cognitive experiment data
#'
#' Creates a realistic RT dataset with congruity and sequential effects
#'
#' @param n_participants Number of participants
#' @param n_trials Trials per participant
#' @param seed Random seed for reproducibility
#'
#' @return Tibble with columns: participant_id, trial, cong, prev_cong, rt
#'
generate_test_data <- function(n_participants, n_trials, seed = 42) {
  set.seed(seed)

  # Generate base structure
  data <- tidyr::expand_grid(
    participant_id = 1:n_participants,
    trial = 1:n_trials
  ) %>%
    dplyr::mutate(
      # Congruity (with persistence)
      cong = sample(c(-1, 1), n(), replace = TRUE, prob = c(0.5, 0.5)),
      participant_id = as.character(participant_id),

      # Previous congruity (shifted within participant)
      prev_cong = dplyr::case_when(
        trial == 1 ~ 1, # First trial baseline
        TRUE ~ dplyr::lag(cong)
      ),
      .by = participant_id,

      # Reaction time with congruity and sequential effects
      congruity_effect = (cong == 1) * 50, # 50ms incongruity cost
      sequence_effect = (cong != prev_cong) * 20, # 20ms switch cost
      participant_effect = rnorm(n_participants, 0, 30)[participant_id],
      trial_effect = trial * 0.05, # Slight slowing over trials
      noise = rnorm(n(), 0, 50),
      rt = 400 + congruity_effect + sequence_effect + participant_effect + trial_effect + noise
    ) %>%
    # Keep only meaningful columns, ensure rt > 0
    dplyr::select(participant_id, cong, prev_cong, rt) %>%
    dplyr::filter(rt > 100) # Remove impossible RTs

  data
}

#' Validate raw experiment data
#'
#' Checks schema, data types, and value ranges
#'
#' @param data Data frame to validate
#'
#' @return Invisibly data if valid, stops with error if not
#'
validate_raw_data <- function(data) {
  # Required columns
  required_cols <- c("participant_id", "cong", "prev_cong", "rt")
  missing <- setdiff(required_cols, names(data))

  if (length(missing) > 0) {
    stop(glue::glue("Missing columns: {paste(missing, collapse=', ')}"))
  }

  # Type checks
  if (!is.numeric(data$rt)) {
    stop("rt must be numeric")
  }

  if (!is.character(data$participant_id)) {
    stop("participant_id must be character")
  }

  if (!is.numeric(data$cong) && !is.numeric(data$prev_cong)) {
    stop("cong & prev_cong must be numeric")
  }

  # Range checks
  if (any(data$rt < 0)) {
    stop("rt values cannot be negative")
  }

  if (any(data$rt > 5000)) {
    logger::log_warn("Some RT values > 5000ms, may indicate errors/instructions")
  }

  # Sample size checks
  n_participants <- dplyr::n_distinct(data$participant_id)
  n_obs <- nrow(data)

  if (n_participants < 1) {
    stop("Must have at least 1 participant")
  }

  if (n_obs < n_participants) {
    stop("Must have multiple trials per participant")
  }

  logger::log_debug(
    "Data validated: {n_participants} participants, {n_obs} total observations"
  )

  data
}
