# R/functions/rust_interop.R
# Rust processor integration and communication

#' Call Rust data processor
#'
#' Handles compilation, execution, and error management for Rust processor
#'
#' @param raw_data Input data frame
#' @param paths Project paths object
#' @param config Configuration list
#'
#' @return List of processed file paths
#'
call_rust_processor <- function(raw_data, paths, config) {
  logger::log_info("Preparing Rust processor")

  # Write raw data to temp CSV
  tmp_input <- tempfile(fileext = ".csv")
  write_csv(raw_data, tmp_input)

  logger::log_debug("Raw data written to: {tmp_input}")

  # Build Rust command
  rust_cmd <- build_rust_command(
    input_file = tmp_input,
    output_dir = paths$data_processed,
    config = config
  )

  logger::log_info("Executing Rust processor")
  logger::log_debug("Command: {substr(rust_cmd, 1, 100)}...")

  status <- tryCatch(
    system(rust_cmd, ignore.stderr = FALSE),
    error = function(e) {
      logger::log_error("Rust execution failed: {e$message}")
      1 # Non-zero status
    }
  )

  if (status != 0) {
    logger::log_error("Rust processor exited with status {status}")
    stop("Data processing failed")
  }

  # Cleanup temp file
  unlink(tmp_input)

  # Return list of generated files
  processed_files <- list.files(
    paths$data_processed,
    pattern = "\\.parquet$",
    full.names = TRUE
  )

  logger::log_info("Rust processor generated {length(processed_files)} files")

  processed_files
}

#' Build Rust processor command
#'
#' @param input_file Path to input CSV
#' @param output_dir Path to output directory
#' @param config Configuration list
#'
#' @return String containing full command
#'
build_rust_command <- function(input_file, output_dir, config) {
  # Sanitize paths
  input_file <- normalizePath(input_file)
  output_dir <- normalizePath(output_dir)

  # Build parameter lists
  sample_sizes <- paste(config$sample_sizes, collapse = ",")
  transformations <- paste(config$transformations, collapse = ",")
  outliers <- paste(config$outlier_methods, collapse = ",")
  effect_conditions <- paste(config$effect_conditions, collapse = ",")
  strip_methods <- paste(config$strip_methods, collapse = ",")

  # Build command
  cmd <- glue::glue(
    "cd {shQuote(file.path(config$project_root, 'rust'))} && ",
    "cargo run {if (config$rust_release) '--release' else ''} --bin process -- ",
    "--input {input_file} ",
    "--output-dir {output_dir} ",
    "--sample-sizes {sample_sizes} ",
    "--transformations {transformations} ",
    "--outliers {outliers} ",
    "--effect-conditions {effect_conditions} ",
    "--strip-methods {strip_methods} ",
    "--seed {config$random_seed} ",
    "{if (config$save_metadata) '--save-metadata' else ''} ",
    "--log-level {tolower(config$log_level)}"
  )

  cmd
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
