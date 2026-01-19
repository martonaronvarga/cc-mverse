# R/functions/config.R - Configuration Management
# Centralized configuration with validation

#' Load and validate pipeline configuration
#'
#' @details
#' Configuration is loaded from environment variables with sensible defaults.
#' All paths are validated and created if necessary.
#'
#' @param run_mode One of: "local", "test", "hpc"
#'
#' @return List of validated configuration parameters
#'
load_pipeline_config <- function(run_mode = "local") {
  logger::log_info("Loading pipeline configuration for mode: {run_mode}")

  config <- list(
    # Runtime mode
    run_mode = run_mode,
    is_local = run_mode %in% c("local", "test"),
    is_hpc = run_mode == "hpc",
    is_test = run_mode == "test",
    use_hpc = as.logical(Sys.getenv("USE_HPC", tolower(run_mode) == "hpc")),
    project_root = normalizePath(getwd()),


    # Data generation
    use_simulated_data = as.logical(Sys.getenv("USE_SIMULATED_DATA", "TRUE")),
    n_participants = as.numeric(Sys.getenv("N_PARTICIPANTS", "20")),
    n_trials = as.numeric(Sys.getenv("N_TRIALS", "100")),
    random_seed = as.numeric(Sys.getenv("RANDOM_SEED", "42")),

    # Analysis axes
    sample_sizes = c(0.5, 0.75, 1.0),
    transformations = c("log_rt", "no_log_rt"),
    outlier_methods = c(
      "sd_2", "sd_2.5", "sd_3", "mad_2", "mad_2.5", "mad_3",
      "range_1000", "range_1250", "range_1500", "none"
    ),
    effect_conditions = c("present", "null_interaction", "null_both"),
    strip_methods = c("shuffle", "qmap_5"),

    # Model specifications
    models = list(
      rmanova = list(
        type = "rmanova",
        formula_full = "rt ~ cong * prev_cong + Error(participant_id/(cong * prev_cong))",
        formula_null = "NA"
      ),
      lmm_intercept = list(
        type = "lmm",
        formula_full = "rt ~ cong * prev_cong + (1 | participant_id)",
        formula_null = "rt ~ 1 + (1 | participant_id)",
        control = list(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
      ),
      lmm_cong_slope = list(
        type = "lmm",
        formula_full = "rt ~ cong * prev_cong + (1 + cong | participant_id)",
        formula_null = "rt ~ 1 + (1 + cong | participant_id)",
        control = list(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
      ),
      lmm_full_slope = list(
        type = "lmm",
        formula_full = "rt ~ cong * prev_cong + (1 + cong * prev_cong | participant_id)",
        formula_null = "rt ~ 1 + (1 + cong * prev_cong | participant_id)",
        control = list(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
      )
    ),

    # Logging configuration
    log_level = Sys.getenv("LOG_LEVEL", "INFO"),
    log_targets = TRUE,
    save_logs = TRUE,
    show_warnings = TRUE,

    # HPC configuration
    n_workers = as.numeric(Sys.getenv("N_WORKERS", "4")),
    slurm_mem_gb = as.numeric(Sys.getenv("SLURM_MEM_GB", "4")),
    slurm_time_min = as.numeric(Sys.getenv("SLURM_TIME_MIN", "60")),
    slurm_partition = Sys.getenv("SLURM_PARTITION", "hpc2019"),

    # Rust compilation
    rust_release = TRUE, # Always use release builds for performance
    rust_threads = as.numeric(Sys.getenv("RUST_THREADS", "0")), # 0 = auto

    # Output configuration
    save_processed_data = TRUE, # Keep intermediate parquets
    save_metadata = TRUE, # Save processing metadata
    overwrite_results = as.logical(Sys.getenv("OVERWRITE_RESULTS", "FALSE"))
  )

  # Validate configuration
  validate_config(config)

  logger::log_info("Configuration loaded: mode={run_mode}, workers={config$n_workers}")

  config
}

#' Validate configuration parameters
#'
#' @param config Configuration list
#'
validate_config <- function(config) {
  # Validate modes
  if (!config$run_mode %in% c("local", "test", "hpc")) {
    stop(glue::glue("Invalid run_mode: {config$run_mode}"))
  }

  # Validate numeric parameters
  if (config$n_participants < 1) {
    stop("n_participants must be >= 1")
  }

  if (config$n_trials < 1) {
    stop("n_trials must be >= 1")
  }

  # For test mode, reduce data
  if (config$is_test) {
    logger::log_warn("TEST MODE: reducing data and branch count")
    config$sample_sizes <- c(0.75)
    config$transformations <- c("log_rt")
    config$outlier_methods <- c("none")
    config$effect_conditions <- c("present")
    config$strip_methods <- c("shuffle")
    config$n_workers <- 1
  }

  if (config$use_hpc) {
    if (config$slurm_mem_gb < 2) {
      stop("SLURM memory must be >= 2GB")
    }
    if (config$slurm_time_min < 10) {
      stop("SLURM time must be >= 10 minutes")
    }
  }

  invisible(config)
}

#' Generate all branch combinations
#'
#' @param config Configuration list
#'
#' @return Tibble with one row per branch combination
#'
generate_all_branches <- function(config) {
  logger::log_info("Generating branch specifications")

  branches_present <- tidyr::expand_grid(
    sample_size = config$sample_sizes,
    transformation = config$transformations,
    outlier = config$outlier_methods,
    model = names(config$models),
    effect_condition = c("present", "null_both"),
    strip_method = "none"
  )

  branches_other <- tidyr::expand_grid(
    sample_size = config$sample_sizes,
    transformation = config$transformations,
    outlier = config$outlier_methods,
    model = as.character(names(config$models)),
    effect_condition = setdiff(config$effect_conditions, c("present", "null_both")),
    strip_method = config$strip_methods
  )

  branches <- dplyr::bind_rows(branches_present, branches_other) |>
    dplyr::mutate(
      branch_id = paste(sample_size, transformation, outlier, model, effect_condition, strip_method, sep = "__"),
      n_combinations = dplyr::n()
    ) |>
    tibble::rowid_to_column("idx")

  logger::log_info("Generated {nrow(branches)} branch combinations")

  branches
}

format_sample_size_for_rust <- function(x) {
  # Use formatC with "fg" and then strip trailing .0
  s <- formatC(x, digits = 15, format = "fg")
  sub("\\.0$", "", s)
}

# Validate and normalize components to the exact strings used by Rust.
normalize_transformation <- function(x) {
  x <- as.character(x)
  if (x %in% c("log_rt", "no_log_rt")) {
    return(x)
  }
  stop("Unknown transformation: ", x)
}

normalize_outlier <- function(x) {
  x <- as.character(x)
  allowed <- c(
    "sd_2", "sd_2.5", "sd_3",
    "mad_2", "mad_2.5", "mad_3",
    "range_1000", "range_1250", "range_1500",
    "none"
  )
  if (x %in% allowed) {
    return(x)
  }
  stop("Unknown outlier method: ", x)
}

normalize_effect_condition <- function(x) {
  x <- as.character(x)
  if (x %in% c("present", "null_interaction", "null_both")) {
    return(x)
  }
  stop("Unknown effect condition: ", x)
}

normalize_strip_method <- function(x, effect_condition) {
  effect_condition <- normalize_effect_condition(effect_condition)
  x <- as.character(x)
  if (effect_condition == "null_interaction") {
    if (x %in% c("shuffle", "qmap_5")) {
      return(x)
    }
    stop("Unknown strip method for null_interaction: ", x)
  } else {
    # For present and null_both, Rust uses 'none' in branch_id and filenames
    return("none")
  }
}

# Compose the Rust-consistent branch_id
compose_branch_id <- function(sample_size, transformation, outlier, effect_condition, strip_method) {
  paste0(
    format_sample_size_for_rust(sample_size), "__",
    normalize_transformation(transformation), "__",
    normalize_outlier(outlier), "__",
    normalize_effect_condition(effect_condition), "__",
    normalize_strip_method(strip_method, effect_condition)
  )
}

#' Get model specification by name
#'
#' @param config Configuration
#' @param model_name Name of model
#'
#' @return Model specification list
#'
get_model_spec <- function(config, model_name) {
  spec <- config$models[[model_name]]

  if (is.null(spec)) {
    stop(glue::glue("Unknown model: {model_name}"))
  }

  spec
}

# ============================================================================
# TARGETS PIPELINE OPTIONS
# ============================================================================

#' Configure targets pipeline options
#'
#' Sets tar_option_set() with sensible defaults
#'
configure_targets <- function(config, paths) {
  logger::log_info("Configuring targets pipeline")

  controller <- create_crew_controller(config, paths)

  targets::tar_option_set(
    tidy_eval = TRUE,

    # Parallelization options
    packages = packages_core,

    # Storage
    format = "rds",
    repository = "local",

    # Branching and aggregation type
    iteration = "list",

    # Error handling
    error = "continue", # Don't stop entire pipeline on branch error

    # Deployment (worker/local)
    deployment = "worker",
    # deployment = "main",
    resources = targets::tar_resources(
      crew = targets::tar_resources_crew(
        controller = controller$name
      )
    ),
    # Generate error workspaces for debugging
    workspace_on_error = TRUE,
    # Set the seed
    seed = set_reproducibility(config),

    # Set the controller
    controller = controller,

    # Workspace
    envir = parent.frame()
  )

  logger::log_debug("Targets pipeline configured with {config$n_workers} workers")
  invisible(controller)
}

# ============================================================================
# CREW OPTIONS
# ============================================================================

#' Create crew controller configuration
#'
#' @param config Configuration list
#'
#' @return crew_controller object
#'
create_crew_controller <- function(config, paths) {
  if (config$use_hpc) {
    logger::log_info("Creating SLURM crew controller")

    if (is.null(config$slurm_partition) || config$slurm_partition == "") {
      config$slurm_partition <- "default"
    }


    return(
      crew.cluster::crew_controller_slurm(
        name = "multiverse_slurm_controller",
        workers = config$n_workers,

        # Timeout and lifecycle
        seconds_idle = 300,
        seconds_timeout = 3600,

        # SLURM resource allocation
        options_cluster = crew.cluster::crew_options_slurm(
          verbose = config$verbose_crew,
          script_lines = c(
            "#!/bin/bash",
            "source ~/.bashrc"
          ),
          log_output = "crew_log_%A.log",
          log_error = "crew_log_%A.log",
          memory_gigabytes_required = config$slurm_mem_gb,
          cpus_per_task = 1,
          time_minutes = config$slurm_time_min,
          partition = config$slurm_partition
        ),
        options_metrics = crew::crew_options_metrics(
          path = paths$logs
        )
      )
    )
  } else {
    logger::log_info("Creating local crew controller ({config$n_workers} workers)")

    return(
      crew::crew_controller_local(
        name = "multiverse_local_controller",
        workers = parallelly::availableCores(omit = 1),
        seconds_idle = 60,
        seconds_timeout = 3600,
        options_local = crew::crew_options_local(
          log_directory = paths$logs,
          log_join = FALSE
        ),
        options_metrics = crew::crew_options_metrics(
          path = paths$logs
        )
      )
    )
  }
}



# ============================================================================
# SEED AND REPRODUCIBILITY
# ============================================================================

#' Set reproducibility options
#'
#' @param config Configuration list
#'
set_reproducibility <- function(config) {
  # Ensure factors ordered consistently
  options(
    stringsAsFactors = FALSE,
    warnPartialMatchDollar = TRUE,
    warnPartialMatchAttr = TRUE,
    warnPartialMatchArgs = TRUE
  )

  set.seed(config$random_seed)

  # Suppress warnings for cleaner logs (use with caution)
  if (!config$show_warnings) {
    options(warn = -1)
  }

  logger::log_debug("Reproducibility seed set: {config$random_seed}")

  config$random_seed
}
