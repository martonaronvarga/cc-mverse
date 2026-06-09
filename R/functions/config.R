# R/functions/config.R - Configuration Management
# Centralized configuration with validation

load_config <- function(mode = "local",
                        config_path = find_config_yaml(),
                        overrides = list()) {
  if (!file.exists(config_path)) {
    stop("Config not found: ", config_path)
  }

  raw <- yaml::read_yaml(config_path)

  # Start with defaults from top-level sections
  config <- list(
    mode = mode,
    is_hpc = (mode == "hpc"),
    is_test = (mode == "test"),

    # Data
    raw_csv = raw$data$raw_csv,
    n_participants = raw$data$n_participants,
    n_trials = raw$data$n_trials,
    random_seed = raw$data$random_seed,
    use_simulated_data = raw$data$use_simulated %||% TRUE,

    # Analysis axes
    sample_sizes = as.numeric(raw$analysis$sample_sizes),
    n_subsamples = raw$analysis$n_subsamples,
    transformations = as.character(raw$analysis$transformations),
    outlier_methods = as.character(raw$analysis$outlier_methods),
    effect_conditions = as.character(raw$analysis$effect_conditions),
    strip_methods = as.character(raw$analysis$strip_methods),
    alpha = raw$analysis$alpha %||% 0.05,

    # Models
    models = build_model_specs(raw$models),

    # SLURM
    slurm = raw$slurm,

    # Rust
    rust_release = raw$rust$release %||% TRUE,
    rust_threads = raw$rust$threads %||% 0L,
    writer_threads = raw$rust$writer_threads %||% 0L,
    save_metadata = raw$rust$save_metadata %||% FALSE,

    # Logging
    log_level = raw$logging$level %||% "info",

    # Execution/cache policy
    model_chunk_size = raw$execution$model_chunk_size %||% 50L,
    overwrite_results = isTRUE(raw$execution$overwrite_results %||% FALSE),

    # Workers (will be overridden by mode)
    n_workers = 4L
  )

  # Apply mode-specific overrides
  mode_cfg <- raw$modes[[mode]]
  if (!is.null(mode_cfg)) {
    for (key in names(mode_cfg)) {
      config[[key]] <- mode_cfg[[key]]
    }
  }

  # Apply CLI overrides (highest priority)
  for (key in names(overrides)) {
    if (!is.null(overrides[[key]])) {
      config[[key]] <- overrides[[key]]
    }
  }

  # Ensure numeric types
  config$sample_sizes <- as.numeric(config$sample_sizes)
  config$n_subsamples <- as.integer(config$n_subsamples)
  config$n_workers <- as.integer(config$n_workers)
  config$model_chunk_size <- as.integer(config$model_chunk_size %||% 50L)
  config$n_participants <- as.integer(config$n_participants)
  config$n_trials <- as.integer(config$n_trials)

  validate_config(config)
  config
}

#' Find pipeline.yaml by walking up from the working directory
find_config_yaml <- function() {
  candidates <- c(
    "pipeline.yaml",
    file.path("..", "pipeline.yaml"),
    file.path(Sys.getenv("TAR_PROJECT", unset = "."), "pipeline.yaml")
  )
  for (p in candidates) {
    if (file.exists(p)) {
      return(normalizePath(p))
    }
  }
  stop("Cannot find pipeline.yaml. Looked in: ", paste(candidates, collapse = ", "))
}

#' Build model specifications from YAML
build_model_specs <- function(models_yaml) {
  optimizer <- models_yaml$lmm_optimizer %||% list()
  make_lmer_check <- getFromNamespace(".makeCC", "lme4")
  control <- list(
    optimizer = optimizer$name %||% "bobyqa",
    check.conv.grad = make_lmer_check("warning", tol = optimizer$check_conv_grad_tol %||% 1e-5),
    check.conv.hess = make_lmer_check("warning", tol = optimizer$check_conv_hess_tol %||% 1e-6),
    calc.derivs = TRUE,
    optCtrl = list(maxfun = optimizer$maxfun %||% 300000L)
  )

  specs <- list()
  for (name in setdiff(names(models_yaml), "lmm_optimizer")) {
    m <- models_yaml[[name]]
    spec <- list(
      type = m$type,
      formula_full = m$formula_full
    )
    if (!is.null(m$formula_null)) {
      spec$formula_null <- m$formula_null
    }
    if (m$type == "lmm") {
      spec$control <- control
    }
    specs[[name]] <- spec
  }
  specs
}

#' Validate configuration
validate_config <- function(config) {
  stopifnot(
    "mode must be test/local/focused/resampling_pilot/hpc" = config$mode %in% c("test", "local", "focused", "resampling_pilot", "hpc"),
    "n_participants >= 1" = config$n_participants >= 1,
    "n_trials >= 1" = config$n_trials >= 1,
    "n_workers >= 1" = config$n_workers >= 1,
    "model_chunk_size >= 1" = config$model_chunk_size >= 1,
    "sample_sizes non-empty" = length(config$sample_sizes) > 0,
    "models non-empty" = length(config$models) > 0
  )
  invisible(config)
}

#' Parse CLI arguments
#'
#' Returns mode, config path, and no other overrides —
#' overrides are already baked into the resolved YAML by multiverse.sh.
parse_cli <- function(args = commandArgs(trailingOnly = TRUE)) {
  result <- list(
    mode = "local",
    config_path = find_config_yaml()
  )

  i <- 1L
  while (i <= length(args)) {
    arg <- args[i]
    nxt <- if (i < length(args)) args[i + 1L] else NULL

    switch(arg,
      "--mode" = {
        result$mode <- nxt
        i <- i + 2L
      },
      "--config" = {
        result$config_path <- nxt
        i <- i + 2L
      },
      {
        i <- i + 1L
      }
    )
  }
  result
}

#' Generate all branch combinations
#'
#' Branch design:
#'   - Data branches (processed by Rust): unique by
#'     (sample_size, subsample_id, transformation, outlier, effect_condition, strip_method)
#'   - Model branches (fitted by R): each data branch × each model
#'
#' At sample_size = 1.0, only subsample_id = 1 (deterministic, no sampling).
#' At all other fractions, n_subsamples independent draws.
#'
#' @param config Configuration list
#' @return Tibble with one row per branch combination
generate_all_branches <- function(config) {
  log_pipeline(logger::INFO, "Generating branch specifications")

  # Build subsample grid: at 1.0, only 1 subsample
  subsample_grid <- purrr::map_dfr(config$sample_sizes, function(ss) {
    n_sub <- if (abs(ss - 1.0) < 1e-6) 1L else config$n_subsamples
    tidyr::expand_grid(
      sample_size = ss,
      subsample_id = seq_len(n_sub)
    )
  })

  # Present branches use the unstripped data with strip_method = "none".
  branches_direct <- tidyr::expand_grid(
    subsample_grid,
    transformation = config$transformations,
    outlier = config$outlier_methods,
    model = names(config$models),
    effect_condition = "present",
    strip_method = "none"
  )

  # null_interaction: real strip methods
  branches_stripped <- tidyr::expand_grid(
    subsample_grid,
    transformation = config$transformations,
    outlier = config$outlier_methods,
    model = names(config$models),
    effect_condition = "null_interaction",
    strip_method = config$strip_methods
  )

  branches <- dplyr::bind_rows(branches_direct, branches_stripped) |>
    dplyr::mutate(
      # Branch ID includes model (unique per R fitting target)
      branch_id = compose_branch_id(
        sample_size, subsample_id, transformation, outlier,
        model, effect_condition, strip_method
      ),
      # Data ID excludes model (unique per Rust processing unit)
      data_id = data_id_from_branch_id(branch_id),
      n_total_branches = dplyr::n()
    ) |>
    tibble::rowid_to_column("idx")

  # Validate constraints
  stopifnot(
    "present with non-none strip_method" =
      !any(branches$effect_condition == "present" & branches$strip_method != "none"),
    "null_both is absent" =
      !any(branches$effect_condition == "null_both"),
    "null_interaction with none strip_method" =
      !any(branches$effect_condition == "null_interaction" & branches$strip_method == "none"),
    "duplicate branch_id" =
      length(unique(branches$branch_id)) == nrow(branches)
  )

  log_pipeline(logger::INFO, "Generated {nrow(branches)} branch combinations")
  log_pipeline(
    logger::INFO,
    "  Unique data branches (Rust): {length(unique(branches$data_id))}"
  )
  log_pipeline(
    logger::INFO,
    "  Subsample levels: {paste(config$sample_sizes, collapse=', ')} x {config$n_subsamples} draws"
  )

  branches
}

format_sample_size_for_rust <- function(x) {
  # Use formatC with "fg" and then strip trailing .0
  s <- formatC(x, digits = 15, format = "fg")
  sub("\\.0$", "", s)
}

normalize_transformation <- function(x) {
  x <- as.character(x)
  unknown <- x[!x %in% c("log_rt", "no_log_rt")]
  if (length(unknown) > 0) stop("Unknown transformation(s): ", paste(unknown, collapse = ", "))
  x
}

normalize_outlier <- function(x) {
  x <- as.character(x)
  allowed <- c(
    "sd_2", "sd_2.5", "sd_3",
    "mad_2", "mad_2.5", "mad_3",
    "range_1000", "range_1250", "range_1500",
    "none"
  )
  unknown <- x[!x %in% allowed]
  if (length(unknown) > 0) stop("Unknown outlier method(s): ", paste(unknown, collapse = ", "))
  x
}

normalize_effect_condition <- function(x) {
  x <- as.character(x)
  unknown <- x[!x %in% c("present", "null_interaction")]
  if (length(unknown) > 0) stop("Unknown effect condition(s): ", paste(unknown, collapse = ", "))
  x
}

normalize_strip_method <- function(x, effect_condition) {
  effect_condition <- normalize_effect_condition(effect_condition)
  x <- as.character(x)
  x <- dplyr::recode(x, qmap_5 = "additive_qmap", qmap_5_trial_bin = "additive_qmap_trial_bin")
  bad_interaction <- effect_condition == "null_interaction" & !x %in% c("shuffle", "additive_qmap", "additive_qmap_trial_bin", "local_mean_residual", "local_median_residual")
  if (any(bad_interaction)) {
    stop(
      "Unknown strip method(s) for null_interaction: ",
      paste(unique(x[bad_interaction]), collapse = ", ")
    )
  }
  dplyr::if_else(effect_condition == "null_interaction", x, "none")
}

# Compose the Rust-consistent branch_id
compose_branch_id <- function(sample_size, subsample_id, transformation, outlier, model, effect_condition, strip_method) {
  model <- as.character(model)
  if (any(grepl("__", model, fixed = TRUE))) {
    stop("Model names cannot contain '__' because branch_id uses it as a field separator: ", paste(model[grepl("__", model, fixed = TRUE)], collapse = ", "))
  }
  paste0(
    trimws(format_sample_size_for_rust(sample_size)), "__",
    trimws(as.integer(subsample_id)), "__",
    trimws(normalize_transformation(transformation)), "__",
    trimws(normalize_outlier(outlier)), "__",
    trimws(as.character(model)), "__",
    trimws(normalize_effect_condition(effect_condition)), "__",
    trimws(normalize_strip_method(strip_method, effect_condition))
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
configure_targets <- function(config, paths) {
  log_pipeline(logger::INFO, "Configuring targets pipeline")

  controller <- create_crew_controller(config, paths)

  targets::tar_option_set(
    tidy_eval = TRUE,
    packages = character(),
    format = "rds",
    repository = "local",
    iteration = "list",
    error = "continue", # Don't stop entire pipeline on branch error
    deployment = "worker",
    resources = targets::tar_resources(
      crew = targets::tar_resources_crew(
        controller = controller$name
      )
    ),
    workspace_on_error = TRUE,
    seed = config$random_seed,
    controller = controller,
    envir = parent.frame()
  )

  log_pipeline(logger::DEBUG, "Targets pipeline configured with {config$n_workers} workers")
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
  if (config$is_hpc) {
    slurm <- config$slurm
    worker <- slurm$worker
    log_pipeline(logger::INFO, "Creating SLURM crew controller ({config$n_workers} workers)")

    script_lines <- c(
      "export OMP_NUM_THREADS=1",
      "export MKL_NUM_THREADS=1",
      "export OPENBLAS_NUM_THREADS=1",
      "export RAYON_NUM_THREADS=1",
      "export MALLOC_ARENA_MAX=2"
    )
    if (!is.null(slurm$bashrc_source) && nzchar(slurm$bashrc_source)) {
      bashrc <- path.expand(slurm$bashrc_source)
      # crew.cluster may run these lines under /bin/sh, so use POSIX dot instead of bash `source`.
      script_lines <- c(script_lines, sprintf("[ -f %s ] && . %s > /dev/null", shQuote(bashrc), shQuote(bashrc)))
    }

    crew.cluster::crew_controller_slurm(
      name = "multiverse_slurm",
      workers = config$n_workers,

      # Retire idle workers quickly after targets finishes dispatching chunks.
      # Long model chunks are protected by seconds_timeout, not idle time.
      seconds_idle = 120,
      seconds_timeout = as.integer(worker$time_min * 60),

      # SLURM resource allocation
      options_cluster = crew.cluster::crew_options_slurm(
        verbose = FALSE,
        script_directory = paths$logs,
        script_lines = script_lines,
        log_output = "/dev/null",
        log_error = file.path(paths$logs, "crew_worker_%A.err"),
        memory_gigabytes_required = worker$mem_gb,
        cpus_per_task = worker$cpus,
        time_minutes = worker$time_min,
        partition = slurm$partition
      ),
      options_metrics = crew::crew_options_metrics(
        path = paths$logs
      )
    )
  } else {
    log_pipeline(logger::INFO, "Creating local crew controller ({config$n_workers} workers)")

    crew::crew_controller_local(
      name = "multiverse_local",
      workers = config$n_workers,
      seconds_idle = 300,
      seconds_timeout = 5 * 24 * 60 * 60,
      options_local = crew::crew_options_local(
        log_directory = paths$logs,
        log_join = FALSE
      ),
      options_metrics = crew::crew_options_metrics(
        path = paths$logs
      )
    )
  }
}
