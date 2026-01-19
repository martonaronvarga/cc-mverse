# R/functions/packages.R
# Package loading and options configuration
# Single source of truth for all dependencies

# Core packages (loaded once, before pipeline)
packages_core <- c(
  # Data manipulation & IO
  "tidyverse", # dplyr, tidyr, ggplot2, readr, etc.
  "arrow", # Parquet I/O
  "readr", # CSV reading/writing

  # Logging & monitoring
  "logger", # Structured logging
  "autometric", # Targets task monitoring

  # Pipeline orchestration
  "targets", # Pipeline framework
  "tarchetypes",
  "crew", # Local parallelization
  "crew.cluster", # HPC cluster integration
  "callr", # R subprocess execution

  # Model fitting
  "lme4", # Linear mixed models
  "afex", # ANOVA/LMM tools
  "lmerTest", # LMM p-values
  "car", # Statistical tests (leveneTest, etc)

  # Model utilities
  "broom", # Model tidying
  "broom.mixed", # Mixed model tidying

  # Data storage
  "qs2", # Fast serialization

  # Metaprogramming
  "rlang", # Quoting, errors
  "glue" # String interpolation
)

#' Load all required packages
#'
#' @return Invisibly TRUE if successful
#'
load_all_packages <- function() {
  missing <- setdiff(packages_core, rownames(installed.packages()))

  if (length(missing) > 0) {
    logger::log_warn("Installing missing packages: {paste(missing, collapse=', ')}")

    templib <- tempfile("rpkgs_")
    dir.create(templib)

    old_libpaths <- .libPaths()
    .libPaths(c(templib, old_libpaths))

    on.exit(
      {
        .libPaths(old_libpaths)
        unlink(templib, recursive = TRUE)
      },
      add = FALSE
    )

    install.packages(missing, lib = templib, repos = "https://cran.r-project.org")
  }

  for (pkg in packages_core) {
    tryCatch(
      suppressPackageStartupMessages(library(pkg, character.only = TRUE, quietly = TRUE)),
      error = function(e) {
        logger::log_error("Failed to load package {pkg}: {e$message}")
        stop(e)
      }
    )
  }

  logger::log_debug("Loaded {length(packages_core)} packages")

  invisible(TRUE)
}

#' Ensure required packages in worker environment (targets crew)
#'
#' Called on each worker to load packages without installing
#'
#' @return Invisible list of loaded packages
#'
ensure_worker_packages <- function() {
  for (pkg in packages_core) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      stop(glue::glue("Package {pkg} not available on worker"))
    }
  }
  invisible(packages_core)
}
