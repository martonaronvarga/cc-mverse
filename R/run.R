# R/run.R - Entry point for pipeline execution
# Usage: Rscript run.R [local|test|hpc]

library(logger)
library(glue)
library(targets)
library(rlang)


# ---------------------------------------------------------------------------
# Parse CLI
# ---------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)

cli_args <- list(
  mode = "local",
  participants = NULL,
  trials = NULL,
  workers = NULL,
  log_level = "info",
  cluster_host = NULL,
  cluster_user = NULL
)

# Parse arguments
i <- 1
while (i <= length(args)) {
  arg <- args[i]

  if (arg == "--mode" && i < length(args)) {
    cli_args$mode <- args[i + 1]
    i <- i + 2
  } else if (arg == "--participants" && i < length(args)) {
    cli_args$participants <- as.numeric(args[i + 1])
    i <- i + 2
  } else if (arg == "--trials" && i < length(args)) {
    cli_args$trials <- as.numeric(args[i + 1])
    i <- i + 2
  } else if (arg == "--workers" && i < length(args)) {
    cli_args$workers <- as.numeric(args[i + 1])
    i <- i + 2
  } else if (arg == "--log-level" && i < length(args)) {
    cli_args$log_level <- args[i + 1]
    i <- i + 2
  } else if (arg == "--cluster-host" && i < length(args)) {
    cli_args$cluster_host <- args[i + 1]
    i <- i + 2
  } else if (arg == "--cluster-user" && i < length(args)) {
    cli_args$cluster_user <- args[i + 1]
    i <- i + 2
  } else {
    i <- i + 1
  }
}

logger::log_appender(
  logger::appender_console
)
logger::log_layout(layout = logger::layout_glue_colors)

level_map <- list(
  debug = logger::DEBUG, info = logger::INFO,
  warn = logger::WARN, error = logger::ERROR
)
logger::log_threshold(level_map[[cli_args$log_level]] %||% logger::INFO)

logger::log_info("=== Multiverse Analysis Pipeline ===")
logger::log_info("Mode: {cli_args$mode}")
logger::log_info("Start time: {format(Sys.time(), '%Y-%m-%d %H:%M:%S')}")

# Load all utility functions
logger::log_debug("Loading utility functions...")

invisible(lapply(
  list.files("functions", pattern = "\\.R$", full.names = TRUE),
  source
))

logger::log_debug("Utility functions loaded")


# ============================================================================
# LOAD AND CONFIGURE
# ============================================================================

logger::log_debug("Loading packages...")
load_all_packages()

# Load and validate configuration
logger::log_debug("Initializing configuration...")
config <- load_pipeline_config(cli_args$mode)

# Override with CLI arguments if provided
if (!is.null(cli_args$participants)) config$n_participants <- cli_args$participants
if (!is.null(cli_args$trials)) config$n_trials <- cli_args$trials
if (!is.null(cli_args$workers)) config$n_workers <- cli_args$workers
if (cli_args$mode == "hpc") config$use_hpc <- TRUE


# Store cluster info if provided
config$cluster_host <- cli_args$cluster_host %||% Sys.getenv("SSH_CLUSTER_HOST", "")
config$cluster_user <- cli_args$cluster_user %||% Sys.getenv("SSH_CLUSTER_USER", Sys.getenv("USER"))

# Initialize paths
logger::log_debug("Initializing paths...")
paths <- init_project_paths(".")
config$project_root <- paths$root

# Config logging with paths
logger::log_debug("Setting logging options...")
logging <- configure_logging(config, paths)

# Generate pipeline branches
logger::log_debug("Generating branch specifications...")
branches <- generate_all_branches(config)

# Config targets options
logger::log_debug("Configuring targets options...")
targets_conf <- configure_targets(config, paths)


logger::log_info("Configuration loaded: {nrow(branches)} branches")
logger::log_info("Target execution directory: _targets")

logger::log_debug("Setting up targets option objects...")
save_pipeline_state(
  config        = config,
  paths         = paths,
  branches      = branches,
  targets_conf  = targets_conf,
  logging       = logging
)
logger::log_info("Global configs saved for _targets.R")

# Run targets pipeline
# if (config$use_hpc) {
#   logger::log_info("Starting targets with HPC controller")
#   if (nchar(config$cluster_host) == 0) {
#     logger::log_error("HPC mode requires SSH_CLUSTER_HOST")
#     stop("SSH_CLUSTER_HOST not configured")
#   }
#   ssh_conn <- new_ssh_connection(
#     cluster_host = config$cluster_host,
#     cluster_user = config$cluster_user,
#     ssh_key = Sys.getenv("SSH_KEY", "~/.ssh/id_ed25519")
#   )

#   # Store in config for later use
#   config$ssh_conn <- ssh_conn
# }


tryCatch(
  {
    logger::log_info("Starting targets pipeline...")
    targets::tar_make(
      reporter = "verbose", # if (config$is_test) "verbose" else "silent",
      callr_function = NULL # if (config$use_hpc) callr::r_bg else callr::r
    )
  },
  error = function(e) {
    logger::log_error("Pipeline failed: {e$message}")
    stop(e)
  }
)

# --------------------------
# Summary
# --------------------------
logger::log_info("Pipeline execution completed")
logger::log_info("End time: {format(Sys.time(), '%Y-%m-%d %H:%M:%S')}")

logger::log_info("Results saved to: {paths$outputs_results}")
logger::log_info("Logs saved to: {paths$logs}")

cat("Check results: head(arrow::read_parquet('{get_results_path(paths)}'))\n")
cat("View analysis: list.files('{paths$outputs_analysis}')\n")
