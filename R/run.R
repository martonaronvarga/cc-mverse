# R/run.R — Pipeline entry point
#
# Reads resolved YAML (produced by multiverse.sh), saves state, runs pipeline.
# Usage: Rscript run.R --mode local --config /path/to/.pipeline.resolved.yaml


invisible(lapply(
  list.files("functions", pattern = "\\.R$", full.names = TRUE),
  source
))
load_all_packages()

# ---- Load config (YAML + CLI overrides) ----
cli <- parse_cli()
config <- load_config(mode = cli$mode, config_path = cli$config_path)

# ---- Paths & logging ----
paths <- init_project_paths(".")
run_id <- setup_logging(log_level = config$log_level, log_dir = paths$logs)

logger::log_info("=== Multiverse Pipeline ===")
logger::log_info("Mode: {config$mode} | Workers: {config$n_workers} | Run: {run_id}")

# ---- Generate branches & save state for _targets.R ----
branches <- generate_all_branches(config)

# Do not configure crew/targets here. targets::tar_make() evaluates _targets.R
# in its own callr process, and _targets.R configures the controller there.
# Configuring here creates a second unused controller and noisy duplicate logs.
save_pipeline_state(
  config   = config,
  paths    = paths,
  branches = branches,
  logging  = list(log_level = config$log_level, run_id = run_id)
)

# ---- Initialize results schema & run ----
initialize_results_schema(paths$outputs_results)

tryCatch(
  {
    logger::log_info("Starting targets pipeline...")
    targets::tar_make()
  },
  error = function(e) {
    logger::log_error("Pipeline failed: {e$message}")
    stop(e)
  }
)
logger::log_info("Results: {paths$outputs_results}")
logger::log_info("Logs: {file.path(paths$logs, paste0('pipeline_', run_id, '.log'))}")
