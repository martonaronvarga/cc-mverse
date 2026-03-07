# R/_targets.R
#
# Reads state saved by run.R. Defines targets.

project_root <- Sys.getenv("TAR_PROJECT", unset = normalizePath("."))
fn_dir <- file.path(project_root, "functions")
invisible(lapply(list.files(fn_dir, pattern = "\\.R$", full.names = TRUE), source))
load_all_packages()

# ---- Restore state from run.R ----
state <- load_pipeline_state("_config")
config <- state$config
paths <- state$paths
branch_specs <- state$branches

setup_logging(log_level = state$logging$log_level, log_dir = paths$logs)
initialize_results_schema(paths$outputs_results)
configure_targets(config, paths)

# ============================================================================
# PIPELINE
# ============================================================================

# Step 1: Raw data anchor
target1 <- targets::tar_target(
  raw_data_path,
  {
    p <- file.path(paths$data_raw, "merged_data.csv")
    stopifnot("Raw data missing" = file.exists(p))
    p
  },
  format = "file"
)
# Step 2: Verify Rust output (HPC: separate job; local: run inline)
target2 <- targets::tar_target(
  processed_data_dir,
  {
    setup_logging(log_level = config$log_level, log_dir = paths$logs)
    expected <- file.path(
      paths$data_processed,
      paste0("processed__", unique(branch_specs$data_id), ".parquet")
    )
    missing <- expected[!file.exists(expected)]

    # Local fallback: run Rust inline
    if (length(missing) > 0 && !config$is_hpc) {
      logger::log_info("Running Rust processor inline ({length(missing)} files missing)...")
      # call_rust_processor(raw_data_path, paths, config)
      missing <- expected[!file.exists(expected)]
    }

    if (length(missing) > 0) {
      stop(sprintf(
        "%d/%d processed files missing. On HPC, ensure Rust job completed first.",
        length(missing), length(expected)
      ))
    }

    logger::log_info("All {length(expected)} processed data files verified")
    paths$data_processed
  },
  format = "file"
)

# Step 3: Model fitting (one target per branch)
targets3 <- tarchetypes::tar_map(
  values = branch_specs,
  names = tidyselect::any_of("branch_id"),
  targets::tar_target(
    name = model_results,
    command = {
      processed_data_dir
      setup_logging(
        log_level = config$log_level,
        log_dir   = paths$logs
      )

      fit_and_save_branch(
        idx = idx,
        branch_id = branch_id,
        sample_size = sample_size,
        subsample_id = subsample_id,
        transformation = transformation,
        outlier = outlier,
        model = model,
        effect_condition = effect_condition,
        strip_method = strip_method,
        paths = paths,
        config = config
      )
    },
    format = "file"
  )
)

# Step 4: Aggregate
target4 <- tarchetypes::tar_combine(
  results_all,
  targets3,
  command = {
    setup_logging(log_level = config$log_level, log_dir = paths$logs)
    !!!.x
    logger::log_info("Aggregating results...")
    results <- load_results(paths, validate = TRUE)
    logger::log_info("Aggregated {nrow(results)} results")
    results
  }
)

# Step 5: Analysis
target5 <- targets::tar_target(
  name = analysis,
  command = {
    setup_logging(
      log_level = config$log_level,
      log_dir   = paths$logs
    )
    analyze_and_save(results_all, paths)
  }
)

# Step 6: Plots
target6 <- targets::tar_target(
  plots,
  {
    setup_logging(
      log_level = config$log_level,
      log_dir   = paths$logs
    )
    source("functions/analysis_plots.R")
    fig_dir <- file.path(paths$outputs_analysis, "figures")
    generate_multiverse_dashboard(analysis, output_dir = fig_dir, save_individual = TRUE)
    list.files(fig_dir, full.names = TRUE)
  },
  format = "file"
)

list(target1, target2, targets3, target4, target5, target6)
