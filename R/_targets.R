# R/_targets.R
#
# This pipeline is driven by run.R which sets global variables:
# - PIPELINE_CONFIG: Configuration list
# - PROJECT_PATHS: Project paths object
# - BRANCH_SPECS: All branch specifications

# ---- STEP 0: Load CONFIG from run.R ----

project_root <- Sys.getenv("TAR_PROJECT", unset = normalizePath("."))
fn_dir <- file.path(project_root, "functions")
if (!dir.exists(fn_dir)) {
  stop("Cannot locate R/functions directory. Checked: ", fn_dir)
}
invisible(lapply(list.files(fn_dir, pattern = "\\.R$", full.names = TRUE), source))

load_all_packages()

# ---------------------------------------------------------------------------
# Restore pipeline state
# ---------------------------------------------------------------------------
pipeline_state <- load_pipeline_state("_config")
config <- pipeline_state$config
paths <- pipeline_state$paths
branch_specs <- pipeline_state$branches
logging_opts <- pipeline_state$logging
targets_conf <- pipeline_state$targets_conf

# Initialize results schema
initialize_results_schema(paths$outputs_results)
configure_logging(config, paths)

# Set global pipeline options *before defining any targets*
configure_targets(config, paths)


# ============================================================================
# MAIN PIPELINE
# ============================================================================

list(
  # ---- STEP 1: Load/Generate Data ----
  # tar_target(
  #   raw_data_path,
  #   file.path(paths$data_raw, "merged_data.csv"),
  #   format = "file"
  # ),
  # tar_target(
  #   name = raw_data,
  #   command = {
  #     logger::log_info("Loading raw data...")
  #     if (config$use_simulated_data) {
  #       logger::log_info("Generating simulated data (n={config$n_participants})")
  #       data <- generate_test_data(
  #         n_participants = config$n_participants,
  #         n_trials = config$n_trials,
  #         seed = config$random_seed
  #       )
  #       readr::write_csv(data, file.path(paths$data_raw, glue::glue("simulated_{format(Sys.time(), '%Y%m%d_%H%M%S')}.csv")))
  #     } else {
  #       logger::log_info("Loading real/empirical data from file")
  #       data <- readr::read_csv(raw_data_path, show_col_types = FALSE)
  #     }
  #     # validate_raw_data(data)
  #   }
  # ),

  # # ---- STEP 2: Process Data (Rust) ----
  # tar_target(
  #   name = rust_outputs,
  #   command = {
  #     logger::log_info("Processing data with Rust...")
  #     call_rust_processor(raw_data, paths, config)
  #     logger::log_info("Rust generated {length(processed_files)} files")
  #   },
  #   format = "file"
  # ),
  # tar_target(
  #   branch_spec,
  #   branch_specs,
  #   iteration = "list"
  # ),
  # tar_target(
  #   processed_data_path,
  #   {
  #     # ensure rust_outputs is upstream
  #     rust_outputs
  #     get_processed_data_path(paths, branch_spec)
  #   },
  #   format = "file",
  #   iteration = "list"
  # ),
  # tar_target(
  #   processed_data,
  #   safe_read_parquet(processed_data_path),
  #   iteration = "list"
  # ),
  # tar_target(
  #   model_spec,
  #   get_model_spec(config, branch_spec$model),
  #   iteration = "list"
  # ),
  # tar_target(
  #   model_result,
  #   fit_model(
  #     data       = processed_data,
  #     model_spec = model_spec,
  #     model_name = branch_spec$model,
  #     branch_id  = branch_spec$branch_id
  #   ),
  #   iteration = "list"
  # ),
  # tar_target(
  #   result_row,
  #   {
  #     tbl <- extract_results(model_result, branch_spec, branch_spec$idx)
  #     append_results(tbl, paths, branch_spec$branch_id)
  #     tbl
  #   },
  #   iteration = "list"
  # ),

  # # 4) Aggregate all model results
  # tar_target(
  #   all_results,
  #   load_results(paths)
  # ),

  # # 5) Downstream analysis on the aggregated results (implement analyze_results)
  # tar_target(
  #   analysis_outputs,
  #   analyze_and_save(all_results, paths, alpha = 0.05),
  #   format = "file"
  # )


  # ---- STEP 3: Fit Models (per branch, parallelized) ----
  tarchetypes::tar_map(
    values = branch_specs,
    names = branch_id,
    tar_target(
      name = model_results,
      command = {
        fit_and_save_branch(
          idx = idx,
          branch_id = branch_id,
          sample_size = sample_size,
          transformation = transformation,
          outlier = outlier,
          model = model,
          effect_condition = effect_condition,
          strip_method = strip_method,
          paths = paths,
          config = config
        )
        format <- "file"
      }
    )
  ),

  # ---- STEP 4: Aggregate Results ----
  tar_target(
    name = results_aggregated,
    command = {
      logger::log_info("Aggregating results...")
      results <- load_results(paths$outputs_results, validate = TRUE)
      logger::log_info("Aggregated {nrow(results)} results")
      results
    }
  ),

  # ---- STEP 5: Analysis ----
  tar_target(
    name = discovery_analysis,
    command = {
      results_aggregated
      logger::log_info("Computing discovery rates...")
      analyze_and_save(results_aggregated, paths)
      logger::log_info("Discovery analysis complete")
    },
    format = "file"
  ),
  tar_target(
    analysis_plots,
    {
      source("functions/analysis_plots.R")
      plots <- generate_multiverse_dashboard(
        results_aggregated,
        output_dir = file.path(paths$outputs_analysis, "figures"),
        save_individual = TRUE
      )
      logger::log_info("Generated {length(plots)} plots")

      list.files(
        file.path(paths$outputs_analysis, "figures"),
        full.names = TRUE
      )
    }
  ),
  tar_target(
    analysis_plots_c,
    {
      source("functions/analysis_plot_c.R")

      plots_c <- cont_plots(
        results_aggregated,
        output_dir = file.path(paths$outputs_analysis, "figures2")
      )

      list.files(
        file.path(paths$outputs_analysis, "figures2"),
        full.names = TRUE
      )
    }
  )
)
