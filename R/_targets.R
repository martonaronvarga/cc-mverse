# R/_targets.R
#
# Reads state saved by run.R. Defines coarse targets. The high-cardinality
# multiverse branch table is data, not target graph structure.

project_root <- Sys.getenv("TAR_PROJECT", unset = normalizePath("."))
fn_dir <- file.path(project_root, "functions")
invisible(lapply(list.files(fn_dir, pattern = "\\.R$", full.names = TRUE), source))
invisible(capture.output(suppressMessages(suppressWarnings(load_all_packages()))))

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

target0 <- targets::tar_target(
  dependency_preflight,
  {
    report <- build_dependency_report()
    missing_packages <- report$r_packages$package[!report$r_packages$installed]
    if (length(missing_packages) > 0) {
      stop("Missing R packages: ", paste(missing_packages, collapse = ", "))
    }
    cargo_metadata <- report$rust$value[report$rust$key == "cargo_metadata_available"]
    if (length(cargo_metadata) == 0 || grepl("^ERROR:", cargo_metadata)) {
      stop("Cargo metadata unavailable; Rust dependencies are not ready")
    }
    report
  },
  deployment = "main"
)

target1 <- targets::tar_target(
  raw_data_path,
  {
    dependency_preflight
    p <- file.path(project_root, config$raw_csv)
    stopifnot("Raw data missing" = file.exists(p))
    p
  },
  format = "file",
  deployment = "main"
)

target2 <- targets::tar_target(
  processed_cache_signature,
  {
    raw_data_path
    build_processed_cache_signature(config, paths, branch_specs, raw_data_path)
  },
  deployment = "main"
)

target3 <- targets::tar_target(
  processed_data_dir,
  {
    processed_cache_signature
    setup_logging(log_level = config$log_level, log_dir = paths$logs)
    expected <- file.path(
      paths$data_processed,
      paste0("processed__", unique(branch_specs$data_id), ".parquet")
    )
    missing <- expected[!file.exists(expected)]

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

    skip_rust <- tolower(Sys.getenv("SKIP_RUST", unset = "false")) %in% c("1", "true", "yes")
    if (skip_rust) {
      logger::log_warn(
        "Skipping processed-cache signature validation because SKIP_RUST=true; verified file count only"
      )
    } else {
      validate_processed_cache_signature(
        config = config,
        paths = paths,
        branch_specs = branch_specs,
        input_csv = raw_data_path,
        current = processed_cache_signature
      )
      logger::log_info("All {length(expected)} processed data files verified with current cache signature")
    }
    paths$data_processed
  },
  deployment = "main"
)

target4 <- targets::tar_target(
  nullification_diagnostics_file,
  {
    processed_data_dir
    output <- file.path(paths$outputs_analysis, "nullification_diagnostics.csv")
    write_nullification_diagnostics_for_dir(paths$data_processed, output)
    output
  },
  format = "file"
)

target5 <- targets::tar_target(
  cse_definition_comparison_files,
  {
    nullification_diagnostics_file
    outputs <- write_cse_definition_comparison(
      diagnostics_csv = nullification_diagnostics_file,
      output_dir = paths$outputs_analysis
    )
    unlist(outputs, use.names = FALSE)
  },
  format = "file"
)

target6 <- targets::tar_target(
  shuffle_adversarial_diagnostics_file,
  {
    processed_data_dir
    output <- file.path(paths$outputs_analysis, "shuffle_adversarial_diagnostics.csv")
    write_shuffle_adversarial_diagnostics_for_dir(paths$data_processed, output)
    output
  },
  format = "file"
)

# Model fitting: chunk branch table into coarse dynamic branches. This replaces
# the previous tar_map(values = branch_specs), which forced targets to construct
# >500k static target definitions before dispatching any worker.
target7a <- targets::tar_target(
  branch_chunks,
  {
    processed_data_dir
    chunks <- split_branch_specs(branch_specs, config)
    write_branch_chunk_manifest(chunks, paths)
    chunks
  },
  iteration = "list",
  deployment = "main"
)

target7b <- targets::tar_target(
  model_chunk_files,
  run_model_chunks_slurm_array(branch_chunks, paths, config),
  format = "file",
  deployment = "main"
)

target8 <- targets::tar_target(
  results_all,
  {
    model_chunk_files
    missing_model_chunks <- model_chunk_files[!file.exists(model_chunk_files)]
    if (length(missing_model_chunks) > 0L) {
      stop("Model chunk outputs are incomplete: ", length(missing_model_chunks), " missing. First missing: ", missing_model_chunks[[1]])
    }
    setup_logging(log_level = config$log_level, log_dir = paths$logs)
    logger::log_info("Aggregating chunked model results...")
    results <- load_results(paths, validate = TRUE)
    logger::log_info("Aggregated {nrow(results)} results")
    results
  },
  deployment = "main"
)

target9 <- targets::tar_target(
  analysis,
  {
    nullification_diagnostics_file
    setup_logging(log_level = config$log_level, log_dir = paths$logs)
    analyze_and_save(results_all, paths, diagnostics_csv = nullification_diagnostics_file, alpha = config$alpha)
  },
  deployment = "main"
)

target10 <- targets::tar_target(
  nullification_operating_characteristics_files,
  {
    nullification_diagnostics_file
    results_all
    diagnostics <- readr::read_csv(nullification_diagnostics_file, show_col_types = FALSE)
    tables <- build_nullification_operating_characteristics(results_all, diagnostics, alpha = config$alpha)
    output_dir <- file.path(paths$outputs_analysis, "nullification_operating_characteristics")
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    paths_out <- c(
      file.path(output_dir, "nullification_fpr_coarse.csv"),
      file.path(output_dir, "nullification_fpr_by_sample.csv"),
      file.path(output_dir, "nullification_fpr_by_outlier.csv"),
      file.path(output_dir, "nullification_failure_aware_rates.csv")
    )
    readr::write_csv(tables$fpr_coarse, paths_out[[1]])
    readr::write_csv(tables$fpr_by_sample, paths_out[[2]])
    readr::write_csv(tables$fpr_by_outlier, paths_out[[3]])
    readr::write_csv(tables$failure_aware, paths_out[[4]])
    paths_out
  },
  format = "file",
  deployment = "main"
)

target11 <- targets::tar_target(
  plots,
  {
    setup_logging(log_level = config$log_level, log_dir = paths$logs)
    source("functions/analysis_plots.R")
    fig_dir <- file.path(paths$outputs_analysis, "figures")
    generate_multiverse_dashboard(analysis, output_dir = fig_dir, save_individual = TRUE)
    list.files(fig_dir, full.names = TRUE)
  },
  format = "file",
  deployment = "main"
)

target12 <- targets::tar_target(
  csv_output_plot_files,
  {
    analysis
    nullification_diagnostics_file
    cse_definition_comparison_files
    shuffle_adversarial_diagnostics_file
    nullification_operating_characteristics_files
    if (!tolower(Sys.getenv("PLOT_ALL_CSV_OUTPUTS", unset = "false")) %in% c("1", "true", "yes")) {
      logger::log_info("Skipping exhaustive CSV plot gallery; set PLOT_ALL_CSV_OUTPUTS=true to enable")
      character()
    } else {
      output_dir <- file.path(paths$outputs_analysis, "figures", "csv_outputs")
      manifest <- write_csv_output_plots(paths$outputs_analysis, output_dir)
      manifest_path <- file.path(output_dir, "csv_output_plot_manifest.csv")
      readr::write_csv(manifest, manifest_path)
      c(manifest$plot[manifest$plotted], manifest_path)
    }
  },
  format = "file",
  deployment = "main"
)

list(
  target0, target1, target2, target3, target4, target5, target6,
  target7a, target7b, target8, target9, target10, target11, target12
)
