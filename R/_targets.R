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

# Step 0: dependency preflight
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
  }
)

# Step 1: Raw data anchor
target1 <- targets::tar_target(
  raw_data_path,
  {
    dependency_preflight
    p <- file.path(project_root, config$raw_csv)
    stopifnot("Raw data missing" = file.exists(p))
    p
  },
  format = "file"
)

# Step 2: Build the current processed-data cache signature.
target2 <- targets::tar_target(
  processed_cache_signature,
  {
    raw_data_path
    build_processed_cache_signature(config, paths, branch_specs, raw_data_path)
  }
)

# Step 3: Verify Rust output (HPC: separate job; local: run inline)
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

    validate_processed_cache_signature(
      config = config,
      paths = paths,
      branch_specs = branch_specs,
      input_csv = raw_data_path,
      current = processed_cache_signature
    )

    logger::log_info("All {length(expected)} processed data files verified with current cache signature")
    expected
  },
  format = "file"
)

# Step 4: Nullification diagnostics over processed parquet
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

# Step 5: Location-vs-distributional CSE comparison diagnostics
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

# Step 6: Empirical shuffle adversarial diagnostics
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

# Step 7: Model fitting (one target per branch)
targets7 <- tarchetypes::tar_map(
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

# Step 8: Aggregate
target8 <- tarchetypes::tar_combine(
  results_all,
  targets7,
  command = {
    setup_logging(log_level = config$log_level, log_dir = paths$logs)
    !!!.x
    logger::log_info("Aggregating results...")
    results <- load_results(paths, validate = TRUE)
    logger::log_info("Aggregated {nrow(results)} results")
    results
  }
)

# Step 9: Analysis
target9 <- targets::tar_target(
  name = analysis,
  command = {
    setup_logging(
      log_level = config$log_level,
      log_dir   = paths$logs
    )
    analyze_and_save(results_all, paths)
  }
)

# Step 10: Nullification operating-characteristic tables
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
  format = "file"
)

# Step 11: Multiverse dashboard plots
target11 <- targets::tar_target(
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

# Step 12: Visual coverage for every CSV artifact
target12 <- targets::tar_target(
  csv_output_plot_files,
  {
    analysis
    nullification_diagnostics_file
    cse_definition_comparison_files
    shuffle_adversarial_diagnostics_file
    nullification_operating_characteristics_files
    output_dir <- file.path(paths$outputs_analysis, "figures", "csv_outputs")
    manifest <- write_csv_output_plots(paths$outputs_analysis, output_dir)
    manifest_path <- file.path(output_dir, "csv_output_plot_manifest.csv")
    readr::write_csv(manifest, manifest_path)
    c(manifest$plot[manifest$plotted], manifest_path)
  },
  format = "file"
)

list(target0, target1, target2, target3, target4, target5, target6, targets7, target8, target9, target10, target11, target12)
