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

target4a <- targets::tar_target(
  processed_diagnostic_paths,
  {
    processed_data_dir
    list_processed_diagnostic_paths(paths$data_processed)
  },
  deployment = "main"
)

target4b <- targets::tar_target(
  diagnostic_path_chunks,
  split_diagnostic_paths(processed_diagnostic_paths),
  iteration = "list",
  deployment = "main"
)

target4c <- targets::tar_target(
  nullification_diagnostic_cache_files,
  compute_nullification_diagnostic_cache_paths(
    diagnostic_path_chunks,
    cache_dir = file.path(paths$outputs_analysis, "diagnostics_cache", "nullification"),
    overwrite = diagnostics_overwrite()
  ),
  pattern = map(diagnostic_path_chunks),
  iteration = "list",
  format = "file"
)

target4d <- targets::tar_target(
  nullification_diagnostic_chunk_files,
  write_nullification_diagnostic_cache_chunk(
    nullification_diagnostic_cache_files,
    chunk_dir = file.path(paths$outputs_analysis, "diagnostics_cache", "nullification_chunks")
  ),
  pattern = map(nullification_diagnostic_cache_files),
  iteration = "list",
  format = "file"
)

target4e <- targets::tar_target(
  nullification_diagnostics_file,
  aggregate_nullification_diagnostic_chunks(
    nullification_diagnostic_chunk_files,
    output_path = file.path(paths$outputs_analysis, "nullification_diagnostics.csv")
  ),
  format = "file",
  deployment = "main"
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

target6a <- targets::tar_target(
  shuffle_diagnostic_path_chunks,
  {
    shuffle_paths <- processed_diagnostic_paths[
      grepl("__null_interaction__shuffle[.](parquet|csv)$", basename(processed_diagnostic_paths), ignore.case = TRUE)
    ]
    split_diagnostic_paths(shuffle_paths, chunk_size = shuffle_diagnostic_chunk_size())
  },
  iteration = "list",
  deployment = "main"
)

target6b <- targets::tar_target(
  shuffle_adversarial_diagnostic_cache_files,
  compute_shuffle_adversarial_cache_paths(
    shuffle_diagnostic_path_chunks,
    all_paths = processed_diagnostic_paths,
    cache_dir = file.path(paths$outputs_analysis, "diagnostics_cache", "shuffle_adversarial"),
    overwrite = diagnostics_overwrite()
  ),
  pattern = map(shuffle_diagnostic_path_chunks),
  iteration = "list",
  format = "file"
)

target6c <- targets::tar_target(
  shuffle_adversarial_diagnostics_file,
  aggregate_shuffle_adversarial_diagnostic_cache(
    shuffle_adversarial_diagnostic_cache_files,
    output_csv = file.path(paths$outputs_analysis, "shuffle_adversarial_diagnostics.csv")
  ),
  format = "file",
  deployment = "main"
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
    expected_model_chunks <- chunk_result_paths(branch_chunks, paths)
    missing_model_chunks <- expected_model_chunks[!file.exists(expected_model_chunks)]
    if (length(missing_model_chunks) > 0L) {
      stop("Model chunk outputs are incomplete: ", length(missing_model_chunks), " missing. First missing: ", missing_model_chunks[[1]])
    }
    setup_logging(log_level = config$log_level, log_dir = paths$logs)
    logger::log_info("Aggregating chunked model results...")
    results <- load_results(paths, validate = TRUE)

    branch_lookup <- branch_specs |>
      dplyr::select(branch_id, data_id) |>
      dplyr::distinct()

    if (!"branch_id" %in% names(results)) {
      stop("Aggregated results are missing branch_id; cannot attach data_id")
    }

    if (!"data_id" %in% names(results)) {
      missing_lookup <- dplyr::anti_join(
        results |> dplyr::distinct(branch_id),
        branch_lookup,
        by = "branch_id"
      )

      if (nrow(missing_lookup) > 0L) {
        stop(
          "Cannot attach data_id: ",
          nrow(missing_lookup),
          " result branch_id values are absent from branch_specs. First missing: ",
          missing_lookup$branch_id[[1]]
        )
      }

      duplicate_lookup <- branch_lookup |>
        dplyr::count(branch_id) |>
        dplyr::filter(n > 1L)

      if (nrow(duplicate_lookup) > 0L) {
        stop(
          "Cannot attach data_id: branch_specs has duplicate branch_id mappings. First duplicate: ",
          duplicate_lookup$branch_id[[1]]
        )
      }

      results <- results |>
        dplyr::left_join(branch_lookup, by = "branch_id", relationship = "many-to-one") |>
        dplyr::relocate(data_id, .after = branch_id)
    }

    if (anyNA(results$data_id)) {
      stop("data_id attachment produced NA values")
    }

    logger::log_info(
      "Aggregated {nrow(results)} results with {dplyr::n_distinct(results$data_id)} data_id values"
    )

    results
  },
  deployment = "main"
)

target9 <- targets::tar_target(
  analysis,
  {
    nullification_diagnostics_file
    model_chunk_files
    results_all

    setup_logging(log_level = config$log_level, log_dir = paths$logs)
    verbose_log <- log_post_model_event(
      paths,
      "analysis",
      "start",
      list(
        results_rows = nrow(results_all),
        results_columns = ncol(results_all),
        diagnostics_csv = nullification_diagnostics_file,
        model_chunk_files = length(model_chunk_files),
        analysis_run_dir = analysis_run_dir(paths),
        latest_dir_before = analysis_latest_dir(paths)
      )
    )

    analysis_result <- analyze_and_save(
      results_all,
      paths,
      diagnostics_csv = nullification_diagnostics_file,
      alpha = config$alpha
    )

    log_post_model_event(
      paths,
      "analysis",
      "complete",
      list(
        tables = names(analysis_result),
        data_frame_tables = names(analysis_result)[vapply(analysis_result, is.data.frame, logical(1))],
        latest_dir_after = analysis_latest_dir(paths),
        verbose_log = verbose_log
      )
    )

    analysis_result
  },
  deployment = "main"
)

target10 <- targets::tar_target(
  nullification_operating_characteristics_files,
  {
    nullification_diagnostics_file
    results_all
    analysis

    setup_logging(log_level = config$log_level, log_dir = paths$logs)
    log_post_model_event(
      paths,
      "nullification_operating_characteristics",
      "start",
      list(
        diagnostics_csv = nullification_diagnostics_file,
        results_rows = nrow(results_all),
        analysis_latest_dir = analysis_latest_dir(paths)
      )
    )

    diagnostics <- readr::read_csv(nullification_diagnostics_file, show_col_types = FALSE)

    tables <- build_nullification_operating_characteristics(
      results_all,
      diagnostics,
      alpha = config$alpha
    )

    output_dir <- file.path(
      analysis_latest_dir(paths),
      "nullification_operating_characteristics"
    )

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

    log_post_model_event(
      paths,
      "nullification_operating_characteristics",
      "complete",
      list(
        output_dir = output_dir,
        output_files = paths_out,
        table_rows = vapply(tables, nrow, integer(1))
      )
    )

    paths_out
  },
  format = "file",
  deployment = "main"
)

target11 <- targets::tar_target(
  plots,
  {
    analysis
    results_all
    nullification_operating_characteristics_files

    setup_logging(log_level = config$log_level, log_dir = paths$logs)

    fig_dir <- file.path(analysis_latest_dir(paths), "figures")
    dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

    before_files <- list.files(fig_dir, full.names = TRUE, recursive = TRUE)
    log_post_model_event(
      paths,
      "plots",
      "start",
      list(
        fig_dir = fig_dir,
        existing_files = length(before_files),
        analysis_tables = names(analysis),
        results_rows = nrow(results_all),
        operating_characteristics_files = nullification_operating_characteristics_files
      )
    )

    dashboard_plots <- generate_multiverse_dashboard(
      analysis,
      output_dir = fig_dir,
      save_individual = TRUE
    )

    files <- list.files(fig_dir, full.names = TRUE, recursive = TRUE)
    new_files <- setdiff(files, before_files)

    log_post_model_event(
      paths,
      "plots",
      "complete",
      list(
        fig_dir = fig_dir,
        returned_plot_names = names(dashboard_plots),
        total_files = length(files),
        new_files = length(new_files),
        files = files
      )
    )

    if (length(files) == 0L) {
      stop("Plot generation produced zero files in: ", fig_dir)
    }

    files
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
    plots

    setup_logging(log_level = config$log_level, log_dir = paths$logs)
    gallery_enabled <- tolower(Sys.getenv("PLOT_ALL_CSV_OUTPUTS", unset = "false")) %in% c("1", "true", "yes")
    log_post_model_event(
      paths,
      "csv_output_plot_files",
      "start",
      list(
        enabled = gallery_enabled,
        PLOT_ALL_CSV_OUTPUTS = Sys.getenv("PLOT_ALL_CSV_OUTPUTS", unset = "<unset>"),
        analysis_latest_dir = analysis_latest_dir(paths)
      )
    )

    if (!gallery_enabled) {
      log_post_model_event(
        paths,
        "csv_output_plot_files",
        "skipped",
        list(reason = "Set PLOT_ALL_CSV_OUTPUTS=true to enable")
      )
      character()
    } else {
      analysis_dir <- analysis_latest_dir(paths)
      output_dir <- file.path(analysis_dir, "figures", "csv_outputs")
      before_files <- list.files(output_dir, full.names = TRUE, recursive = TRUE)

      manifest <- write_csv_output_plots(
        analysis_dir,
        output_dir
      )

      manifest_path <- file.path(output_dir, "csv_output_plot_manifest.csv")
      readr::write_csv(manifest, manifest_path)
      output_files <- c(manifest$plot[manifest$plotted], manifest_path)

      log_post_model_event(
        paths,
        "csv_output_plot_files",
        "complete",
        list(
          analysis_dir = analysis_dir,
          output_dir = output_dir,
          csvs_seen = nrow(manifest),
          plotted = sum(manifest$plotted),
          failed = sum(!manifest$plotted),
          manifest_path = manifest_path,
          new_files = length(setdiff(list.files(output_dir, full.names = TRUE, recursive = TRUE), before_files)),
          failed_csvs = manifest$csv[!manifest$plotted]
        )
      )

      output_files
    }
  },
  format = "file",
  deployment = "main"
)

list(
  target0, target1, target2, target3,
  target4a, target4b, target4c, target4d, target4e,
  target5,
  target6a, target6b, target6c,
  target7a, target7b,
  target8, target9, target10, target11, target12
)
