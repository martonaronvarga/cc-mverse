# R/functions/pipeline_readiness.R
# Lightweight preflight report for local/HPC pipeline readiness.

readiness_row <- function(check, status, detail) {
  data.frame(
    check = check,
    status = status,
    detail = detail,
    stringsAsFactors = FALSE
  )
}

readiness_status <- function(ok, fail_status = "fail") {
  if (isTRUE(ok)) "pass" else fail_status
}

expected_processed_paths <- function(paths, branch_specs) {
  file.path(paths$data_processed, paste0("processed__", sort(unique(branch_specs$data_id)), ".parquet"))
}

check_branch_contract <- function(branch_specs) {
  failures <- character()
  if (any(branch_specs$effect_condition == "present" & branch_specs$strip_method != "none")) {
    failures <- c(failures, "present_non_none_strip")
  }
  if (any(branch_specs$effect_condition == "null_both")) {
    failures <- c(failures, "null_both_present")
  }
  if (any(branch_specs$effect_condition == "null_interaction" & branch_specs$strip_method == "none")) {
    failures <- c(failures, "null_interaction_none_strip")
  }
  if (any(duplicated(branch_specs$branch_id))) {
    failures <- c(failures, "duplicate_branch_id")
  }
  failures
}

pipeline_signature_status <- function(config, paths, branch_specs, raw_data_path) {
  status <- tryCatch(
    {
      validate_processed_cache_signature(config, paths, branch_specs, raw_data_path)
      "current"
    },
    error = function(e) paste("stale_or_missing:", conditionMessage(e))
  )
  status
}

build_pipeline_readiness_report <- function(
  config,
  paths,
  branch_specs,
  mode = config$mode %||% NA_character_,
  check_dependencies = TRUE
) {
  rows <- list()
  add <- function(check, status, detail) {
    rows[[length(rows) + 1L]] <<- readiness_row(check, status, detail)
  }

  raw_data_path <- if (grepl("^/", config$raw_csv)) config$raw_csv else file.path(paths$root, config$raw_csv)
  add(
    "raw_csv_exists",
    readiness_status(file.exists(raw_data_path)),
    raw_data_path
  )

  add(
    "branch_count",
    readiness_status(nrow(branch_specs) > 0),
    paste0(nrow(branch_specs), " model branches; ", length(unique(branch_specs$data_id)), " data branches")
  )

  contract_failures <- check_branch_contract(branch_specs)
  add(
    "branch_contract",
    readiness_status(length(contract_failures) == 0L),
    if (length(contract_failures) == 0L) "present/null_interaction branch constraints satisfied" else paste(contract_failures, collapse = ";")
  )

  target_script <- file.path(paths$root, "_targets.R")
  parse_ok <- tryCatch({ parse(target_script); TRUE }, error = function(e) e)
  add(
    "targets_script_parse",
    readiness_status(isTRUE(parse_ok)),
    if (isTRUE(parse_ok)) target_script else conditionMessage(parse_ok)
  )

  expected <- expected_processed_paths(paths, branch_specs)
  missing <- expected[!file.exists(expected)]
  add(
    "processed_files",
    readiness_status(length(missing) == 0L, fail_status = "warn"),
    paste0(length(expected) - length(missing), "/", length(expected), " expected processed parquet files exist")
  )

  sig_status <- pipeline_signature_status(config, paths, branch_specs, raw_data_path)
  add(
    "processed_cache_signature",
    readiness_status(identical(sig_status, "current"), fail_status = "warn"),
    sig_status
  )

  if (identical(mode, "hpc") || isTRUE(config$is_hpc)) {
    budget_ok <- (config$rust_threads + config$writer_threads) <= config$slurm$rust$cpus
    add(
      "hpc_rust_thread_budget",
      readiness_status(budget_ok),
      paste0("rust_threads=", config$rust_threads, "; writer_threads=", config$writer_threads, "; slurm.rust.cpus=", config$slurm$rust$cpus)
    )
    add(
      "hpc_worker_resources",
      readiness_status(!is.null(config$slurm$worker$mem_gb) && !is.null(config$slurm$worker$time_min) && !is.null(config$slurm$partition)),
      paste0("partition=", config$slurm$partition, "; worker_mem_gb=", config$slurm$worker$mem_gb, "; worker_time_min=", config$slurm$worker$time_min)
    )
  }

  if (isTRUE(check_dependencies)) {
    dep <- build_dependency_report()
    missing_packages <- dep$r_packages$package[!dep$r_packages$installed]
    add(
      "r_packages",
      readiness_status(length(missing_packages) == 0L),
      if (length(missing_packages) == 0L) "core R packages installed" else paste(missing_packages, collapse = ",")
    )
    cargo_metadata <- dep$rust$value[dep$rust$key == "cargo_metadata_available"]
    add(
      "cargo_metadata",
      readiness_status(length(cargo_metadata) > 0L && !grepl("^ERROR:", cargo_metadata)),
      if (length(cargo_metadata) == 0L) "missing cargo metadata result" else substr(cargo_metadata[[1]], 1L, 200L)
    )
  }

  dplyr::bind_rows(rows)
}

write_pipeline_readiness_report <- function(
  config,
  paths,
  branch_specs,
  output_csv = file.path(paths$outputs_analysis, "pipeline_readiness_report.csv"),
  mode = config$mode %||% NA_character_,
  check_dependencies = TRUE
) {
  report <- build_pipeline_readiness_report(config, paths, branch_specs, mode, check_dependencies)
  dir.create(dirname(output_csv), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(report, output_csv)
  invisible(output_csv)
}
