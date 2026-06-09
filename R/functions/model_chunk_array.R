# R/functions/model_chunk_array.R - submit model chunks as a SLURM array

compress_integer_ranges <- function(ids) {
  ids <- sort(unique(as.integer(ids)))
  if (!length(ids)) return("")
  breaks <- c(TRUE, diff(ids) != 1L)
  starts <- ids[breaks]
  ends <- c(ids[which(breaks)[-1L] - 1L], ids[length(ids)])
  paste(ifelse(starts == ends, starts, paste0(starts, "-", ends)), collapse = ",")
}

format_slurm_minutes <- function(minutes) {
  minutes <- as.integer(minutes)
  days <- minutes %/% 1440L
  rem <- minutes %% 1440L
  hours <- rem %/% 60L
  mins <- rem %% 60L
  if (days > 0L) sprintf("%d-%02d:%02d:00", days, hours, mins) else sprintf("%02d:%02d:00", hours, mins)
}

chunk_result_paths <- function(branch_chunks, paths) {
  vapply(branch_chunks, function(chunk) {
    result_path_for_chunk(paths, unique(chunk$chunk_id))
  }, character(1))
}

missing_chunk_ids <- function(branch_chunks, paths, overwrite = FALSE) {
  ids <- vapply(branch_chunks, function(chunk) as.integer(unique(chunk$chunk_id)), integer(1))
  if (isTRUE(overwrite)) return(ids)
  paths_out <- chunk_result_paths(branch_chunks, paths)
  ids[!file.exists(paths_out)]
}

submit_model_chunk_array <- function(chunk_ids, paths, config) {
  if (!length(chunk_ids)) return(NA_character_)
  array_spec <- compress_integer_ranges(chunk_ids)
  concurrency <- as.integer(config$n_workers %||% 100L)
  if (is.na(concurrency) || concurrency < 1L) concurrency <- 100L

  slurm <- config$slurm
  worker <- slurm$worker
  script <- file.path(paths$root, "slurm_fit_model_chunk_array.sh")
  if (!file.exists(script)) stop("Missing SLURM array script: ", script)

  args <- c(
    "--parsable",
    "--job-name=model_chunk",
    paste0("--array=", array_spec, "%", concurrency),
    paste0("--output=", file.path(paths$logs, "model_chunk_%A_%a.out")),
    paste0("--error=", file.path(paths$logs, "model_chunk_%A_%a.err")),
    paste0("--partition=", slurm$partition),
    paste0("--cpus-per-task=", worker$cpus),
    paste0("--mem=", worker$mem_gb, "G"),
    paste0("--time=", format_slurm_minutes(worker$time_min)),
    script
  )

  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(paths$root)
  out <- system2("sbatch", args, stdout = TRUE, stderr = TRUE)
  status <- attr(out, "status") %||% 0L
  if (!identical(as.integer(status), 0L)) {
    stop("sbatch failed: ", paste(out, collapse = "\n"))
  }
  job_id <- sub(";.*$", "", trimws(out[[1]]))
  log_pipeline(logger::INFO, "Submitted model chunk SLURM array job {job_id} for {length(chunk_ids)} chunk(s), concurrency={concurrency}")
  job_id
}

slurm_job_running <- function(job_id) {
  out <- suppressWarnings(system2("squeue", c("-h", "-j", job_id), stdout = TRUE, stderr = FALSE))
  length(out) > 0L
}

run_model_chunks_slurm_array <- function(branch_chunks, paths, config, poll_seconds = 30L) {
  expected <- chunk_result_paths(branch_chunks, paths)
  pending <- missing_chunk_ids(branch_chunks, paths, overwrite = config$overwrite_results)

  log_pipeline(
    logger::INFO,
    "Model chunks pending before SLURM array: {length(pending)} missing / {length(branch_chunks)} total"
  )

  if (!length(pending)) return(normalizePath(expected, winslash = "/", mustWork = TRUE))

  if (!isTRUE(config$is_hpc)) {
    for (chunk_id in pending) {
      fit_and_save_branch_chunk(branch_chunks[[chunk_id]], paths, config)
    }
    return(normalizePath(expected, winslash = "/", mustWork = TRUE))
  }

  job_id <- submit_model_chunk_array(pending, paths, config)
  repeat {
    missing <- expected[!file.exists(expected)]
    if (!length(missing)) break
    if (!slurm_job_running(job_id)) {
      stop(
        "Model chunk array job ", job_id, " finished but ", length(missing),
        " result file(s) are still missing. First missing: ", missing[[1]]
      )
    }
    log_pipeline(logger::INFO, "Waiting for model chunk array {job_id}: {length(missing)} file(s) still missing")
    Sys.sleep(poll_seconds)
  }

  log_pipeline(logger::INFO, "Model chunk SLURM array {job_id} completed all outputs")
  normalizePath(expected, winslash = "/", mustWork = TRUE)
}
