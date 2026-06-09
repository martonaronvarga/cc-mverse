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
  chunk_ids <- sort(unique(as.integer(chunk_ids)))
  concurrency <- as.integer(config$n_workers %||% 100L)
  if (is.na(concurrency) || concurrency < 1L) concurrency <- 100L

  slurm <- config$slurm
  worker <- slurm$worker
  script <- file.path(paths$root, "slurm_fit_model_chunk_array.sh")
  if (!file.exists(script)) stop("Missing SLURM array script: ", script)

  id_file <- file.path(paths$config_objects, "model_chunk_array_ids.txt")
  dir.create(dirname(id_file), recursive = TRUE, showWarnings = FALSE)
  writeLines(as.character(chunk_ids), id_file)

  args <- c(
    "--parsable",
    "--job-name=model_chunk",
    paste0("--array=1-", length(chunk_ids), "%", concurrency),
    paste0("--output=", file.path(paths$logs, "model_chunk_%A_%a.out")),
    paste0("--error=", file.path(paths$logs, "model_chunk_%A_%a.err")),
    paste0("--partition=", slurm$partition),
    paste0("--cpus-per-task=", worker$cpus),
    paste0("--mem=", worker$mem_gb, "G"),
    paste0("--time=", format_slurm_minutes(worker$time_min)),
    paste0("--export=ALL,MODEL_CHUNK_ID_FILE=", normalizePath(id_file, winslash = "/", mustWork = TRUE)),
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
  log_pipeline(logger::INFO, "Submitted model chunk SLURM array job {job_id} for {length(chunk_ids)} chunk(s), concurrency={concurrency}, id_file={id_file}")
  job_id
}

slurm_job_running <- function(job_id) {
  out <- suppressWarnings(system2("squeue", c("-h", "-j", job_id), stdout = TRUE, stderr = FALSE))
  length(out) > 0L
}

split_chunk_id_batches <- function(ids, batch_size = 1000L) {
  ids <- sort(unique(as.integer(ids)))
  if (!length(ids)) return(list())
  batch_size <- max(1L, as.integer(batch_size))
  split(ids, ceiling(seq_along(ids) / batch_size))
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

  batches <- split_chunk_id_batches(pending, batch_size = config$slurm$array_batch_size %||% 1000L)
  for (batch_index in seq_along(batches)) {
    batch <- batches[[batch_index]]
    batch_expected <- vapply(batch, function(chunk_id) result_path_for_chunk(paths, chunk_id), character(1))
    log_pipeline(logger::INFO, "Submitting model chunk array batch {batch_index}/{length(batches)}: {length(batch)} chunk(s)")
    job_id <- submit_model_chunk_array(batch, paths, config)
    Sys.sleep(2)
    repeat {
      missing <- batch_expected[!file.exists(batch_expected)]
      if (!length(missing)) break
      if (!slurm_job_running(job_id)) {
        stop(
          "Model chunk array job ", job_id, " finished but ", length(missing),
          " result file(s) are still missing in batch ", batch_index,
          ". First missing: ", missing[[1]]
        )
      }
      log_pipeline(logger::INFO, "Waiting for model chunk array {job_id}: {length(missing)} file(s) still missing in batch {batch_index}/{length(batches)}")
      Sys.sleep(poll_seconds)
    }
    log_pipeline(logger::INFO, "Model chunk SLURM array {job_id} completed batch {batch_index}/{length(batches)}")
  }

  normalizePath(expected, winslash = "/", mustWork = TRUE)
}
