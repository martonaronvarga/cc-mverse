#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(flag, default = NA_character_) {
  hit <- match(flag, args)
  if (is.na(hit) || hit == length(args)) default else args[[hit + 1L]]
}
has_flag <- function(flag) flag %in% args

chunk_id_file <- get_arg("--chunk-id-file", Sys.getenv("MODEL_CHUNK_ID_FILE", unset = NA_character_))
array_task_id <- Sys.getenv("SLURM_ARRAY_TASK_ID", unset = NA_character_)
chunk_id <- get_arg("--chunk-id", NA_character_)

if (!is.na(chunk_id_file) && nzchar(chunk_id_file)) {
  if (is.na(array_task_id) || !nzchar(array_task_id)) {
    stop("MODEL_CHUNK_ID_FILE requires SLURM_ARRAY_TASK_ID", call. = FALSE)
  }
  ids <- readLines(chunk_id_file, warn = FALSE)
  ids <- ids[nzchar(ids)]
  idx <- as.integer(array_task_id)
  if (is.na(idx) || idx < 1L || idx > length(ids)) {
    stop("SLURM_ARRAY_TASK_ID ", array_task_id, " is out of range for ", chunk_id_file, call. = FALSE)
  }
  chunk_id <- ids[[idx]]
} else if (is.na(chunk_id) || !nzchar(chunk_id)) {
  chunk_id <- array_task_id
}

if (is.na(chunk_id) || !nzchar(chunk_id)) {
  stop("Provide --chunk-id, --chunk-id-file, or run with SLURM_ARRAY_TASK_ID", call. = FALSE)
}
chunk_id <- as.integer(chunk_id)
if (is.na(chunk_id) || chunk_id < 1L) stop("chunk_id must be a positive integer", call. = FALSE)

config_dir <- get_arg("--config-dir", "_config")
overwrite <- has_flag("--overwrite") || tolower(Sys.getenv("OVERWRITE_RESULTS", unset = "false")) %in% c("1", "true", "yes")

quiet_load <- function(expr) invisible(capture.output(suppressMessages(suppressWarnings(force(expr)))))
quiet_load({
  invisible(lapply(list.files("functions", pattern = "\\.R$", full.names = TRUE), source))
  load_all_packages()
})

state <- load_pipeline_state(config_dir)
config <- state$config
paths <- state$paths
branch_specs <- state$branches
if (overwrite) config$overwrite_results <- TRUE

setup_logging(log_level = config$log_level, log_dir = paths$logs)
initialize_results_schema(paths$outputs_results)

chunks <- split_branch_specs(branch_specs, config)
if (chunk_id > length(chunks)) {
  stop("chunk_id ", chunk_id, " exceeds available chunks ", length(chunks), call. = FALSE)
}
chunk <- chunks[[chunk_id]]
out <- result_path_for_chunk(paths, chunk_id)

if (file.exists(out) && !isTRUE(config$overwrite_results)) {
  log_pipeline(logger::INFO, "Array chunk {chunk_id}: output exists, skipping: {out}")
  cat(out, "\n", sep = "")
  quit(status = 0L)
}

log_pipeline(logger::INFO, "Array chunk {chunk_id}: fitting {nrow(chunk)} branch(es)")
path <- fit_and_save_branch_chunk(chunk, paths, config)
log_pipeline(logger::INFO, "Array chunk {chunk_id}: done: {path}")
cat(path, "\n", sep = "")
