#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(flag, default = NA_character_) {
  hit <- match(flag, args)
  if (is.na(hit) || hit == length(args)) default else args[[hit + 1L]]
}
has_flag <- function(flag) flag %in% args

config_dir <- get_arg("--config-dir", "_config")
mode <- get_arg("--mode", "missing")
if (!mode %in% c("missing", "all")) stop("--mode must be 'missing' or 'all'", call. = FALSE)

quiet_load <- function(expr) invisible(capture.output(suppressMessages(suppressWarnings(force(expr)))))
quiet_load({
  invisible(lapply(list.files("functions", pattern = "\\.R$", full.names = TRUE), source))
  load_all_packages()
})

state <- load_pipeline_state(config_dir)
config <- state$config
paths <- state$paths
branch_specs <- state$branches
chunks <- split_branch_specs(branch_specs, config)
ids <- seq_along(chunks)

if (mode == "missing") {
  existing <- vapply(ids, function(id) file.exists(result_path_for_chunk(paths, id)), logical(1))
  ids <- ids[!existing]
}

compress_ids <- function(ids) {
  ids <- sort(unique(as.integer(ids)))
  if (!length(ids)) return("")
  breaks <- c(TRUE, diff(ids) != 1L)
  starts <- ids[breaks]
  ends <- c(ids[which(breaks)[-1L] - 1L], ids[length(ids)])
  paste(ifelse(starts == ends, starts, paste0(starts, "-", ends)), collapse = ",")
}

spec <- compress_ids(ids)
message(sprintf("model_chunk_array_spec: %d pending chunk(s) / %d total", length(ids), length(chunks)))
if (has_flag("--count")) {
  cat(length(ids), "\n", sep = "")
} else if (nzchar(spec)) {
  cat(spec, "\n", sep = "")
} else {
  cat("\n")
}
