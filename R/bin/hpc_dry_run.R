#!/usr/bin/env Rscript

source("R/functions/logging.R")
source("R/functions/paths.R")
source("R/functions/config.R")
source("R/functions/rust_interop.R")

config_path <- "R/pipeline.yaml"
config <- load_config("hpc", config_path = config_path)
paths <- init_project_paths("R")
project_root <- paths$root %||% paths$project_root
input_csv <- if (grepl("^/", config$raw_csv)) config$raw_csv else file.path(project_root, config$raw_csv)
if (!file.exists(input_csv)) stop("Configured input CSV does not exist: ", input_csv)

if (config$rust_threads + config$writer_threads > config$slurm$rust$cpus) {
  stop("Rust threads + writer threads exceed slurm.rust.cpus")
}

args <- build_rust_args(config, paths, input_csv)
strip_arg <- args[match("--strip-methods", args) + 1]
effect_arg <- args[match("--effect-conditions", args) + 1]
if (grepl("null_both", effect_arg, fixed = TRUE)) stop("HPC config still includes null_both")
if (!grepl("local_median_residual", strip_arg, fixed = TRUE)) stop("HPC config missing local_median_residual")
if (!grepl("additive_qmap", strip_arg, fixed = TRUE)) stop("HPC config missing additive_qmap sensitivity")

cat("HPC dry run ok\n")
cat("input_csv=", input_csv, "\n", sep = "")
cat("effect_conditions=", effect_arg, "\n", sep = "")
cat("strip_methods=", strip_arg, "\n", sep = "")
cat("rust_cpus=", config$slurm$rust$cpus, " rust_threads=", config$rust_threads, " writer_threads=", config$writer_threads, "\n", sep = "")
cat("rust_args=", paste(args, collapse = " "), "\n", sep = "")
