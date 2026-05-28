#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

source("R/functions/nullification_diagnostics.R")

input <- "R/all_indexed.csv"
if (!file.exists(input)) stop("Missing primary input: ", input)

work_dir <- file.path(tempdir(), "tdk_all_indexed_nullification_smoke")
subset_csv <- file.path(work_dir, "all_indexed_subset.csv")
out_dir <- file.path(work_dir, "out")
diag_csv <- file.path(work_dir, "nullification_diagnostics.csv")
dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

cat("Reading ", input, "\n", sep = "")
df <- readr::read_csv(input, show_col_types = FALSE)
needed <- c("study", "participant_id", "rt", "cong", "prev_cong", "trial_index", "source_row")
missing <- setdiff(needed, names(df))
if (length(missing) > 0) stop("Missing required smoke columns: ", paste(missing, collapse = ", "))

keep <- df %>%
  distinct(study, participant_id) %>%
  group_by(study) %>%
  slice_head(n = 30) %>%
  ungroup()
subset <- df %>% semi_join(keep, by = c("study", "participant_id"))
readr::write_csv(subset, subset_csv)
cat("Wrote smoke subset: ", nrow(subset), " rows\n", sep = "")

args <- c(
  "run", "--manifest-path", "R/rust/Cargo.toml", "--bin", "process", "--",
  "--input", subset_csv,
  "--output-dir", out_dir,
  "--sample-sizes", "1.0",
  "--transformations", "no_log_rt",
  "--outliers", "none",
  "--effect-conditions", "present,null_interaction",
  "--strip-methods", "local_median_residual",
  "--threads", "1",
  "--writer-threads", "1",
  "--log-level", "warn"
)
status <- system2("cargo", args)
if (!identical(status, 0L)) stop("Rust nullification smoke failed with status ", status)

write_nullification_diagnostics_for_dir(out_dir, diag_csv)
diag <- readr::read_csv(diag_csv, show_col_types = FALSE)
stripped <- diag %>% filter(effect_condition == "null_interaction", strip_method == "local_median_residual")
if (nrow(stripped) != 1L) stop("Expected one local_median_residual diagnostic row")
if (!isTRUE(stripped$preservation_pass[[1]])) {
  warning("Nullification smoke did not pass preservation gates on this bounded subset: ", stripped$preservation_warnings[[1]])
}
if (abs(stripped$mean_cse[[1]]) > 5 || abs(stripped$q050_cse[[1]]) > 5) {
  stop("Residual pooled CSE is too large for smoke: mean=", stripped$mean_cse[[1]], " q050=", stripped$q050_cse[[1]])
}

cat("All-indexed nullification smoke completed.\n")
cat("Diagnostics: ", diag_csv, "\n", sep = "")
cat("mean_cse=", stripped$mean_cse[[1]],
    " q050_cse=", stripped$q050_cse[[1]],
    " max_abs_timebin_q050_cse=", stripped$max_abs_timebin_q050_cse[[1]], "\n", sep = "")
