#!/usr/bin/env Rscript

source_path <- function(path) {
  if (file.exists(path)) return(path)
  file.path("R", path)
}
source(source_path(file.path("functions", "cse_definition_comparison.R")))

check <- function(label, value) {
  if (!isTRUE(value)) stop("FAILED: ", label)
  cat("ok -", label, "\n")
}

diagnostics <- data.frame(
  data_id = c("present", "null_a", "null_b"),
  effect_condition = c("present", "null_interaction", "null_interaction"),
  strip_method = c("none", "local_median_residual", "additive_qmap"),
  sample_size = c(1, 1, 1),
  outlier = c("none", "none", "none"),
  transformation = c("no_log_rt", "no_log_rt", "no_log_rt"),
  nullification_verdict = c("reference_present", "interpretable_nullifier", "fails_preservation_gates"),
  preservation_pass = c(NA, TRUE, FALSE),
  mean_cse = c(25, 1, 2),
  median_cse = c(22, 1, 2),
  q010_cse = c(10, 0, 2),
  q025_cse = c(15, 2, 3),
  q050_cse = c(22, 1, 4),
  q075_cse = c(30, 3, 20),
  q090_cse = c(40, 4, 35),
  max_abs_timebin_q050_cse = c(35, 6, 25),
  stringsAsFactors = FALSE
)

long <- build_cse_definition_long(diagnostics)
check("long output includes all rows and available metrics", nrow(long) == nrow(diagnostics) * 8L)
check("metric families are labelled", all(c("location", "distributional", "time_local_distributional") %in% long$definition_family))

summary <- summarise_cse_definition_comparison(diagnostics)
flag <- summary$distributional_flag[summary$strip_method == "additive_qmap"]
check("distributional residual can be flagged despite small mean CSE", identical(flag, "distributional_residual_without_location_cse"))

out_dir <- tempfile("cse_defs_")
paths <- write_cse_definition_comparison(
  diagnostics_csv = local({ p <- tempfile(fileext = ".csv"); write.csv(diagnostics, p, row.names = FALSE); p }),
  output_dir = out_dir
)
check("summary artifact is written", file.exists(paths$summary))
check("long artifact is written", file.exists(paths$long))
unlink(out_dir, recursive = TRUE, force = TRUE)

cat("All CSE definition comparison checks passed.\n")
