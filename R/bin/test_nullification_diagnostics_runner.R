#!/usr/bin/env Rscript

function_path <- file.path("functions", "nullification_diagnostics.R")
if (!file.exists(function_path)) {
  function_path <- file.path("R", "functions", "nullification_diagnostics.R")
}
source(function_path)

check <- function(label, value) {
  if (!isTRUE(value)) stop("FAILED: ", label)
  cat("ok -", label, "\n")
}

make_df <- function(cse = 0) {
  rows <- list()
  idx <- 1L
  for (pid in paste0("p", 1:3)) {
    for (cong in c(-1, 1)) {
      for (prev in c(-1, 1)) {
        for (rep in 1:10) {
          rows[[idx]] <- data.frame(
            participant_id = pid,
            cong = cong,
            prev_cong = prev,
            rt = 600 + 10 * cong + 5 * prev + cse * cong * prev + rep,
            stringsAsFactors = FALSE
          )
          idx <- idx + 1L
        }
      }
    }
  }
  do.call(rbind, rows)
}

input_dir <- file.path("R", "outputs", "analysis", "diagnostic_runner_smoke_inputs")
if (!dir.exists(input_dir)) dir.create(input_dir, recursive = TRUE, showWarnings = FALSE)
write.csv(
  make_df(0),
  file.path(input_dir, "processed__1__1__no_log_rt__none__null_interaction__shuffle.csv"),
  row.names = FALSE
)
write.csv(
  make_df(20),
  file.path(input_dir, "processed__1__1__no_log_rt__none__present__none.csv"),
  row.names = FALSE
)

output <- file.path("R", "outputs", "analysis", "nullification_diagnostics_runner_smoke.csv")
write_nullification_diagnostics_for_dir(input_dir, output)
out <- read.csv(output, stringsAsFactors = FALSE)

check("runner wrote two diagnostic rows", nrow(out) == 2L)
check("data_id metadata parsed", all(c("null_interaction", "present") %in% out$effect_condition))
check("null synthetic CSE near zero", abs(out$mean_cse[out$effect_condition == "null_interaction"]) < 1e-9)
check("present synthetic CSE reflects known effect", abs(out$mean_cse[out$effect_condition == "present"] - 80) < 1e-9)

cat("All nullification diagnostic runner checks passed.\n")
