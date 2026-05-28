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
  for (pid in paste0("p", 1:4)) {
    for (cong in c(-1, 1)) {
      for (prev in c(-1, 1)) {
        for (rep in 1:20) {
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

null_diag <- build_nullification_diagnostic(make_df(0), data_id = "null")
present_diag <- build_nullification_diagnostic(make_df(25), data_id = "present")

check("null mean CSE near zero", abs(null_diag$mean_cse) < 1e-9)
check("present mean CSE reflects known interaction scale", abs(present_diag$mean_cse - 100) < 1e-9)
check("cell minimum is correct", null_diag$cell_min_n == 80L)
check("quantile columns exist", all(c("q010_cse", "q025_cse", "q050_cse", "q075_cse", "q090_cse") %in% names(null_diag)))

invalid_diag <- build_nullification_diagnostic(transform(make_df(0), rt = NA_real_), data_id = "invalid")
check("all-invalid diagnostic mean is NA", is.na(invalid_diag$mean_cse))
check("all-invalid diagnostic max CSE is NA", is.na(invalid_diag$max_abs_participant_cse))
check("all-invalid diagnostic warns", grepl("no_finite_participant_cse", invalid_diag$warnings))

output <- file.path("R", "outputs", "analysis", "nullification_diagnostics_smoke.csv")
write_nullification_diagnostics(rbind(null_diag, present_diag), output)
check("smoke output written", file.exists(output))

cat("All nullification diagnostic checks passed.\n")
