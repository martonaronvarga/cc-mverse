#!/usr/bin/env Rscript

function_path <- file.path("functions", "data_validation.R")
if (!file.exists(function_path)) {
  function_path <- file.path("R", "functions", "data_validation.R")
}
source(function_path)

check <- function(label, value) {
  if (!isTRUE(value)) stop("FAILED: ", label)
  cat("ok -", label, "\n")
}

make_valid <- function() {
  rows <- list()
  idx <- 1L
  for (pid in c("p1", "p2")) {
    for (cong in c(-1, 1)) {
      for (prev in c(-1, 1)) {
        rows[[idx]] <- data.frame(
          participant_id = pid,
          cong = cong,
          prev_cong = prev,
          rt = 600,
          trial_index = idx,
          stringsAsFactors = FALSE
        )
        idx <- idx + 1L
      }
    }
  }
  do.call(rbind, rows)
}

valid <- make_valid()
check("valid branch data passes", isTRUE(validate_cse_branch_data(valid, context = "branch=test")))

missing_col <- valid
missing_col$rt <- NULL
check("missing column fails", inherits(try(validate_cse_branch_data(missing_col, context = "branch=missing"), silent = TRUE), "try-error"))

bad_code <- valid
bad_code$cong[1] <- 0
check("bad congruency coding fails", inherits(try(validate_cse_branch_data(bad_code, context = "branch=bad_code"), silent = TRUE), "try-error"))

bad_rt <- valid
bad_rt$rt[1] <- 0
check("nonpositive RT fails", inherits(try(validate_cse_branch_data(bad_rt, context = "branch=bad_rt"), silent = TRUE), "try-error"))
check("log transformation rejects invalid RT", inherits(try(validate_cse_branch_data(bad_rt, context = "branch=bad_log", transformation = "log_rt"), silent = TRUE), "try-error"))

bad_order <- valid
bad_order$trial_index[bad_order$participant_id == "p1"] <- rev(bad_order$trial_index[bad_order$participant_id == "p1"])
check("decreasing trial order fails", inherits(try(validate_cse_branch_data(bad_order, context = "branch=bad_order"), silent = TRUE), "try-error"))

sparse <- valid[!(valid$participant_id == "p1" & valid$cong == 1 & valid$prev_cong == 1), ]
check("incomplete participant cell fails", inherits(try(validate_cse_branch_data(sparse, context = "branch=sparse"), silent = TRUE), "try-error"))

cat("All data validation checks passed.\n")
