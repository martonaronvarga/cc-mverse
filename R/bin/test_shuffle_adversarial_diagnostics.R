#!/usr/bin/env Rscript

source_path <- function(path) {
  if (file.exists(path)) return(path)
  file.path("R", path)
}
source(source_path(file.path("functions", "paths.R")))
source(source_path(file.path("functions", "nullification_diagnostics.R")))
source(source_path(file.path("functions", "shuffle_adversarial_diagnostics.R")))

check <- function(label, value) {
  if (!isTRUE(value)) stop("FAILED: ", label)
  cat("ok -", label, "\n")
}

present <- data.frame(
  participant_id = rep(c("p1", "p2"), each = 8),
  cong = rep(rep(c(-1, 1), each = 4), 2),
  prev_cong = rep(c(-1, -1, 1, 1), 4),
  rt = c(510, 520, 560, 570, 600, 610, 640, 650, 500, 505, 550, 555, 590, 595, 630, 635),
  stringsAsFactors = FALSE
)
shuffle <- present
shuffle$rt <- unlist(lapply(split(present, list(present$participant_id, present$cong), drop = TRUE), function(x) rev(x$rt)), use.names = FALSE)
# Restore row order within split construction for an exact processed-style table.
shuffle <- do.call(rbind, lapply(split(present, list(present$participant_id, present$cong), drop = TRUE), function(x) { x$rt <- rev(x$rt); x }))
row.names(shuffle) <- NULL

multiset <- compare_shuffle_multisets(present, shuffle)
check("shuffle preserves RT multiset within participant x cong", isTRUE(multiset$multiset_preserved))

diag <- build_shuffle_adversarial_diagnostic(present, shuffle, data_id = "shuffle", present_data_id = "present")
check("diagnostic reports matching row counts", isTRUE(diag$row_count_match))
check("diagnostic carries conditional slopes", is.finite(diag$present_prev_cong_rt_slope) && is.finite(diag$shuffle_prev_cong_rt_slope))

bad <- shuffle
bad$rt[1] <- bad$rt[1] + 1
bad_multiset <- compare_shuffle_multisets(present, bad)
check("multiset change is detected", !isTRUE(bad_multiset$multiset_preserved))

tmp <- tempfile("shuffle_diag_")
dir.create(tmp)
write.csv(present, file.path(tmp, "processed__1__1__no_log_rt__none__present__none.csv"), row.names = FALSE)
write.csv(shuffle, file.path(tmp, "processed__1__1__no_log_rt__none__null_interaction__shuffle.csv"), row.names = FALSE)
out <- file.path(tmp, "shuffle_adversarial_diagnostics.csv")
path <- write_shuffle_adversarial_diagnostics_for_dir(tmp, out)
check("directory writer creates output", file.exists(path))
written <- read.csv(path)
check("directory writer finds one shuffle branch", nrow(written) == 1L)
unlink(tmp, recursive = TRUE, force = TRUE)

cat("All shuffle adversarial diagnostic checks passed.\n")
