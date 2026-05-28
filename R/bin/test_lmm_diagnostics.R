#!/usr/bin/env Rscript

function_path <- file.path("functions", "lmm_diagnostics.R")
if (!file.exists(function_path)) {
  function_path <- file.path("R", "functions", "lmm_diagnostics.R")
}
source(function_path)

check <- function(label, value) {
  if (!isTRUE(value)) stop("FAILED: ", label)
  cat("ok -", label, "\n")
}

check("NULL model singularity returns NA", is.na(safe_lmm_singularity(NULL)))
check("NULL model rePCA min sd returns NA", is.na(safe_repca_min_sd(NULL)))

cat("All LMM diagnostic helper checks passed.\n")
