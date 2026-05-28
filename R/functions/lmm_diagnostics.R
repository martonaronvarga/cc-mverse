# R/functions/lmm_diagnostics.R - singularity and rePCA diagnostics

safe_lmm_singularity <- function(model, tol = 1e-4) {
  if (is.null(model) || !requireNamespace("lme4", quietly = TRUE)) {
    return(NA)
  }
  tryCatch(
    lme4::isSingular(model, tol = tol),
    error = function(e) NA
  )
}

safe_repca_min_sd <- function(model) {
  if (is.null(model) || !requireNamespace("lme4", quietly = TRUE)) {
    return(NA_real_)
  }
  tryCatch({
    pcs <- lme4::rePCA(model)
    sds <- unlist(lapply(pcs, function(x) x$sdev), use.names = FALSE)
    if (length(sds) == 0 || !any(is.finite(sds))) return(NA_real_)
    min(sds[is.finite(sds)])
  }, error = function(e) NA_real_)
}
