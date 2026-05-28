# R/functions/random_effect_extraction.R - robust VarCorr variance helpers

extract_varcorr_stddev <- function(varcorr, group = "participant_id", term = "(Intercept)") {
  if (is.null(varcorr) || is.null(varcorr[[group]])) {
    return(NA_real_)
  }
  stddev <- attr(varcorr[[group]], "stddev")
  if (is.null(stddev) || is.null(names(stddev)) || !term %in% names(stddev)) {
    return(NA_real_)
  }
  out <- as.numeric(stddev[[term]])
  if (!is.finite(out)) NA_real_ else out
}

extract_varcorr_variance <- function(varcorr, group = "participant_id", term = "(Intercept)") {
  stddev <- extract_varcorr_stddev(varcorr, group = group, term = term)
  if (is.na(stddev)) NA_real_ else stddev^2
}
