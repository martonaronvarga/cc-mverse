# R/functions/cse_term_extraction.R - explicit CSE term selection helpers

normalize_model_term <- function(term) {
  gsub("`", "", as.character(term), fixed = TRUE)
}

term_component_matches_variable <- function(component, variable) {
  component <- normalize_model_term(component)
  component == variable || grepl(paste0("^", variable, "([^[:alpha:]_].*)?$"), component)
}

is_cse_interaction_term <- function(term) {
  term <- normalize_model_term(term)
  parts <- strsplit(term, ":", fixed = TRUE)
  vapply(parts, function(x) {
    length(x) == 2L &&
      ((term_component_matches_variable(x[[1]], "cong") &&
        term_component_matches_variable(x[[2]], "prev_cong")) ||
        (term_component_matches_variable(x[[1]], "prev_cong") &&
          term_component_matches_variable(x[[2]], "cong")))
  }, logical(1))
}

select_cse_coefficient_row <- function(coefs, term_col = "term") {
  if (!term_col %in% names(coefs)) {
    stop("Coefficient table is missing term column: ", term_col)
  }
  idx <- which(is_cse_interaction_term(coefs[[term_col]]))
  if (length(idx) == 0L) {
    return(coefs[0, , drop = FALSE])
  }
  if (length(idx) > 1L) {
    stop("Multiple CSE interaction rows found: ", paste(coefs[[term_col]][idx], collapse = ", "))
  }
  coefs[idx, , drop = FALSE]
}

select_rmanova_cse_row <- function(stats, effect_col = NULL) {
  if (is.null(effect_col)) {
    candidates <- c("Effect", "effect", "term", "Term", "Parameter")
    effect_col <- candidates[candidates %in% names(stats)][1]
  }
  if (is.na(effect_col) || length(effect_col) == 0L) {
    stop("RMANOVA table has no recognizable effect/term column")
  }

  idx <- which(is_cse_interaction_term(stats[[effect_col]]))
  if (length(idx) == 0L) {
    return(stats[0, , drop = FALSE])
  }
  if (length(idx) > 1L) {
    stop("Multiple RMANOVA CSE rows found: ", paste(stats[[effect_col]][idx], collapse = ", "))
  }
  stats[idx, , drop = FALSE]
}
