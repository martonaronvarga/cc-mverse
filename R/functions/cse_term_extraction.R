# R/functions/cse_term_extraction.R - explicit CSE term selection helpers
#
# The CSE estimand is the cong × prev_cong interaction. Extraction must be
# name-based, not row-position-based, because afex/broom/lmerTest tables differ
# in row order and column names across model classes and package versions.

normalize_model_term <- function(term) {
  term <- as.character(term)
  term <- gsub("`", "", term, fixed = TRUE)
  term <- gsub("\\s+", "", term, perl = TRUE)
  term
}

term_component_matches_variable <- function(component, variable) {
  component <- normalize_model_term(component)
  variable <- normalize_model_term(variable)

  if (identical(component, variable)) return(TRUE)
  if (!startsWith(component, variable)) return(FALSE)

  suffix <- substring(component, nchar(variable) + 1L)

  # Common coefficient names for treatment/sum contrasts append the factor level
  # directly to the variable name, e.g. congpositive:prev_congpositive. Keep
  # this whitelist explicit so unrelated variables such as congruency do not
  # accidentally match cong.
  allowed_suffixes <- c(
    "1", "2", "TRUE", "FALSE", "true", "false",
    "positive", "negative", "pos", "neg",
    "congruent", "incongruent", "compatible", "incompatible",
    "high", "low"
  )

  suffix %in% allowed_suffixes || grepl("^[[:digit:]._-]", suffix, perl = TRUE)
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

# afex::anova() often returns an anova/data.frame with the effects stored in
# row names rather than in an explicit column. Convert it to a tibble while
# preserving those row names as Effect. If the package already provides an
# effect column, keep it.
as_rmanova_stats_table <- function(x) {
  df <- as.data.frame(x, check.names = FALSE)
  rn <- rownames(df)
  has_effect_col <- any(c("Effect", "effect", "term", "Term", "Parameter") %in% names(df))
  if (!has_effect_col) {
    df <- cbind(Effect = rn, df, stringsAsFactors = FALSE)
  }
  tibble::as_tibble(df)
}

rmanova_effect_column <- function(stats, effect_col = NULL) {
  if (is.null(effect_col)) {
    candidates <- c("Effect", "effect", "term", "Term", "Parameter")
    effect_col <- candidates[candidates %in% names(stats)][1]
  }
  if (is.na(effect_col) || length(effect_col) == 0L || !effect_col %in% names(stats)) {
    stop("RMANOVA table has no recognizable effect/term column. Columns: ", paste(names(stats), collapse = ", "))
  }
  effect_col
}

select_rmanova_cse_row <- function(stats, effect_col = NULL) {
  stats <- as_rmanova_stats_table(stats)
  effect_col <- rmanova_effect_column(stats, effect_col)

  idx <- which(is_cse_interaction_term(stats[[effect_col]]))
  if (length(idx) == 0L) {
    return(stats[0, , drop = FALSE])
  }
  if (length(idx) > 1L) {
    stop("Multiple RMANOVA CSE rows found: ", paste(stats[[effect_col]][idx], collapse = ", "))
  }
  stats[idx, , drop = FALSE]
}

first_existing_column <- function(df, candidates = character(), regex = NULL) {
  hit <- candidates[candidates %in% names(df)][1]
  if (!is.na(hit) && length(hit) > 0L) return(hit)
  if (!is.null(regex)) {
    hit <- grep(regex, names(df), value = TRUE, perl = TRUE)[1]
    if (!is.na(hit) && length(hit) > 0L) return(hit)
  }
  NA_character_
}

pluck_numeric <- function(row, candidates = character(), regex = NULL, default = NA_real_) {
  col <- first_existing_column(row, candidates, regex)
  if (is.na(col)) return(default)
  out <- suppressWarnings(as.numeric(row[[col]][1]))
  if (length(out) == 0L || is.na(out)) default else out
}

extract_rmanova_cse_stats <- function(stats) {
  row <- select_rmanova_cse_row(stats)
  if (nrow(row) == 0L) {
    return(list(
      row = row,
      f_stat = NA_real_,
      p_value = NA_real_,
      num_df = NA_integer_,
      den_df = NA_real_,
      effect_size = NA_real_
    ))
  }

  list(
    row = row,
    f_stat = pluck_numeric(row, c("F"), "(^|[._ ])F([._ ]|$)"),
    p_value = pluck_numeric(row, c("Pr(>F)", "Pr(>F[GG])", "Pr(>F[HF])", "p.value", "p"), "Pr\\(>F|p[._ ]?value|^p$"),
    num_df = as.integer(pluck_numeric(row, c("num Df", "num.Df", "df", "Df", "ndf"), "num[._ ]?Df|^df$|^Df$")),
    den_df = pluck_numeric(row, c("den Df", "den.Df", "ddf"), "den[._ ]?Df|ddf"),
    effect_size = pluck_numeric(row, c("ges", "pes", "eta2", "eta_sq", "eta.sq"), "ges|pes|eta")
  )
}
