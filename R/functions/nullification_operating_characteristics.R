# R/functions/nullification_operating_characteristics.R
# Build empirical nullification operating-characteristic tables.

join_nullification_diagnostics <- function(results_df, diagnostics_df) {
  stopifnot(is.data.frame(results_df), is.data.frame(diagnostics_df))
  diag_cols <- c(
    "data_id", "preservation_pass", "preservation_warnings", "nullification_verdict",
    "mean_cse", "q050_cse", "max_abs_timebin_q050_cse"
  )
  available_diag_cols <- intersect(diag_cols, names(diagnostics_df))
  if (!"data_id" %in% available_diag_cols) {
    stop("diagnostics_df must contain data_id")
  }

  results_df %>%
    dplyr::left_join(
      diagnostics_df %>% dplyr::select(dplyr::all_of(available_diag_cols)),
      by = "data_id",
      suffix = c("", "_diagnostic")
    )
}

build_nullification_operating_characteristics <- function(results_df, diagnostics_df, alpha = 0.05) {
  source("R/functions/analysis.R")
  joined <- join_nullification_diagnostics(results_df, diagnostics_df)
  prepared <- prepare_analysis_df(joined)
  fpr_tables <- compute_fpr_tables(prepared, alpha = alpha)
  failure_aware <- compute_failure_aware_nullification_rates(prepared, alpha = alpha)

  list(
    prepared = prepared,
    fpr_coarse = fpr_tables$coarse,
    fpr_by_sample = fpr_tables$by_sample,
    fpr_by_outlier = fpr_tables$by_outlier,
    fpr_per_branch = fpr_tables$per_branch,
    failure_aware = failure_aware
  )
}

write_nullification_operating_characteristics <- function(results_csv, diagnostics_csv, output_dir, alpha = 0.05) {
  if (!file.exists(results_csv)) stop("Results CSV not found: ", results_csv)
  if (!file.exists(diagnostics_csv)) stop("Diagnostics CSV not found: ", diagnostics_csv)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  results_df <- readr::read_csv(results_csv, show_col_types = FALSE)
  diagnostics_df <- readr::read_csv(diagnostics_csv, show_col_types = FALSE)
  tables <- build_nullification_operating_characteristics(results_df, diagnostics_df, alpha = alpha)

  paths <- c(
    fpr_coarse = file.path(output_dir, "nullification_fpr_coarse.csv"),
    fpr_by_sample = file.path(output_dir, "nullification_fpr_by_sample.csv"),
    fpr_by_outlier = file.path(output_dir, "nullification_fpr_by_outlier.csv"),
    failure_aware = file.path(output_dir, "nullification_failure_aware_rates.csv")
  )
  readr::write_csv(tables$fpr_coarse, paths[["fpr_coarse"]])
  readr::write_csv(tables$fpr_by_sample, paths[["fpr_by_sample"]])
  readr::write_csv(tables$fpr_by_outlier, paths[["fpr_by_outlier"]])
  readr::write_csv(tables$failure_aware, paths[["failure_aware"]])
  invisible(paths)
}
