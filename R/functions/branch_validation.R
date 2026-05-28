# R/functions/branch_validation.R - branch-generation invariants

validate_branch_table <- function(branches, config = NULL) {
  required <- c(
    "sample_size", "subsample_id", "transformation", "outlier", "model",
    "effect_condition", "strip_method", "branch_id", "data_id"
  )
  missing <- setdiff(required, names(branches))
  if (length(missing) > 0) {
    stop("Branch table missing columns: ", paste(missing, collapse = ", "))
  }

  failures <- list()
  add_failure <- function(name, value) {
    if (!isTRUE(value)) failures[[name]] <<- FALSE
  }

  add_failure(
    "present branches use strip_method none",
    !any(branches$effect_condition == "present" & branches$strip_method != "none")
  )
  add_failure(
    "null_both branches are absent",
    !any(branches$effect_condition == "null_both")
  )
  add_failure(
    "null_interaction branches use only supported strip methods",
    !any(branches$effect_condition == "null_interaction" & !branches$strip_method %in% c("shuffle", "additive_qmap", "additive_qmap_trial_bin", "local_mean_residual", "local_median_residual"))
  )
  add_failure("branch_id is unique", length(unique(branches$branch_id)) == nrow(branches))

  data_key <- paste(
    branches$sample_size, branches$subsample_id, branches$transformation,
    branches$outlier, branches$effect_condition, branches$strip_method,
    sep = "__"
  )
  expected_data_branches <- length(unique(data_key))
  add_failure("data_id count matches model-agnostic data key", length(unique(branches$data_id)) == expected_data_branches)

  if (!is.null(config)) {
    subsample_counts <- vapply(config$sample_sizes, function(ss) {
      if (abs(ss - 1.0) < 1e-6) 1L else as.integer(config$n_subsamples)
    }, integer(1))
    expected_subsample_rows <- sum(subsample_counts)
    expected_per_model_free_axes <- expected_subsample_rows *
      length(config$transformations) * length(config$outlier_methods)
    expected_rows <- expected_per_model_free_axes * length(config$models) *
      (1L + length(config$strip_methods))
    expected_data_rows <- expected_per_model_free_axes * (1L + length(config$strip_methods))

    add_failure("branch row count matches config axes", nrow(branches) == expected_rows)
    add_failure("data branch count matches config axes", length(unique(branches$data_id)) == expected_data_rows)
  }

  if (length(failures) > 0) {
    stop("Branch validation failed: ", paste(names(failures), collapse = "; "))
  }

  data.frame(
    n_branches = nrow(branches),
    n_data_branches = length(unique(branches$data_id)),
    n_models = length(unique(branches$model)),
    n_sample_size_levels = length(unique(branches$sample_size)),
    n_subsample_ids = length(unique(branches$subsample_id)),
    n_transformations = length(unique(branches$transformation)),
    n_outliers = length(unique(branches$outlier)),
    n_effect_conditions = length(unique(branches$effect_condition)),
    n_strip_methods = length(unique(branches$strip_method)),
    stringsAsFactors = FALSE
  )
}

write_branch_validation_summary <- function(branches, config = NULL, output_dir = file.path("outputs", "analysis")) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  summary <- validate_branch_table(branches, config)
  output_path <- file.path(output_dir, "branch_validation_summary.csv")
  write.csv(summary, output_path, row.names = FALSE)
  invisible(output_path)
}
