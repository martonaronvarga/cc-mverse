# R/functions/analysis_strategy_table.R - planned CSE analysis strategies

build_analysis_strategy_table <- function() {
  data.frame(
    strategy_id = c(
      "rmanova_cell_mean_raw",
      "rmanova_cell_mean_log",
      "trial_lm_participant_fe_raw",
      "trial_lm_participant_fe_log",
      "lmm_random_intercept_raw",
      "lmm_random_intercept_log",
      "lmm_maximal_raw",
      "lmm_maximal_log"
    ),
    model_family = c(
      "rmANOVA", "rmANOVA",
      "basic_lm", "basic_lm",
      "LMM", "LMM", "LMM", "LMM"
    ),
    outcome_scale = c("rt", "log_rt", "rt", "log_rt", "rt", "log_rt", "rt", "log_rt"),
    formula_full = c(
      "cell_mean ~ cong * prev_cong + Error(participant_id/(cong*prev_cong))",
      "cell_mean_log ~ cong * prev_cong + Error(participant_id/(cong*prev_cong))",
      "rt ~ cong * prev_cong + factor(participant_id)",
      "log(rt) ~ cong * prev_cong + factor(participant_id)",
      "rt ~ cong * prev_cong + (1 | participant_id)",
      "log(rt) ~ cong * prev_cong + (1 | participant_id)",
      "rt ~ cong * prev_cong + (cong * prev_cong | participant_id)",
      "log(rt) ~ cong * prev_cong + (cong * prev_cong | participant_id)"
    ),
    formula_null = c(
      "cell_mean ~ cong + prev_cong + Error(participant_id/(cong*prev_cong))",
      "cell_mean_log ~ cong + prev_cong + Error(participant_id/(cong*prev_cong))",
      "rt ~ cong + prev_cong + factor(participant_id)",
      "log(rt) ~ cong + prev_cong + factor(participant_id)",
      "rt ~ cong + prev_cong + (1 | participant_id)",
      "log(rt) ~ cong + prev_cong + (1 | participant_id)",
      "rt ~ cong + prev_cong + (cong + prev_cong | participant_id)",
      "log(rt) ~ cong + prev_cong + (cong + prev_cong | participant_id)"
    ),
    cse_extraction_rule = c(
      "Named rmANOVA effect cong:prev_cong; never row position",
      "Named rmANOVA effect cong:prev_cong; never row position",
      "Exact coefficient cong:prev_cong or prev_cong:cong",
      "Exact coefficient cong:prev_cong or prev_cong:cong",
      "Exact fixed-effect coefficient cong:prev_cong or prev_cong:cong",
      "Exact fixed-effect coefficient cong:prev_cong or prev_cong:cong",
      "Exact fixed-effect coefficient cong:prev_cong or prev_cong:cong",
      "Exact fixed-effect coefficient cong:prev_cong or prev_cong:cong"
    ),
    denominator_rule = c(
      rep("All planned fits for unconditional FPR/TPR; usable complete-cell fits for conditional sensitivity", 2),
      rep("All planned fits for unconditional FPR/TPR; non-aliased finite coefficient fits for conditional sensitivity", 2),
      rep("All planned fits for unconditional FPR/TPR; converged nonsingular primary fits for conditional sensitivity", 4)
    ),
    failure_modes = c(
      "missing participant cell; too few complete participants; named effect absent",
      "missing participant cell; nonpositive RT before log; too few complete participants; named effect absent",
      "nonfinite RT; aliased participant FE or interaction coefficient",
      "nonpositive RT before log; aliased participant FE or interaction coefficient",
      "nonfinite RT; non-convergence; singularity; extraction failure",
      "nonpositive RT before log; non-convergence; singularity; extraction failure",
      "nonfinite RT; non-convergence; singularity; rank-deficient random effects; extraction failure",
      "nonpositive RT before log; non-convergence; singularity; rank-deficient random effects; extraction failure"
    ),
    primary = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
}

write_analysis_strategy_table <- function(output_path = file.path("outputs", "analysis", "analysis_strategy_table.csv")) {
  dir <- dirname(output_path)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  table <- build_analysis_strategy_table()
  write.csv(table, output_path, row.names = FALSE)
  invisible(output_path)
}
