# R/functions/location_operating_characteristics.R - isolated smoke simulator for location CSE

source_design_integrity_once <- function() {
  if (!exists("read_indexed_design", mode = "function")) {
    local_path <- file.path("functions", "design_integrity.R")
    if (!file.exists(local_path)) {
      local_path <- file.path("R", "functions", "design_integrity.R")
    }
    source(local_path)
  }
}

prepare_location_design <- function(input_path, studies = c("flanker"), max_participants_per_study = 12) {
  source_design_integrity_once()
  df <- read_indexed_design(input_path)
  df <- canonical_previous_fields(df)

  df$rt_num <- suppressWarnings(as.numeric(df$rt))
  df$cong_num <- suppressWarnings(as.numeric(df$cong))
  df$prev_cong_num <- suppressWarnings(as.numeric(df$canonical_prev_cong))
  df$block_num <- suppressWarnings(as.numeric(df$block_index))
  df$trial_num <- suppressWarnings(as.numeric(df$trial_index))
  df$correct_bool <- coerce_logicalish(df$correct)
  df$prev_correct_bool <- coerce_logicalish(df$canonical_prev_correct)

  keep <- df$study %in% studies &
    is.finite(df$rt_num) & df$rt_num > 0 &
    !is.na(df$cong_num) & !is.na(df$prev_cong_num)
  df <- df[keep, , drop = FALSE]

  chosen <- unlist(lapply(split(df$participant_id, df$study), function(ids) {
    unique(ids)[seq_len(min(length(unique(ids)), max_participants_per_study))]
  }), use.names = FALSE)
  df <- df[df$participant_id %in% chosen, , drop = FALSE]
  df <- df[order(df$study, df$participant_id, df$block_num, df$trial_num, df$source_row), , drop = FALSE]

  df$cong_c <- ifelse(df$cong_num == 1, 0.5, -0.5)
  df$prev_cong_c <- ifelse(df$prev_cong_num == 1, 0.5, -0.5)
  df$trial_scaled <- ave(df$trial_num, df$participant_id, FUN = function(x) {
    if (length(unique(x)) <= 1 || stats::sd(x, na.rm = TRUE) == 0) return(rep(0, length(x)))
    as.numeric(scale(x))
  })
  df
}

estimate_location_nuisance <- function(df) {
  rhs <- "cong_c + prev_cong_c + trial_scaled + factor(participant_id)"
  if (length(unique(df$study)) > 1L) {
    rhs <- paste(rhs, "+ factor(study)")
  }
  fit <- stats::lm(stats::as.formula(paste("log(rt_num) ~", rhs)), data = df)
  fitted <- stats::fitted(fit)
  residual <- stats::residuals(fit)

  residual_sd_by_participant <- tapply(residual, df$participant_id, stats::sd, na.rm = TRUE)
  residual_sd_by_participant[!is.finite(residual_sd_by_participant) | residual_sd_by_participant <= 0] <-
    stats::sd(residual, na.rm = TRUE)

  list(
    fit = fit,
    fitted_log_rt = fitted,
    residual_sd_by_participant = residual_sd_by_participant,
    global_residual_sd = stats::sd(residual, na.rm = TRUE)
  )
}

simulate_location_dataset <- function(df, nuisance, interaction_log = 0, seed = 1) {
  set.seed(seed)
  pid_sd <- nuisance$residual_sd_by_participant[as.character(df$participant_id)]
  pid_sd[!is.finite(pid_sd) | pid_sd <= 0] <- nuisance$global_residual_sd

  eps <- stats::rnorm(nrow(df), mean = 0, sd = pid_sd)
  log_rt <- nuisance$fitted_log_rt + interaction_log * df$cong_c * df$prev_cong_c + eps

  out <- df[, c("study", "participant_id", "block_index", "trial_index", "cong_num", "prev_cong_num")]
  names(out)[names(out) == "cong_num"] <- "cong"
  names(out)[names(out) == "prev_cong_num"] <- "prev_cong"
  out$rt <- exp(log_rt)
  out$truth_interaction_log <- interaction_log
  out
}

participant_cell_cse <- function(df, outcome = "rt") {
  split(df, df$participant_id) |>
    lapply(function(x) {
      means <- tapply(x[[outcome]], list(x$cong, x$prev_cong), mean)
      if (!all(c("-1", "1") %in% rownames(means)) || !all(c("-1", "1") %in% colnames(means))) {
        return(NA_real_)
      }
      (means["1", "1"] - means["1", "-1"]) - (means["-1", "1"] - means["-1", "-1"])
    }) |>
    unlist(use.names = FALSE)
}

cell_mean_cse <- function(df, outcome = "rt") {
  mean(participant_cell_cse(df, outcome = outcome), na.rm = TRUE)
}

fit_smoke_models <- function(df) {
  df$cong_c <- ifelse(df$cong == 1, 0.5, -0.5)
  df$prev_cong_c <- ifelse(df$prev_cong == 1, 0.5, -0.5)

  lm_fit <- stats::lm(log(rt) ~ cong_c * prev_cong_c + factor(participant_id), data = df)
  lm_coef <- summary(lm_fit)$coefficients
  term <- "cong_c:prev_cong_c"

  participant_cse <- participant_cell_cse(df, outcome = "rt")
  complete <- all(is.finite(participant_cse)) && length(participant_cse) > 1L
  if (complete) {
    tt <- stats::t.test(participant_cse)
    rmanova_p <- unname(tt$p.value)
    rmanova_estimate <- unname(mean(participant_cse, na.rm = TRUE))
  } else {
    rmanova_p <- NA_real_
    rmanova_estimate <- NA_real_
  }

  df$log_rt <- log(df$rt)
  participant_log_cse <- participant_cell_cse(df, outcome = "log_rt")
  complete_log <- all(is.finite(participant_log_cse)) && length(participant_log_cse) > 1L
  if (complete_log) {
    tt_log <- stats::t.test(participant_log_cse)
    rmanova_log_p <- unname(tt_log$p.value)
    rmanova_log_estimate <- unname(mean(participant_log_cse, na.rm = TRUE))
  } else {
    rmanova_log_p <- NA_real_
    rmanova_log_estimate <- NA_real_
  }

  data.frame(
    model = c("participant_fixed_lm", "cell_mean_t_test", "cell_mean_t_test_log"),
    estimate = c(unname(lm_coef[term, "Estimate"]), rmanova_estimate, rmanova_log_estimate),
    p_value = c(unname(lm_coef[term, "Pr(>|t|)"]), rmanova_p, rmanova_log_p),
    error = c(FALSE, !complete, !complete_log),
    stringsAsFactors = FALSE
  )
}

run_location_smoke_simulation <- function(
  input_path = "all_indexed.csv",
  output_dir = file.path("outputs", "analysis"),
  studies = c("flanker"),
  max_participants_per_study = 12,
  n_replicates = 5,
  alternative_interaction_log = c(0.04),
  seed = 20240522
) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  design <- prepare_location_design(input_path, studies, max_participants_per_study)
  nuisance <- estimate_location_nuisance(design)

  rows <- list()
  idx <- 1L
  scenarios <- data.frame(
    condition = c("null_location", rep("present_location", length(alternative_interaction_log))),
    interaction = c(0, alternative_interaction_log),
    stringsAsFactors = FALSE
  )
  scenarios <- unique(scenarios)

  for (scenario_idx in seq_len(nrow(scenarios))) {
    condition <- scenarios$condition[[scenario_idx]]
    interaction <- scenarios$interaction[[scenario_idx]]
    for (replicate_id in seq_len(n_replicates)) {
      sim <- simulate_location_dataset(
        design,
        nuisance,
        interaction_log = interaction,
        seed = seed + replicate_id + scenario_idx * 100000L
      )
      sim$log_rt <- log(sim$rt)
      residual_cse <- cell_mean_cse(sim, outcome = "rt")
      residual_log_cse <- cell_mean_cse(sim, outcome = "log_rt")
      fits <- fit_smoke_models(sim)
      fits$condition <- condition
      fits$replicate_id <- replicate_id
      fits$truth_interaction_log <- interaction
      fits$residual_cell_mean_cse <- residual_cse
      fits$residual_log_cell_mean_cse <- residual_log_cse
      fits$n_rows <- nrow(sim)
      fits$n_participants <- length(unique(sim$participant_id))
      rows[[idx]] <- fits
      idx <- idx + 1L
    }
  }

  out <- do.call(rbind, rows)
  out$is_significant <- !out$error & !is.na(out$p_value) & out$p_value < 0.05
  output_path <- file.path(output_dir, "smoke_operating_characteristics.csv")
  write.csv(out, output_path, row.names = FALSE)
  invisible(output_path)
}
