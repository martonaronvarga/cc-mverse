# R/functions/logging.R
# ============================================================================
# CENTRALIZED LOGGING — worker-safe, idempotent, re-entrancy safe
# ============================================================================

.logging_env <- new.env(parent = emptyenv())
.logging_env$initialized <- FALSE
.logging_env$run_id <- NULL
.logging_env$in_layout <- FALSE

get_run_id <- function() {
  env_id <- Sys.getenv("PIPELINE_RUN_ID", unset = "")
  if (nzchar(env_id)) {
    return(env_id)
  }
  run_id <- format(Sys.time(), "%Y%m%d_%H%M%S")
  Sys.setenv(PIPELINE_RUN_ID = run_id)
  run_id
}

colorize_level <- function(level_str) {
  if (!requireNamespace("crayon", quietly = TRUE) || !crayon::has_color()) {
    return(level_str)
  }
  switch(toupper(level_str),
    "FATAL"   = crayon::combine_styles("bold", "red")(level_str),
    "ERROR"   = crayon::red(level_str),
    "WARN"    = crayon::yellow(level_str),
    "SUCCESS" = crayon::green(level_str),
    "INFO"    = crayon::cyan(level_str),
    "DEBUG"   = crayon::silver(level_str),
    "TRACE"   = crayon::silver(level_str),
    level_str
  )
}

layout_file_structured <- function() {
  function(level, msg, namespace = NA_character_, .logcall = NULL,
           .topcall = NULL, .topenv = NULL) {
    if (isTRUE(.logging_env$in_layout)) {
      return(paste0(
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ",
        attr(level, "level"), " | ", msg
      ))
    }
    .logging_env$in_layout <- TRUE
    on.exit(.logging_env$in_layout <- FALSE, add = TRUE)

    pid <- Sys.getpid()
    ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    ns_tag <- if (!is.na(namespace) && namespace != "global") {
      paste0(" [", namespace, "]")
    } else {
      ""
    }
    paste0(ts, " | ", attr(level, "level"), " | [PID:", pid, "]", ns_tag, " ", msg)
  }
}

layout_console_structured <- function() {
  function(level, msg, namespace = NA_character_, .logcall = NULL,
           .topcall = NULL, .topenv = NULL) {
    if (isTRUE(.logging_env$in_layout)) {
      return(paste0(
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ",
        attr(level, "level"), " | ", msg
      ))
    }
    .logging_env$in_layout <- TRUE
    on.exit(.logging_env$in_layout <- FALSE, add = TRUE)

    pid <- Sys.getpid()
    level_str <- attr(level, "level")
    ns_tag <- if (!is.na(namespace) && namespace != "global") {
      paste0(" [", namespace, "]")
    } else {
      ""
    }
    ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    paste0(ts, " | ", colorize_level(level_str), " | [PID:", pid, "]", ns_tag, " ", msg)
  }
}

setup_logging <- function(log_level = "info",
                          log_dir = NULL,
                          force = FALSE) {
  run_id <- get_run_id()
  if (.logging_env$initialized && identical(.logging_env$run_id, run_id) && !force) {
    return(invisible(run_id))
  }

  level <- switch(tolower(log_level),
    "debug" = logger::DEBUG,
    "info" = logger::INFO,
    "warn" = logger::WARN,
    "warning" = logger::WARN,
    "error" = logger::ERROR,
    logger::INFO
  )

  # --- Index 1: Console ---
  logger::log_threshold(level)
  logger::log_layout(layout_console_structured())
  logger::log_appender(logger::appender_console)
  logger::log_formatter(logger::formatter_glue)

  # --- Index 2: File ---
  if (!is.null(log_dir) && nzchar(log_dir)) {
    if (!dir.exists(log_dir)) {
      dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
    }
    log_file <- file.path(log_dir, paste0("pipeline_", run_id, ".log"))
    logger::log_threshold(level, index = 2)
    logger::log_layout(layout_file_structured(), index = 2)
    logger::log_appender(
      logger::appender_file(file = log_file, append = TRUE),
      index = 2
    )
    logger::log_formatter(logger::formatter_glue, index = 2)
  }

  .logging_env$initialized <- TRUE
  .logging_env$run_id <- run_id

  invisible(run_id)
}

#' Log with branch context. Safe for tryCatch handlers and messages with %.
#' Evaluates glue in the CALLER's frame, then passes a plain string to logger.
log_branch <- function(level, msg, branch_id) {
  formatted_msg <- tryCatch(
    as.character(glue::glue(msg, .envir = parent.frame())),
    error = function(glue_err) paste0(msg, " [glue error: ", glue_err$message, "]")
  )
  logger::log_level(
    level = level,
    logger::skip_formatter(formatted_msg),
    namespace = paste0("branch:", branch_id)
  )
}

#' Log a pipeline message. Safe for tryCatch handlers and messages with %.
log_pipeline <- function(level, msg) {
  formatted_msg <- tryCatch(
    as.character(glue::glue(msg, .envir = parent.frame())),
    error = function(glue_err) paste0(msg, " [glue error: ", glue_err$message, "]")
  )
  logger::log_level(level, logger::skip_formatter(formatted_msg))
}

format_verbose_log_value <- function(value, max_items = 40L) {
  if (is.null(value)) {
    return("NULL")
  }
  if (is.data.frame(value)) {
    return(sprintf("<data.frame: %d rows x %d columns>", nrow(value), ncol(value)))
  }
  if (is.list(value) && !is.atomic(value)) {
    value <- vapply(value, format_verbose_log_value, character(1), max_items = 8L)
  }

  value <- as.character(value)
  if (length(value) == 0L) {
    return("<empty>")
  }
  if (length(value) > max_items) {
    value <- c(value[seq_len(max_items)], sprintf("... (%d more)", length(value) - max_items))
  }
  paste(value, collapse = ", ")
}

post_model_verbose_log_file <- function(paths) {
  log_dir <- paths$logs
  if (is.null(log_dir) || !nzchar(log_dir)) {
    log_dir <- file.path(".", "logs")
  }
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  }
  file.path(log_dir, paste0("post_model_", get_run_id(), ".log"))
}

log_post_model_event <- function(paths, stage, event, details = list(), level = logger::INFO) {
  log_file <- post_model_verbose_log_file(paths)
  level_name <- attr(level, "level")
  if (is.null(level_name)) {
    level_name <- as.character(level)
  }

  header <- paste0(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    " | ", level_name,
    " | [PID:", Sys.getpid(), "] ",
    stage,
    " | ",
    event
  )
  detail_lines <- character()
  if (length(details) > 0L) {
    detail_names <- names(details)
    if (is.null(detail_names)) {
      detail_names <- paste0("detail_", seq_along(details))
    }
    detail_lines <- vapply(
      seq_along(details),
      function(i) paste0("  - ", detail_names[[i]], ": ", format_verbose_log_value(details[[i]])),
      character(1)
    )
  }

  cat(paste(c(header, detail_lines), collapse = "\n"), "\n", file = log_file, append = TRUE, sep = "")
  log_pipeline(level, "Post-model {stage}: {event}; verbose log: {log_file}")
  invisible(log_file)
}
