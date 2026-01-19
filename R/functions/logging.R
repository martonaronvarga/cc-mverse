# R/functions/logging.R
# ============================================================================
# LOGGING OPTIONS
# ============================================================================

#' Configure structured logging
#'
#' @param config Configuration list
#'
configure_logging <- function(config, paths) {
  # Console appender
  logger::log_appender(
    logger::appender_console
  )
  logger::log_layout(layout = logger::layout_glue_colors)

  # File appender (append to existing)
  logger::log_appender(
    logger::appender_file(
      file = file.path(paths$logs, glue::glue("pipeline_{format(Sys.time(), '%Y%m%d_%H%M%S')}.log")),
      append = TRUE
    ),
    index = 2
  )

  # Set threshold
  level <- switch(config$log_level,
    debug = logger::DEBUG,
    info = logger::INFO,
    warn = logger::WARN,
    error = logger::ERROR,
    logger::INFO
  )

  logger::log_threshold(level)

  logger::log_info("Logging configured: level={config$log_level}")

  invisible(NULL)
}
