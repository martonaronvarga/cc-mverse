# R/functions/debug_helper.R
# Helpers to localize and debug targets pipeline errors quickly.

inspect_targets_options <- function() {
  if (!requireNamespace("targets", quietly = TRUE)) {
    stop("Package 'targets' is not available.")
  }

  # Documented option names to query explicitly
  option_names <- c(
    "packages",
    "envir",
    "format",
    "repository",
    "iteration",
    "error",
    "cue",
    "memory",
    "garbage_collection",
    "seed",
    "controller",
    "deployment",
    "priority",
    "storage",
    "retrieval",
    "resources"
  )

  cat("targets options:\n")
  for (nm in option_names) {
    val <- try(targets::tar_option_get(nm), silent = TRUE)
    if (inherits(val, "try-error")) {
      cat(sprintf("  %-20s : <error>\n", nm))
    } else {
      # Compact representation
      captured <- capture.output(str(val, max.level = 1))
      cat(sprintf("  %-20s : %s\n", nm, if (length(captured)) captured[1] else "<NULL>"))
    }
  }
  as.list(captured)
}

#' Run targets in the current session with verbose diagnostics
#'
#' @param script Path to the targets plan script. Default "R/_targets.R".
#' @param reporter Reporter to use, default "verbose".
#' @param trace Enable internal targets trace logs (TAR_TRACE=1).
#' @param save_workspace Save workspace on error for failed targets.
#' @return Invisibly TRUE on success; otherwise prints diagnostics and returns FALSE.
debug_targets <- function(
    script = "R/_targets.R",
    reporter = "verbose",
    trace = TRUE,
    save_workspace = TRUE) {
  # Ensure targets is loaded
  if (!requireNamespace("targets", quietly = TRUE)) {
    stop("Package 'targets' not available.")
  }
  library(targets)

  # Point to the correct script
  try(targets::tar_config_set(script = script), silent = TRUE)

  # Internal tracing and workspace capture
  if (isTRUE(trace)) Sys.setenv(TAR_TRACE = "1")
  if (isTRUE(save_workspace)) targets::tar_option_set(workspace_on_error = TRUE)

  cat("=== Debugging targets ===\n")
  cat("script:", targets::tar_config_get("script"), "\n")
  cat("wd:", normalizePath(getwd()), "\n")

  # Try to load the plan and print basic diagnostics
  cat("\n--- Plan diagnostics ---\n")
  plan_ok <- TRUE
  try(
    {
      manifest <- targets::tar_manifest()
      print(utils::head(manifest, 10))
      cat("Targets in plan:", nrow(manifest), "\n")
    },
    silent = TRUE
  )

  # Options diagnostics
  cat("\n--- Options diagnostics ---\n")
  opts <- inspect_targets_options()
  # Common culprits: packages must be character; controller must be a crew controller
  pkgs <- opts$packages
  cat("packages_core typeof:", typeof(pkgs), " length:", length(pkgs), "\n")
  if (!is.null(pkgs)) print(utils::head(pkgs, 10))
  ctrl <- opts$controller
  cat("controller class:", paste(class(ctrl), collapse = " / "), "\n")
  cat("format:", opts$format, " repository:", opts$repository, " iteration:", opts$iteration, "\n")
  cat("deployment:", opts$deployment, " error:", opts$error, "\n")

  # Schema dir check (if paths object is available)
  if (exists("paths", inherits = TRUE)) {
    cat("\n--- Paths/schema diagnostics ---\n")
    cat("results dir:", paths$outputs_results, "\n")
    if (dir.exists(paths$outputs_results)) {
      schema_files <- list.files(paths$outputs_results, pattern = "\\.parquet$", full.names = TRUE)
      cat("parquet files found:", length(schema_files), "\n")
      if (length(schema_files)) print(utils::head(schema_files, 5))
    } else {
      cat("results dir does not exist\n")
    }
  }

  # Run tar_validate first to catch structural issues
  cat("\n--- tar_validate ---\n")
  val_ok <- TRUE
  tryCatch(
    {
      targets::tar_validate()
      cat("tar_validate: OK\n")
    },
    error = function(e) {
      val_ok <<- FALSE
      cat("tar_validate ERROR:", conditionMessage(e), "\n")
    }
  )

  cat("\n--- Executing tar_make (in-session, no callr) ---\n")
  res <- TRUE
  err <- NULL
  tryCatch(
    {
      targets::tar_make(reporter = reporter, callr_function = NULL)
      cat("tar_make: completed\n")
    },
    error = function(e) {
      res <<- FALSE
      err <<- e
      cat("\n*** tar_make ERROR ***\n")
      cat(conditionMessage(e), "\n")
      cat("\n--- traceback() ---\n")
      utils::traceback()
      if (requireNamespace("rlang", quietly = TRUE)) {
        cat("\n--- rlang::last_trace() ---\n")
        print(rlang::last_trace())
      }
      cat("\n--- Failed targets (if any) ---\n")
      if (exists("targets::tar_meta")) {
        meta <- try(targets::tar_meta(fields = c("name", "progress", "error"), complete_only = FALSE), silent = TRUE)
        if (!inherits(meta, "try-error")) {
          print(meta)
          failed <- subset(meta, !is.na(error) | progress == "errored")
          if (nrow(failed) > 0) {
            cat("\nWorkspace inspection hints:\n")
            for (nm in failed$name) {
              cat("  targets::tar_workspace(target = '", nm, "')\n", sep = "")
            }
          }
        }
      }
    }
  )

  cat("\n=== Done debugging ===\n")
  invisible(res)
}

#' Inspect a target's saved workspace after a failure
#'
#' @param name Target name (character scalar)
#' @return Loads the workspace into the current session for interactive inspection.
inspect_workspace <- function(name) {
  if (!requireNamespace("targets", quietly = TRUE)) {
    stop("Package 'targets' not available.")
  }
  if (!is.character(name) || length(name) != 1) {
    stop("name must be a character scalar (target name).")
  }
  cat("Loading workspace for target:", name, "\n")
  targets::tar_workspace(target = name)
}

#' Quick check for Arrow schema directory consistency
#'
#' @param dir Results directory path
#' @param schema Arrow schema object (default RESULTS_SCHEMA if available)
#' @return Invisibly TRUE if OK, FALSE otherwise
check_results_dir <- function(dir, schema = get0("RESULTS_SCHEMA", ifnotfound = NULL, inherits = TRUE)) {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' not available.")
  }
  if (!dir.exists(dir)) {
    cat("Directory does not exist:", dir, "\n")
    return(invisible(FALSE))
  }
  files <- list.files(dir, pattern = "\\.parquet$", full.names = TRUE)
  cat("Parquet files:", length(files), "\n")
  if (length(files) == 0) {
    cat("No parquet files present; schema anchoring file may be missing.\n")
    return(invisible(TRUE))
  }
  ok <- TRUE
  for (f in files) {
    cat("Reading:", f, "\n")
    tbl <- try(arrow::read_parquet(f), silent = TRUE)
    if (inherits(tbl, "try-error")) {
      cat("ERROR reading", f, ":", as.character(tbl), "\n")
      ok <- FALSE
      next
    }
    if (!is.null(schema)) {
      missing <- setdiff(schema$names, names(tbl))
      if (length(missing) > 0) {
        cat("Missing columns in", f, ":", paste(missing, collapse = ", "), "\n")
        ok <- FALSE
      }
    }
  }
  invisible(ok)
}
