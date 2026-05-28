# R/functions/dependency_report.R - dependency/environment inventory

safe_system_output <- function(command, args = character()) {
  out <- tryCatch(
    system2(command, args = args, stdout = TRUE, stderr = TRUE),
    error = function(e) paste("ERROR:", conditionMessage(e))
  )
  paste(out, collapse = "\n")
}

installed_pkg_version <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) return(NA_character_)
  as.character(utils::packageVersion(pkg))
}

build_dependency_report <- function(core_packages = c(
  "targets", "tarchetypes", "dplyr", "tidyr", "purrr", "readr", "arrow",
  "lme4", "afex", "broom", "broom.mixed", "ggplot2", "logger", "yaml"
)) {
  r_packages <- data.frame(
    package = core_packages,
    installed = vapply(core_packages, requireNamespace, logical(1), quietly = TRUE),
    version = vapply(core_packages, installed_pkg_version, character(1)),
    stringsAsFactors = FALSE
  )

  tools <- data.frame(
    tool = c("R", "rustc", "cargo"),
    version = c(
      paste(R.version$major, R.version$minor, sep = "."),
      safe_system_output("rustc", "--version"),
      safe_system_output("cargo", "--version")
    ),
    stringsAsFactors = FALSE
  )

  cargo_lock <- file.exists(file.path("R", "rust", "Cargo.lock")) || file.exists(file.path("rust", "Cargo.lock"))
  cargo_toml <- if (file.exists(file.path("R", "rust", "Cargo.toml"))) {
    file.path("R", "rust", "Cargo.toml")
  } else if (file.exists(file.path("rust", "Cargo.toml"))) {
    file.path("rust", "Cargo.toml")
  } else {
    NA_character_
  }

  rust <- data.frame(
    key = c("cargo_toml", "cargo_lock_present", "cargo_metadata_available"),
    value = c(
      cargo_toml,
      as.character(cargo_lock),
      if (!is.na(cargo_toml)) safe_system_output("cargo", c("metadata", "--manifest-path", cargo_toml, "--no-deps", "--format-version", "1")) else NA_character_
    ),
    stringsAsFactors = FALSE
  )

  list(r_packages = r_packages, tools = tools, rust = rust)
}

write_dependency_report <- function(output_dir = file.path("outputs", "analysis")) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  report <- build_dependency_report()
  r_path <- file.path(output_dir, "dependency_report_r_packages.csv")
  tools_path <- file.path(output_dir, "dependency_report_tools.csv")
  rust_path <- file.path(output_dir, "dependency_report_rust.csv")
  write.csv(report$r_packages, r_path, row.names = FALSE)
  write.csv(report$tools, tools_path, row.names = FALSE)
  write.csv(report$rust, rust_path, row.names = FALSE)
  invisible(list(r_packages = r_path, tools = tools_path, rust = rust_path))
}
