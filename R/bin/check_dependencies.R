#!/usr/bin/env Rscript

function_path <- file.path("functions", "dependency_report.R")
if (!file.exists(function_path)) {
  function_path <- file.path("R", "functions", "dependency_report.R")
}
source(function_path)

report <- build_dependency_report()
errors <- character()

missing_packages <- report$r_packages$package[!report$r_packages$installed]
if (length(missing_packages) > 0) {
  errors <- c(errors, paste0(
    "Missing R packages: ", paste(missing_packages, collapse = ", "),
    ". Install them in the project environment before running the audit; do not install at runtime."
  ))
}

rustc_version <- report$tools$version[report$tools$tool == "rustc"]
cargo_version <- report$tools$version[report$tools$tool == "cargo"]
if (grepl("^ERROR:", rustc_version)) {
  errors <- c(errors, "rustc is unavailable; install/use the pinned Rust toolchain before compiling Rust processing code.")
}
if (grepl("^ERROR:", cargo_version)) {
  errors <- c(errors, "cargo is unavailable; install/use the pinned Rust toolchain before compiling Rust processing code.")
}

cargo_lock <- report$rust$value[report$rust$key == "cargo_lock_present"]
if (!identical(cargo_lock, "TRUE")) {
  errors <- c(errors, "R/rust/Cargo.lock is missing; lock Rust dependencies before final reproducible runs.")
}

metadata <- report$rust$value[report$rust$key == "cargo_metadata_available"]
if (length(metadata) == 0 || grepl("^ERROR:", metadata)) {
  errors <- c(errors, "cargo metadata failed; check R/rust/Cargo.toml and Rust dependency resolution.")
}

if (length(errors) > 0) {
  cat("Dependency check failed:\n", paste0("- ", errors, collapse = "\n"), "\n", sep = "")
  quit(status = 1)
}

cat("Dependency check passed: core R packages, Rust tools, Cargo lock, and Cargo metadata are available.\n")
