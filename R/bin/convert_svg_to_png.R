#!/usr/bin/env Rscript

usage <- function() {
  cat(
"Convert SVG plot outputs to PNG while preserving SVG dimensions.

Usage:
  Rscript R/bin/convert_svg_to_png.R [--input DIR] [--output DIR] [--dpi N] [--force]

Defaults:
  --input  outputs/analysis/figures
  --output same directory as each SVG
  --dpi    300  # used when SVG dimensions are in inches/cm/mm/pt

If --output is supplied, directory structure under --input is mirrored there.
", sep = "")
}

args <- commandArgs(trailingOnly = TRUE)
opts <- list(input = "outputs/analysis/figures", output = NA_character_, dpi = 300, force = FALSE)
i <- 1L
while (i <= length(args)) {
  arg <- args[[i]]
  if (arg %in% c("-h", "--help")) {
    usage(); quit(status = 0L)
  } else if (arg == "--input") {
    i <- i + 1L; opts$input <- args[[i]]
  } else if (arg == "--output") {
    i <- i + 1L; opts$output <- args[[i]]
  } else if (arg == "--dpi") {
    i <- i + 1L; opts$dpi <- as.numeric(args[[i]])
  } else if (arg == "--force") {
    opts$force <- TRUE
  } else {
    stop("Unknown argument: ", arg, call. = FALSE)
  }
  i <- i + 1L
}

if (!dir.exists(opts$input)) stop("Input directory not found: ", opts$input, call. = FALSE)
if (!is.finite(opts$dpi) || opts$dpi <= 0) stop("--dpi must be positive", call. = FALSE)
if (!requireNamespace("rsvg", quietly = TRUE)) {
  stop("Package 'rsvg' is required. Install it or use the shell converter fallback.", call. = FALSE)
}

parse_attrs <- function(svg) {
  txt <- paste(readLines(svg, warn = FALSE, n = 80L), collapse = "\n")
  tag <- regmatches(txt, regexpr("<svg[^>]*>", txt, perl = TRUE))
  if (!length(tag) || is.na(tag)) return(list(width = NA_character_, height = NA_character_, viewBox = NA_character_))
  get_attr <- function(name) {
    pattern <- paste0(name, "\\s*=\\s*['\\\"]([^'\\\"]+)['\\\"]")
    m <- regexec(pattern, tag, perl = TRUE)
    hit <- regmatches(tag, m)[[1]]
    if (length(hit) >= 2L) hit[[2]] else NA_character_
  }
  list(width = get_attr("width"), height = get_attr("height"), viewBox = get_attr("viewBox"))
}

unit_to_px <- function(x, dpi) {
  if (is.na(x) || !nzchar(x)) return(NA_real_)
  x <- trimws(x)
  value <- suppressWarnings(as.numeric(sub("^([0-9.]+).*", "\\1", x)))
  if (!is.finite(value)) return(NA_real_)
  unit <- trimws(sub("^[0-9.]+", "", x))
  if (!nzchar(unit) || unit == "px") return(value)
  switch(unit,
    "in" = value * dpi,
    "cm" = value / 2.54 * dpi,
    "mm" = value / 25.4 * dpi,
    "pt" = value / 72 * dpi,
    "pc" = value / 6 * dpi,
    value
  )
}

svg_dimensions <- function(svg, dpi) {
  attrs <- parse_attrs(svg)
  width <- unit_to_px(attrs$width, dpi)
  height <- unit_to_px(attrs$height, dpi)
  if ((!is.finite(width) || !is.finite(height)) && !is.na(attrs$viewBox)) {
    nums <- suppressWarnings(as.numeric(strsplit(gsub(",", " ", attrs$viewBox), "\\s+")[[1]]))
    nums <- nums[is.finite(nums)]
    if (length(nums) == 4L) {
      width <- if (is.finite(width)) width else nums[[3]]
      height <- if (is.finite(height)) height else nums[[4]]
    }
  }
  list(width = if (is.finite(width)) as.integer(round(width)) else NULL,
       height = if (is.finite(height)) as.integer(round(height)) else NULL)
}

input_root <- normalizePath(opts$input, winslash = "/", mustWork = TRUE)
svgs <- list.files(input_root, pattern = "[.]svg$", recursive = TRUE, full.names = TRUE)
if (!length(svgs)) {
  cat("No SVG files found under ", input_root, "\n", sep = "")
  quit(status = 0L)
}

converted <- 0L
skipped <- 0L
for (svg in svgs) {
  rel <- substring(normalizePath(svg, winslash = "/", mustWork = TRUE), nchar(input_root) + 2L)
  if (is.na(opts$output)) {
    png <- sub("[.]svg$", ".png", svg, ignore.case = TRUE)
  } else {
    png <- file.path(opts$output, sub("[.]svg$", ".png", rel, ignore.case = TRUE))
  }
  dir.create(dirname(png), recursive = TRUE, showWarnings = FALSE)

  if (file.exists(png) && !opts$force && file.info(png)$mtime >= file.info(svg)$mtime) {
    skipped <- skipped + 1L
    next
  }

  dims <- svg_dimensions(svg, opts$dpi)
  rsvg::rsvg_png(svg, file = png, width = dims$width, height = dims$height)
  converted <- converted + 1L
  size <- paste0(if (is.null(dims$width)) "auto" else dims$width, "x", if (is.null(dims$height)) "auto" else dims$height)
  cat("Wrote ", png, " (", size, ")\n", sep = "")
}

cat("Converted ", converted, " SVG file(s); skipped ", skipped, " up-to-date PNG file(s).\n", sep = "")
