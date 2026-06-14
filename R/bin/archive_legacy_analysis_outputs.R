#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

apply <- "--apply" %in% args
args <- setdiff(args, "--apply")

root <- if (length(args) >= 1L) {
  args[[1L]]
} else {
  file.path("outputs", "analysis")
}

root <- normalizePath(root, mustWork = TRUE)

archive_root <- file.path(root, "archive")
stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
archive_dir <- file.path(archive_root, paste0("flat-legacy-", stamp))

entries <- list.files(
  root,
  all.files = TRUE,
  no.. = TRUE,
  full.names = TRUE
)

base <- basename(entries)

timestamped_csv <- grepl("^.+_[0-9]{8}_[0-9]{6}[.]csv$", base)
legacy_dirs <- base %in% c("figures", "nullification_operating_characteristics") & dir.exists(entries)
protected <- base %in% c("runs", "latest", "archive", "diagnostics_cache")

move <- entries[(timestamped_csv | legacy_dirs) & !protected]

cat("Analysis root: ", root, "\n", sep = "")
cat("Archive dir:  ", archive_dir, "\n", sep = "")
cat("Mode:         ", if (apply) "APPLY" else "DRY-RUN", "\n\n", sep = "")

if (!length(move)) {
  cat("No legacy analysis outputs found.\n")
  quit(status = 0L)
}

for (p in move) {
  cat(if (dir.exists(p)) "DIR  " else "FILE ", basename(p), "\n", sep = "")
}

if (!apply) {
  cat("\nDry run only. Re-run with --apply to move these outputs.\n")
  quit(status = 0L)
}

dir.create(archive_dir, recursive = TRUE, showWarnings = FALSE)

manifest <- data.frame(
  original = move,
  archived = file.path(archive_dir, basename(move)),
  is_dir = dir.exists(move),
  size_bytes = ifelse(dir.exists(move), NA_real_, file.info(move)$size),
  stringsAsFactors = FALSE
)

for (i in seq_along(move)) {
  src <- move[[i]]
  dst <- file.path(archive_dir, basename(src))

  if (file.exists(dst)) {
    stop("Archive destination already exists: ", dst)
  }

  ok <- file.rename(src, dst)

  if (!ok) {
    stop("Failed to move: ", src, " -> ", dst)
  }
}

utils::write.csv(
  manifest,
  file.path(archive_dir, "archive_manifest.csv"),
  row.names = FALSE
)

cat("\nArchived ", length(move), " legacy outputs.\n", sep = "")
