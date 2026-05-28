#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(ggplot2))

cat("R version:", R.version.string, "\n")
cat("DISPLAY=", Sys.getenv("DISPLAY"), "\n", sep = "")
cat("bitmapType=", getOption("bitmapType"), "\n", sep = "")
print(capabilities())

packages <- c("ggplot2", "ragg", "svglite", "Cairo", "systemfonts", "textshaping")
cat("\nPackages:\n")
for (p in packages) cat(p, "=", requireNamespace(p, quietly = TRUE), "\n", sep = "")

probe_dir <- tempfile("tdk_graphics_probe_")
dir.create(probe_dir)
p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + labs(title = "HPC graphics probe")

try_device <- function(label, file, expr) {
  cat("\n== ", label, " ==\n", sep = "")
  ok <- tryCatch({
    force(expr)
    cat("OK: ", file, " exists=", file.exists(file), " size=", if (file.exists(file)) file.info(file)$size else NA, "\n", sep = "")
    TRUE
  }, error = function(e) {
    cat("FAILED: ", conditionMessage(e), "\n", sep = "")
    FALSE
  })
  invisible(ok)
}

try_device("base pdf", file.path(probe_dir, "probe.pdf"), {
  grDevices::pdf(file.path(probe_dir, "probe.pdf"), width = 6, height = 4)
  print(p)
  grDevices::dev.off()
})

try_device("base png", file.path(probe_dir, "probe_base.png"), {
  grDevices::png(file.path(probe_dir, "probe_base.png"), width = 900, height = 600, res = 150)
  print(p)
  grDevices::dev.off()
})

if (requireNamespace("ragg", quietly = TRUE)) {
  try_device("ragg png", file.path(probe_dir, "probe_ragg.png"), {
    ragg::agg_png(file.path(probe_dir, "probe_ragg.png"), width = 900, height = 600, res = 150)
    print(p)
    grDevices::dev.off()
  })
}

if (requireNamespace("svglite", quietly = TRUE)) {
  try_device("svglite svg", file.path(probe_dir, "probe.svg"), {
    svglite::svglite(file.path(probe_dir, "probe.svg"), width = 6, height = 4)
    print(p)
    grDevices::dev.off()
  })
}

cat("\nProbe directory: ", probe_dir, "\n", sep = "")
