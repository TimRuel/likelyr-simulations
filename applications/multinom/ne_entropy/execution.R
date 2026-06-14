# ============================================================
# execution.R — Execution factory
# ============================================================

make_execution <- function(config) {
  cfg <- config$execution

  if (is.null(cfg)) {
    stop("Config must contain an 'execution' section.", call. = FALSE)
  }

  serial_spec(
    packages = cfg$packages,
    name = "Serial execution"
  )
}
