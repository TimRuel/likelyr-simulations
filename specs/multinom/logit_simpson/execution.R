# ============================================================
# execution.R — Execution factory
# ============================================================

# make_execution <- function(config) {
#   cfg <- config$execution
#   if (is.null(cfg)) {
#     stop("Config must contain an 'execution' section.", call. = FALSE)
#   }
#
#   parallel_spec(
#     num_workers   = cfg$num_workers,
#     packages      = cfg$packages,
#     name          = "Parallel execution"
#   )
# }

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
