# ============================================================
# execution.R — Parallel execution factory
# ============================================================

make_execution <- function(config) {
  cfg <- config$execution
  if (is.null(cfg)) {
    stop("Config must contain an 'execution' section.", call. = FALSE)
  }

  parallel_spec(
    num_workers = cfg$num_workers,
    chunk_size = cfg$chunk_size,
    packages = cfg$packages,
    seed = cfg$seed,
    name = "Parallel execution"
  )
}
