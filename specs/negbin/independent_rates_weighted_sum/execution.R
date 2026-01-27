# ============================================================
# execution.R — Parallel execution factory
# ============================================================

make_execution <- function(config) {
  cfg <- config$execution
  if (is.null(cfg)) {
    stop("Config must contain an 'execution' section.", call. = FALSE)
  }

  seed <- cfg$seed
  num_workers <- cfg$num_workers
  chunk_size <- cfg$chunk_size
  packages <- cfg$packages

  # --- Minimal validation
  if (is.null(num_workers)) {
    stop("execution$num_workers must be specified.", call. = FALSE)
  }
  if (is.null(chunk_size)) {
    stop("execution$chunk_size must be specified.", call. = FALSE)
  }
  if (is.null(packages)) {
    stop("execution$packages must be specified.", call. = FALSE)
  }

  parallel_spec(
    num_workers = num_workers,
    chunk_size = chunk_size,
    packages = packages,
    seed = seed,
    name = "Parallel execution"
  )
}
