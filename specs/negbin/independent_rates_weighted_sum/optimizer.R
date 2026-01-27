# ============================================================
# optimizer.R — Optimizer factory
# ============================================================

make_optimizer <- function(config) {
  cfg <- config$optimizer
  if (is.null(cfg)) {
    stop("Config must contain an 'optimizer' section.", call. = FALSE)
  }

  localsolver <- cfg$localsolver
  control <- cfg$control
  localtol <- cfg$localtol
  max_retries <- cfg$max_retries
  drop_mult <- cfg$drop_mult

  # --- Minimal validation
  if (is.null(localsolver)) {
    stop("optimizer$localsolver must be specified.", call. = FALSE)
  }
  if (is.null(control) || !is.list(control)) {
    stop("optimizer$control must be a list.", call. = FALSE)
  }
  if (is.null(localtol)) {
    stop("optimizer$localtol must be specified.", call. = FALSE)
  }
  if (is.null(max_retries)) {
    stop("optimizer$max_retries must be specified.", call. = FALSE)
  }
  if (is.null(drop_mult)) {
    stop("optimizer$drop_mult must be specified.", call. = FALSE)
  }

  optimizer_spec(
    localsolver = localsolver,
    control = control,
    localtol = localtol,
    max_retries = max_retries,
    drop_mult = drop_mult,
    name = "Optimizer spec"
  )
}
