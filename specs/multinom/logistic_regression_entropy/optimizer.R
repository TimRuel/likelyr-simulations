# ============================================================
# optimizer.R — Optimizer factory
# ============================================================

make_optimizer <- function(config) {
  cfg <- config$optimizer
  if (is.null(cfg)) {
    stop("Config must contain an 'optimizer' section.", call. = FALSE)
  }

  optimizer_spec(
    localsolver = cfg$localsolver,
    control = cfg$control,
    localtol = cfg$localtol,
    max_retries = cfg$max_retries,
    drop_mult = cfg$drop_mult,
    name = "Optimizer spec"
  )
}
