# ======================================================================
# Solver Specification (No Effects Multinomial, Logit Parameterization)
# ======================================================================

make_solver <- function(config) {
  cfg <- config$solver

  if (is.null(cfg)) {
    stop("Config must contain a 'solver' section.", call. = FALSE)
  }

  likelyr::solver_spec(
    localsolver = cfg$localsolver %||% "SLSQP",
    control = cfg$control %||% list(),
    localtol = cfg$localtol %||% 1e-6,
    max_retries = cfg$max_retries %||% 10L,
    name = "Auglag() controls"
  )
}
