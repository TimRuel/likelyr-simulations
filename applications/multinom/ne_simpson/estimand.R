# ======================================================================
# Estimand Specification — Simpson's Index
#   ψ(η) = D(θ(η)) = Σ p_j^2
# ======================================================================

psi_fn <- function(param, data = NULL) {
  p <- softmax_from_eta(param)
  sum(p^2)
}

psi_jac <- function(param, data = NULL) {
  p <- softmax_from_eta(param)
  D <- sum(p^2)
  2 * p[-length(p)] * (p[-length(p)] - D)
}

make_estimand <- function(config, parameter = NULL, ...) {
  cfg <- config$estimand

  if (is.null(cfg)) {
    stop("Config must contain an 'estimand' section.", call. = FALSE)
  }

  J <- parameter$J

  if (is.null(J)) {
    stop(
      "J could not be determined: supply parameter.J in config, ",
      "set parameter.mode = 'manual' with a theta_0 vector, or ",
      "pass the parameter spec to make_estimand().",
      call. = FALSE
    )
  }

  # psi_lower = 1/J is computed here, not read from config — with J now
  # fixed at 30 (parameter.R, 2026-08-03) this is a constant 1/30 for
  # every site. No other estimand-level config keys are consumed:
  # increment/confidence_levels/cutoff_buffer now live under traversal:
  # and gamma/uniroot_expand_factor are no longer part of the
  # estimand_spec() API (see the entropy application's identical fix).
  likelyr::estimand_spec(
    psi_fn = psi_fn,
    psi_jac = psi_jac,
    psi_lower = 1 / J,
    psi_upper = 1.0,
    psi_closed = c(lower = TRUE, upper = FALSE),
    name = "Simpson's index (psi)"
  )
}