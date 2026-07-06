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

make_estimand <- function(config) {
  J <- config$parameter$J

  likelyr::estimand_spec(
    psi_fn = psi_fn,
    psi_jac = psi_jac,
    psi_lower = 1 / J,
    psi_upper = 1.0,
    psi_closed = c(lower = TRUE, upper = FALSE),
    name = "Simpson's index (psi)"
  )
}
