# ======================================================================
# Multinomial Nuisance (Logit Parameterization)
# ======================================================================

# ----------------------------------------------------------------------
# Expected log-likelihood
#   Q(η ; ω̂)
#
#   = N ∑ θ_j(ω̂) log θ_j(η)
#
# omega_hat is assumed to be a logit vector (η-space).
# ----------------------------------------------------------------------

E_loglik <- function(param, omega_hat, data = NULL) {
  # probabilities under ω̂
  p_omega <- softmax_from_eta(omega_hat)

  # stable log θ(η)
  z <- c(param, 0)
  z <- z - max(z)

  log_sum_exp <- log(sum(exp(z)))

  N <- sum(data$count)

  N * sum(p_omega * (z - log_sum_exp))
}

# ----------------------------------------------------------------------
# Gradient wrt η
#
# ∇_η Q = N [ p_omega[1:J-1] − p_eta[1:J-1] ]
# ----------------------------------------------------------------------

E_loglik_grad <- function(param, omega_hat, data = NULL) {
  p_omega <- softmax_from_eta(omega_hat)
  p_eta <- softmax_from_eta(param)

  N <- sum(data$count)

  N * (p_omega[-length(p_omega)] - p_eta[-length(p_eta)])
}

# ----------------------------------------------------------------------
# Nuisance Spec Constructor
# ----------------------------------------------------------------------

make_nuisance <- function(config) {
  cfg <- config$nuisance

  if (is.null(cfg)) {
    stop("Config must contain a 'nuisance' section.", call. = FALSE)
  }

  nuisance_spec(
    name = cfg$name %||% "Multinomial logit nuisance parameter",
    E_loglik = E_loglik,
    E_loglik_grad = E_loglik_grad
  )
}
