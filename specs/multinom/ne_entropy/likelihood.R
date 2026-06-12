# ======================================================================
# Multinomial Likelihood (No Effects, Logit Parameterization)
# ======================================================================

# ----------------------------------------------------------------------
# Log-likelihood in η-space
#   ℓ(η) = Σ n_j z_j − N log(Σ exp(z_j)),  z = (η, 0) shifted for stability
# ----------------------------------------------------------------------

loglik <- function(param, data) {
  z <- c(param, 0)
  z <- z - max(z)
  N <- sum(data$count)
  sum(data$count * z) - N * log(sum(exp(z)))
}

# ----------------------------------------------------------------------
# Expected log-likelihood
# ----------------------------------------------------------------------

E_loglik <- function(param, omega_hat, data = NULL) {
  p_omega <- softmax_from_eta(omega_hat)
  z <- c(param, 0)
  z <- z - max(z)
  log_sum_exp <- log(sum(exp(z)))
  sum(p_omega * (z - log_sum_exp))
}

# ----------------------------------------------------------------------
# Gradient wrt η
# ----------------------------------------------------------------------

E_loglik_grad <- function(param, omega_hat, data = NULL) {
  p_omega <- softmax_from_eta(omega_hat)
  p_eta <- softmax_from_eta(param)
  p_omega[-length(p_omega)] - p_eta[-length(p_eta)]
}

# ----------------------------------------------------------------------
# Likelihood Spec Constructor
# ----------------------------------------------------------------------

make_likelihood <- function(config) {
  cfg <- config$likelihood

  if (is.null(cfg)) {
    stop("Config must contain a 'likelihood' section.", call. = FALSE)
  }

  likelyr::likelihood_spec(
    name = cfg$name %||% "Multinomial likelihood (logit parameterization)",
    loglik = loglik,
    E_loglik = E_loglik,
    E_loglik_grad = E_loglik_grad
  )
}
