# ======================================================================
# Multinomial Likelihood (Logit Parameterization)
# ======================================================================

# ----------------------------------------------------------------------
# Log-likelihood in η-space
# Uses stable multinomial form:
#   sum(n_j * z_j) - N log(sum(exp(z)))
# ----------------------------------------------------------------------

loglik <- function(param, data) {
  z <- c(param, 0)
  z <- z - max(z)

  N <- sum(data$count)

  sum(data$count * z) - N * log(sum(exp(z)))
}

# ----------------------------------------------------------------------
# Closed-form MLE in η-space
# θ̂_j = n_j / N
# η̂_j = log(θ̂_j / θ̂_J)
# ----------------------------------------------------------------------

theta_to_eta <- function(theta) {
  log(theta[-length(theta)] / theta[length(theta)])
}

eta_mle_fn <- function(data) {
  delta <- 1e-8

  counts <- data$count

  J <- length(counts)

  smoothed_counts <- counts + delta

  theta_hat <- smoothed_counts / sum(smoothed_counts)

  eta_hat <- theta_to_eta(theta_hat)

  names(eta_hat) <- paste0("eta_", seq_len(J - 1))

  eta_hat
}

# ----------------------------------------------------------------------
# Likelihood Spec Constructor
# ----------------------------------------------------------------------

make_likelihood <- function(config) {
  cfg <- config$likelihood

  if (is.null(cfg)) {
    stop("Config must contain a 'likelihood' section.", call. = FALSE)
  }

  likelihood_spec(
    name = cfg$name %||% "Multinomial likelihood (logit parameterization)",
    loglik = loglik,
    param_mle_fn = eta_mle_fn
  )
}
