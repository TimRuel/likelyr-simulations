# ======================================================================
# Fixed Effects Multinomial Likelihood
#
# Parameter: b — length p*(J-1) vector (column-major B matrix)
# Data:      list with $counts (n x J) and $X (n x p)
# ======================================================================

# ----------------------------------------------------------------------
# Log-likelihood: sum over sites of multinomial log-likelihoods
# ----------------------------------------------------------------------

loglik <- function(param, data) {
  counts <- data$counts
  X <- data$X
  n <- nrow(counts)
  J <- ncol(counts)
  p <- ncol(X)

  B_mat <- matrix(param, nrow = p, ncol = J - 1L)

  ll <- 0
  for (i in seq_len(n)) {
    eta_i <- as.numeric(X[i, ] %*% B_mat)
    theta_i <- softmax_from_eta(eta_i)
    ll <- ll + sum(counts[i, ] * log(pmax(theta_i, 1e-300)))
  }
  ll
}

# ----------------------------------------------------------------------
# Expected log-likelihood
#
# omega_hat is a J-1 vector of logits representing the conditional
# category distribution at x_0: theta(x_0; omega_hat_B) = softmax(omega_hat)
#
# Q(b; omega_hat) = sum_j p_j(omega_hat) * log theta_j(x_0; B)
#                 = sum_j p_j(omega_hat) * (x_0'B_j - log sum_k exp(x_0'B_k))
#
# where B_j is the j-th column of B_mat and x_0 is retrieved from the
# parameter spec via the data argument (passed as a named list containing
# x_0 alongside the observed data).
# ----------------------------------------------------------------------

E_loglik <- function(param, omega_hat, data = NULL) {
  x_0 <- data$x_0
  J <- length(omega_hat) + 1L
  p <- length(x_0)
  B_mat <- matrix(param, nrow = p, ncol = J - 1L)

  p_omega <- softmax_from_eta(omega_hat)

  eta_x0 <- as.numeric(x_0 %*% B_mat)
  z <- c(eta_x0, 0)
  z <- z - max(z)
  log_sum_exp <- log(sum(exp(z)))

  sum(p_omega * (z - log_sum_exp))
}

# ----------------------------------------------------------------------
# Gradient of E_loglik wrt b (vec of B_mat, column-major)
#
# dQ/db = vec( x_0 (p_omega - theta(x_0; B))' )
# where the outer product is p x (J-1) and we drop the J-th category.
# ----------------------------------------------------------------------

E_loglik_grad <- function(param, omega_hat, data = NULL) {
  x_0 <- data$x_0
  J <- length(omega_hat) + 1L
  p <- length(x_0)
  B_mat <- matrix(param, nrow = p, ncol = J - 1L)

  p_omega <- softmax_from_eta(omega_hat)
  eta_x0 <- as.numeric(x_0 %*% B_mat)
  theta <- softmax_from_eta(eta_x0)

  # Residual for first J-1 categories
  resid <- p_omega[-J] - theta[-J]

  # Gradient is outer product x_0 %o% resid, vectorised column-major
  as.numeric(outer(x_0, resid))
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
    name = cfg$name %||%
      "Multinomial likelihood (fixed effects, logit parameterization)",
    loglik = loglik,
    E_loglik = E_loglik,
    E_loglik_grad = E_loglik_grad
  )
}
