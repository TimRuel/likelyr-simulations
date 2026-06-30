# ======================================================================
# Sampler Specification (No Effects Multinomial, Logit Parameterization)
# Target: Shannon Entropy H(theta) = -sum(p_j * log(p_j))
#
# Samples omega_hat from the entropy level set
#
#   Omega_psi_hat = { theta in Delta^{J-1} : H(theta) = psi_hat }
#
# following the approach of Severini (2007). A random direction
# u = (u_1, ..., u_J) is drawn uniformly from the probability simplex,
# and the point on the level set that maximizes the pseudo-likelihood
#
#   sum_j u_j * log(theta_j(eta))
#
# is located via constrained optimization in eta-space. This is
# equivalent to Severini's step 2 in logit parameterization: the
# pseudo-likelihood is a linear functional of log(theta), and
# maximizing it over the level set selects a point whose location
# depends on the random direction u but not on psi_mle itself.
#
# The MLE eta_hat is used as the warm start since it is already on
# the level set by construction.
#
# omega_hat is returned in logit (eta) space: log(theta_j / theta_J)
# for j = 1, ..., J-1.
# ======================================================================

# ======================================================================
# 1. Entropy helpers
# ======================================================================

entropy_of <- function(theta) {
  p <- theta[theta > 0]
  -sum(p * log(p))
}

dominant_prob_for_entropy <- function(H_target, J) {
  H_of_a <- function(a) {
    if (a <= 0 || a >= 1) {
      return(-Inf)
    }
    prest <- (1 - a) / (J - 1)
    -a * log(a) - (J - 1) * prest * log(prest)
  }
  root <- tryCatch(
    uniroot(
      function(a) H_of_a(a) - H_target,
      lower = 1 / J + 1e-10,
      upper = 1 - 1e-10
    ),
    error = function(e) NULL
  )
  if (is.null(root)) {
    return(NULL)
  }
  root$root
}

# ======================================================================
# 2. Pseudo-likelihood objective (Severini step 2 in eta-space)
# ======================================================================

pseudo_loglik <- function(eta, u) {
  theta <- softmax_from_eta(eta)
  sum(u * log(pmax(theta, 1e-300)))
}

pseudo_loglik_neg <- function(eta, u) -pseudo_loglik(eta, u)

# ======================================================================
# 3. Sampler constructor
# ======================================================================

entropy_sampler_fn <- function(param_dim, psi_mle, data, ...) {
  J <- param_dim + 1L

  delta <- 1e-2
  counts <- data$count
  theta_hat <- (counts + delta) / sum(counts + delta)
  eta_hat <- log(theta_hat[-J]) - log(theta_hat[J])

  sample_from_level_set <- function(u) {
    # Maximize sum(u * log theta(eta)) subject to H(theta(eta)) = psi_mle
    res <- tryCatch(
      nloptr::slsqp(
        x0 = eta_hat,
        fn = function(eta) pseudo_loglik_neg(eta, u),
        heq = function(eta) psi_fn(eta) - psi_mle,
        lower = rep(-500, J - 1L),
        upper = rep(500, J - 1L),
        control = list(xtol_rel = 1e-8, maxeval = 2000)
      ),
      error = function(e) NULL
    )

    if (is.null(res)) return(NULL)

    eta_opt <- res$par
    if (abs(psi_fn(eta_opt) - psi_mle) > 1e-4) return(NULL)

    eta_opt
  }

  function(history = NULL) {
    n_attempts <- 0L

    repeat {
      # Draw u uniformly from the probability simplex
      u <- -log(stats::runif(J))
      u <- u / sum(u)

      eta_opt <- sample_from_level_set(u)
      n_attempts <- n_attempts + 1L

      if (!is.null(eta_opt)) break

      if (n_attempts > 100L) {
        stop(sprintf(
          "entropy_sampler_fn: failed to sample from level set after %d attempts (psi_mle = %.6f, J = %d).",
          n_attempts, psi_mle, J
        ), call. = FALSE)
      }
    }

    list(
      candidate = eta_opt,
      diag = list(regime = "severini", n_attempts = n_attempts)
    )
  }
}

# ======================================================================
# 4. Spec constructor
# ======================================================================

make_sampler <- function(config) {
  cfg <- config$sampler

  if (is.null(cfg)) {
    stop("Config must contain a 'sampler' section.", call. = FALSE)
  }

  likelyr::sampler_spec(
    sampler_fn = entropy_sampler_fn,
    min_branches = cfg$min_branches,
    branch_buffer = cfg$branch_buffer %||% 0L,
    name = "Shannon entropy Severini sampler (no effects)"
  )
}