# ======================================================================
# Sampler Specification (No Effects Multinomial, Logit Parameterization)
# Target: Shannon Entropy H(theta) = -sum(p_j * log(p_j))
#
# Samples omega_hat from the entropy level set
#
#   Omega_psi_hat = { theta in Delta^{J-1} : H(theta) = psi_hat }
#
# following the approach of Severini (2007), adapted for sparse data.
# A random direction u is drawn uniformly from the simplex restricted
# to the support of the observed data — categories with zero counts
# receive u_j = 0. This keeps omega_hat vectors consistent with the
# observed support while still exploring the level set within that
# support, avoiding the pathology where uniform simplex draws assign
# mass to unobserved categories and produce branch modes far from
# psi_mle.
#
# The point on the level set maximizing the pseudo-likelihood
#
#   sum_j u_j * log(theta_j(eta))
#
# is located via constrained optimization in eta-space, with eta_hat
# as the warm start since it is already on the level set.
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

  counts <- data$count
  support <- which(counts > 0)
  n_support <- length(support)

  delta <- 1e-8
  theta_hat <- (counts + delta) / sum(counts + delta)
  eta_hat <- log(theta_hat[-J]) - log(theta_hat[J])

  sample_from_level_set <- function(u) {
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
      # Draw u uniformly from the simplex restricted to observed support
      u <- rep(0, J)
      u_support <- -log(stats::runif(n_support))
      u[support] <- u_support / sum(u_support)

      eta_opt <- sample_from_level_set(u)
      n_attempts <- n_attempts + 1L

      if (!is.null(eta_opt)) break

      if (n_attempts > 100L) {
        stop(sprintf(
          "entropy_sampler_fn: failed to sample from level set after %d attempts (psi_mle = %.6f, J = %d, n_support = %d).",
          n_attempts, psi_mle, J, n_support
        ), call. = FALSE)
      }
    }

    list(
      candidate = eta_opt,
      diag = list(
        regime = "severini_support",
        n_attempts = n_attempts,
        n_support = n_support
      )
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
    name = "Shannon entropy Severini sampler (support-restricted, no effects)"
  )
}