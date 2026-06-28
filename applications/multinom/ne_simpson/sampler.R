# ======================================================================
# Sampler Specification (No Effects Multinomial, Logit Parameterization)
# Target: Shannon Entropy H(theta) = -sum(p_j * log(p_j))
#
# Samples omega_hat from the entropy level set
#
#   Omega_psi_hat = { theta in Delta^{J-1} : H(theta) = psi_hat }
#
# via projection. A random draw from Dirichlet(alpha * theta_hat),
# where theta_hat is the MLE, is projected onto the level set via
# constrained optimization. This biases proposals toward the region
# of the level set that is consistent with the observed data, which
# is critical when many categories have zero counts.
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
# 2. Sampler constructor
# ======================================================================

entropy_sampler_fn <- function(param_dim, psi_mle, data, ...) {
  J <- param_dim + 1L

  delta <- 1e-2
  counts <- data$count
  theta_hat <- (counts + delta) / sum(counts + delta)
  alpha <- 100.0

  project_to_level_set <- function(eta_init) {
    eta_hat <- log(theta_hat[-J]) - log(theta_hat[J])
    
    # Interpolate between eta_hat (H = psi_mle exactly) and eta_init
    # along the path: eta(t) = eta_hat + t * (eta_init - eta_hat)
    # At t = 0: eta = eta_hat, H = psi_mle
    # At t = 1: eta = eta_init, H ≈ psi_mle (since alpha is large)
    # We find t such that H(eta(t)) = psi_mle exactly.
    
    H_of_t <- function(t) psi_fn(eta_hat + t * (eta_init - eta_hat))
    
    H_init <- H_of_t(1.0)
    
    if (abs(H_init - psi_mle) <= 1e-4) return(eta_init)
    
    # Since H_of_t(0) = psi_mle and H_of_t(1) ≈ psi_mle but not exact,
    # bracket around t = 1 on whichever side crosses psi_mle
    if (H_init > psi_mle) {
      t_lo <- 1.0
      t_hi <- 2.0
      while (H_of_t(t_hi) > psi_mle && t_hi < 100) t_hi <- t_hi * 2
    } else {
      t_lo <- 0.0
      t_hi <- 1.0
    }
    
    root <- tryCatch(
      uniroot(function(t) H_of_t(t) - psi_mle,
              lower = t_lo, upper = t_hi, tol = 1e-8),
      error = function(e) NULL
    )
    
    if (is.null(root)) return(NULL)
    
    eta_proj <- eta_hat + root$root * (eta_init - eta_hat)
    if (abs(psi_fn(eta_proj) - psi_mle) > 1e-4) return(NULL)
    
    eta_proj
  }

  function(history = NULL) {
    n_attempts <- 0L

    repeat {
      gamma_draws <- stats::rgamma(J, shape = alpha * theta_hat, rate = 1)
      theta_init <- gamma_draws / sum(gamma_draws)
      eta_init <- log(theta_init[-J]) - log(theta_init[J])

      eta_proj <- project_to_level_set(eta_init)
      n_attempts <- n_attempts + 1L

      if (!is.null(eta_proj)) break

      if (n_attempts > 100L) {
        stop(sprintf(
          "entropy_sampler_fn: failed to project onto level set after %d attempts (psi_mle = %.6f, J = %d).",
          n_attempts, psi_mle, J
        ), call. = FALSE)
      }
    }

    list(
      candidate = eta_proj,
      diag = list(regime = "projection", n_attempts = n_attempts)
    )
  }
}

# ======================================================================
# 3. Spec constructor
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
    name = "Shannon entropy projection sampler (no effects)"
  )
}