# ======================================================================
# Sampler Specification (No Effects Multinomial, Logit Parameterization)
# Target: Shannon Entropy H(theta) = -sum(p_j * log(p_j))
#
# Samples omega_hat from the entropy level set
#
#   Omega_psi_hat = { theta in Delta^{J-1} : H(theta) = psi_hat }
#
# following the approach of Severini (2007). A random direction u is
# drawn uniformly from the FULL J-dimensional simplex — no restriction
# to the observed support. This is the cleanest adherence to the
# theoretical level set: any probability vector over all J = 30 cells
# with entropy equal to psi_mle is a legitimate element of
# Omega_psi_hat, regardless of which cells happen to have nonzero
# counts in this particular sample.
#
# Support restriction was previously used to keep omega_hat consistent
# with the observed data, but it produced omega_hat vectors whose
# unsupported components converged to an anomalous, solver-tolerance-
# driven near-zero value (e.g. all landing at the same ~1e-7 floor)
# rather than a genuine corner solution. This created a numerically
# delicate target for the branch's constrained optimization to match
# near singular points of the constraint surface (e.g. psi =
# log(k) for small k), producing sharp, non-concave notches in
# individual branches at those points. Drawing u over the full
# simplex avoids this: unsupported cells receive genuine, well-behaved
# random weight from the same Dirichlet(1) draw as everything else,
# so there's no artificial near-zero floor for the branch optimizer
# to chase.
#
# J = 30 is fixed for every site (see ne_data_dune.R / ne_parameter.R),
# so psi_upper = log(30) provides a large, fixed buffer above psi_mle,
# giving branches genuine room to have interior modes.
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

entropy_sampler_fn <- function(param_dim, psi_mle, data, sampler_cfg = list(), ...) {
  J <- param_dim + 1L

  counts <- data$count
  observed <- which(counts > 0)
  unobserved <- which(counts == 0)
  n_observed <- length(observed)
  n_unobserved <- length(unobserved)

  # Maximum number of additional (unobserved) cells to activate per draw,
  # beyond the always-included observed cells. Defaults to allowing up
  # to all unobserved cells to be eligible.
  max_extra <- sampler_cfg$max_extra %||% n_unobserved

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
      # Randomly choose how many additional (previously-zero) cells to
      # activate this draw, then which ones, then draw u over the
      # resulting subset (always-included observed cells + the random
      # extras). Cells outside this subset get u_j = 0.
      n_extra <- sample.int(max_extra + 1L, size = 1L) - 1L
      extra <- if (n_extra > 0L) {
        sample(unobserved, size = n_extra)
      } else {
        integer(0)
      }
      active <- c(observed, extra)

      u <- rep(0, J)
      u_active_raw <- -log(stats::runif(length(active)))
      u[active] <- u_active_raw / sum(u_active_raw)

      eta_opt <- sample_from_level_set(u)
      n_attempts <- n_attempts + 1L

      if (!is.null(eta_opt)) break

      if (n_attempts > 100L) {
        stop(sprintf(
          "entropy_sampler_fn: failed to sample from level set after %d attempts (psi_mle = %.6f, J = %d, n_active = %d).",
          n_attempts, psi_mle, J, length(active)
        ), call. = FALSE)
      }
    }

    list(
      candidate = eta_opt,
      diag = list(
        regime = "severini_random_subset",
        n_attempts = n_attempts,
        n_active = length(active),
        n_extra = length(extra)
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
    name = "Shannon entropy Severini sampler (full simplex, no effects)"
  )
}