# ======================================================================
# Sampler Specification (No Effects Multinomial, Logit Parameterization)
# Target: Shannon Entropy H(theta) = -sum(p_j * log(p_j))
#
# Samples omega_hat from the entropy level set
#
#   Omega_psi_hat = { theta in Delta^{J_obs-1} : H(theta) = psi_hat }
#
# following the approach of Severini (2007), adapted for sparse and
# near-boundary data.
#
# GEOMETRIC ISSUE NEAR THE BOUNDARY:
#   The entropy level set shrinks to a single point (the uniform
#   distribution) as psi_mle -> log(J_obs). When psi_mle is close to
#   this boundary, every omega_hat sampled directly in the J_obs-
#   dimensional space collapses to nearly the same point regardless of
#   the random direction u, producing degenerate branch mode diversity.
#   This is a property of the entropy constraint itself, not a defect
#   in how u is drawn.
#
# FIX — sampler-internal augmentation:
#   The parameter space used ONLY by the sampler is augmented with
#   n_phantom zero-count categories, chosen so that
#     log(J_obs + n_phantom) - psi_mle >= psi_buffer
#   This keeps the augmented level set away from its own boundary,
#   giving the constrained optimization genuine room to explore. The
#   solution is then re-referenced from the phantom reference category
#   back to the last observed category and truncated to the original
#   J_obs - 1 dimensions before being returned. The data, likelihood,
#   and profile are never touched by this augmentation — only the
#   sampler's internal proposal mechanism.
#
# omega_hat is returned in logit (eta) space: log(theta_j / theta_{J_obs})
# for j = 1, ..., J_obs - 1.
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
  J_obs <- param_dim + 1L

  counts <- data$count
  support <- which(counts > 0)
  n_support <- length(support)

  # Target buffer between psi_mle and the augmented upper boundary
  # log(J_aug). Larger buffer gives the sampler more room to produce
  # diverse omega_hat, at the cost of more phantom categories.
  psi_buffer <- sampler_cfg$psi_buffer %||% 0.5
  max_phantom <- sampler_cfg$max_phantom %||% 200L

  n_phantom <- max(1L, ceiling(exp(psi_mle + psi_buffer) - J_obs))
  n_phantom <- min(n_phantom, max_phantom)

  J_aug <- J_obs + n_phantom
  counts_aug <- c(counts, rep(0L, n_phantom))

  delta <- 1e-8
  theta_hat_aug <- (counts_aug + delta) / sum(counts_aug + delta)

  # eta_hat in augmented space: log(theta_j / theta_phantom_last)
  eta_hat_aug <- log(theta_hat_aug[-J_aug]) - log(theta_hat_aug[J_aug])

  psi_fn_aug <- function(eta) {
    p <- softmax_from_eta(eta)
    -sum(p * log(pmax(p, 1e-300)))
  }

  sample_from_level_set <- function(u) {
    res <- tryCatch(
      nloptr::slsqp(
        x0 = eta_hat_aug,
        fn = function(eta) -sum(u * log(pmax(softmax_from_eta(eta), 1e-300))),
        heq = function(eta) psi_fn_aug(eta) - psi_mle,
        lower = rep(-500, J_aug - 1L),
        upper = rep(500, J_aug - 1L),
        control = list(xtol_rel = 1e-8, maxeval = 2000)
      ),
      error = function(e) NULL
    )

    if (is.null(res)) return(NULL)

    eta_opt <- res$par
    if (abs(psi_fn_aug(eta_opt) - psi_mle) > 1e-4) return(NULL)

    # Re-reference from the phantom reference category (J_aug) to the
    # last observed category (J_obs). This formula is unchanged by the
    # number of phantom categories, since only the first J_obs
    # components of eta_opt are ever read:
    #   log(theta_j / theta_{J_obs})
    #     = log(theta_j / theta_phantom) - log(theta_{J_obs} / theta_phantom)
    #     = eta_opt[j] - eta_opt[J_obs]
    eta_reref <- eta_opt[seq_len(J_obs - 1L)] - eta_opt[J_obs]

    # Diagnostic only: entropy of the re-referenced candidate evaluated
    # back in the original J_obs-dimensional space.
    psi_reref <- entropy_of(softmax_from_eta(eta_reref))

    list(eta_reref = eta_reref, psi_reref = psi_reref)
  }

  function(history = NULL) {
    n_attempts <- 0L

    repeat {
      # Draw u uniformly from the simplex restricted to the observed
      # support (phantom categories always get u_j = 0).
      u <- rep(0, J_aug)
      u_support <- -log(stats::runif(n_support))
      u[support] <- u_support / sum(u_support)

      result <- sample_from_level_set(u)
      n_attempts <- n_attempts + 1L

      if (!is.null(result)) break

      if (n_attempts > 100L) {
        stop(sprintf(
          "entropy_sampler_fn: failed to sample from level set after %d attempts (psi_mle = %.6f, J_obs = %d, n_phantom = %d).",
          n_attempts, psi_mle, J_obs, n_phantom
        ), call. = FALSE)
      }
    }

    list(
      candidate = result$eta_reref,
      diag = list(
        regime = "severini_support_augmented",
        n_attempts = n_attempts,
        n_support = n_support,
        n_phantom = n_phantom,
        psi_reref = result$psi_reref,
        psi_reref_gap = result$psi_reref - psi_mle
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
    extra = list(
      sampler_cfg = list(
        psi_buffer = cfg$psi_buffer,
        max_phantom = cfg$max_phantom
      )
    ),
    name = "Shannon entropy Severini sampler (support-restricted, boundary-augmented, no effects)"
  )
}