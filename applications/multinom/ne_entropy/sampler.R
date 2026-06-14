# ======================================================================
# Sampler Specification (No Effects Multinomial, Logit Parameterization)
# Target: Shannon Entropy H(theta) = -sum(p_j * log(p_j))
#
# Samples omega_hat from the entropy level set
#
#   Omega_psi_hat = { theta in Delta^{J-1} : H(theta) = psi_hat }
#
# via rejection sampling. A Dirichlet(1,...,1) proposal (uniform on the
# simplex) is accepted if |H(candidate) - psi_hat| < tol, where tol is
# scaled to 1% of H_max = log(J).
#
# In regimes where plain rejection is slow, targeted proposals are used:
#
#   Near-degenerate (psi_hat < 0.15 * log(J)):
#     Perturb around the one-dominant + uniform remainder family member
#     that achieves psi_hat, solved via uniroot().
#
#   Near-uniform (psi_hat > 0.85 * log(J)):
#     Draw from Dirichlet(20,...,20), which concentrates near uniform.
#
# omega_hat is returned in logit (eta) space: log(theta_j / theta_J)
# for j = 1, ..., J-1. This matches the parameterisation used by
# E_loglik, E_loglik_grad, and the warm start in ne_traversal.R.
#
# Returns function(history = NULL) -> list(candidate, diag):
#   $candidate          — numeric vector of length J-1 (omega-hat in eta-space)
#   $diag$regime        — character: "degenerate", "uniform", or "interior"
#   $diag$n_rejections  — integer: rejected draws before acceptance
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

entropy_sampler_fn <- function(param_dim, psi_mle, counts, ...) {
  J <- param_dim + 1L
  H_max <- log(J)

  project_to_level_set <- function(eta_init) {
    res <- tryCatch(
      nloptr::auglag(
        x0 = eta_init,
        fn = function(eta) abs(psi_fn(eta) - psi_mle),
        heq = NULL,
        hin = NULL,
        lower = rep(-20, J - 1L),
        upper = rep(20, J - 1L),
        localsolver = "LBFGS",
        localtol = 1e-10,
        control = list(xtol_rel = 1e-10, maxeval = 1000),
        deprecatedBehavior = FALSE
      ),
      error = function(e) NULL
    )

    if (is.null(res)) {
      return(NULL)
    }

    eta_proj <- res$par
    achieved <- abs(psi_fn(eta_proj) - psi_mle)
    if (achieved > 1e-4) {
      return(NULL)
    }

    eta_proj
  }

  function(history = NULL) {
    n_attempts <- 0L

    repeat {
      theta_init <- -log(runif(J))
      theta_init <- theta_init / sum(theta_init)
      eta_init <- log(theta_init[-J]) - log(theta_init[J])

      eta_proj <- project_to_level_set(eta_init)
      n_attempts <- n_attempts + 1L

      if (!is.null(eta_proj)) {
        break
      }

      if (n_attempts > 100L) {
        stop(
          sprintf(
            "entropy_sampler_fn: failed to project onto level set after %d attempts (psi_mle = %.6f, J = %d).",
            n_attempts,
            psi_mle,
            J
          ),
          call. = FALSE
        )
      }
    }

    list(
      candidate = eta_proj,
      diag = list(
        regime = "projection",
        n_attempts = n_attempts
      )
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
    name = "Shannon entropy rejection sampler (no effects)"
  )
}
