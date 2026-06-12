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
  tol <- 0.01 * H_max

  psi_low_threshold <- 0.15 * H_max
  psi_high_threshold <- 0.85 * H_max

  regime <- if (psi_mle < psi_low_threshold) {
    "degenerate"
  } else if (psi_mle > psi_high_threshold) {
    "uniform"
  } else {
    "interior"
  }

  make_proposal <- switch(
    regime,

    interior = function() {
      x <- -log(runif(J))
      x / sum(x)
    },

    degenerate = {
      a_star <- dominant_prob_for_entropy(psi_mle, J)
      if (is.null(a_star)) {
        function() {
          x <- -log(runif(J))
          x / sum(x)
        }
      } else {
        function() {
          a <- a_star + rnorm(1, sd = tol)
          a <- min(max(a, 1e-8), 1 - 1e-8)
          rest <- (1 - a) / (J - 1) + rnorm(J - 1L, sd = tol / J)
          rest <- pmax(rest, 1e-8)
          theta <- c(a, rest)
          theta / sum(theta)
        }
      }
    },

    uniform = {
      alpha <- 20
      function() {
        x <- rgamma(J, shape = alpha, rate = 1)
        x / sum(x)
      }
    }
  )

  function(history = NULL) {
    n_rejections <- 0L

    repeat {
      candidate <- make_proposal()
      if (abs(entropy_of(candidate) - psi_mle) < tol) {
        break
      }
      n_rejections <- n_rejections + 1L
    }

    list(
      candidate = log(candidate[seq_len(J - 1L)]) - log(candidate[J]),
      diag = list(
        regime = regime,
        n_rejections = n_rejections
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
