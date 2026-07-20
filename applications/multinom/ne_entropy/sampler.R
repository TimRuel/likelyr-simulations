# ======================================================================
# Sampler Specification (No Effects Multinomial, Logit Parameterization)
# Target: Shannon Entropy H(theta) = -sum(p_j * log(p_j))
#
# Samples omega_hat from the entropy level set
#
#   Omega_psi_hat = { theta in Delta^{J-1} : H(theta) = psi_hat }
#
# following the approach of Severini (2007), adapted for sparse data.
#
# ACTIVE SET CONSTRUCTION:
#   On each draw, the "active" set of cells consists of:
#     (a) all cells with positive observed count (always included), and
#     (b) a randomly chosen subset of the zero-count cells, of random
#         size between 0 and max_extra.
#   A single Dirichlet(1) draw for u is then taken over the ENTIRE
#   active set (observed cells and any extra zero-count cells treated
#   identically — no weight-shrinking of the extras). Cells outside the
#   active set get u_j = 0.
#
#   This differs from strict support-restriction (which always used
#   exactly the observed cells and produced omega_hat vectors whose
#   unsupported components converged to an identical solver-tolerance
#   floor rather than a genuine corner solution — a numerically
#   delicate target for the branch's constrained optimization near
#   points like psi = log(k)) and from full-simplex sampling (which
#   gives every zero-count cell real weight on every draw, reintroducing
#   the extreme-eta / long-flat-tail problems seen on sparse sites
#   before J was fixed at 30). Randomizing which zero-count cells are
#   active, and how many, means no single subset's boundary geometry
#   dominates every branch, while genuinely varying the branch
#   population draw to draw.
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
# 2b. Nuisance-population floor (Lever 1, integrated-likelihood only)
#
#   p_omega <- (1 - eps) * p_omega + eps / J
#
# Mixing an accepted omega-hat draw toward uniform gives its cell
# distribution FULL support, which makes the branch objective
# E_loglik = Σ p_omega·log θ strictly concave in every coordinate. That
# damps the competing concentration-corner optima on the entropy level
# set (the source of jagged branches) and keeps the optimal θ off the
# simplex vertices so η stays finite / well-conditioned.
#
# The floor lives HERE (on the sieve-drawn omega-hats), NOT in E_loglik,
# so the profile curve — which evaluates E_loglik at omega_hat = param_mle
# — stays classical/unfloored. It deliberately nudges the draw off the
# exact entropy level set; that is the intended regularization, applied
# only after the raw draw has been accepted. eps = 0 is a no-op. See
# dev/multinomial/ne_entropy/baseline_il.R for the sweep motivating
# eps ~ 0.02 on this data.
# ======================================================================

apply_omega_floor <- function(p_omega, eps) {
  if (eps <= 0) return(p_omega)
  (1 - eps) * p_omega + eps / length(p_omega)
}

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

  # Maximum number of additional (zero-count) cells to activate per
  # draw, beyond the always-included observed cells. Defaults to
  # allowing up to all unobserved cells to be eligible.
  max_extra <- sampler_cfg$max_extra %||% n_unobserved

  # Integrated-likelihood-only nuisance-population floor (Lever 1).
  eps_floor <- sampler_cfg$eps_floor %||% 0
  if (!is.numeric(eps_floor) || length(eps_floor) != 1L ||
      eps_floor < 0 || eps_floor >= 1) {
    stop("sampler$eps_floor must be a scalar in [0, 1).", call. = FALSE)
  }

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

    # Lever 1: floor the ACCEPTED draw toward uniform (integrated-only).
    if (eps_floor > 0) {
      theta_floored <- apply_omega_floor(softmax_from_eta(eta_opt), eps_floor)
      eta_opt <- theta_to_eta(theta_floored)
    }

    list(
      candidate = eta_opt,
      diag = list(
        regime = "severini_random_active_set",
        n_attempts = n_attempts,
        n_extra = n_extra,
        n_active = length(active),
        eps_floor = eps_floor
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
    name = "Shannon entropy Severini sampler (random active set, no effects)",
    sampler_cfg = cfg
  )
}