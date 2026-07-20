# ======================================================================
# Multinomial Likelihood (No Effects, Logit Parameterization)
# ======================================================================

# ----------------------------------------------------------------------
# Log-likelihood in η-space
#   ℓ(η) = Σ n_j z_j − N log(Σ exp(z_j)),  z = (η, 0) shifted for stability
# ----------------------------------------------------------------------

loglik <- function(param, data) {
  z <- c(param, 0)
  z <- z - max(z)
  N <- sum(data$count)
  sum(data$count * z) - N * log(sum(exp(z)))
}

# ----------------------------------------------------------------------
# Expected log-likelihood
#
# NOTE: the Lever-1 nuisance-population floor (mixing p_omega toward
# uniform) used to live here, which meant it also perturbed the PROFILE
# curve (profile calls E_loglik with omega_hat = param_mle). As of
# 2026-07-17 the floor was moved to the SAMPLER (specs/sampler.R) so it
# regularizes ONLY the integrated-likelihood branches and leaves the
# profile classical. E_loglik is therefore the plain surrogate again.
#
# SIDE EFFECT (observed 2026-07-20): this split also explains why branch
# solves converge MUCH faster than profile solves at the same psi grid
# density, independent of branch length. loglik() above is built from the
# RAW data counts — for dune row 2, 20 of 30 species have count = 0, so
# the unconstrained pull on those logits is toward -Inf, in direct tension
# with the entropy constraint psi(theta) = psi_k, which forces some mass
# to stay spread across every category (more so as psi_k rises). That
# tension is exactly the kind of landscape that makes constrained SLSQP
# solves slow (many profile grid points hit the solver's maxeval cap).
# E_loglik's p_omega, by contrast, is floor-conditioned (sampler$eps_floor,
# specs/sampler.R) and never exactly zero anywhere, so the branch's
# unconstrained optimum is already well-behaved everywhere and the
# constrained solve converges quickly regardless of psi_k. eps_floor was
# introduced to numerically condition the SAMPLER's own draws (keep
# omega_hat off simplex vertices); this fast-branch-convergence effect is
# a bonus, not something it was designed for. Also secondary: under
# branch_selection = "continuation" a converging branch mostly pays for
# one solve per grid point (only failed warm starts trigger the anchor /
# jitter fallback), whereas profile's profile_retry_on = "constraint" with
# max_retries = 25 (inherited from solver$max_retries) will burn up to 25
# full retries — each potentially hitting maxeval — on any point that
# doesn't converge cleanly, compounding the difficulty above.
# ----------------------------------------------------------------------

E_loglik <- function(param, omega_hat, data = NULL) {
  p_omega <- softmax_from_eta(omega_hat)
  z <- c(param, 0)
  z <- z - max(z)
  log_sum_exp <- log(sum(exp(z)))
  sum(p_omega * (z - log_sum_exp))
}

# ----------------------------------------------------------------------
# Gradient wrt η
# ----------------------------------------------------------------------

E_loglik_grad <- function(param, omega_hat, data = NULL) {
  p_omega <- softmax_from_eta(omega_hat)
  p_eta <- softmax_from_eta(param)
  p_omega[-length(p_omega)] - p_eta[-length(p_eta)]
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
    name = cfg$name %||% "Multinomial likelihood (logit parameterization)",
    loglik = loglik,
    E_loglik = E_loglik,
    E_loglik_grad = E_loglik_grad
  )
}
