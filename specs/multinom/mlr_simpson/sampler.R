# ======================================================================
# Sampler Specification (Multinomial Logistic Regression Parameterization)
# Target: Simpson's Index D(theta_bar) = sum(theta_bar_j^2)
#
# Samples omega_hat directly from the level set
#
#   Omega_psi_hat = { B ∈ R^{p(J-1)} : psi(B) = psi_hat }
#
# where psi(B) = sum_j theta_bar_j(B)^2 and theta_bar(B) = (1/n) X Theta(B)
# is the vector of marginal category probabilities under B.
#
# Since Omega_psi_hat has no closed-form geometric characterization in
# vec(B) space (unlike the simple logit case where it is a sphere in
# Delta^{J-1}), we sample feasible points via auglag: minimize a dummy
# objective f(B) = 0 subject to the equality constraint psi(B) = psi_hat.
# A fresh random initial guess is drawn for each call to provide variety
# across draws.
#
# Returns function(history = NULL) -> list(candidate, diag):
#   $candidate      — numeric vector vec(B) of length p*(J-1), a draw
#                     from Omega_psi_hat
#   $diag$convergence — integer convergence code from auglag
# ======================================================================

# ======================================================================
# 1. Sampler constructor
# ======================================================================

simpson_sampler_fn <- function(
  param_dim,
  psi_mle,
  data,
  coefficient_distribution,
  ...
) {
  X_design <- get_X_design(data)
  p <- ncol(X_design)
  J <- param_dim / p + 1L

  function(history = NULL) {
    b0 <- draw_from(coefficient_distribution, param_dim)

    res <- nloptr::auglag(
      x0 = b0,
      fn = function(b) 0,
      heq = function(b) {
        beta <- matrix(b, nrow = p, ncol = J - 1L)
        theta_bar <- compute_theta_bar(beta, X_design)
        sum(theta_bar^2) - psi_mle
      },
      heqjac = function(b) psi_jac(b, data),
      localsolver = "SLSQP",
      localtol = 1e-6,
      deprecatedBehavior = FALSE
    )

    list(
      candidate = res$par,
      diag = list(convergence = res$convergence)
    )
  }
}

# ======================================================================
# 2. Spec constructor
# ======================================================================

#' Build a sampler_spec for Simpson's index under the MLR model
#'
#' @param config  Simulation config list. Must contain a 'sampler' section.
#' @return        A \code{sampler_spec} object.
make_sampler <- function(config) {
  cfg <- config$sampler

  if (is.null(cfg)) {
    stop("Config must contain a 'sampler' section.", call. = FALSE)
  }

  likelyr::sampler_spec(
    sampler_fn = simpson_sampler_fn,
    min_branches = cfg$min_branches,
    branch_buffer = cfg$branch_buffer %||% 0L,
    name = "Simpson's index MLR sampler",
    coefficient_distribution = config$parameter$coefficient_distribution
  )
}
