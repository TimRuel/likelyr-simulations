# ======================================================================
# Estimand Specification (Random Effects Multinomial Logistic Regression)
# Target: Simpson's Index D(theta(x_0; B, 0)) = sum(theta_j(x_0; B, 0)^2)
#
# The estimand is evaluated at the reference covariate x_0 = colMeans of
# the observed (pre-smoothing) rows of the design matrix, and at the
# median cluster u_i = 0. This eliminates both the covariate dependence
# and the random effects dependence simultaneously.
#
# Because u_i = 0 is substituted before evaluating the estimand, the
# probability vector theta(x_0; B, 0) depends on B only through B^T x_0,
# exactly as in the fixed effects model. The level set geometry in
# Delta^{J-1} is therefore identical, and the same sphere sampling
# machinery applies without modification. See Section sec:parameters-of-
# interest for the full justification.
#
# psi_fn, psi_jac, theta_at_x0, and make_psi_fns are identical to the
# fixed effects model. The only change is in make_estimand, where psi_0
# is now defined conditionally as D(theta(x_bar_mc; B_0, 0)) rather than
# as D(theta_bar(B_0)). This is consistent with the conditioning strategy
# adopted for this model.
#
# x_0 is computed from observed rows only (attr "n_obs"), excluding
# pseudo-observations added by epsilon smoothing.
# ======================================================================

softmax_scalar <- function(x) {
  z <- x - max(x)
  exp(z) / sum(exp(z))
}

#' Compute the reference covariate vector x_0
x_reference <- function(data) {
  n_obs <- attr(data, "n_obs") %||% nrow(data)
  X_design <- get_X_design(data)
  colMeans(X_design[seq_len(n_obs), , drop = FALSE])
}

#' Conditional category probabilities at x_0 under B, with u_i = 0
#'
#' Setting u_i = 0 (median cluster) eliminates the random effects
#' dependence. The resulting probability vector is identical to that
#' of the fixed effects model evaluated at x_0.
theta_at_x0 <- function(param, x0, J) {
  p <- length(x0)
  beta <- matrix(param, nrow = p, ncol = J - 1L)
  eta <- as.numeric(x0 %*% beta)
  softmax_scalar(c(0, eta))
}

#' Simpson's index at x_0 with u_i = 0
psi_fn <- function(param, data) {
  J <- attr(data, "J")
  x0 <- x_reference(data)
  sum(theta_at_x0(param, x0, J)^2)
}

#' Jacobian of Simpson's index at x_0 with u_i = 0
psi_jac <- function(param, data) {
  J <- attr(data, "J")
  x0 <- x_reference(data)
  th <- theta_at_x0(param, x0, J)
  D <- sum(th^2)
  matrix(outer(x0, 2 * th[-1L] * (th[-1L] - D)), nrow = 1L)
}

#' Build optimized psi_fn and psi_jac closures
make_psi_fns <- function(data) {
  x0 <- x_reference(data)
  J <- attr(data, "J")
  list(
    psi_fn = function(param) {
      sum(theta_at_x0(param, x0, J)^2)
    },
    psi_jac = function(param) {
      th <- theta_at_x0(param, x0, J)
      D <- sum(th^2)
      matrix(outer(x0, 2 * th[-1L] * (th[-1L] - D)), nrow = 1L)
    }
  )
}

# ── Estimand Spec Constructor ───────────────────────────────────────────

#' Build an estimand_spec for Simpson's index under the random effects model
#'
#' psi_0 is defined conditionally as D(theta(x_bar_mc; B_0, 0)), where
#' x_bar_mc = colMeans(X_mc) is the Monte Carlo covariate mean. This
#' is a population-level quantity consistent with the conditioning
#' strategy: we evaluate at the median cluster (u_i = 0) and the
#' population mean covariate. Because u_i = 0 is substituted, no
#' Monte Carlo integration over the random effects distribution is
#' needed, and psi_0 is simply D(softmax(c(0, B_0^T x_bar_mc))).
#'
#' @param config     Simulation config list. Must contain parameter$J.
#' @param parameter  Parameter spec with param_0 and extra$x_bar_mc.
#' @return           An estimand_spec object.
make_estimand <- function(config, parameter) {
  J <- config$parameter$J
  x_bar_mc <- parameter$extra$x_bar_mc
  psi_0 <- sum(theta_at_x0(parameter$param_0, x_bar_mc, J)^2)

  likelyr::estimand_spec(
    name = "Simpson's index (conditional at x_0, u_i = 0)",
    psi_fn = psi_fn,
    psi_jac = psi_jac,
    psi_lower = 1 / J,
    psi_upper = 1.0,
    psi_closed = c(lower = TRUE, upper = FALSE),
    psi_0 = psi_0,
    make_psi_fns = make_psi_fns
  )
}
