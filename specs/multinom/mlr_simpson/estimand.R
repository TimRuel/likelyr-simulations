# ======================================================================
# Estimand Specification (Multinomial Logistic Regression Parameterization)
# Target: Simpson's Index D(theta(x_0; B)) = sum(theta_j(x_0; B)^2)
#
# The parameter of interest is the Simpson's index of the conditional
# category probability vector at a reference covariate value x_0,
# taken to be the column means of the OBSERVED (pre-smoothing) design
# matrix rows, stored as attribute "n_obs" on the data frame:
#
#   psi(B) = D(theta(x_0; B)) = sum_j theta_j(x_0; B)^2
#
#   theta_j(x_0; B) = softmax(B^T x_0)_j
#
# Category 1 is the baseline (β_1 = 0). The conditional probability
# vector at x_0 is:
#
#   theta(x_0; B) = softmax(c(0, x_0^T β_2, ..., x_0^T β_J))
#
# make_psi_fns precomputes x_0 and J once during calibration, returning
# psi_fn and psi_jac closures that take only param. This avoids calling
# x_reference(data) and get_X_design(data) on every auglag iteration.
#
# The Jacobian with respect to vec(B) = (β_2^T,...,β_J^T)^T is:
#
#   ∂D/∂β_k = x_0 · 2θ_k(θ_k - D),  k = 2,...,J
#
# giving outer(x_0, 2 * th[-1] * (th[-1] - D)) as a p x (J-1) matrix,
# flattened to a 1 x p(J-1) row vector.
#
# psi_0 is the marginal D(theta_bar(beta_0)), computed via Monte Carlo
# integration in make_parameter() and stored as parameter$extra$theta_bar_0.
# ======================================================================

#' Numerically stable softmax over a single numeric vector (length J)
softmax_scalar <- function(x) {
  z <- x - max(x)
  exp(z) / sum(exp(z))
}

#' Compute the reference covariate vector x_0
#'
#' Column means of the observed (pre-smoothing) rows of the design matrix.
#'
#' @param data  Data frame with "terms" and optionally "n_obs" attributes.
#' @return      Numeric vector of length p.
x_reference <- function(data) {
  n_obs <- attr(data, "n_obs") %||% nrow(data)
  X_design <- get_X_design(data)
  colMeans(X_design[seq_len(n_obs), , drop = FALSE])
}

#' Conditional category probabilities at x_0 under B
#'
#' Category 1 is the baseline (β_1 = 0), so the linear predictor
#' vector at x_0 is c(0, x_0^T β_2, ..., x_0^T β_J).
#'
#' @param param  Numeric vector vec(B) of length p*(J-1) = (β_2^T,...,β_J^T)^T.
#' @param x0     Numeric vector of length p (reference covariate).
#' @param J      Number of categories.
#' @return       Numeric vector of length J, with theta_1 first.
theta_at_x0 <- function(param, x0, J) {
  p <- length(x0)
  beta <- matrix(param, nrow = p, ncol = J - 1L)
  eta <- as.numeric(x0 %*% beta)
  softmax_scalar(c(0, eta))
}

#' Simpson's index at the reference covariate x_0
#'
#' @param param  Numeric vector vec(B) of length p*(J-1).
#' @param data   Data frame with "terms" and "J" attributes.
#' @return       Scalar Simpson's index D(theta(x_0; B)).
psi_fn <- function(param, data) {
  J <- attr(data, "J")
  x0 <- x_reference(data)
  sum(theta_at_x0(param, x0, J)^2)
}

#' Jacobian of Simpson's index at x_0 with respect to vec(B)
#'
#' ∂D/∂β_k = x_0 · 2θ_k(θ_k - D) for k = 2,...,J,
#' i.e. the non-baseline slice th[-1].
#'
#' @param param  Numeric vector vec(B) of length p*(J-1).
#' @param data   Data frame with "terms" and "J" attributes.
#' @return       1 x p*(J-1) matrix (Jacobian row vector).
psi_jac <- function(param, data) {
  J <- attr(data, "J")
  x0 <- x_reference(data)
  th <- theta_at_x0(param, x0, J)
  D <- sum(th^2)
  matrix(outer(x0, 2 * th[-1L] * (th[-1L] - D)), nrow = 1L)
}

# ── Optimized psi closure factory ──────────────────────────────────────

#' Build optimized psi_fn and psi_jac closures for the auglag inner loop
#'
#' Called once during calibrate_estimand() with data to precompute x_0
#' and J. Returns psi_fn and psi_jac that take only param, avoiding
#' calls to x_reference(data) and get_X_design(data) on every auglag
#' function/gradient evaluation.
#'
#' @param data  Data frame with "terms", "J", and "n_obs" attributes.
#' @return      list(psi_fn = function(param), psi_jac = function(param))
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

#' Build an estimand_spec for Simpson's index under the MLR model
#'
#' @param config     Simulation config list. Must contain parameter$J.
#' @param parameter  Parameter spec with theta_bar_0 stored as an extra field.
#' @return           An \code{estimand_spec} object.
make_estimand <- function(config, parameter) {
  J <- config$parameter$J
  theta_bar_0 <- parameter$extra$theta_bar_0
  psi_0 <- sum(theta_bar_0^2)

  likelyr::estimand_spec(
    name = "Simpson's index (conditional at x_0)",
    psi_fn = psi_fn,
    psi_jac = psi_jac,
    psi_lower = 1 / J,
    psi_upper = 1.0,
    psi_closed = c(lower = TRUE, upper = FALSE),
    psi_0 = psi_0,
    make_psi_fns = make_psi_fns
  )
}
