# ======================================================================
# Estimand Specification (Multinomial Logistic Regression Parameterization)
# Target: Simpson's Index D(theta_bar) = sum(theta_bar_j^2)
#
# The parameter of interest is the Simpson's index of the marginal
# category probability vector theta_bar, obtained by averaging the
# softmax conditional probabilities over the empirical covariate
# distribution:
#
#   theta_bar_j = (1/n) * sum_i theta_j(x_i; B)
#
# psi_fn and psi_jac both require access to the data (for the design
# matrix), so they take a data argument.
#
# psi_0 is computed in make_estimand() from theta_bar_0, which is
# stored on the parameter spec by make_parameter() after marginalising
# over a large Monte Carlo covariate sample under the true beta_0.
# ======================================================================

#' Numerically stable softmax over a single numeric vector (length J)
#'
#' Named with a dot prefix to avoid shadowing the matrix-valued softmax()
#' defined in likelihood.R which operates on n x (J-1) linear predictor
#' matrices.
softmax_scalar <- function(x) {
  z <- x - max(x)
  exp(z) / sum(exp(z))
}

#' Simpson's index of the marginal category probabilities
#'
#' @param param  Numeric vector vec(B) of length p*(J-1).
#' @param data   Data frame with "terms" and "J" attributes.
#' @return       Scalar Simpson's index D(theta_bar).
psi_fn <- function(param, data) {
  X_design <- get_X_design(data)
  theta_bar <- compute_theta_bar(param, X_design)
  sum(theta_bar^2)
}

#' Jacobian of Simpson's index with respect to vec(B)
#'
#' By the chain rule:
#'   d D / d vec(B) = d D / d theta_bar * d theta_bar / d vec(B)
#'
#' d D / d theta_bar_j = 2 * theta_bar_j  (for all j)
#'
#' d theta_bar_j / d beta_k = (1/n) sum_i d theta_j(x_i) / d beta_k
#'
#' For the softmax, d theta_j / d beta_k = theta_k(x_i)(1(j=k) - theta_j(x_i))
#' which gives the closed-form gradient below.
#'
#' @param param  Numeric vector vec(B) of length p*(J-1).
#' @param data   Data frame with "terms" and "J" attributes.
#' @return       1 x p*(J-1) matrix (Jacobian row vector).
psi_jac <- function(param, data) {
  X_design <- get_X_design(data)
  J <- attr(data, "J")
  p <- ncol(X_design)
  n <- nrow(X_design)
  beta <- matrix(param, nrow = p, ncol = J - 1L)

  # n x J matrix of conditional probabilities (all J categories)
  eta <- X_design %*% beta
  probs <- t(apply(cbind(eta, 0), 1, softmax_scalar)) # n x J

  # marginal probabilities theta_bar (length J)
  theta_bar <- colMeans(probs)

  # gradient: d psi / d beta_k for k = 1, ..., J-1
  # = (2/n) * sum_i theta_k(x_i) * (theta_bar_k - sum_j theta_bar_j * theta_j(x_i)) * x_i
  inner <- as.numeric(probs %*% (2 * theta_bar)) # n-vector
  grad <- matrix(0, nrow = p, ncol = J - 1L)

  for (k in seq_len(J - 1L)) {
    weights <- probs[, k] * (2 * theta_bar[k] - inner)
    grad[, k] <- colSums(weights * X_design) / n
  }

  matrix(grad, nrow = 1L)
}

# ── Estimand Spec Constructor ───────────────────────────────────────────

#' Build an estimand_spec for Simpson's index under the MLR model
#'
#' psi_0 is derived from theta_bar_0 stored on the parameter spec by
#' make_parameter() after Monte Carlo marginalisation over the covariate
#' distribution under the true coefficient matrix beta_0.
#'
#' @param config     Simulation config list. Must contain parameter$J.
#' @param parameter  Parameter spec with theta_bar_0 stored as an extra field.
#' @return           An \code{estimand_spec} object.
make_estimand <- function(config, parameter) {
  J <- config$parameter$J
  theta_bar_0 <- parameter$extra$theta_bar_0
  psi_0 <- sum(theta_bar_0^2)

  likelyr::estimand_spec(
    name = "Simpson's index (marginal)",
    psi_fn = psi_fn,
    psi_jac = psi_jac,
    psi_lower = 1 / J,
    psi_upper = 1.0,
    psi_closed = c(lower = TRUE, upper = FALSE),
    psi_0 = psi_0
  )
}
