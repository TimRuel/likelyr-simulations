# ======================================================================
# Multinomial Likelihood (Logistic Regression Parameterization)
#
# Implements the log-likelihood, expected log-likelihood, and its
# gradient for the multinomial logistic regression model. The model
# parameter is vec(B) ∈ R^{p(J-1)}, where B = [β_1 | ... | β_{J-1}]
# and category J is the baseline.
#
# All functions share the same design matrix helpers:
#   get_X_design(data) — n × p design matrix
#   get_Y_design(data) — n × (J-1) indicator matrix (baseline dropped)
# ======================================================================

# ── Shared helpers ─────────────────────────────────────────────────────

#' Compute softmax probabilities for non-baseline categories
#'
#' @param eta  n × (J-1) matrix of linear predictors.
#' @param J    Number of categories.
#' @return     n × (J-1) matrix of predicted probabilities (baseline dropped).
softmax <- function(eta) {
  eta_aug <- cbind(eta, 0)
  row_max <- apply(eta_aug, 1, max)
  shifted <- eta_aug - row_max
  exp_eta <- exp(shifted)
  probs <- exp_eta / rowSums(exp_eta)

  # Replace NaN rows (complete underflow) with one-hot vectors
  nan_rows <- apply(probs, 1, \(r) any(is.nan(r)))
  if (any(nan_rows)) {
    dominant <- apply(eta_aug[nan_rows, , drop = FALSE], 1, which.max)
    probs[nan_rows, ] <- 0
    probs[cbind(which(nan_rows), dominant)] <- 1
  }

  J <- ncol(probs)
  probs[, -J, drop = FALSE]
}

#' Compute the masked linear predictor matrix
#'
#' Sets columns for missing categories to -Inf so they contribute
#' exp(-Inf) = 0 to the normalizing constant.
#'
#' @param Y_hat        n × (J-1) matrix of linear predictors.
#' @param missing_mask Logical vector of length J-1.
#' @return             Y_hat with missing columns set to -Inf.
mask_missing <- function(Y_hat, missing_mask) {
  Y_hat[, missing_mask] <- -Inf
  Y_hat
}

# ── Log-likelihood ─────────────────────────────────────────────────────

#' Multinomial log-likelihood
#'
#' @param param  Numeric vector of length p*(J-1) = vec(B).
#' @param data   Data frame with "terms" and "J" attributes.
#' @return       Scalar log-likelihood value.
loglik <- function(param, data) {
  X_design <- get_X_design(data)
  Y_design <- get_Y_design(data)
  J <- attr(data, "J")
  p <- ncol(X_design)
  beta <- matrix(param, nrow = p, ncol = J - 1L)
  Y_hat <- mask_missing(X_design %*% beta, colSums(Y_design) == 0)

  sum(
    rowSums(Y_design * Y_hat, na.rm = TRUE) -
      matrixStats::rowLogSumExps(cbind(Y_hat, 0), na.rm = TRUE)
  )
}

# ── Expected log-likelihood ────────────────────────────────────────────

#' Expected log-likelihood E_{omega_hat}[ell(B)]
#'
#' Replaces the observed indicator matrix Y_design with the matrix of
#' predicted probabilities under omega_hat.
#'
#' @param param     Numeric vector vec(B) at which to evaluate.
#' @param omega_hat Numeric vector vec(B_hat) defining the expectation.
#' @param data      Data frame with "terms" and "J" attributes.
#' @return          Scalar expected log-likelihood value.
E_loglik <- function(param, omega_hat, data) {
  X_design <- get_X_design(data)
  p <- ncol(X_design)
  theta_hat <- softmax(
    X_design %*% matrix(omega_hat, nrow = p)
  )
  Y_hat <- mask_missing(
    X_design %*% matrix(param, nrow = p),
    colSums(theta_hat) == 0
  )
  sum(
    rowSums(theta_hat * Y_hat, na.rm = TRUE) -
      matrixStats::rowLogSumExps(cbind(Y_hat, 0), na.rm = TRUE)
  )
}

# ── Gradient of expected log-likelihood ───────────────────────────────

#' Gradient of E_{omega_hat}[ell(B)] with respect to vec(B)
#'
#' d/d(vec(B)) E[ell(B)] = vec(X^T (Theta_hat - Theta)),
#' where Theta_hat and Theta are the n x (J-1) predicted probability
#' matrices under omega_hat and param respectively.
#'
#' @param param     Numeric vector vec(B) at which to evaluate.
#' @param omega_hat Numeric vector vec(B_hat) defining the expectation.
#' @param data      Data frame with "terms" and "J" attributes.
#' @return          Numeric vector of length p*(J-1).
E_loglik_grad <- function(param, omega_hat, data) {
  X_design <- get_X_design(data)
  p <- ncol(X_design)
  theta_hat <- softmax(
    X_design %*% matrix(omega_hat, nrow = p)
  )
  theta <- softmax(
    X_design %*% matrix(param, nrow = p)
  )
  as.numeric(t(X_design) %*% (theta_hat - theta))
}

# ── Likelihood Spec Constructor ────────────────────────────────────────

#' Build a likelihood_spec for the multinomial logistic regression model
#'
#' @param config  Simulation config list. Must contain a 'likelihood' section.
#' @return        A \code{likelihood_spec} object.
make_likelihood <- function(config) {
  cfg <- config$likelihood

  if (is.null(cfg)) {
    stop("Config must contain a 'likelihood' section.", call. = FALSE)
  }

  likelyr::likelihood_spec(
    name = cfg$name %||% "Multinomial likelihood (logistic regression)",
    loglik = loglik,
    E_loglik = E_loglik,
    E_loglik_grad = E_loglik_grad
  )
}
