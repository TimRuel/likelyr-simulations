# ======================================================================
# Multinomial Likelihood (Logistic Regression Parameterization)
#
# Implements the log-likelihood, expected log-likelihood, and its
# gradient for the multinomial logistic regression model. The model
# parameter is vec(B) ∈ R^{p(J-1)}, where B = [β_2 | ... | β_J]
# and category 1 is the baseline.
#
# omega_hat is a length-J probability vector drawn from Omega_psi_hat
# in Delta^{J-1} by the sphere sampler, representing the conditional
# category probabilities at the reference covariate x_0 = colMeans
# of the observed (pre-smoothing) rows of X_design.
#
# E_loglik constructs B_hat from omega_hat by adjusting B_mle along
# x_0 to satisfy theta(x_0; B_hat) = omega_hat, leaving all directions
# orthogonal to x_0 unchanged. This gives observation-specific theta_hat
# rows that exploit the full covariate structure.
#
# make_branch_fns provides an optimized branch objective for the auglag
# inner loop. It precomputes X_design, x_0, B_mle_mat, B_hat, and
# theta_hat once per omega_hat (in the branch binder), rather than
# recomputing them on every auglag function/gradient evaluation.
# E_loglik and E_loglik_grad are still used for diagnostics (called
# once post-solve, not in the inner loop).
#
# x_0 is computed from the observed rows only (attr "n_obs"), excluding
# pseudo-observations added by epsilon smoothing which would bias x_0.
#
# All functions share the same design matrix helpers:
#   get_X_design(data) — n × p design matrix
#   get_Y_design(data) — n × (J-1) indicator matrix (baseline dropped)
# ======================================================================

# ── Shared helpers ─────────────────────────────────────────────────────

#' Compute softmax probabilities for non-baseline categories
#'
#' Category 1 is the baseline (coefficient fixed at 0), so the
#' augmented linear predictor prepends a 0 column: cbind(0, eta).
#' The baseline column (column 1) is dropped from the return value.
#'
#' @param eta  n × (J-1) matrix of linear predictors for categories 2,...,J.
#' @return     n × (J-1) matrix of predicted probabilities (baseline dropped).
softmax <- function(eta) {
  eta_aug <- cbind(0, eta)
  row_max <- apply(eta_aug, 1, max)
  shifted <- eta_aug - row_max
  exp_eta <- exp(shifted)
  probs <- exp_eta / rowSums(exp_eta)
  probs[, -1L, drop = FALSE]
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

#' Compute the reference covariate vector x_0
#'
#' Column means of the observed (pre-smoothing) rows of the design matrix.
#' Uses the "n_obs" attribute to exclude pseudo-observations added by
#' epsilon smoothing, which would otherwise bias x_0.
#'
#' @param data  Data frame with "terms" and optionally "n_obs" attributes.
#' @return      Numeric vector of length p.
x_reference <- function(data) {
  n_obs <- attr(data, "n_obs") %||% nrow(data)
  X_design <- get_X_design(data)
  colMeans(X_design[seq_len(n_obs), , drop = FALSE])
}

#' Construct B_hat from omega_hat by adjusting B_mle along x_0
#'
#' Given omega_hat as a length-J probability vector satisfying
#' theta(x_0; B_hat) = omega_hat, the logit vector at x_0 is:
#'
#'   eta_0 = log(omega_hat[-1]) - log(omega_hat[1])
#'
#' B_hat is the unique matrix that satisfies:
#'   1. B_hat^T x_0 = eta_0        (omega_hat constraint)
#'   2. B_hat^T v   = B_mle^T v    for all v orthogonal to x_0
#'
#' This gives the rank-1 update:
#'   B_hat = B_mle + outer(x_0, delta) / ||x_0||^2
#' where delta = eta_0 - B_mle^T x_0.
#'
#' @param omega_hat  Numeric vector of length J (probability vector).
#' @param B_mle      p x (J-1) MLE coefficient matrix [β_2,...,β_J].
#' @param x_0        Numeric vector of length p (reference covariate).
#' @return           p x (J-1) coefficient matrix B_hat.
make_B_hat <- function(omega_hat, B_mle, x_0) {
  eta_0 <- log(omega_hat[-1L]) - log(omega_hat[1L])
  eta_mle <- as.numeric(x_0 %*% B_mle)
  delta <- eta_0 - eta_mle
  x0_norm2 <- sum(x_0^2)
  B_mle + outer(x_0, delta) / x0_norm2
}

#' Compute marginal category probabilities by Monte Carlo integration
#'
#' @param param     Numeric vector vec(B) of length p*(J-1).
#' @param X_design  N x p design matrix.
#' @return          Numeric vector of length J of marginal probabilities,
#'   with theta_1 (baseline) first.
compute_theta_bar <- function(param, X_design) {
  p <- ncol(X_design)
  beta_mat <- matrix(param, nrow = p)
  eta_aug <- cbind(0, X_design %*% beta_mat)
  exp_eta <- exp(eta_aug - apply(eta_aug, 1, max))
  probs <- exp_eta / rowSums(exp_eta)
  colMeans(probs)
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
      matrixStats::rowLogSumExps(cbind(0, Y_hat), na.rm = TRUE)
  )
}

# ── Expected log-likelihood ────────────────────────────────────────────

#' Expected log-likelihood E_{omega_hat}[ell(B)]
#'
#' Used for diagnostics (E_loglik_at_hat, E_loglik_gap) — called once
#' post-solve, not in the auglag inner loop. The inner loop uses the
#' precomputed closures from make_branch_fns instead.
#'
#' @param param      Numeric vector vec(B) of length p*(J-1).
#' @param omega_hat  Numeric vector of length J (probability vector at x_0).
#' @param data       Data frame with "terms" and "J" attributes.
#' @param param_mle  Numeric vector vec(B_mle) of length p*(J-1).
#' @return           Scalar expected log-likelihood value.
E_loglik <- function(param, omega_hat, data, param_mle) {
  X_design <- get_X_design(data)
  J <- attr(data, "J")
  p <- ncol(X_design)
  x_0 <- x_reference(data)
  B_mle <- matrix(param_mle, nrow = p, ncol = J - 1L)

  B_hat <- make_B_hat(omega_hat, B_mle, x_0)
  theta_hat <- softmax(X_design %*% B_hat)

  Y_hat <- mask_missing(
    X_design %*% matrix(param, nrow = p, ncol = J - 1L),
    colSums(theta_hat) == 0
  )

  sum(
    rowSums(theta_hat * Y_hat, na.rm = TRUE) -
      matrixStats::rowLogSumExps(cbind(0, Y_hat), na.rm = TRUE)
  )
}

# ── Gradient of expected log-likelihood ───────────────────────────────

#' Gradient of E_{omega_hat}[ell(B)] with respect to vec(B)
#'
#' Used for diagnostics only — the auglag inner loop uses the precomputed
#' gradient from make_branch_fns instead.
#'
#' @param param      Numeric vector vec(B) of length p*(J-1).
#' @param omega_hat  Numeric vector of length J (probability vector at x_0).
#' @param data       Data frame with "terms" and "J" attributes.
#' @param param_mle  Numeric vector vec(B_mle) of length p*(J-1).
#' @return           Numeric vector of length p*(J-1).
E_loglik_grad <- function(param, omega_hat, data, param_mle) {
  X_design <- get_X_design(data)
  J <- attr(data, "J")
  p <- ncol(X_design)
  x_0 <- x_reference(data)
  B_mle <- matrix(param_mle, nrow = p, ncol = J - 1L)

  B_hat <- make_B_hat(omega_hat, B_mle, x_0)
  theta_hat <- softmax(X_design %*% B_hat)
  theta <- softmax(X_design %*% matrix(param, nrow = p, ncol = J - 1L))

  as.numeric(t(X_design) %*% (theta_hat - theta))
}

# ── Optimized branch objective factory ────────────────────────────────

#' Build optimized fn/gr closures for the auglag inner loop
#'
#' Called once during calibrate_likelihood() with (data, param_mle) to
#' precompute X_design, x_0, B_mle_mat, and missing_mask. Returns a
#' function of omega_hat that precomputes B_hat and theta_hat once per
#' branch, then returns fn and gr that reference theta_hat via closure.
#'
#' This avoids recomputing get_X_design(), x_reference(), make_B_hat(),
#' and softmax(X %*% B_hat) on every auglag function/gradient call.
#'
#' @param data       Data frame with "terms", "J", and "n_obs" attributes.
#' @param param_mle  Numeric vector vec(B_mle) of length p*(J-1).
#' @return           function(omega_hat) -> list(fn, gr)
make_branch_fns <- function(data, param_mle) {
  X_design <- get_X_design(data)
  x_0 <- x_reference(data)
  J <- attr(data, "J")
  p <- ncol(X_design)
  B_mle_mat <- matrix(param_mle, nrow = p, ncol = J - 1L)
  missing_mask <- colSums(get_Y_design(data)) == 0

  function(omega_hat) {
    # Precompute once per omega_hat — constant for the branch's lifetime
    B_hat <- make_B_hat(omega_hat, B_mle_mat, x_0)
    theta_hat <- softmax(X_design %*% B_hat)

    list(
      fn = function(param) {
        Y_hat <- mask_missing(
          X_design %*% matrix(param, nrow = p, ncol = J - 1L),
          missing_mask
        )
        -sum(
          rowSums(theta_hat * Y_hat, na.rm = TRUE) -
            matrixStats::rowLogSumExps(cbind(0, Y_hat), na.rm = TRUE)
        )
      },
      gr = function(param) {
        theta <- softmax(X_design %*% matrix(param, nrow = p, ncol = J - 1L))
        -as.numeric(t(X_design) %*% (theta_hat - theta))
      }
    )
  }
}

# ── Profile omega_hat converter ────────────────────────────────────────

#' Convert param_mle to the profile reference omega_hat
#'
#' @param param_mle  Numeric vector vec(B_mle) of length p*(J-1).
#' @param data       Data frame with "terms" and "J" attributes.
#' @return           Numeric vector of length J.
omega_hat_from_param_mle <- function(param_mle, data) {
  J <- attr(data, "J")
  x_0 <- x_reference(data)
  p <- length(x_0)
  B_mle <- matrix(param_mle, nrow = p, ncol = J - 1L)
  eta_0 <- as.numeric(x_0 %*% B_mle)
  exp_eta <- exp(c(0, eta_0) - max(c(0, eta_0)))
  exp_eta / sum(exp_eta)
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
    E_loglik_grad = E_loglik_grad,
    needs_param_mle = TRUE,
    omega_hat_from_param_mle = omega_hat_from_param_mle,
    make_branch_fns = make_branch_fns
  )
}
