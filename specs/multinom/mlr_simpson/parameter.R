# ======================================================================
# Multinomial Parameter Specification (Logistic Regression Parameterization)
#
# The model parameter is the matrix of regression coefficients
#   B = [β_2 | ... | β_J] ∈ R^{p × (J-1)},
# stored as a flat vector in column-major order:
#   θ = vec(B) = (β_2^T, ..., β_J^T)^T ∈ R^{p(J-1)}.
#
# Category 1 is the baseline; its coefficient vector is fixed at 0.
# The conditional probability vector at covariate x is
#   theta_j(x; B) = exp(x^T β_j) / sum_{k=1}^{J} exp(x^T β_k),
# where β_1 = 0. In matrix form, the n x J linear predictor matrix is
# cbind(0, X %*% B), with the baseline column prepended.
#
# The inference target is Simpson's index of the conditional probability
# vector at the observed covariate mean x_0 = colMeans(X_design):
#
#   psi(B) = D(theta(x_0; B)) = sum_j theta_j(x_0; B)^2
#
# The true parameter value psi_0 is defined via the marginal distribution:
#
#   psi_0 = D(theta_bar(B_0)) = sum_j theta_bar_j(B_0)^2
#
# where theta_bar_j(B_0) = E_X[theta_j(X; B_0)] is approximated by
# Monte Carlo integration over a large covariate sample. This ensures
# psi_0 is a population-level quantity independent of the observed data.
#
# True coefficient generation:
#   draw_X_mc()         — draw large MC covariate sample
#   compute_theta_bar() — marginalise softmax over covariates
#   generate_beta_0()   — find B satisfying D(theta_bar(B)) = psi_target
# ======================================================================

# ── Monte Carlo helpers ─────────────────────────────────────────────────

#' Draw a large Monte Carlo covariate sample and build its design matrix
#'
#' @param config  Simulation config list.
#' @param N       Number of draws. Default: 1e5.
#' @return        N x p design matrix.
draw_X_mc <- function(config, N = 1e5L) {
  data_cfg <- config$data
  J <- config$parameter$J
  formula_str <- data_cfg$formula

  covariate_df <- lapply(data_cfg$predictors, \(pred) {
    vals <- draw_from(pred$distribution, N)
    setNames(data.frame(vals), pred$symbol)
  }) |>
    do.call(what = cbind)

  tmp_data <- covariate_df
  tmp_data[["Y"]] <- factor(rep(1L, N), levels = seq_len(J))
  attr(tmp_data, "terms") <- terms(as.formula(formula_str), data = tmp_data)
  attr(tmp_data, "J") <- J

  get_X_design(tmp_data)
}

#' Compute marginal category probabilities by Monte Carlo integration
#'
#' Averages the softmax conditional probabilities over the rows of X_mc,
#' approximating E_X[theta(X; B)] under the config covariate distribution.
#'
#' @param param     Numeric vector vec(B) of length p*(J-1), or p x (J-1) matrix.
#'   Encodes [β_2,...,β_J]; category 1 (baseline) is implicit with β_1 = 0.
#' @param X_design  N x p design matrix.
#' @return          Numeric vector of length J of marginal probabilities,
#'   with theta_1 first.
compute_theta_bar <- function(param, X_design) {
  p <- ncol(X_design)
  beta_mat <- matrix(param, nrow = p)
  eta_aug <- cbind(0, X_design %*% beta_mat)
  exp_eta <- exp(eta_aug - apply(eta_aug, 1, max))
  probs <- exp_eta / rowSums(exp_eta)
  colMeans(probs)
}

# ── True coefficient generation ─────────────────────────────────────────

#' Find beta_0 satisfying D(theta_bar(beta_0)) = psi_target
#'
#' Minimises (D(theta_bar(B)) - psi_target)^2 via L-BFGS-B using a large
#' Monte Carlo covariate sample to approximate the marginal probabilities.
#' Warm-starts from a random draw from the config coefficient distribution.
#' Retries with fresh warm starts if the optimizer does not converge.
#'
#' @param config      Simulation config list.
#' @param X_mc        N x p Monte Carlo design matrix (precomputed).
#' @param psi_target  Target Simpson's index value.
#' @param tol         Convergence tolerance on |D - psi_target|. Default: 1e-4.
#' @param max_tries   Maximum number of warm-start retries. Default: 20.
#' @return            p x (J-1) coefficient matrix [β_2,...,β_J].
generate_beta_0 <- function(
  config,
  X_mc,
  psi_target,
  tol = 1e-4,
  max_tries = 20L
) {
  J <- config$parameter$J
  coef_dist <- config$parameter$coefficient_distribution
  p <- ncol(X_mc)
  np <- p * (J - 1L)

  objective <- function(b) {
    theta_bar <- compute_theta_bar(b, X_mc)
    (sum(theta_bar^2) - psi_target)^2
  }

  for (attempt in seq_len(max_tries)) {
    fit <- optim(
      par = draw_from(coef_dist, np),
      fn = objective,
      method = "L-BFGS-B",
      control = list(maxit = 2000, factr = 1e6)
    )

    beta_mat <- matrix(fit$par, nrow = p, ncol = J - 1L)
    psi_achieved <- sum(compute_theta_bar(beta_mat, X_mc)^2)

    if (abs(psi_achieved - psi_target) <= tol) {
      return(beta_mat)
    }
  }

  warning(sprintf(
    "generate_beta_0(): failed to achieve psi_target = %.4f within tol = %.4f after %d attempts.",
    psi_target,
    tol,
    max_tries
  ))

  beta_mat
}

# ── MLE ────────────────────────────────────────────────────────────────

#' Compute the MLE of vec(B) via nnet::multinom
#'
#' data$Y has natural factor ordering (levels = 1:J), so nnet::multinom
#' automatically uses category 1 as the reference (baseline). coef(fit)
#' returns a (J-1) x p matrix with rows for categories 2,...,J.
#' Transposing to p x (J-1) and flattening column-major gives
#' vec(B) = (β_2^T,...,β_J^T)^T, matching our parameterization convention.
#'
#' A small L2 penalty (decay) is applied to prevent coefficient
#' divergence when some categories are rare or absent from the data.
#'
#' @param data  Data frame with "terms" and "J" attributes.
#' @return      Numeric vector of length p*(J-1).
beta_mle_fn <- function(data) {
  fit <- nnet::multinom(
    formula(attr(data, "terms")),
    data = data,
    maxit = 2000,
    decay = 0.01,
    trace = FALSE
  )

  t(coef(fit)) |> as.numeric()
}

# ── Parameter Spec Constructor ──────────────────────────────────────────

#' Build a parameter_spec for the multinomial logistic regression model
#'
#' Finds beta_0 satisfying D(theta_bar(beta_0)) = psi_target via Monte
#' Carlo marginalisation over the covariate distribution. Stores
#' theta_bar_0 as an extra field for use by make_estimand(), which
#' computes psi_0 = D(theta_bar_0) without requiring the observed data.
#'
#' @param config  Simulation config list. Must contain a 'parameter' section.
#' @return        A \code{parameter_spec} object.
make_parameter <- function(config) {
  if (is.null(config$parameter)) {
    stop("Config must contain a 'parameter' section.", call. = FALSE)
  }

  J <- config$parameter$J
  f <- config$parameter$index_target_frac
  psi_target <- 1 / J + f * (1 - 1 / J)
  X_mc <- draw_X_mc(config)
  beta_0 <- generate_beta_0(config, X_mc, psi_target)
  theta_bar_0 <- compute_theta_bar(beta_0, X_mc)

  likelyr::parameter_spec(
    name = "Multinomial logistic regression coefficients",
    param_mle_fn = beta_mle_fn,
    param_0 = as.numeric(beta_0),
    param_lower = NULL,
    param_upper = NULL,
    omega_dim = J, # omega_hat lives in Delta^{J-1}, not R^{p(J-1)}
    eq = NULL,
    eq_jac = NULL,
    theta_bar_0 = theta_bar_0
  )
}
