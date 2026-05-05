# ======================================================================
# Multinomial Parameter Specification (Logistic Regression Parameterization)
#
# The model parameter is the matrix of regression coefficients
#   B = [β_1 | ... | β_{J-1}] ∈ R^{p × (J-1)},
# stored as a flat vector in column-major order:
#   θ = vec(B) = (β_1^T, ..., β_{J-1}^T)^T ∈ R^{p(J-1)}.
#
# Category J is the baseline; its coefficient vector is fixed at 0.
#
# True coefficient generation:
#   draw_X_mc()         — draw large MC covariate sample
#   compute_theta_bar() — marginalise softmax over covariates
#   generate_beta()    — find B satisfying D(theta_bar) = psi_target
#
# These are called from generate_data() in data.R, which attaches
# beta_true and psi_0 to the returned data frame. make_parameter()
# reads both from the data attributes.
# ======================================================================

# ── True coefficient helpers ────────────────────────────────────────────

#' Draw a large Monte Carlo covariate sample and build its design matrix
#'
#' @param config  Simulation config list.
#' @param N       Number of draws. Default: 1e5.
#' @return        N x p design matrix.
draw_X_mc <- function(config, N = 1e5L) {
  data_cfg <- config$data
  model_cfg <- config$model
  J <- config$parameter$J
  formula_str <- model_cfg$formula

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
#' approximating E_{X}[theta(X; B)] under the config covariate distribution.
#'
#' @param beta_mat  p x (J-1) true coefficient matrix.
#' @param X_mc      N x p Monte Carlo design matrix.
#' @return          Numeric vector of length J of marginal probabilities.
compute_theta_bar <- function(param, X_design) {
  p <- ncol(X_design)
  beta_mat <- matrix(param, nrow = p)
  eta <- X_design %*% beta_mat
  eta_aug <- cbind(eta, 0)
  exp_eta <- exp(eta_aug - apply(eta_aug, 1, max))
  probs <- exp_eta / rowSums(exp_eta)
  colMeans(probs)
}

#' Find beta_0 satisfying D(theta_bar(beta_0)) = psi_target
#'
#' Minimises (D(theta_bar(B)) - psi_target)^2 via L-BFGS-B, warm-started
#' from a random draw from the config coefficient distribution. Retries
#' with fresh warm starts if the optimizer does not converge within tol.
#'
#' @param config      Simulation config list.
#' @param X_mc        N x p Monte Carlo design matrix (precomputed).
#' @param psi_target  Target Simpson's index value.
#' @param tol         Convergence tolerance on |D - psi_target|. Default: 1e-4.
#' @param max_tries   Maximum number of warm-start retries. Default: 20.
#' @return            p x (J-1) coefficient matrix.
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
    theta_bar <- compute_theta_bar(matrix(b, nrow = p, ncol = J - 1L), X_mc)
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
    ".find_beta_true(): failed to achieve psi_target = %.4f within tol = %.4f after %d attempts.",
    psi_target,
    tol,
    max_tries
  ))

  beta_mat
}

# ── MLE ────────────────────────────────────────────────────────────────

#' Compute the MLE of vec(B) via L-BFGS-B
#'
#' Precomputes design matrices and missing category mask outside the
#' optimization loop for efficiency.
#'
#' @param data  Data frame with "terms" and "J" attributes.
#' @return      Numeric vector of length p*(J-1).
beta_mle_fn <- function(data) {
  X_design <- get_X_design(data)
  Y_design <- get_Y_design(data)
  J <- attr(data, "J")
  p <- ncol(X_design)
  missing_mask <- colSums(Y_design) == 0

  neg_ll <- function(b) {
    Y_hat <- mask_missing(
      X_design %*% matrix(b, nrow = p, ncol = J - 1L),
      missing_mask
    )
    -sum(
      rowSums(Y_design * Y_hat, na.rm = TRUE) -
        matrixStats::rowLogSumExps(cbind(Y_hat, 0), na.rm = TRUE)
    )
  }

  fit <- optim(
    par = rep(0, p * (J - 1L)),
    fn = neg_ll,
    method = "L-BFGS-B",
    control = list(maxit = 1000, factr = 1e7)
  )

  if (fit$convergence != 0) {
    warning(
      "beta_mle_fn(): optim did not converge (code ",
      fit$convergence,
      ")."
    )
  }

  fit$par
}

# ── Parameter Spec Constructor ──────────────────────────────────────────

#' Build a parameter_spec for the multinomial logistic regression model
#'
#' Reads beta_0 and psi_0 from attributes set by generate_data() in
#' data.R via Monte Carlo marginalisation over the covariate distribution.
#'
#' @param config  Simulation config list. Must contain a 'parameter' section.
#' @param data    Data frame returned by generate_data(), with "beta_0"
#'   and "psi_0" attributes set.
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
    eq = NULL,
    eq_jac = NULL,
    theta_bar_0 = theta_bar_0
  )
}
