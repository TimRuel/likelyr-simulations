# ======================================================================
# Parameter Specification (Random Effects Multinomial Logistic Regression)
#
# The model parameter for inference is:
#   fix_Sigma = TRUE:  vec(B), length p*(J-1)
#   fix_Sigma = FALSE: c(vec(B), vech(chol(Sigma))), length p*(J-1) + (J-1)*J/2
#
# where B = [beta_2 | ... | beta_J] and category 1 is the baseline.
# The fix_Sigma flag is read from config$likelihood$fix_Sigma (default TRUE)
# and attached to param_mle as attr(param_mle, "fix_Sigma") for use by
# likelihood functions.
#
# The random effects covariance Sigma_0 = sigma2_u * I_{J-1} (diagonal,
# sigma2_u specified in config) is the true covariance used for data
# generation. Sigma_hat is estimated from data via mblogit() and attached
# to param_mle as attr(param_mle, "Sigma_hat").
#
# True coefficient generation:
#   psi_0 is defined marginally as D(theta_bar_re(B_0, Sigma_0)), where
#   theta_bar_re averages the softmax over the joint distribution of
#   (X, u) via Monte Carlo integration. U_mc is drawn once and held fixed
#   to give a deterministic objective surface.
#
# MLE:
#   beta_mle_fn fits mblogit() with catCov = "free" and returns param_mle
#   with attr "Sigma_hat", "theta_bar_hat", and "fix_Sigma" attached.
#
#   fix_Sigma = TRUE:  param_mle = vec(B_hat), length p*(J-1).
#   fix_Sigma = FALSE: param_mle = c(vec(B_hat), vech(chol(Sigma_hat))),
#     providing a warm start for Sigma in the branch optimizer.
#
# param_0 dimension mirrors param_mle:
#   fix_Sigma = TRUE:  vec(B_0), length p*(J-1)
#   fix_Sigma = FALSE: c(vec(B_0), vech(chol(Sigma_0))),
#                      length p*(J-1) + (J-1)*J/2
# ======================================================================

# ── Monte Carlo helpers ─────────────────────────────────────────────────

draw_from <- function(dist_cfg, n = 1L) {
  fn <- match.fun(dist_cfg$name)
  args <- c(list(n), dist_cfg$args)
  do.call(fn, args)
}

softmax_scalar <- function(x) {
  z <- x - max(x)
  exp(z) / sum(exp(z))
}

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

#' Marginal category probabilities averaged over joint (X, u) distribution
#'
#' @param param  Numeric vector vec(B) of length p*(J-1).
#' @param X_mc   N x p Monte Carlo design matrix.
#' @param U_mc   N x (J-1) Monte Carlo random effects draws.
#' @return       Numeric vector of length J.
compute_theta_bar_re <- function(param, X_mc, U_mc) {
  p <- ncol(X_mc)
  J <- ncol(U_mc) + 1L
  beta_mat <- matrix(param, nrow = p, ncol = J - 1L)
  eta_mc <- X_mc %*% beta_mat + U_mc
  probs_mc <- t(apply(cbind(0, eta_mc), 1, softmax_scalar))
  colMeans(probs_mc)
}

# ── True coefficient generation ─────────────────────────────────────────

#' Find B_0 satisfying D(theta_bar_re(B_0, Sigma_0)) = psi_target
#'
#' U_mc is drawn once and held fixed across function evaluations to give
#' a deterministic, smooth objective surface.
#'
#' @param config      Simulation config list.
#' @param X_mc        N x p Monte Carlo design matrix (precomputed).
#' @param U_mc        N x (J-1) Monte Carlo random effects draws (precomputed).
#' @param psi_target  Target Simpson's index value.
#' @param tol         Convergence tolerance. Default: 1e-4.
#' @param max_tries   Maximum warm-start retries. Default: 20.
#' @return            p x (J-1) coefficient matrix [beta_2,...,beta_J].
generate_beta_0 <- function(
  config,
  X_mc,
  U_mc,
  psi_target,
  tol = 1e-4,
  max_tries = 20L
) {
  J <- config$parameter$J
  coef_dist <- config$parameter$coefficient_distribution
  p <- ncol(X_mc)
  np <- p * (J - 1L)

  objective <- function(b) {
    theta_bar <- compute_theta_bar_re(b, X_mc, U_mc)
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
    psi_achieved <- sum(compute_theta_bar_re(beta_mat, X_mc, U_mc)^2)

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

#' Build beta_mle_fn for the random effects model
#'
#' Fits mblogit() with catCov = "free". Returns param_mle with attributes:
#'   "Sigma_hat"     — estimated (J-1) x (J-1) covariance matrix
#'   "theta_bar_hat" — estimated conditional probability vector at x_0, u=0
#'   "fix_Sigma"     — logical, read from config$likelihood$fix_Sigma
#'
#' fix_Sigma = TRUE:  param_mle = vec(B_hat), length p*(J-1).
#' fix_Sigma = FALSE: param_mle = c(vec(B_hat), vech(chol(Sigma_hat))),
#'   providing a warm start for Sigma in the branch optimizer.
#'
#' @param config  Simulation config list.
#' @return        function(data) -> numeric vector with attached attributes.
make_beta_mle_fn <- function(config) {
  fix_Sigma <- config$likelihood$fix_Sigma %||% TRUE

  function(data) {
    J <- attr(data, "J")
    X <- get_X_design(data)
    p <- ncol(X)

    fit <- mclogit::mblogit(
      formula = formula(attr(data, "terms")),
      random = ~ 1 | cluster,
      catCov = "free",
      data = data[seq_len(attr(data, "n_obs")), ]
    )

    # Extract B_hat: reshape from all-categories-per-predictor ordering
    cf <- coef(fit)
    B_hat <- t(matrix(cf, nrow = J - 1L, ncol = p)) # p x (J-1)

    # Extract Sigma_hat
    Sigma_hat <- fit$VarCov$cluster

    # Build param_mle
    if (fix_Sigma) {
      param <- as.numeric(B_hat)
    } else {
      L <- t(chol(Sigma_hat))
      param <- c(as.numeric(B_hat), L[lower.tri(L, diag = TRUE)])
    }

    # Compute theta_bar_hat at x_0 and u_i = 0
    x_0 <- x_reference(data)
    eta_hat <- as.numeric(x_0 %*% B_hat)
    theta_bar_hat <- softmax_scalar(c(0, eta_hat))

    attr(param, "Sigma_hat") <- Sigma_hat
    attr(param, "theta_bar_hat") <- theta_bar_hat
    attr(param, "fix_Sigma") <- fix_Sigma

    param
  }
}

# ── Parameter Spec Constructor ──────────────────────────────────────────

#' Build a parameter_spec for the random effects multinomial model
#'
#' param_0 dimension matches the inference parameter:
#'   fix_Sigma = TRUE:  vec(B_0), length p*(J-1)
#'   fix_Sigma = FALSE: c(vec(B_0), vech(chol(Sigma_0))),
#'                      length p*(J-1) + (J-1)*J/2
#'
#' Extra fields stored on the parameter spec:
#'   $x_bar_mc    — Monte Carlo covariate mean, for estimand evaluation
#'   $Sigma_0     — true random effects covariance (vectorised), for data
#'                  generation in generate_data()
#'   $theta_bar_0 — true marginal probability vector (averaged over X and u),
#'                  for psi_0 = sum(theta_bar_0^2) in make_estimand()
#'
#' @param config  Simulation config list.
#' @return        A parameter_spec object.
make_parameter <- function(config) {
  if (is.null(config$parameter)) {
    stop("Config must contain a 'parameter' section.", call. = FALSE)
  }

  J <- config$parameter$J
  f <- config$parameter$index_target_frac
  sigma2_u <- config$parameter$sigma2_u %||% 1.0
  fix_Sigma <- config$likelihood$fix_Sigma %||% TRUE
  Sigma_0 <- sigma2_u * diag(J - 1L)

  psi_target <- 1 / J + f * (1 - 1 / J)
  X_mc <- draw_X_mc(config)
  U_mc <- MASS::mvrnorm(nrow(X_mc), mu = rep(0, J - 1L), Sigma = Sigma_0)

  beta_0 <- generate_beta_0(config, X_mc, U_mc, psi_target)
  theta_bar_0 <- compute_theta_bar_re(beta_0, X_mc, U_mc)

  # param_0 mirrors the param_mle convention so the branch optimizer
  # always receives a consistently-dimensioned warm start.
  param_0 <- if (fix_Sigma) {
    as.numeric(beta_0)
  } else {
    L_0 <- t(chol(Sigma_0))
    c(as.numeric(beta_0), L_0[lower.tri(L_0, diag = TRUE)])
  }

  likelyr::parameter_spec(
    name = "Multinomial RE logistic regression coefficients",
    param_mle_fn = make_beta_mle_fn(config),
    param_0 = param_0,
    param_lower = NULL,
    param_upper = NULL,
    omega_dim = J,
    eq = NULL,
    eq_jac = NULL,
    x_bar_mc = colMeans(X_mc),
    Sigma_0 = as.numeric(Sigma_0),
    theta_bar_0 = theta_bar_0
  )
}
