# ======================================================================
# Data Generation (Multinomial Logistic Regression Parameterization)
#
# Generates data for a multinomial logistic regression model. Covariates
# are drawn from config-specified distributions. True regression
# coefficients are found via generate_beta_0() (defined in parameter.R)
# to satisfy a target Simpson's index psi_0 = 1/J + f*(1 - 1/J).
#
# Depends on parameter.R for:
#   draw_X_mc()          — Monte Carlo covariate design matrix
#   compute_theta_bar()  — marginal category probabilities
#   generate_beta_0()    — constrained coefficient search
#
# Parameterization convention:
#   Category 1 is the baseline; its coefficient vector is fixed at 0.
#   The free coefficient matrix is B = [β_2 | ... | β_J] ∈ R^{p×(J-1)},
#   stored as vec(B) = (β_2^T, ..., β_J^T)^T ∈ R^{p(J-1)}.
#   The conditional probability vector is
#     theta_j = exp(x^T β_j) / sum_{k=1}^{J} exp(x^T β_k),
#   where β_1 = 0. In matrix form: cbind(0, X %*% B) gives the n x J
#   linear predictor matrix with the baseline column prepended.
#
#   data$Y uses natural factor ordering (levels = 1:J), so nnet::multinom
#   automatically uses category 1 as its reference. table(data$Y) returns
#   counts in natural category order and is safe to index by position.
#
# Zero-count categories:
#   No epsilon smoothing is applied to the generated data. When psi_0 is
#   high, the data-generating distribution is concentrated and zero-count
#   categories are expected. Resampling or augmenting the dataset to
#   ensure all categories appear would bias the simulation by excluding
#   datasets that are typical at that parameter value. MLE stabilization
#   for zero-count categories is handled in beta_mle_fn() via
#   pseudo-observation augmentation scoped to MLE fitting only.
#
# Config structure:
#   data:
#     formula: "Y ~ X1 + X2"
#     n_obs: 20
#     predictors:
#       - symbol: X1
#         distribution: {name: rnorm, args: [0.0, 1.0]}
#       - symbol: X2
#         distribution: {name: rnorm, args: [0.0, 1.0]}
#   parameter:
#     J: 5
#     index_target_frac: 0.10
#     coefficient_distribution:
#       name: rnorm
#       args: [0.0, 0.5]
# ======================================================================

# ── Design matrix helpers ───────────────────────────────────────────────

#' Build the model design matrix from a data frame with a "terms" attribute
#'
#' @param data  Data frame with "terms" attribute encoding the model formula.
#' @return      n x p model matrix with "terms" and "formula" attributes.
get_X_design <- function(data) {
  trms <- attr(data, "terms")
  model_frame <- model.frame(trms, data = data)
  X_design <- model.matrix(trms, data = model_frame)

  attr(X_design, "original_model_frame") <- model_frame
  attr(X_design, "terms") <- trms
  attr(X_design, "formula") <- formula(trms)

  X_design
}

#' Build the response indicator matrix from a data frame with "terms" and "J"
#'
#' @param data  Data frame with "terms" and "J" attributes.
#' @return      n x (J-1) integer indicator matrix (baseline category 1 dropped).
get_Y_design <- function(data) {
  trms <- attr(data, "terms")
  response <- all.vars(trms)[attr(trms, "response")]
  y <- data[[response]]
  J <- attr(data, "J")
  y_int <- as.integer(as.character(y))
  non_baseline <- seq(2L, J)

  outer(y_int, non_baseline, `==`) * 1L
}

# ── Internal helpers ────────────────────────────────────────────────────

#' Draw n values from a named distribution specified in config
#'
#' @param dist_cfg  List with fields: name (character), args (list of scalars).
#' @param n         Number of draws.
#' @return          Numeric vector of length n.
draw_from <- function(dist_cfg, n = 1L) {
  fn <- match.fun(dist_cfg$name)
  args <- c(list(n), dist_cfg$args)
  do.call(fn, args)
}

#' Numerically stable softmax over a single numeric vector (length J)
#'
#' Named with a dot prefix to avoid shadowing the matrix-valued softmax()
#' defined in likelihood.R which operates on n x (J-1) linear predictor
#' matrices.
softmax_scalar <- function(x) {
  z <- x - max(x)
  exp(z) / sum(exp(z))
}

# ── Data generation ─────────────────────────────────────────────────────

#' Generate multinomial logistic regression data
#'
#' Uses the true coefficient vector param_0 from the parameter spec to
#' generate categorical responses. No smoothing or augmentation is applied
#' to the generated data — zero-count categories are a natural consequence
#' of sampling from a concentrated distribution at high psi_0 and are
#' included as-is. MLE stabilization is handled separately in beta_mle_fn().
#'
#' @param config     Simulation config list.
#' @param parameter  Parameter spec object with param_0 set.
#' @return           A data frame with response Y and all covariates, with
#'   attributes "terms", "J", and "n_obs" set.
generate_data <- function(config, parameter) {
  data_cfg <- config$data
  param_cfg <- config$parameter
  n <- data_cfg$n_obs
  formula_str <- data_cfg$formula
  J <- param_cfg$J

  # ── Draw observed covariates ──────────────────────────────────────────
  covariate_df <- lapply(data_cfg$predictors, \(pred) {
    vals <- draw_from(pred$distribution, n)
    setNames(data.frame(vals), pred$symbol)
  }) |>
    do.call(what = cbind)

  # ── Build design matrix for observed data ─────────────────────────────
  tmp_data <- covariate_df
  tmp_data[["Y"]] <- factor(rep(1L, n), levels = seq_len(J))
  attr(tmp_data, "terms") <- terms(as.formula(formula_str), data = tmp_data)
  attr(tmp_data, "J") <- J

  X_design <- get_X_design(tmp_data)

  # ── Draw categorical responses using true coefficients ────────────────
  # beta_0 is p x (J-1) = [β_2,...,β_J]; prepend 0 column for category 1.
  beta_0 <- matrix(parameter$param_0, nrow = ncol(X_design), ncol = J - 1L)
  eta <- X_design %*% beta_0
  probs <- t(apply(cbind(0, eta), 1, softmax_scalar))
  Y <- apply(probs, 1, \(p) sample.int(J, 1L, prob = p))

  # ── Assemble final data frame ─────────────────────────────────────────
  # Natural factor ordering (levels = 1:J) so nnet::multinom uses
  # category 1 as baseline and table(data$Y) is safe to index by position.
  Y_factor <- factor(Y, levels = seq_len(J))

  data <- data.frame(Y = Y_factor) |>
    cbind(covariate_df)

  attr(data, "terms") <- terms(as.formula(formula_str), data = data)
  attr(data, "J") <- J
  attr(data, "n_obs") <- n

  data
}

# ── Data Spec Constructor ───────────────────────────────────────────────

#' Build a data_spec for the multinomial logistic regression model
#'
#' @param config  Simulation config list. Must contain a 'data' section.
#' @return        A \code{data_spec} object.
make_data <- function(config) {
  if (is.null(config$data)) {
    stop("Config must contain a 'data' section.", call. = FALSE)
  }

  likelyr::data_spec(
    name = "Multinomial logistic regression data",
    generate_data = generate_data
  )
}
