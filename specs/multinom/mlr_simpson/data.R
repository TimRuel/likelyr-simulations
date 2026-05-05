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
# Config structure:
#   model:
#     formula: "Y ~ X1 + X2"
#   parameter:
#     J: 5
#     index_target_frac: 0.10
#     coefficient_distribution:
#       name: rnorm
#       args: [0.0, 0.5]
#   data:
#     n_obs: 20
#     epsilon: 0.5
#     predictors:
#       - symbol: X1
#         distribution: {name: rexp, args: [1.0]}
#       - symbol: X2
#         distribution: {name: rnorm, args: [0.0, 1.0]}
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
#' @return      n x (J-1) integer indicator matrix (baseline category J dropped).
get_Y_design <- function(data) {
  trms <- attr(data, "terms")
  response <- all.vars(trms)[attr(trms, "response")]
  y <- data[[response]]
  J <- attr(data, "J")
  y_int <- as.integer(as.character(y))
  non_baseline <- seq_len(J - 1L)

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
#' generate categorical responses. Applies epsilon smoothing to ensure
#' all categories appear at least once.
#'
#' @param config     Simulation config list.
#' @param parameter  Parameter spec object with param_0 set.
#' @return           A data frame with response Y and all covariates, with
#'   attributes "terms" and "J" set.
generate_data <- function(config, parameter) {
  data_cfg <- config$data
  model_cfg <- config$model
  param_cfg <- config$parameter
  n <- data_cfg$n_obs
  epsilon <- data_cfg$epsilon %||% 0
  formula_str <- model_cfg$formula
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
  beta_0 <- matrix(parameter$param_0, nrow = ncol(X_design), ncol = J - 1L)
  eta <- X_design %*% beta_0
  probs <- t(apply(cbind(eta, 0), 1, softmax_scalar))
  Y <- apply(probs, 1, \(p) sample.int(J, 1L, prob = p))

  # ── Epsilon smoothing: add pseudo-obs for zero-count categories ───────
  if (epsilon > 0) {
    zero_cats <- setdiff(seq_len(J), unique(Y))
    if (length(zero_cats) > 0L) {
      pseudo <- covariate_df[rep(1L, length(zero_cats)), , drop = FALSE]
      for (sym in names(covariate_df)) {
        pseudo[[sym]] <- mean(covariate_df[[sym]])
      }
      rownames(pseudo) <- NULL
      covariate_df <- rbind(covariate_df, pseudo)
      Y <- c(Y, zero_cats)
    }
  }

  # ── Assemble final data frame ─────────────────────────────────────────
  data <- cbind(data.frame(Y = factor(Y, levels = seq_len(J))), covariate_df)
  attr(data, "terms") <- terms(as.formula(formula_str), data = data)
  attr(data, "J") <- J

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
