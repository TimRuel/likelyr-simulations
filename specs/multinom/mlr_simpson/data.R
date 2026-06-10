# ======================================================================
# Data Generation (Multinomial Logistic Regression Parameterization)
#
# Generates data for a multinomial logistic regression model. Supports
# both continuous and factor covariates. The reference covariate vector
# x_0 is determined at generation time from the config reference level
# and stored as attr(data, "x_0"), making it fixed and independent of
# the observed sample. This eliminates the mismatch between the true
# psi_0 (defined conditionally at the config reference level) and the
# estimated psi_hat (evaluated at the same fixed x_0) that arises when
# x_0 = colMeans(X_design) is used as a random, data-dependent reference.
#
# Predictor config structure:
#
#   Continuous predictor:
#     predictors:
#       - symbol: X1
#         distribution: {name: rnorm, args: [0.0, 1.0]}
#
#   Factor predictor:
#     predictors:
#       - symbol: habitat
#         type: factor
#         levels: [forest, grassland, wetland, scrub]
#         reference: forest
#         probabilities: [0.25, 0.25, 0.25, 0.25]   # optional, uniform default
#
# The reference level of each factor predictor defines x_0 via the
# model formula's design matrix. For continuous predictors, the
# reference is taken as 0 (centering is assumed or irrelevant when
# discrete factors carry all the structural interest).
#
# Parameterization convention:
#   Category 1 is the baseline; its coefficient vector is fixed at 0.
#   The free coefficient matrix is B = [β_2 | ... | β_J] ∈ R^{p×(J-1)},
#   stored as vec(B) = (β_2^T, ..., β_J^T)^T ∈ R^{p(J-1)}.
#
# Zero-count categories:
#   One pseudo-observation is added per absent category, placed at the
#   observed covariate column means (for continuous) or the reference
#   level (for factor). The "n_obs" attribute records the original
#   observation count so that the stored x_0 is used, not recomputed.
# ======================================================================

# ── Design matrix helpers ───────────────────────────────────────────────

get_X_design <- function(data) {
  trms <- attr(data, "terms")
  model_frame <- model.frame(trms, data = data)
  X_design <- model.matrix(trms, data = model_frame)
  attr(X_design, "original_model_frame") <- model_frame
  attr(X_design, "terms") <- trms
  attr(X_design, "formula") <- formula(trms)
  X_design
}

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

#' Draw n values from a distribution specified in config (numeric predictors)
draw_from <- function(dist_cfg, n = 1L) {
  fn <- match.fun(dist_cfg$name)
  args <- c(list(n), dist_cfg$args)
  do.call(fn, args)
}

#' Numerically stable softmax over a single numeric vector (length J)
softmax_scalar <- function(x) {
  z <- x - max(x)
  exp(z) / sum(exp(z))
}

#' Generate n covariate values for one predictor (numeric or factor)
generate_covariate <- function(pred, n) {
  if (identical(pred$type, "factor")) {
    probs <- pred$probabilities %||%
      rep(1 / length(pred$levels), length(pred$levels))
    factor(
      sample(pred$levels, n, replace = TRUE, prob = probs),
      levels = pred$levels
    )
  } else {
    draw_from(pred$distribution, n)
  }
}

#' Build the reference covariate row from the config predictor specs
#'
#' For factor predictors, uses pred$reference. For numeric predictors,
#' uses 0. Returns a single-row data frame with correct types for use
#' with model.matrix via get_X_design.
#'
#' @param config  Simulation config list.
#' @return        Named list suitable for as.data.frame.
build_reference_row <- function(config) {
  lapply(config$data$predictors, function(pred) {
    if (identical(pred$type, "factor")) {
      factor(pred$reference, levels = pred$levels)
    } else {
      0
    }
  }) |>
    setNames(sapply(config$data$predictors, `[[`, "symbol"))
}

# ── Data generation ─────────────────────────────────────────────────────

#' Generate multinomial logistic regression data
#'
#' Supports continuous and factor predictors. Stores attr(data, "x_0")
#' as the design vector for the config reference level — fixed across
#' all datasets from the same population. This ensures psi_fn and
#' psi_jac evaluate at the same reference as psi_0.
#'
#' @param config     Simulation config list.
#' @param parameter  Parameter spec object with param_0 set.
#' @return           Data frame with "terms", "J", "n_obs", "x_0" attributes.
generate_data <- function(config, parameter) {
  data_cfg <- config$data
  param_cfg <- config$parameter
  n <- data_cfg$n_obs
  formula_str <- data_cfg$formula
  J <- param_cfg$J

  # ── Draw covariates ───────────────────────────────────────────────────
  covariate_df <- lapply(data_cfg$predictors, \(pred) {
    vals <- generate_covariate(pred, n)
    setNames(data.frame(vals), pred$symbol)
  }) |>
    do.call(what = cbind)

  # Fix factor levels after cbind (cbind can coerce factors to integers)
  for (pred in data_cfg$predictors) {
    if (identical(pred$type, "factor")) {
      covariate_df[[pred$symbol]] <- factor(
        covariate_df[[pred$symbol]],
        levels = pred$levels
      )
    }
  }

  # ── Build design matrix ───────────────────────────────────────────────
  tmp_data <- covariate_df
  tmp_data[["Y"]] <- factor(rep(1L, n), levels = seq_len(J))
  attr(tmp_data, "terms") <- terms(as.formula(formula_str), data = tmp_data)
  attr(tmp_data, "J") <- J

  X_design <- get_X_design(tmp_data)

  # ── Build fixed reference vector x_0 from config ─────────────────────
  # x_0 is determined by the predictor reference levels, not the sample.
  # This eliminates the psi_0 / psi_hat mismatch from x_0 = colMeans.
  ref_row <- build_reference_row(config)
  ref_df <- as.data.frame(ref_row)
  ref_df[["Y"]] <- factor(1L, levels = seq_len(J))
  attr(ref_df, "terms") <- terms(as.formula(formula_str), data = ref_df)
  attr(ref_df, "J") <- J
  x_0 <- as.numeric(get_X_design(ref_df))

  # ── Draw categorical responses ────────────────────────────────────────
  beta_0 <- matrix(parameter$param_0, nrow = ncol(X_design), ncol = J - 1L)
  eta <- X_design %*% beta_0
  probs <- t(apply(cbind(0, eta), 1, softmax_scalar))
  Y <- apply(probs, 1, \(p) sample.int(J, 1L, prob = p))

  # ── Add pseudo-obs for zero-count categories ──────────────────────────
  zero_cats <- setdiff(seq_len(J), unique(Y))
  if (length(zero_cats) > 0L) {
    # Place pseudo-obs at the reference level (factor) or column means (numeric)
    pseudo_row <- lapply(data_cfg$predictors, function(pred) {
      if (identical(pred$type, "factor")) {
        factor(pred$reference, levels = pred$levels)
      } else {
        mean(covariate_df[[pred$symbol]])
      }
    }) |>
      setNames(sapply(data_cfg$predictors, `[[`, "symbol"))
    pseudo_df <- as.data.frame(pseudo_row)[
      rep(1L, length(zero_cats)),
      ,
      drop = FALSE
    ]
    rownames(pseudo_df) <- NULL
    covariate_df <- rbind(covariate_df, pseudo_df)
    Y <- c(Y, zero_cats)
  }

  # ── Assemble data frame ───────────────────────────────────────────────
  Y_factor <- factor(Y, levels = seq_len(J))
  data <- data.frame(Y = Y_factor) |> cbind(covariate_df)

  # Re-attach factor levels after cbind
  for (pred in data_cfg$predictors) {
    if (identical(pred$type, "factor")) {
      data[[pred$symbol]] <- factor(data[[pred$symbol]], levels = pred$levels)
    }
  }

  attr(data, "terms") <- terms(as.formula(formula_str), data = data)
  attr(data, "J") <- J
  attr(data, "n_obs") <- n
  attr(data, "x_0") <- x_0 # fixed reference vector

  data
}

# ── Data Spec Constructor ───────────────────────────────────────────────

make_data <- function(config) {
  if (is.null(config$data)) {
    stop("Config must contain a 'data' section.", call. = FALSE)
  }
  likelyr::data_spec(
    name = "Multinomial logistic regression data",
    generate_data = generate_data
  )
}
