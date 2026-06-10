# ======================================================================
# Multinomial Parameter Specification (Logistic Regression Parameterization)
#
# The model parameter is the matrix of regression coefficients
#   B = [β_2 | ... | β_J] ∈ R^{p × (J-1)},
# stored as a flat vector in column-major order:
#   θ = vec(B) = (β_2^T, ..., β_J^T)^T ∈ R^{p(J-1)}.
#
# Category 1 is the baseline; its coefficient vector is fixed at 0.
#
# Estimand and reference covariate:
#   The parameter of interest is Simpson's index at a fixed reference
#   covariate vector x_0, determined by the config predictor reference
#   levels (e.g., x_0 = (1,0,0,0)^T for the reference level of a
#   four-level factor). x_0 is fixed across all datasets from the same
#   population — it is not computed from observed data.
#
#   psi(B) = D(theta(x_0; B)) = sum_j theta_j(x_0; B)^2
#
#   psi_0 = D(theta(x_0; B_0))
#
#   Both are evaluated at the same fixed x_0, so there is no mismatch
#   between the true parameter value and the estimand as n grows.
#
# True coefficient generation:
#   build_x_reference() — construct x_0 from config reference levels
#   generate_beta_0()   — find B satisfying D(theta(x_0; B)) = psi_target
#
#   Only the component of B along x_0 (i.e., x_0^T B) is constrained
#   by psi_target. The remaining components are drawn from the config
#   coefficient distribution, giving a family of B matrices that all
#   achieve psi_target at x_0 while varying freely elsewhere.
#
# MLE stabilization:
#   beta_mle_fn() augments the data with one pseudo-observation per
#   zero-count category before fitting nnet::multinom. Scoped to MLE
#   fitting only; does not affect the likelihood or sieve.
# ======================================================================

# ── Helpers (shared with data.R) ────────────────────────────────────────

draw_from <- function(dist_cfg, n = 1L) {
  fn <- match.fun(dist_cfg$name)
  args <- c(list(n), dist_cfg$args)
  do.call(fn, args)
}

softmax_scalar <- function(x) {
  z <- x - max(x)
  exp(z) / sum(exp(z))
}

# ── Reference covariate construction ───────────────────────────────────

#' Build the fixed reference covariate vector x_0 from config
#'
#' Constructs x_0 as the design vector corresponding to the reference
#' level of each factor predictor and 0 for each numeric predictor.
#' For a four-level factor (forest/grassland/wetland/scrub) with forest
#' as the reference, x_0 = (1, 0, 0, 0)^T (intercept plus three zeros).
#'
#' This vector is fixed for all datasets generated from the same config,
#' ensuring psi_0 = D(theta(x_0; B_0)) and psi_hat = D(theta(x_0; B_hat))
#' target the same population quantity.
#'
#' @param config  Simulation config list.
#' @return        Numeric vector of length p (design vector at reference level).
build_x_reference <- function(config) {
  J <- config$parameter$J
  formula_str <- config$data$formula
  pred_list <- config$data$predictors

  ref_row <- lapply(pred_list, function(pred) {
    if (identical(pred$type, "factor")) {
      factor(pred$reference, levels = pred$levels)
    } else {
      0
    }
  }) |>
    setNames(sapply(pred_list, `[[`, "symbol"))

  ref_df <- as.data.frame(ref_row)
  ref_df[["Y"]] <- factor(1L, levels = seq_len(J))
  attr(ref_df, "terms") <- terms(as.formula(formula_str), data = ref_df)
  attr(ref_df, "J") <- J

  as.numeric(get_X_design(ref_df))
}

# ── Monte Carlo helpers (retained for random effects model compat.) ─────

draw_X_mc <- function(config, N = 1e5L) {
  data_cfg <- config$data
  J <- config$parameter$J
  formula_str <- data_cfg$formula

  covariate_df <- lapply(data_cfg$predictors, \(pred) {
    if (identical(pred$type, "factor")) {
      probs <- pred$probabilities %||%
        rep(1 / length(pred$levels), length(pred$levels))
      vals <- factor(
        sample(pred$levels, N, replace = TRUE, prob = probs),
        levels = pred$levels
      )
    } else {
      vals <- draw_from(pred$distribution, N)
    }
    setNames(data.frame(vals), pred$symbol)
  }) |>
    do.call(what = cbind)

  tmp_data <- covariate_df
  tmp_data[["Y"]] <- factor(rep(1L, N), levels = seq_len(J))
  attr(tmp_data, "terms") <- terms(as.formula(formula_str), data = tmp_data)
  attr(tmp_data, "J") <- J

  get_X_design(tmp_data)
}

compute_theta_bar <- function(param, X_design) {
  p <- ncol(X_design)
  beta_mat <- matrix(param, nrow = p)
  eta_aug <- cbind(0, X_design %*% beta_mat)
  exp_eta <- exp(eta_aug - apply(eta_aug, 1, max))
  probs <- exp_eta / rowSums(exp_eta)
  colMeans(probs)
}

# ── True coefficient generation ─────────────────────────────────────────

#' Find B_0 satisfying D(theta(x_0; B_0)) = psi_target
#'
#' The conditional Simpson's index at x_0 depends only on x_0^T B.
#' The optimizer finds B such that D(softmax(c(0, x_0^T B))) = psi_target.
#' Components of B orthogonal to x_0 are drawn from the config coefficient
#' distribution — they affect the likelihood structure (nuisance parameter
#' richness) but not the estimand at x_0.
#'
#' @param config      Simulation config list.
#' @param x_0         Numeric vector of length p (fixed reference vector).
#' @param psi_target  Target Simpson's index value.
#' @param tol         Convergence tolerance. Default: 1e-4.
#' @param max_tries   Maximum warm-start retries. Default: 20.
#' @return            p x (J-1) coefficient matrix [β_2,...,β_J].
generate_beta_0 <- function(
  config,
  x_0,
  psi_target,
  tol = 1e-4,
  max_tries = 20L
) {
  J <- config$parameter$J
  coef_dist <- config$parameter$coefficient_distribution
  p <- length(x_0)
  np <- p * (J - 1L)

  objective <- function(b) {
    beta_mat <- matrix(b, nrow = p, ncol = J - 1L)
    eta <- as.numeric(x_0 %*% beta_mat)
    theta <- softmax_scalar(c(0, eta))
    (sum(theta^2) - psi_target)^2
  }

  for (attempt in seq_len(max_tries)) {
    fit <- optim(
      par = draw_from(coef_dist, np),
      fn = objective,
      method = "L-BFGS-B",
      control = list(maxit = 2000, factr = 1e6)
    )

    beta_mat <- matrix(fit$par, nrow = p, ncol = J - 1L)
    eta <- as.numeric(x_0 %*% beta_mat)
    theta <- softmax_scalar(c(0, eta))
    psi_achieved <- sum(theta^2)

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

beta_mle_fn <- function(data) {
  J <- attr(data, "J")
  n_obs <- attr(data, "n_obs")
  formula_str <- formula(attr(data, "terms"))

  y <- as.integer(as.character(data$Y[seq_len(n_obs)]))
  zero_cats <- setdiff(seq_len(J), unique(y))

  data_fit <- if (length(zero_cats) > 0L) {
    obs_data <- data[seq_len(n_obs), , drop = FALSE]
    covariate_cols <- setdiff(names(obs_data), "Y")
    # Pseudo-obs at reference level for factors, column mean for numeric
    col_refs <- lapply(covariate_cols, function(nm) {
      col <- obs_data[[nm]]
      if (is.factor(col)) levels(col)[1L] else mean(col)
    })
    names(col_refs) <- covariate_cols
    pseudo <- as.data.frame(col_refs)[
      rep(1L, length(zero_cats)),
      ,
      drop = FALSE
    ]
    # Restore factor levels
    for (nm in covariate_cols) {
      if (is.factor(obs_data[[nm]])) {
        pseudo[[nm]] <- factor(pseudo[[nm]], levels = levels(obs_data[[nm]]))
      }
    }
    rownames(pseudo) <- NULL
    pseudo$Y <- factor(zero_cats, levels = levels(data$Y))
    pseudo <- pseudo[c("Y", covariate_cols)]
    rbind(obs_data, pseudo)
  } else {
    data[seq_len(n_obs), , drop = FALSE]
  }

  fit <- nnet::multinom(
    formula_str,
    data = data_fit,
    maxit = 2000,
    decay = 0.01,
    trace = FALSE
  )

  t(coef(fit)) |> as.numeric()
}

# ── Parameter Spec Constructor ──────────────────────────────────────────

#' Build a parameter_spec for the multinomial logistic regression model
#'
#' psi_0 = D(theta(x_0; B_0)) is defined conditionally at the fixed
#' reference vector x_0 derived from the config predictor reference
#' levels. Both x_0 and psi_0 are stored as extra fields so that
#' make_estimand() can use them without accessing observed data.
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

  # Fixed reference vector: determined by config, not observed data
  x_0 <- build_x_reference(config)
  beta_0 <- generate_beta_0(config, x_0, psi_target)

  # psi_0: conditional at fixed x_0 — no Monte Carlo needed
  eta_0 <- as.numeric(x_0 %*% beta_0)
  theta_0 <- softmax_scalar(c(0, eta_0))
  psi_0 <- sum(theta_0^2)

  likelyr::parameter_spec(
    name = "Multinomial logistic regression coefficients",
    param_mle_fn = beta_mle_fn,
    param_0 = as.numeric(beta_0),
    param_lower = NULL,
    param_upper = NULL,
    omega_dim = J,
    eq = NULL,
    eq_jac = NULL,
    x_0 = x_0, # fixed reference vector for estimand evaluation
    psi_0 = psi_0 # D(theta(x_0; B_0)), pre-computed
  )
}
