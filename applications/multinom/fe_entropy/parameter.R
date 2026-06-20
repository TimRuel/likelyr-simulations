# ======================================================================
# Fixed Effects Multinomial Parameter Specification
#
# Parameter: B — a (p x (J-1)) coefficient matrix, stored as a
#   length p*(J-1) vector (column-major: all coefficients for category
#   1, then category 2, ..., then category J-1).
#
# param_0 is a placeholder zero matrix of the correct dimension.
# The true B is unknown in the application context; param_0 is only
# used as an initialisation point and is not interpreted as a true value.
#
# x_0 is read from config$parameter$x_0 and attached to the spec so
# that make_estimand() and E_loglik() can access it without re-reading
# the config.
# ======================================================================

# ----------------------------------------------------------------------
# Softmax applied to a vector of J-1 log-ratios vs baseline category J
# ----------------------------------------------------------------------

softmax_from_eta <- function(eta) {
  z <- c(eta, 0)
  z <- z - max(z)
  exp_z <- exp(z)
  exp_z / sum(exp_z)
}

# ----------------------------------------------------------------------
# Compute theta(x; B) = softmax(x' B) for a single covariate vector x
# B_mat is (p x J-1)
# ----------------------------------------------------------------------

theta_at_x <- function(x, B_mat) {
  eta <- as.numeric(x %*% B_mat)
  softmax_from_eta(eta)
}

# ----------------------------------------------------------------------
# MLE of B via multinomial logistic regression
# ----------------------------------------------------------------------

B_mle_fn <- function(data) {
  counts <- data$counts # n x J matrix
  X <- data$X # n x p design matrix

  n <- nrow(counts)
  J <- ncol(counts)
  p <- ncol(X)

  # Row totals
  N_i <- rowSums(counts)

  # Initialise B at zero
  B_init <- rep(0, p * (J - 1L))

  # Negative log-likelihood
  neg_loglik <- function(b) {
    B_mat <- matrix(b, nrow = p, ncol = J - 1L)
    ll <- 0
    for (i in seq_len(n)) {
      eta_i <- as.numeric(X[i, ] %*% B_mat)
      theta_i <- softmax_from_eta(eta_i)
      ll <- ll + sum(counts[i, ] * log(pmax(theta_i, 1e-300)))
    }
    -ll
  }

  res <- optim(
    par = B_init,
    fn = neg_loglik,
    method = "BFGS",
    control = list(reltol = 1e-10, maxit = 1000)
  )

  res$par
}

# ----------------------------------------------------------------------
# Parse x_0 from config
# ----------------------------------------------------------------------

parse_x_0 <- function(param_cfg, data) {
  x_0_cfg <- param_cfg$x_0

  if (is.null(x_0_cfg)) {
    stop(
      "parameter.x_0 must be specified for the fixed effects model.",
      call. = FALSE
    )
  }

  x_0_df <- data.frame(
    A1 = as.numeric(x_0_cfg$A1),
    Moisture = factor(x_0_cfg$Moisture, levels = c("1", "2", "4", "5")),
    Management = factor(x_0_cfg$Management, levels = c("BF", "HF", "NM", "SF")),
    Use = factor(x_0_cfg$Use, levels = c("Hayfield", "Haypastu", "Pasture")),
    Manure = factor(x_0_cfg$Manure, levels = c("0", "1", "2", "3", "4"))
  )

  X_0 <- model.matrix(
    ~ A1 + Moisture + Management + Use + Manure,
    data = x_0_df
  )

  as.numeric(X_0[1L, ])
}

# ----------------------------------------------------------------------
# Parameter Spec Constructor
# ----------------------------------------------------------------------

make_parameter <- function(config, data = NULL) {
  param_cfg <- config$parameter

  if (is.null(param_cfg)) {
    stop("Config must contain a 'parameter' section.", call. = FALSE)
  }

  if (is.null(data)) {
    stop(
      "make_parameter() requires data for the fixed effects model.",
      call. = FALSE
    )
  }

  J <- ncol(data$counts)
  p <- ncol(data$X)

  x_0 <- parse_x_0(param_cfg, data)

  # Placeholder zero initialisation — not interpreted as a true value
  B_init <- rep(0, p * (J - 1L))

  spec <- likelyr::parameter_spec(
    name = "Multinomial logit coefficients (fixed effects)",
    param_0 = B_init,
    param_lower = rep(-Inf, length(B_init)),
    param_upper = rep(Inf, length(B_init)),
    param_mle_fn = B_mle_fn,
    eq = NULL,
    eq_jac = NULL
  )

  spec$J <- J
  spec$p <- p
  spec$x_0 <- x_0
  spec$n_obs <- nrow(data$counts)

  spec
}
