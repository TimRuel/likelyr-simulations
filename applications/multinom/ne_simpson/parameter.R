# ======================================================================
# Multinomial Parameter Specification (No Effects, Logit Parameterization)
# ======================================================================

# ----------------------------------------------------------------------
# Softmax (η → θ)
# ----------------------------------------------------------------------

softmax_from_eta <- function(eta) {
  z <- c(eta, 0)
  z <- z - max(z)
  exp_z <- exp(z)
  exp_z / sum(exp_z)
}

# ----------------------------------------------------------------------
# Convert θ → η (log-ratio relative to last category)
# ----------------------------------------------------------------------

theta_to_eta <- function(theta) {
  log(theta[-length(theta)] / theta[length(theta)])
}

# ----------------------------------------------------------------------
# Closed-form MLE:  η̂_j = log(n_j / n_J),  j = 1, …, J−1
# Small-count smoothing guards against zero cells.
# ----------------------------------------------------------------------

eta_mle_fn <- function(data) {
  delta <- 1e-8
  counts <- data$count
  J <- length(counts)
  theta_hat <- (counts + delta) / sum(counts + delta)
  eta_hat <- log(theta_hat[-J] / theta_hat[J])
  names(eta_hat) <- paste0("eta_", seq_len(J - 1L))
  eta_hat
}

# ----------------------------------------------------------------------
# Generate η₀ from a manually specified probability or count vector.
# ----------------------------------------------------------------------

generate_eta_0_manual <- function(param_cfg) {
  val <- param_cfg$theta_0

  if (is.null(val)) {
    stop("theta_0 must be supplied when mode = 'manual'.", call. = FALSE)
  }

  if (!is.numeric(val) || length(val) < 2L) {
    stop("theta_0 must be a numeric vector of length >= 2.", call. = FALSE)
  }

  if (any(val < 0)) {
    stop("theta_0 must be non-negative.", call. = FALSE)
  }

  is_counts <- any(val > 1)
  n_obs <- if (is_counts) as.integer(round(sum(val))) else NA_integer_

  theta <- val / sum(val)

  if (any(theta <= 0)) {
    stop("All entries of theta_0 must be strictly positive.", call. = FALSE)
  }

  eta_0 <- theta_to_eta(theta)
  names(eta_0) <- paste0("eta_", LETTERS[seq_len(length(theta) - 1L)])

  list(eta_0 = eta_0, J = length(theta), n_obs = n_obs)
}

# ----------------------------------------------------------------------
# Generate initial η₀ via Simpson-targeted Dirichlet family
# ----------------------------------------------------------------------

generate_eta_0 <- function(param_cfg) {
  param_dim <- param_cfg$param_dim
  J <- param_dim + 1

  # ------------------------------------------------------------
  # Determine Simpson index target
  # ------------------------------------------------------------

  D_min <- 1 / J
  D_max <- 1

  if (!is.null(param_cfg$index_target_frac)) {
    D_target <- D_min + param_cfg$index_target_frac * (D_max - D_min)
  } else if (!is.null(param_cfg$index_target)) {
    D_target <- param_cfg$index_target
  } else {
    stop(
      "Must supply index_target_frac or index_target in parameter config.",
      call. = FALSE
    )
  }

  if (D_target < D_min || D_target > D_max) {
    stop(
      sprintf(
        "Simpson index target must satisfy 1/J ≤ D ≤ 1 (got %.4f).",
        D_target
      ),
      call. = FALSE
    )
  }

  # ------------------------------------------------------------
  # Simpson index for one-big + uniform remainder family
  # ------------------------------------------------------------

  D_of_a <- function(a) {
    if (a <= 0 || a >= 1) {
      return(NA_real_)
    }

    p1 <- a
    prest <- (1 - a) / (J - 1)

    p1^2 + (J - 1) * prest^2
  }

  # ------------------------------------------------------------
  # Solve D(a) = D_target
  # ------------------------------------------------------------

  a_lower <- 1 / J
  a_upper <- 1 - 1e-8

  root <- uniroot(
    function(a) D_of_a(a) - D_target,
    lower = a_lower,
    upper = a_upper
  )

  a_star <- root$root

  # ------------------------------------------------------------
  # Construct probability vector
  # ------------------------------------------------------------

  theta_0 <- c(
    a_star,
    rep((1 - a_star) / (J - 1), J - 1)
  )

  # small jitter to avoid exact symmetry / boundary artifacts
  theta_0 <- theta_0 + runif(J, 0, 1e-6)
  theta_0 <- theta_0 / sum(theta_0)

  # ------------------------------------------------------------
  # Convert to logits
  # ------------------------------------------------------------

  eta_0 <- theta_to_eta(theta_0)

  names(eta_0) <- paste0("eta_", LETTERS[1:(J - 1)])

  eta_0
}

# ----------------------------------------------------------------------
# Generate η₀ from config or data: uniform placeholder of length J.
# J is read from param_cfg$J if data is NULL, otherwise from nrow(data).
# ----------------------------------------------------------------------

generate_eta_0_data <- function(param_cfg, data = NULL) {
  J <- if (!is.null(data)) nrow(data) else param_cfg$J

  if (is.null(J) || J < 2L) {
    stop(
      "mode = 'data' requires either data to be passed or J to be set in config.",
      call. = FALSE
    )
  }

  J <- as.integer(J)
  theta <- rep(1 / J, J)
  eta_0 <- theta_to_eta(theta)
  names(eta_0) <- paste0("eta_", seq_len(J - 1L))
  list(eta_0 = eta_0, J = J, n_obs = NA_integer_)
}

# ----------------------------------------------------------------------
# Parameter Spec Constructor
# ----------------------------------------------------------------------

make_parameter <- function(config, data = NULL) {
  param_cfg <- config$parameter

  if (is.null(param_cfg)) {
    stop("Config must contain a 'parameter' section.", call. = FALSE)
  }

  mode <- param_cfg$mode %||% "entropy"

  result <- switch(
    mode,
    manual  = generate_eta_0_manual(param_cfg),
    entropy = generate_eta_0_entropy(param_cfg),
    data    = generate_eta_0_data(param_cfg, data),
    stop(sprintf("Unknown parameter mode '%s'.", mode), call. = FALSE)
  )

  spec <- likelyr::parameter_spec(
    name = "Multinomial logits (no effects, baseline parameterization)",
    param_0 = result$eta_0,
    param_lower = rep(-Inf, length(result$eta_0)),
    param_upper = rep(Inf, length(result$eta_0)),
    param_mle_fn = eta_mle_fn,
    eq = NULL,
    eq_jac = NULL
  )

  spec$J <- result$J
  spec$n_obs <- result$n_obs

  spec
}