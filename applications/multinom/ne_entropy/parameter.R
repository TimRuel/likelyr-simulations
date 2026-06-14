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
# Generate η₀ via entropy-targeted one-big + uniform remainder family
# ----------------------------------------------------------------------

generate_eta_0_entropy <- function(param_cfg) {
  J <- param_cfg$J

  if (is.null(J) || J < 2L) {
    stop("J must be an integer >= 2.", call. = FALSE)
  }

  H_max <- log(J)

  if (!is.null(param_cfg$entropy_target_frac)) {
    H_target <- param_cfg$entropy_target_frac * H_max
  } else if (!is.null(param_cfg$entropy_target)) {
    H_target <- param_cfg$entropy_target
  } else {
    stop(
      "Must supply entropy_target_frac or entropy_target when mode = 'entropy'.",
      call. = FALSE
    )
  }

  if (H_target <= 0 || H_target >= H_max) {
    stop(
      sprintf(
        "entropy_target must satisfy 0 < H < log(J) = %.4f (got %.4f).",
        H_max,
        H_target
      ),
      call. = FALSE
    )
  }

  H_of_a <- function(a) {
    if (a <= 0 || a >= 1) {
      return(-Inf)
    }
    prest <- (1 - a) / (J - 1)
    -a * log(a) - (J - 1) * prest * log(prest)
  }

  root <- uniroot(
    function(a) H_of_a(a) - H_target,
    lower = 1 / J,
    upper = 1 - 1e-8
  )

  a_star <- root$root
  theta_0 <- c(a_star, rep((1 - a_star) / (J - 1), J - 1))
  theta_0 <- theta_0 + runif(J, 0, 1e-6)
  theta_0 <- theta_0 / sum(theta_0)

  eta_0 <- theta_to_eta(theta_0)
  names(eta_0) <- paste0("eta_", LETTERS[seq_len(J - 1L)])

  list(eta_0 = eta_0, J = J, n_obs = NA_integer_)
}

# ----------------------------------------------------------------------
# Generate η₀ from data: uniform placeholder of length J = nrow(data)
# ----------------------------------------------------------------------

generate_eta_0_data <- function(data) {
  J <- nrow(data)
  theta <- rep(1 / J, J)
  eta_0 <- theta_to_eta(theta)
  names(eta_0) <- paste0("eta_", LETTERS[seq_len(J - 1L)])
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
    manual = generate_eta_0_manual(param_cfg),
    entropy = generate_eta_0_entropy(param_cfg),
    data = {
      if (is.null(data)) {
        stop(
          "mode = 'data' requires data to be passed to make_parameter().",
          call. = FALSE
        )
      }
      generate_eta_0_data(data)
    },
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
