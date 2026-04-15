# ======================================================================
# Multinomial Parameter Specification (Logit Parameterization)
# Target: Simpson's Index D = sum(p_j^2)
# ======================================================================

# ----------------------------------------------------------------------
# Softmax (η → θ)
# ----------------------------------------------------------------------

softmax_from_eta <- function(eta) {
  z <- c(eta, 0) # baseline category
  z <- z - max(z) # numerical stability
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
# Closed-form MLE in η-space
# θ̂_j = n_j / N
# η̂_j = log(θ̂_j / θ̂_J)
# ----------------------------------------------------------------------

eta_mle_fn <- function(data) {
  delta <- 1e-8

  counts <- data$count

  J <- length(counts)

  smoothed_counts <- counts + delta

  theta_hat <- smoothed_counts / sum(smoothed_counts)

  eta_hat <- theta_to_eta(theta_hat)

  names(eta_hat) <- paste0("eta_", seq_len(J - 1))

  eta_hat
}

# ----------------------------------------------------------------------
# Parameter Spec Constructor
# ----------------------------------------------------------------------

make_parameter <- function(config) {
  param_cfg <- config$parameter

  if (is.null(param_cfg)) {
    stop("Config must contain a 'parameter' section.", call. = FALSE)
  }

  set.seed(param_cfg$seed)

  eta_0 <- generate_eta_0(param_cfg)

  likelyr::parameter_spec(
    name = "Multinomial logits",
    param_mle_fn = eta_mle_fn,
    param_0 = eta_0,
    param_lower = "auto",
    param_upper = "auto",
    eq = NULL,
    eq_jac = NULL
  )
}
