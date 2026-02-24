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
  J <- param_cfg$J

  if (J < 2) {
    stop("J must be at least 2.", call. = FALSE)
  }

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
# Parameter Spec Constructor
# ----------------------------------------------------------------------

make_parameter <- function(config) {
  param_cfg <- config$parameter

  if (is.null(param_cfg)) {
    stop("Config must contain a 'parameter' section.", call. = FALSE)
  }

  set.seed(param_cfg$seed)

  eta_0 <- generate_eta_0(param_cfg)

  parameter_spec(
    name = "Multinomial logits (baseline parameterization)",
    param_0 = eta_0,
    param_lower = rep(-Inf, length(eta_0)),
    param_upper = rep(Inf, length(eta_0)),
    eq = NULL,
    eq_jac = NULL
  )
}
