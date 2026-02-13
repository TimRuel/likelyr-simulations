# ======================================================================
# Multinomial Parameter Specification (Logit Parameterization)
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
# Generate initial η₀ via Dirichlet → logit transform
# ----------------------------------------------------------------------

generate_eta_0 <- function(param_cfg) {
  J <- param_cfg$J

  # ------------------------------------------------------------
  # Determine entropy target
  # ------------------------------------------------------------

  H_max <- log(J)

  if (!is.null(param_cfg$entropy_target_frac)) {
    H_target <- param_cfg$entropy_target_frac * H_max
  } else if (!is.null(param_cfg$entropy_target)) {
    H_target <- param_cfg$entropy_target
  } else {
    stop(
      "Must supply entropy_target_frac or entropy_target in parameter config.",
      call. = FALSE
    )
  }

  # ------------------------------------------------------------
  # Entropy function for one-big + uniform remainder family
  # ------------------------------------------------------------

  H_of_a <- function(a) {
    if (a <= 0 || a >= 1) {
      return(-Inf)
    }

    p1 <- a
    prest <- (1 - a) / (J - 1)

    -p1 * log(p1) - (J - 1) * prest * log(prest)
  }

  # ------------------------------------------------------------
  # Solve H(a) = H_target
  # ------------------------------------------------------------

  # lower bound slightly above 1/J (uniform)
  a_lower <- 1 / J
  a_upper <- 1 - 1e-8

  root <- uniroot(
    function(a) H_of_a(a) - H_target,
    lower = a_lower,
    upper = a_upper
  )

  a_star <- root$root

  theta_0 <- c(
    a_star,
    rep((1 - a_star) / (J - 1), J - 1)
  )

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
