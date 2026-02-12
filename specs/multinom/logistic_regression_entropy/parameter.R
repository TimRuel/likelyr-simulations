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
  alpha <- param_cfg$alpha %||% 2

  theta_0 <- LaplacesDemon::rdirichlet(1, rep(alpha, J)) |>
    as.numeric()

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
