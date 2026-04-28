# ======================================================================
# Data Generation (Multinomial, Logit Parameterization)
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
# Generate multinomial data
# ----------------------------------------------------------------------

generate_data <- function(config, parameter) {
  data_cfg <- config$data
  n <- data_cfg$n_obs
  epsilon <- data_cfg$epsilon

  # param_0 is η₀ (length J-1)
  eta_0 <- parameter$param_0

  # Convert to probabilities
  theta_0 <- softmax_from_eta(eta_0)

  J <- length(theta_0)

  counts <- rmultinom(1, n, theta_0) |>
    as.numeric()

  counts <- counts + epsilon * (counts == 0)

  data.frame(
    cell = LETTERS[seq_len(J)],
    count = counts,
    row.names = NULL
  )
}
