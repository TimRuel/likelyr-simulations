# ======================================================================
# Data Generation (Multinomial, Logit Parameterization)
# ======================================================================

# ----------------------------------------------------------------------
# Generate multinomial data
# ----------------------------------------------------------------------

generate_data <- function(config, parameter) {
  data_cfg <- config$data
  n <- data_cfg$n_obs

  # param_0 is η₀ (length J-1)
  eta_0 <- parameter$param_0

  # Convert to probabilities
  theta_0 <- softmax_from_eta(eta_0)

  J <- length(theta_0)

  counts <- rmultinom(1, n, theta_0) |>
    as.numeric()

  data.frame(
    cell = LETTERS[seq_len(J)],
    count = counts,
    row.names = NULL
  )
}
