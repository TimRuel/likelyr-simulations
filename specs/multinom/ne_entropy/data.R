# ======================================================================
# Data Generation (No Effects Multinomial, Logit Parameterization)
# ======================================================================

generate_data <- function(config, parameter) {
  n <- parameter$n_obs %||% config$data$n_obs

  if (is.null(n) || is.na(n)) {
    stop(
      "n_obs could not be determined: supply data.n_obs in config or ",
      "pass counts (not probabilities) to parameter.theta_0.",
      call. = FALSE
    )
  }

  eta_0 <- parameter$param_0
  theta_0 <- softmax_from_eta(eta_0)
  J <- length(theta_0)

  counts <- as.numeric(rmultinom(1L, n, theta_0))

  data.frame(
    cell = LETTERS[seq_len(J)],
    count = counts,
    row.names = NULL
  )
}
