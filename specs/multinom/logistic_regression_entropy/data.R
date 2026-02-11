generate_data <- function(config, parameter) {
  data_cfg <- config$data
  n <- data_cfg$n_obs

  theta_0 <- parameter$param_0

  J <- length(theta_0)

  counts <- rmultinom(1, n, theta_0) |>
    c()

  data.frame(
    cell = names(theta_0),
    count = counts,
    row.names = NULL
  )
}
