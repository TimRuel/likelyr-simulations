generate_theta_0 <- function(param_cfg) {
  J <- param_cfg$J
  theta_0 <- LaplacesDemon::rdirichlet(1, rep(1, J)) |>
    as.numeric()
  names(theta_0) <- LETTERS[1:J]
  theta_0
}

make_parameter <- function(config) {
  param_cfg <- config$parameter
  if (is.null(param_cfg)) {
    stop("Config must contain a 'parameter' section.", call. = FALSE)
  }

  set.seed(param_cfg$seed)

  theta_0 <- generate_theta_0(param_cfg)

  parameter_spec(
    name = "Multinomial cell probabilities",
    param_0 = theta_0,
  )
}
