loglik <- function(param, data) {
  sum(data$count * log(param))
}

theta_mle_fn <- function(data) {
  data$count / sum(data$count)
}

make_likelihood <- function(config) {
  cfg <- config$likelihood
  if (is.null(cfg)) {
    stop("Config must contain a 'likelihood' section.", call. = FALSE)
  }

  likelihood_spec(
    name = cfg$name %||% "Multinomial likelihood",
    loglik = loglik,
    param_mle_fn = theta_mle_fn
  )
}
