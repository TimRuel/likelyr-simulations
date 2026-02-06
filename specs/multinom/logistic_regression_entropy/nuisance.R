E_loglik <- function(param, omega_hat, data) {
  sum(omega_hat * log(param))
}

E_loglik_grad <- function(param, omega_hat, data) {
  omega_hat / param
}

make_nuisance <- function(config) {
  cfg <- config$nuisance
  if (is.null(cfg)) {
    stop("Config must contain a 'nuisance' section.", call. = FALSE)
  }

  nuisance_spec(
    name = cfg$name %||% "Poisson rates weighted sum nuisance parameter (ZSE)",
    E_loglik = E_loglik,
    E_loglik_grad = E_loglik_grad
  )
}
