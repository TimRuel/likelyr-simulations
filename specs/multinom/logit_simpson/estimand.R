psi_fn <- function(param, data = NULL) {
  p <- softmax_from_eta(param)
  sum(p^2)
}

psi_jac <- function(param, data = NULL) {
  p <- softmax_from_eta(param)

  D <- sum(p^2)

  # gradient wrt first J-1 logits
  grad <- 2 * p[-length(p)] * (p[-length(p)] - D)

  grad
}

search_interval_fn <- function(param_mle, data) {
  J <- length(param_mle) + 1
  c(1 / J, 1)
}

make_estimand <- function(config) {
  cfg <- config$estimand
  if (is.null(cfg)) {
    stop("Config must contain an 'estimand' section.", call. = FALSE)
  }

  increment <- cfg$increment
  confidence_levels <- cfg$confidence_levels
  gamma <- cfg$gamma
  cutoff_buffer <- cfg$cutoff_buffer
  uniroot_expand_factor <- cfg$uniroot_expand_factor
  psi_lower <- cfg$psi_lower
  psi_upper <- log(config$parameter$J)

  if (
    any(vapply(
      list(
        increment,
        confidence_levels,
        gamma,
        cutoff_buffer,
        uniroot_expand_factor,
        psi_lower,
        psi_upper
      ),
      is.null,
      logical(1)
    ))
  ) {
    stop(
      "All estimand tuning parameters must be specified in config.",
      call. = FALSE
    )
  }

  estimand_spec(
    psi_fn = psi_fn,
    psi_jac = psi_jac,
    search_interval_fn = search_interval_fn,
    increment = increment,
    confidence_levels = confidence_levels,
    gamma = gamma,
    cutoff_buffer = cutoff_buffer,
    uniroot_expand_factor = uniroot_expand_factor,
    psi_lower = psi_lower,
    psi_upper = psi_upper,
    name = "Simpson's index (psi)"
  )
}
