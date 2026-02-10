psi_fn <- function(param, data) {
  -sum(param * log(param))
}

psi_jac <- function(param, data) {
  -(log(param) + 1)
}

search_interval_fn <- function(param_mle, data) {
  c(0, log(length(param_mle)))
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
    name = "Entropy (psi)"
  )
}
