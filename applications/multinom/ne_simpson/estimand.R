# ======================================================================
# Estimand Specification — Simpson's Index
#   ψ(η) = D(θ(η)) = Σ p_j^2
# ======================================================================

psi_fn <- function(param, data = NULL) {
  p <- softmax_from_eta(param)
  sum(p^2)
}

psi_jac <- function(param, data = NULL) {
  p <- softmax_from_eta(param)
  D <- sum(p^2)
  2 * p[-length(p)] * (p[-length(p)] - D)
}

make_estimand <- function(config, parameter = NULL, ...) {
  cfg <- config$estimand

  if (is.null(cfg)) {
    stop("Config must contain an 'estimand' section.", call. = FALSE)
  }

  J <- parameter$J

  if (is.null(J)) {
    stop(
      "J could not be determined: supply parameter.J in config, ",
      "set parameter.mode = 'manual' with a theta_0 vector, or ",
      "pass the parameter spec to make_estimand().",
      call. = FALSE
    )
  }

  required <- list(
    increment = cfg$increment,
    confidence_levels = cfg$confidence_levels,
    gamma = cfg$gamma,
    cutoff_buffer = cfg$cutoff_buffer,
    uniroot_expand_factor = cfg$uniroot_expand_factor,
    psi_lower = 1 / J
  )

  missing <- names(Filter(is.null, required))

  if (length(missing) > 0L) {
    stop(
      paste0(
        "Missing estimand tuning parameters: ",
        paste(missing, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  likelyr::estimand_spec(
    psi_fn = psi_fn,
    psi_jac = psi_jac,
    increment = required$increment,
    confidence_levels = required$confidence_levels,
    gamma = required$gamma,
    cutoff_buffer = required$cutoff_buffer,
    uniroot_expand_factor = required$uniroot_expand_factor,
    psi_lower = required$psi_lower,
    psi_upper = 1.0,
    psi_closed = c(lower = TRUE, upper = FALSE),
    name = "Simpson's index (psi)"
  )
}