# ======================================================================
# Estimand Specification — Shannon Entropy at x_0
#   ψ(b) = H(θ(x_0; B)) = −Σ p_j(x_0; B) log p_j(x_0; B)
#
# param is the vectorised B matrix (length p*(J-1)).
# x_0 is retrieved from the parameter spec passed to make_estimand().
# ======================================================================

make_psi_fns <- function(x_0, J, p) {
  psi_fn <- function(param, data = NULL) {
    B_mat <- matrix(param, nrow = p, ncol = J - 1L)
    eta <- as.numeric(x_0 %*% B_mat)
    theta <- softmax_from_eta(eta)
    -sum(theta * log(theta))
  }

  # ∂H/∂b = vec( x_0 * (∂H/∂eta)' )
  # ∂H/∂eta_k = -theta_k (log theta_k + H)  for k = 1,...,J-1
  psi_jac <- function(param, data = NULL) {
    B_mat <- matrix(param, nrow = p, ncol = J - 1L)
    eta <- as.numeric(x_0 %*% B_mat)
    theta <- softmax_from_eta(eta)
    H <- -sum(theta * log(theta))
    dH_eta <- -theta[-J] * (log(theta[-J]) + H)
    as.numeric(outer(x_0, dH_eta))
  }

  list(psi_fn = psi_fn, psi_jac = psi_jac)
}

search_interval_fn <- function(param_mle, data) {
  # param_mle is vec(B_hat); J and p recovered from dimensions via data
  # psi lives in (0, log(J)) regardless of parameterisation
  J <- data$J %||% stop("data$J required by search_interval_fn.")
  c(0, log(J))
}

make_estimand <- function(config, parameter = NULL, ...) {
  cfg <- config$estimand

  if (is.null(cfg)) {
    stop("Config must contain an 'estimand' section.", call. = FALSE)
  }

  if (is.null(parameter)) {
    stop(
      "make_estimand() requires the parameter spec for the fixed effects model.",
      call. = FALSE
    )
  }

  J <- parameter$J
  p <- parameter$p
  x_0 <- parameter$x_0

  if (is.null(J) || is.null(p) || is.null(x_0)) {
    stop(
      "parameter spec must have $J, $p, and $x_0 set before make_estimand().",
      call. = FALSE
    )
  }

  fns <- make_psi_fns(x_0, J, p)

  required <- list(
    increment = cfg$increment,
    confidence_levels = cfg$confidence_levels,
    gamma = cfg$gamma,
    cutoff_buffer = cfg$cutoff_buffer,
    uniroot_expand_factor = cfg$uniroot_expand_factor,
    psi_lower = cfg$psi_lower
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
    psi_fn = fns$psi_fn,
    psi_jac = fns$psi_jac,
    search_interval_fn = search_interval_fn,
    increment = required$increment,
    confidence_levels = required$confidence_levels,
    gamma = required$gamma,
    cutoff_buffer = required$cutoff_buffer,
    uniroot_expand_factor = required$uniroot_expand_factor,
    psi_lower = required$psi_lower,
    psi_upper = log(J),
    name = "Shannon entropy at x_0"
  )
}
