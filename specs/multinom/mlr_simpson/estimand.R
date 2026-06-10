# ======================================================================
# Estimand Specification (Multinomial Logistic Regression Parameterization)
# Target: Simpson's Index D(theta(x_0; B)) = sum(theta_j(x_0; B)^2)
#
# The reference covariate x_0 is the fixed design vector corresponding
# to the config predictor reference levels (e.g., the reference level
# of a factor covariate). It is stored as attr(data, "x_0") by
# generate_data() and as parameter$extra$x_0 by make_parameter().
#
# Using a fixed x_0 ensures that:
#   - psi_0 = D(theta(x_0; B_0))  [defined in make_parameter]
#   - psi_hat = D(theta(x_0; B_hat))  [evaluated at the same x_0]
# are targeting the same quantity, eliminating the coverage collapse
# that occurs when x_0 = colMeans(X_design) varies across datasets.
#
# x_reference() returns attr(data, "x_0") when present (fixed reference),
# and falls back to colMeans(X_design[1:n_obs,]) for backward
# compatibility with configs that do not set a reference level.
#
# The Jacobian with respect to vec(B) is:
#   ∂D/∂β_k = x_0 · 2θ_k(θ_k - D),  k = 2,...,J
# giving outer(x_0, 2 * th[-1] * (th[-1] - D)) as a p x (J-1) matrix,
# flattened to a 1 x p(J-1) row vector.
#
# psi_0 is read from parameter$extra$psi_0 (precomputed in make_parameter)
# rather than recomputed here, to ensure full consistency.
# ======================================================================

softmax_scalar <- function(x) {
  z <- x - max(x)
  exp(z) / sum(exp(z))
}

#' Compute the reference covariate vector x_0
#'
#' Returns attr(data, "x_0") if set (fixed reference from config).
#' Falls back to colMeans of observed rows for backward compatibility.
#'
#' @param data  Data frame with optional "x_0", "terms", and "n_obs" attributes.
#' @return      Numeric vector of length p.
x_reference <- function(data) {
  x_0 <- attr(data, "x_0")
  if (!is.null(x_0)) {
    return(x_0)
  }
  # Backward-compatible fallback: sample mean of observed rows
  n_obs <- attr(data, "n_obs") %||% nrow(data)
  X_design <- get_X_design(data)
  colMeans(X_design[seq_len(n_obs), , drop = FALSE])
}

#' Conditional category probabilities at x_0 under B
theta_at_x0 <- function(param, x0, J) {
  p <- length(x0)
  beta <- matrix(param, nrow = p, ncol = J - 1L)
  eta <- as.numeric(x0 %*% beta)
  softmax_scalar(c(0, eta))
}

#' Simpson's index at the reference covariate x_0
psi_fn <- function(param, data) {
  J <- attr(data, "J")
  x0 <- x_reference(data)
  sum(theta_at_x0(param, x0, J)^2)
}

#' Jacobian of Simpson's index at x_0 with respect to vec(B)
psi_jac <- function(param, data) {
  J <- attr(data, "J")
  x0 <- x_reference(data)
  th <- theta_at_x0(param, x0, J)
  D <- sum(th^2)
  matrix(outer(x0, 2 * th[-1L] * (th[-1L] - D)), nrow = 1L)
}

#' Build optimized psi_fn and psi_jac closures for the auglag inner loop
#'
#' Closes over x_0 (from attr(data, "x_0") or colMeans fallback) and J,
#' avoiding x_reference(data) and get_X_design(data) calls on every
#' auglag function/gradient evaluation.
make_psi_fns <- function(data) {
  x0 <- x_reference(data)
  J <- attr(data, "J")

  list(
    psi_fn = function(param) {
      sum(theta_at_x0(param, x0, J)^2)
    },
    psi_jac = function(param) {
      th <- theta_at_x0(param, x0, J)
      D <- sum(th^2)
      matrix(outer(x0, 2 * th[-1L] * (th[-1L] - D)), nrow = 1L)
    }
  )
}

# ── Estimand Spec Constructor ───────────────────────────────────────────

#' Build an estimand_spec for Simpson's index under the MLR model
#'
#' psi_0 is read from parameter$extra$psi_0, which was precomputed in
#' make_parameter() as D(theta(x_0; B_0)) at the fixed reference x_0.
#' This ensures psi_0 and psi_hat are defined at the same reference point.
#'
#' @param config     Simulation config list. Must contain parameter$J.
#' @param parameter  Parameter spec with x_0 and psi_0 stored as extra fields.
#' @return           An \code{estimand_spec} object.
make_estimand <- function(config, parameter) {
  J <- config$parameter$J
  psi_0 <- parameter$extra$psi_0

  if (is.null(psi_0)) {
    stop(
      "parameter$extra$psi_0 not found. Was make_parameter() run with the ",
      "updated fixed-reference spec?",
      call. = FALSE
    )
  }

  likelyr::estimand_spec(
    name = "Simpson's index (conditional at fixed reference x_0)",
    psi_fn = psi_fn,
    psi_jac = psi_jac,
    psi_lower = 1 / J,
    psi_upper = 1.0,
    psi_closed = c(lower = TRUE, upper = FALSE),
    psi_0 = psi_0,
    make_psi_fns = make_psi_fns
  )
}
