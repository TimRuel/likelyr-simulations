# ============================================================
# estimand.R — Estimand factory for weighted-sum ψ
# ============================================================

# ------------------------------------------------------------
# Core ψ definition
# ------------------------------------------------------------

psi_weighted_sum <- function(param, data) {
  weights <- attr(data, "weights")
  J <- length(weights)

  theta <- param[seq_len(J)]
  sum(theta * weights)
}

# ------------------------------------------------------------
# Jacobian dψ / dθ
# ------------------------------------------------------------

psi_jac_weighted_sum <- function(param, data) {
  weights <- attr(data, "weights")
  J <- length(weights)

  cbind(
    matrix(weights, nrow = 1),
    matrix(0, nrow = 1, ncol = J)
  )
}

# ------------------------------------------------------------
# Delta-method SE for ψ̂
# ------------------------------------------------------------

psi_mle_se_weighted_sum <- function(param_mle, data) {
  weights <- attr(data, "weights")

  # Exposure moments by process
  se_terms <- data |>
    dplyr::group_by(process) |>
    dplyr::summarise(
      S1 = sum(t),
      S2 = sum(t^2),
      .groups = "drop"
    )

  J <- length(weights)

  theta <- param_mle[seq_len(J)]
  phi <- param_mle[J + seq_len(J)]

  var_theta <-
    theta / se_terms$S1 + theta^2 / phi * se_terms$S2 / (se_terms$S1^2)

  sqrt(sum(weights^2 * var_theta))
}

# ------------------------------------------------------------
# Search interval helper
# ------------------------------------------------------------

search_interval_weighted_sum <- function(data) {
  # MLEs (delegated to likelihood helpers)
  param_mle <- param_mle_fn(data)

  psi_hat <- psi_weighted_sum(param_mle, data)
  psi_se <- psi_mle_se_weighted_sum(param_mle, data)

  # 6-sigma window (hard-coded by design choice)
  psi_hat + c(-1, 1) * 6 * psi_se
}

# ------------------------------------------------------------
# Factory
# ------------------------------------------------------------

make_estimand <- function(config) {
  cfg <- config$estimand
  if (is.null(cfg)) {
    stop("Config must contain an 'estimand' section.", call. = FALSE)
  }

  increment <- cfg$increment
  confidence_levels <- cfg$confidence_levels
  cutoff_buffer <- cfg$cutoff_buffer
  uniroot_expand_factor <- cfg$uniroot_expand_factor

  if (
    any(vapply(
      list(increment, confidence_levels, cutoff_buffer, uniroot_expand_factor),
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
    psi_fn = psi_weighted_sum,
    psi_jac = psi_jac_weighted_sum,
    search_interval_fn = search_interval_weighted_sum,
    increment = increment,
    confidence_levels = confidence_levels,
    cutoff_buffer = cutoff_buffer,
    uniroot_expand_factor = uniroot_expand_factor,
    name = "Weighted Sum (psi)"
  )
}
