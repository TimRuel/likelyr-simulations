# ============================================================
# likelihood.R — Likelihood factory for NB independent-rates dyad
# ============================================================

# ------------------------------------------------------------
# Log-likelihood (core)
# ------------------------------------------------------------

loglik_nb_independent <- function(param, data) {
  n_per_process <- attr(data, "n_per_process")
  J <- length(n_per_process)

  # Split parameter vector
  theta <- param[seq_len(J)]
  phi <- param[J + seq_len(J)]

  # Expand parameters to observation level
  theta_i <- rep(theta, times = n_per_process)
  phi_i <- rep(phi, times = n_per_process)

  sum(
    dnbinom(
      x = data$Y,
      size = phi_i,
      mu = theta_i * data$t,
      log = TRUE
    )
  )
}

# ------------------------------------------------------------
# Optional: MLE helper (used for initialization / diagnostics)
# ------------------------------------------------------------

fit_model_nb_independent <- function(data) {
  stopifnot(all(c("Y", "t", "process") %in% names(data)))

  glmmTMB::glmmTMB(
    formula = attr(data, "formula"),
    family = glmmTMB::nbinom2(),
    dispformula = ~ 0 + process,
    data = data
  )
}

param_mle_nb_independent <- function(data) {
  fit_model_nb_independent(data) |>
    glmmTMB::fixef() |>
    unlist() |>
    exp()
}

# ------------------------------------------------------------
# Factory
# ------------------------------------------------------------

make_likelihood <- function(config) {
  cfg <- config$likelihood
  if (is.null(cfg)) {
    stop("Config must contain a 'likelihood' section.", call. = FALSE)
  }

  likelihood_spec(
    name = "Negative binomial independent-rate likelihood",
    loglik = loglik_nb_independent,
    param_mle_fn = param_mle_nb_independent
  )
}
