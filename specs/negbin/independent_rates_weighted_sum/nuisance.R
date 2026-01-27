# ============================================================
# nuisance.R — Nuisance factory for NB independent-rates dyad
# ============================================================

# ------------------------------------------------------------
# Expectations
# ------------------------------------------------------------

E_gamma_nb <- function(
  omega_hat,
  phi_i,
  t,
  n_per_process,
  FUN = lgamma,
  p_cutoff,
  max_y_cap
) {
  n <- sum(n_per_process)
  J <- length(n_per_process)

  # Split omega_hat
  theta_hat <- omega_hat[seq_len(J)]
  phi_hat <- omega_hat[J + seq_len(J)]

  # Expand parameters
  mu_hat_i <- t * rep(theta_hat, times = n_per_process)
  phi_hat_i <- rep(phi_hat, times = n_per_process)

  out <- numeric(n)

  for (i in seq_len(n)) {
    y_max <- min(
      max_y_cap,
      ceiling(
        qnbinom(
          1 - p_cutoff,
          size = phi_hat_i[i],
          mu = mu_hat_i[i]
        )
      )
    )

    y <- 0:y_max
    p <- dnbinom(y, size = phi_hat_i[i], mu = mu_hat_i[i])
    p <- p / sum(p)

    out[i] <- sum(FUN(y + phi_i[i]) * p)
  }

  out
}

E_log_gamma_nb <- function(
  omega_hat,
  phi_i,
  t,
  n_per_process,
  p_cutoff,
  max_y_cap
) {
  E_gamma_nb(
    omega_hat,
    phi_i,
    t,
    n_per_process,
    FUN = lgamma,
    p_cutoff = p_cutoff,
    max_y_cap = max_y_cap
  )
}

E_digamma_nb <- function(
  omega_hat,
  phi_i,
  t,
  n_per_process,
  p_cutoff,
  max_y_cap
) {
  E_gamma_nb(
    omega_hat,
    phi_i,
    t,
    n_per_process,
    FUN = digamma,
    p_cutoff = p_cutoff,
    max_y_cap = max_y_cap
  )
}

# ------------------------------------------------------------
# Expected log-likelihood
# ------------------------------------------------------------

E_loglik_nb <- function(param, omega_hat, data, p_cutoff, max_y_cap) {
  n_per_process <- attr(data, "n_per_process")
  t <- data$t
  J <- length(n_per_process)

  # Split parameters
  theta <- param[seq_len(J)]
  phi <- param[J + seq_len(J)]

  # Expand
  theta_i <- rep(theta, times = n_per_process)
  phi_i <- rep(phi, times = n_per_process)
  mu_i <- t * theta_i

  mu_hat_i <- t * rep(omega_hat[seq_len(J)], times = n_per_process)

  # Terms
  lg_phi <- lgamma(phi_i)
  phi_log <- phi_i * log(phi_i)

  E_lg <- E_log_gamma_nb(
    omega_hat,
    phi_i,
    t,
    n_per_process,
    p_cutoff,
    max_y_cap
  )

  theta_term <-
    mu_hat_i * log(mu_i) - (mu_hat_i + phi_i) * log(mu_i + phi_i)

  sum(E_lg - lg_phi + phi_log + theta_term)
}

# ------------------------------------------------------------
# Gradients
# ------------------------------------------------------------

E_loglik_grad_theta_nb <- function(
  phi_i,
  theta_i,
  mu_i,
  mu_hat_i,
  t,
  obs_to_process
) {
  g_obs <-
    (mu_hat_i / theta_i) -
    t * (mu_hat_i + phi_i) / (mu_i + phi_i)

  as.numeric(rowsum(g_obs, group = obs_to_process))
}

E_loglik_grad_phi_nb <- function(
  phi_i,
  mu_i,
  mu_hat_i,
  E_digamma_i,
  obs_to_process
) {
  g_obs <-
    E_digamma_i -
    digamma(phi_i) +
    log(phi_i) +
    1 -
    log(phi_i + mu_i) -
    (mu_hat_i + phi_i) / (mu_i + phi_i)

  as.numeric(rowsum(g_obs, group = obs_to_process))
}

E_loglik_grad_nb <- function(param, omega_hat, data, p_cutoff, max_y_cap) {
  n_per_process <- attr(data, "n_per_process")
  t <- data$t
  J <- length(n_per_process)

  # Split
  theta <- param[seq_len(J)]
  phi <- param[J + seq_len(J)]

  # Expand
  theta_i <- rep(theta, times = n_per_process)
  phi_i <- rep(phi, times = n_per_process)
  mu_i <- t * theta_i

  mu_hat_i <- t * rep(omega_hat[seq_len(J)], times = n_per_process)
  obs_to_process <- rep(seq_len(J), times = n_per_process)

  # Theta gradient
  grad_theta <- E_loglik_grad_theta_nb(
    phi_i,
    theta_i,
    mu_i,
    mu_hat_i,
    t,
    obs_to_process
  )

  # Phi gradient
  E_digamma_i <- E_digamma_nb(
    omega_hat,
    phi_i,
    t,
    n_per_process,
    p_cutoff,
    max_y_cap
  )

  grad_phi <- E_loglik_grad_phi_nb(
    phi_i,
    mu_i,
    mu_hat_i,
    E_digamma_i,
    obs_to_process
  )

  c(grad_theta, grad_phi)
}

# ------------------------------------------------------------
# Factory
# ------------------------------------------------------------

make_nuisance <- function(config) {
  cfg <- config$nuisance
  if (is.null(cfg)) {
    stop("Config must contain a 'nuisance' section.", call. = FALSE)
  }

  p_cutoff <- cfg$p_cutoff
  max_y_cap <- cfg$max_y_cap

  if (is.null(p_cutoff) || is.null(max_y_cap)) {
    stop(
      "nuisance$p_cutoff and nuisance$max_y_cap must be specified.",
      call. = FALSE
    )
  }

  nuisance_spec(
    name = "Negative binomial nuisance (ZSE expectations)",
    E_loglik = function(param, omega_hat, data) {
      E_loglik_nb(param, omega_hat, data, p_cutoff, max_y_cap)
    },
    E_loglik_grad = function(param, omega_hat, data) {
      E_loglik_grad_nb(param, omega_hat, data, p_cutoff, max_y_cap)
    }
  )
}
