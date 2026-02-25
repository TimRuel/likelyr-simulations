# ======================================================================
# Multinomial Nuisance (Logit Parameterization with Simpson's Index)
# ======================================================================

# ----------------------------------------------------------------------
# Expected log-likelihood
#   Q(η ; ω̂)
#
#   = ∑ θ_j(ω̂) log θ_j(η)
# ----------------------------------------------------------------------

E_loglik <- function(param, omega_hat, data = NULL) {
  p_omega <- softmax_from_eta(omega_hat)

  z <- c(param, 0)
  z <- z - max(z)

  log_sum_exp <- log(sum(exp(z)))
  sum(p_omega * (z - log_sum_exp))
}

# ----------------------------------------------------------------------
# Gradient wrt η
# ----------------------------------------------------------------------

E_loglik_grad <- function(param, omega_hat, data = NULL) {
  p_omega <- softmax_from_eta(omega_hat)
  p_eta <- softmax_from_eta(param)

  p_omega[-length(p_omega)] - p_eta[-length(p_eta)]
}

# ======================================================================
# Omega-hat components
# ======================================================================

# ----------------------------------------------------------------------
# Helper: Build Tangent-Space Basis
# ----------------------------------------------------------------------

.tangent_basis <- function(param_mle, psi_jac) {
  if (is.null(psi_jac)) {
    return(NULL)
  }

  g <- psi_jac(param_mle)
  if (!is.numeric(g)) {
    return(NULL)
  }

  g <- as.numeric(g)
  J <- length(g)

  if (!all(is.finite(g)) || sqrt(sum(g * g)) == 0) {
    return(NULL)
  }

  g <- g / sqrt(sum(g * g))
  M <- cbind(g, diag(J)[, -1, drop = FALSE])
  Q <- qr.Q(qr(M), complete = TRUE)

  Q[, -1, drop = FALSE]
}

# ----------------------------------------------------------------------
# Helper: permute logits in full J-space and re-baseline
# ----------------------------------------------------------------------

.permute_eta <- function(eta, perm) {
  J <- length(eta) + 1L
  eta_full <- c(as.numeric(eta), 0.0)

  eta_perm <- eta_full[perm]
  eta_perm <- eta_perm - eta_perm[J]

  eta_perm[1:(J - 1L)]
}

# ----------------------------------------------------------------------
# Initial-guess generator for ω̂
# ----------------------------------------------------------------------

omega_hat_initgen <- function(
  param_mle,
  param_dim,
  param_lower = NULL,
  param_upper = NULL,
  psi_jac = NULL,
  p_permute = 0.75,
  p_recenter = 0.10,
  local_scale = 0.15,
  global_scale = 0.60,
  ...
) {
  J <- param_dim + 1L

  lower <- param_lower %||% rep(-Inf, param_dim)
  upper <- param_upper %||% rep(Inf, param_dim)

  # Tangent-space basis for g(η) = constant
  B <- .tangent_basis(param_mle, psi_jac)

  function(history = NULL, ...) {
    # ---- choose center ----
    if (!is.null(history) && length(history) > 0 && runif(1) < p_recenter) {
      center <- history[[sample.int(length(history), 1L)]]
    } else {
      center <- param_mle
    }

    candidate <- as.numeric(center)

    # ---- permutation move (label symmetry) ----
    if (runif(1) < p_permute) {
      perm <- sample.int(J, J, replace = FALSE)
      candidate <- .permute_eta(candidate, perm)
    }

    # ---- tangent jitter ----
    if (!is.null(B)) {
      s <- if (runif(1) < 0.70) local_scale else global_scale
      a <- rnorm(ncol(B), sd = s)
      candidate <- candidate + c(B %*% a)
    } else {
      candidate <- candidate + rnorm(length(candidate), sd = local_scale)
    }

    # ---- bounds ----
    pmin(pmax(candidate, lower), upper)
  }
}

# ----------------------------------------------------------------------
# Omega-hat sampler (feasibility projection)
# ----------------------------------------------------------------------

omega_hat_sampler <- function(
  psi_fn,
  psi_jac,
  psi_mle,
  eq_fn,
  eq_jac,
  ineq_fn,
  ineq_jac,
  optimizer,
  ...
) {
  localsolver <- optimizer$localsolver
  localtol <- optimizer$localtol
  control <- optimizer$control

  fn0 <- function(theta) 0.0

  # ---- equality constraints: g(η) = ψ̂ (+ optional eq) ----
  heq <- if (is.null(eq_fn)) {
    function(theta) psi_fn(theta) - psi_mle
  } else {
    function(theta) c(psi_fn(theta) - psi_mle, eq_fn(theta))
  }

  heqjac <- if (is.null(psi_jac) && is.null(eq_jac)) {
    NULL
  } else if (!is.null(psi_jac) && is.null(eq_jac)) {
    function(theta) {
      Jpsi <- psi_jac(theta)
      if (is.vector(Jpsi)) matrix(Jpsi, nrow = 1) else Jpsi
    }
  } else if (is.null(psi_jac) && !is.null(eq_jac)) {
    function(theta) eq_jac(theta)
  } else {
    function(theta) {
      Jpsi <- psi_jac(theta)
      if (is.vector(Jpsi)) {
        Jpsi <- matrix(Jpsi, nrow = 1)
      }
      rbind(Jpsi, eq_jac(theta))
    }
  }

  function(init_guess) {
    x0 <- as.numeric(init_guess)

    res <- nloptr::auglag(
      x0 = x0,
      fn = fn0,
      heq = heq,
      heqjac = heqjac,
      hin = ineq_fn,
      hinjac = ineq_jac,
      localsolver = localsolver,
      localtol = localtol,
      control = control,
      deprecatedBehavior = FALSE
    )

    res$par
  }
}

# ======================================================================
# Nuisance Spec Constructor
# ======================================================================

make_nuisance <- function(config) {
  cfg <- config$nuisance

  if (is.null(cfg)) {
    stop("Config must contain a 'nuisance' section.", call. = FALSE)
  }

  nuisance_spec(
    name = cfg$name %||% "Multinomial logit nuisance (Simpson index)",

    E_loglik = E_loglik,
    E_loglik_grad = E_loglik_grad,

    omega_hat = list(
      initgen = omega_hat_initgen,
      sampler = omega_hat_sampler
    )
  )
}
