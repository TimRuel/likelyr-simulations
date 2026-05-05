# ======================================================================
# Traversal Specification (Multinomial Logistic Regression Parameterization)
# Target: Simpson's Index D(theta_bar) = sum(theta_bar_j^2)
# ======================================================================

# ======================================================================
# Simpson's Index Mode Locator (gradient ascent on constraint manifold)
#
# Finds the branch mode by projected gradient ascent in vec(B) space,
# moving omega_hat along the constraint surface psi(B) = const in the
# direction that increases E_loglik. The mode psi value is read off
# from the converged parameter.
#
# This replaces golden section search, which is expensive in the MLR
# case because each evaluation requires a full auglag solve. Projected
# gradient ascent only requires gradient evaluations, which are much
# cheaper.
#
# The projection onto the tangent space of the constraint surface is:
#   grad_proj = grad - (grad . jac / ||jac||^2) * jac
# which removes the component of the E_loglik gradient that would
# change psi, keeping the iterate approximately on the manifold.
# ======================================================================

simpson_mode_locator_fn <- function(
  psi_interval,
  branch_binder,
  increment,
  param_dim,
  psi_fn,
  psi_jac,
  E_loglik_grad,
  ...
) {
  if (is.null(psi_interval)) {
    stop(
      "simpson_mode_locator_fn requires a bounded psi_interval.",
      call. = FALSE
    )
  }

  lower <- min(psi_interval)
  upper <- max(psi_interval)

  function(omega_hat, psi_hint = NULL) {
    # -------------------------------------------------------------------
    # Projected gradient ascent on the constraint manifold
    # -------------------------------------------------------------------
    B <- omega_hat
    alpha <- 0.01
    n_steps <- 30L
    prev_psi <- psi_fn(B)

    for (i in seq_len(n_steps)) {
      grad <- tryCatch(
        E_loglik_grad(B, omega_hat),
        error = function(e) NULL
      )
      if (is.null(grad) || !all(is.finite(grad))) {
        break
      }

      jac <- as.numeric(psi_jac(B))
      jac_norm2 <- sum(jac^2)
      if (jac_norm2 < 1e-12) {
        break
      }

      # Project gradient onto tangent space of constraint surface
      grad_proj <- grad - sum(grad * jac) / jac_norm2 * jac
      grad_norm <- sqrt(sum(grad_proj^2))
      if (grad_norm < 1e-10) {
        break
      }

      B <- B + alpha * grad_proj / grad_norm
    }

    # -------------------------------------------------------------------
    # Read off psi at converged parameter and snap to grid
    # -------------------------------------------------------------------
    psi_hat <- tryCatch(psi_fn(B), error = function(e) prev_psi)
    psi_hat <- max(lower, min(upper, psi_hat))
    psi_hat_snapped <- lower +
      round((psi_hat - lower) / increment) * increment
    psi_hat_snapped <- min(max(psi_hat_snapped, lower), upper)

    # -------------------------------------------------------------------
    # Final evaluation at snapped mode via branch evaluator
    # -------------------------------------------------------------------
    branch_evaluator <- branch_binder(omega_hat)

    result <- tryCatch(
      branch_evaluator(psi_hat_snapped, B),
      error = function(e) NULL
    )

    if (is.null(result)) {
      return(list(
        psi_hat = psi_hat_snapped,
        param_hat = omega_hat,
        loglik_at_mode = -Inf,
        status = "eval_failed"
      ))
    }

    list(
      psi_hat = psi_hat_snapped,
      param_hat = result$param_hat,
      loglik_at_mode = result$branch_val,
      status = "success"
    )
  }
}

# ======================================================================
# Spec constructor
# ======================================================================

make_traversal <- function(config) {
  cfg <- config$traversal
  if (is.null(cfg)) {
    stop("Config must contain a 'traversal' section.", call. = FALSE)
  }

  required <- c("increment", "confidence_levels")
  missing <- required[vapply(
    required,
    function(k) is.null(cfg[[k]]),
    logical(1)
  )]
  if (length(missing) > 0) {
    stop(
      "Missing required traversal config fields: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  likelyr::traversal_spec(
    increment = cfg$increment,
    traversal_method = cfg$traversal_method %||% "topdown",
    warmstart_fn = NULL,
    mode_locator_fn = simpson_mode_locator_fn,
    confidence_levels = cfg$confidence_levels,
    cutoff_buffer = cfg$cutoff_buffer %||% 1.5,
    n_adjacent = cfg$n_adjacent %||% 3L,
    max_mode_shifts = cfg$max_mode_shifts %||% 20L,
    k_recent = cfg$k_recent %||% 3L,
    drop_multiplier = cfg$drop_multiplier %||% 2.0,
    cap_multiplier = cfg$cap_multiplier %||% 10.0,
    mode_gap_multiplier = cfg$mode_gap_multiplier %||% 1.0,
    interval_buffer = cfg$interval_buffer %||% 1.0,
    name = "Branch traversal strategy"
  )
}
