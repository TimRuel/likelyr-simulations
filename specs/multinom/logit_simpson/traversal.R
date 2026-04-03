# ======================================================================
# Traversal Specification (Logit Parameterization)
# Target: Simpson's Index D = sum(p_j^2)
# ======================================================================

# ======================================================================
# Simpson's Index Mode Locator
#
# Exploits unimodality of the branch function over the full ψ domain
# [1/J, 1) to locate the mode via golden section search, with no
# starting point hint. Avoids clustering of branch modes around psi_mle.
# ======================================================================

simpson_mode_locator_fn <- function(
  psi_interval,
  branch_binder,
  increment,
  param_dim,
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
    branch_evaluator <- branch_binder(omega_hat)

    # -------------------------------------------------------------------
    # Golden section search over [lower, upper]
    # -------------------------------------------------------------------
    phi <- (sqrt(5) - 1) / 2
    tol <- increment / 10
    a <- lower
    b <- upper

    x1 <- b - phi * (b - a)
    x2 <- a + phi * (b - a)

    f1 <- tryCatch(
      branch_evaluator(x1, omega_hat)$branch_val,
      error = function(e) -Inf
    )
    f2 <- tryCatch(
      branch_evaluator(x2, omega_hat)$branch_val,
      error = function(e) -Inf
    )

    while ((b - a) > tol) {
      if (f1 < f2) {
        a <- x1
        x1 <- x2
        f1 <- f2
        x2 <- a + phi * (b - a)
        f2 <- tryCatch(
          branch_evaluator(x2, omega_hat)$branch_val,
          error = function(e) -Inf
        )
      } else {
        b <- x2
        x2 <- x1
        f2 <- f1
        x1 <- b - phi * (b - a)
        f1 <- tryCatch(
          branch_evaluator(x1, omega_hat)$branch_val,
          error = function(e) -Inf
        )
      }
    }

    psi_hat <- (a + b) / 2

    # -------------------------------------------------------------------
    # Snap to nearest grid point, anchored at lower
    # -------------------------------------------------------------------
    psi_hat_snapped <- lower +
      round((psi_hat - lower) / increment) * increment
    psi_hat_snapped <- min(max(psi_hat_snapped, lower), upper)

    # -------------------------------------------------------------------
    # Final evaluation at snapped mode
    # -------------------------------------------------------------------
    result <- tryCatch(
      branch_evaluator(psi_hat_snapped, omega_hat),
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

  traversal_spec(
    increment = cfg$increment,
    traversal_method = cfg$traversal_method %||% "topdown",
    mode_locator_fn = simpson_mode_locator_fn,
    confidence_levels = cfg$confidence_levels,
    n_adjacent = cfg$n_adjacent %||% 3L,
    max_mode_shifts = cfg$max_mode_shifts %||% 20L,
    k_recent = cfg$k_recent %||% 3L,
    drop_multiplier = cfg$drop_multiplier %||% 2.0,
    max_drop_fraction = cfg$max_drop_fraction %||% 0.25,
    name = "Branch traversal strategy"
  )
}
