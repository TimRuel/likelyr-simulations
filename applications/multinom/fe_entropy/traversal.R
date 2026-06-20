# ======================================================================
# Traversal Specification (No Effects Multinomial, Logit Parameterization)
# Target: Shannon Entropy H(theta(eta)) = -sum(p_j * log(p_j))
# ======================================================================

# ======================================================================
# Entropy Mode Locator
#
# omega_hat is a length-J probability vector on the entropy level set
# in Delta^{J-1}. The warm start is theta_to_eta(omega_hat), which
# maps omega_hat into the same eta-space as param_dim = J-1.
#
# Golden section search over [psi_lower, log(J)] locates the branch
# mode, exploiting unimodality of the branch log-likelihood in psi.
# ======================================================================

entropy_mode_locator_fn <- function(
  psi_interval,
  branch_binder,
  increment,
  param_dim,
  param_mle,
  omega_dim,
  data,
  ...
) {
  if (is.null(psi_interval)) {
    stop(
      "entropy_mode_locator_fn requires a bounded psi_interval.",
      call. = FALSE
    )
  }

  lower <- min(psi_interval)
  upper <- max(psi_interval)

  function(omega_hat, psi_hint = NULL) {
    branch_evaluator <- branch_binder(omega_hat)

    # Warm start: convert omega_hat probability vector to eta-space
    init_guess <- theta_to_eta(omega_hat)

    phi <- (sqrt(5) - 1) / 2
    tol <- increment / 10
    a <- lower
    b <- upper

    x1 <- b - phi * (b - a)
    x2 <- a + phi * (b - a)

    f1 <- tryCatch(
      branch_evaluator(x1, init_guess)$branch_val,
      error = function(e) -Inf
    )
    f2 <- tryCatch(
      branch_evaluator(x2, init_guess)$branch_val,
      error = function(e) -Inf
    )

    while ((b - a) > tol) {
      if (f1 < f2) {
        a <- x1
        x1 <- x2
        f1 <- f2
        x2 <- a + phi * (b - a)
        f2 <- tryCatch(
          branch_evaluator(x2, init_guess)$branch_val,
          error = function(e) -Inf
        )
      } else {
        b <- x2
        x2 <- x1
        f2 <- f1
        x1 <- b - phi * (b - a)
        f1 <- tryCatch(
          branch_evaluator(x1, init_guess)$branch_val,
          error = function(e) -Inf
        )
      }
    }

    psi_hat <- (a + b) / 2

    # Snap to nearest grid point, anchored at lower
    psi_hat_snapped <- lower +
      round((psi_hat - lower) / increment) * increment
    psi_hat_snapped <- min(max(psi_hat_snapped, lower), upper)

    result <- tryCatch(
      branch_evaluator(psi_hat_snapped, init_guess),
      error = function(e) NULL
    )

    if (is.null(result)) {
      return(list(
        psi_hat = psi_hat_snapped,
        param_hat = init_guess,
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

  if (length(missing) > 0L) {
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
    mode_locator_fn = entropy_mode_locator_fn,
    confidence_levels = cfg$confidence_levels,
    cutoff_buffer = cfg$cutoff_buffer %||% 1.5,
    n_adjacent = cfg$n_adjacent %||% 3L,
    max_mode_shifts = cfg$max_mode_shifts %||% 20L,
    k_recent = cfg$k_recent %||% 3L,
    drop_multiplier = cfg$drop_multiplier %||% 2.0,
    cap_multiplier = cfg$cap_multiplier %||% 10.0,
    mode_gap_multiplier = cfg$mode_gap_multiplier %||% 1.0,
    interval_buffer = cfg$interval_buffer %||% 1.0,
    max_drop_frac = cfg$max_drop_frac %||% 10.0,
    resid_tol = cfg$resid_tol %||% 1e-3,
    profile_retry_on = cfg$profile_retry_on %||%
      c("monotonicity", "constraint", "drop"),
    branch_retry_on = cfg$branch_retry_on %||% character(0),
    use_mode_locator_for_profile = cfg$use_mode_locator_for_profile %||% FALSE,
    rejection_reasons = cfg$rejection_reasons,
    name = "Branch traversal strategy"
  )
}
