# ======================================================================
# Traversal Specification (Multinomial Logistic Regression Parameterization)
# Target: Simpson's Index D(theta(x_0; B)) = sum(theta_j(x_0; B)^2)
# ======================================================================

# ======================================================================
# Simpson's Index Mode Locator
#
# omega_hat is a length-J probability vector on the sphere in Delta^{J-1},
# interpreted as the conditional category distribution at x_0. The branch
# function is evaluated over the full psi domain [1/J, 1) via golden
# section search, exploiting unimodality to locate the branch mode.
#
# param_mle may be vec(B_mle) or c(vec(B_mle), vech(chol(Sigma_hat)))
# depending on fix_Sigma. Only the first p*(J-1) elements are used to
# construct B_mle_mat. The remaining elements (if any) are appended to
# the B_hat warm start so that the branch optimizer always receives a
# consistently-dimensioned param vector.
#
# param_mle is used as the warm start for all branch evaluations because
# omega_hat and the model parameter live in different spaces
# (omega_dim = J vs param_dim = p*(J-1) or p*(J-1) + (J-1)*J/2).
# Passing omega_hat as param_init would produce a dimension mismatch
# inside branch_evaluator.
# ======================================================================

simpson_mode_locator_fn <- function(
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
      "simpson_mode_locator_fn requires a bounded psi_interval.",
      call. = FALSE
    )
  }

  lower <- min(psi_interval)
  upper <- max(psi_interval)

  # Precompute x_0 and B_mle_mat once at construction time.
  # Extract only the B part of param_mle — param_mle may be longer than
  # p*(J-1) when fix_Sigma = FALSE.
  x_0 <- x_reference(data)
  p <- length(x_0)
  J <- omega_dim
  n_B <- p * (J - 1L)
  B_mle_mat <- matrix(param_mle[seq_len(n_B)], nrow = p, ncol = J - 1L)
  x0_norm2 <- sum(x_0^2)

  # Construct a warm start from omega_hat via rank-1 adjustment of B_mle
  # along x_0, matching theta(x_0; B) = omega_hat (identical to
  # make_B_hat() in likelihood.R). When fix_Sigma = FALSE, the Sigma
  # part of param_mle is appended unchanged so the branch optimizer
  # receives a consistently-dimensioned warm start.
  make_warm_start <- function(omega_hat) {
    eta_0 <- log(omega_hat[-1L]) - log(omega_hat[1L])
    eta_mle <- as.numeric(x_0 %*% B_mle_mat)
    delta <- eta_0 - eta_mle
    B_hat <- B_mle_mat + outer(x_0, delta) / x0_norm2
    c(as.numeric(B_hat), param_mle[seq(n_B + 1L, length(param_mle))])
  }

  function(omega_hat, psi_hint = NULL) {
    branch_evaluator <- branch_binder(omega_hat)
    init_guess <- make_warm_start(omega_hat)

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

    # Final evaluation at snapped mode
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
    max_drop_frac = cfg$max_drop_frac %||% 10.0,
    resid_tol = cfg$resid_tol %||% 1e-3,
    profile_retry_on = cfg$profile_retry_on %||%
      c("monotonicity", "constraint", "drop"),
    branch_retry_on = cfg$branch_retry_on %||%
      character(0),
    use_mode_locator_for_profile = cfg$use_mode_locator_for_profile %||% FALSE,
    rejection_reasons = cfg$rejection_reasons,
    name = "Branch traversal strategy"
  )
}
