psi_fn <- function(param, data) {
  X <- attr(data, "X")
  weights <- attr(data, "weights")

  theta <- data.frame(
    theta = exp(X %*% param),
    process = factor(rownames(X))
  ) |>
    dplyr::group_by(process) |>
    dplyr::summarise(theta = mean(theta)) |>
    tibble::deframe()

  sum(theta * weights)
}

psi_jac <- function(param, data) {
  X <- attr(data, "X")
  weights <- attr(data, "weights")

  processes <- rownames(X)

  n_per_process <- table(processes)

  W <- as.numeric(
    weights[as.character(processes)] / n_per_process[as.character(processes)]
  )

  theta <- exp(X %*% param)

  as.numeric(t(X) %*% (W * theta))
}

get_psi_mle_se <- function(model, weights, X) {
  # Extract Beta_MLE as numeric vector
  beta_mle <- model |>
    coef() |>
    as.matrix(ncol = 1)
  beta_cov <- vcov(model)

  # Group membership for each observation
  processes <- model$data$process
  process_labels <- levels(processes)

  # Ensure weights are a named vector (one per group)
  if (is.null(names(weights))) {
    if (length(weights) != length(group_labels)) {
      stop(
        "If 'weights' is unnamed, it must have length equal to number of groups."
      )
    }
    weights <- setNames(weights, process_labels)
  }

  # Linear predictors and exp
  eta <- drop(X %*% beta_mle)
  exp_eta <- exp(eta)

  # Process sizes
  n_per_process <- table(processes)

  # Per-observation effective weights = w_j / n_j
  obs_w <- weights[as.character(processes)] /
    n_per_process[as.character(processes)]

  # Gradient: sum_i obs_w[i] * exp(eta_i) * x_i
  grad <- as.numeric(t(X) %*% (obs_w * exp_eta))
  names(grad) <- colnames(X)

  # Delta-method SE
  se <- sqrt(as.numeric(t(grad) %*% beta_cov %*% grad))

  return(se)
}

search_interval_fn <- function(param_mle, data) {
  X <- attr(data, "X")
  weights <- attr(data, "weights")
  model <- fit_model(data)
  psi_mle <- psi_fn(param_mle, data)
  psi_mle_se <- get_psi_mle_se(model, weights, X)
  psi_mle + c(-1, 1) * 6 * psi_mle_se
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
  gamma <- cfg$gamma
  cutoff_buffer <- cfg$cutoff_buffer
  uniroot_expand_factor <- cfg$uniroot_expand_factor

  if (
    any(vapply(
      list(
        increment,
        confidence_levels,
        gamma,
        cutoff_buffer,
        uniroot_expand_factor
      ),
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
    psi_fn = psi_fn,
    psi_jac = psi_jac,
    search_interval_fn = search_interval_fn,
    increment = increment,
    confidence_levels = confidence_levels,
    gamma = gamma,
    cutoff_buffer = cutoff_buffer,
    uniroot_expand_factor = uniroot_expand_factor,
    name = "Weighted Sum (psi)"
  )
}
