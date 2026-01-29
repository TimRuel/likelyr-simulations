# ============================================================
# parameter.R — Efficient parameter factory
# ============================================================

# ------------------------------------------------------------
# Utilities
# ------------------------------------------------------------

generate_process_labels <- function(n, style = "auto_upper") {
  if (is.character(style) && length(style) == 1) {
    switch(
      style,
      auto_upper = {
        labels <- character(n)
        for (i in seq_len(n)) {
          num <- i - 1
          name <- ""
          repeat {
            name <- paste0(LETTERS[(num %% 26) + 1], name)
            num <- num %/% 26 - 1
            if (num < 0) break
          }
          labels[i] <- name
        }
        labels
      },
      auto_lower = tolower(generate_process_labels(n, "auto_upper")),
      auto_num = as.character(seq_len(n)),
      stop("Unknown label style: ", style)
    )
  } else {
    if (length(style) != n) {
      stop("Custom labels must have length n_processes.")
    }
    as.character(style)
  }
}

validate_vector <- function(x, n, name) {
  if (length(x) != n) {
    stop(sprintf("%s length (%d) != n_processes (%d).", name, length(x), n))
  }
  if (any(x < 0)) {
    stop(sprintf("%s must be non-negative.", name))
  }
  invisible(x)
}

draw_or_use_values <- function(cfg, n, name) {
  if (!is.null(cfg$values)) {
    return(validate_vector(cfg$values, n, name))
  }
  if (is.null(cfg$distribution) || is.null(cfg$args)) {
    stop(name, " must specify either values or (distribution, args).")
  }
  do.call(
    match.fun(cfg$distribution),
    c(list(n = n), as.list(cfg$args))
  )
}

normalize_weights <- function(w, cfg) {
  if (!is.null(cfg$normalize_mean_to)) {
    w / mean(w) * cfg$normalize_mean_to
  } else if (!is.null(cfg$normalize_sum_to)) {
    w / sum(w) * cfg$normalize_sum_to
  } else {
    w
  }
}

# ------------------------------------------------------------
# Core generator
# ------------------------------------------------------------

generate_true_parameters <- function(cfg) {
  proc_cfg <- cfg$processes
  n <- proc_cfg$n_processes

  labels <- generate_process_labels(n, proc_cfg$labels)

  theta <- draw_or_use_values(cfg$theta, n, "theta")
  phi <- draw_or_use_values(cfg$phi, n, "phi")

  weights_raw <- draw_or_use_values(cfg$weights, n, "weights")
  weights <- normalize_weights(weights_raw, cfg$weights)

  names(theta) <- labels
  names(phi) <- labels
  names(weights) <- labels

  list(
    theta_0 = theta,
    phi_0 = phi,
    weights = weights
  )
}

# ------------------------------------------------------------
# Factory
# ------------------------------------------------------------

make_parameter <- function(config) {
  cfg <- config$parameter
  if (is.null(cfg)) {
    stop("Config must contain a 'parameter' section.", call. = FALSE)
  }

  set.seed(cfg$seed)

  true_pars <- generate_true_parameters(cfg)

  lower <- cfg$bounds$lower
  if (is.null(lower)) {
    stop("parameter$bounds$lower must be specified.", call. = FALSE)
  }

  parameter_spec(
    name = "Negative binomial independent-rate parameters",
    param_0 = c(true_pars$theta_0, true_pars$phi_0),
    param_lower = lower,
    weights = true_pars$weights
  )
}
