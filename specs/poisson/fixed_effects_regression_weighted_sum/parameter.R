# ============================================================
# parameter.R — Parameter factory
# ============================================================

# ------------------------------------------------------------
# Utilities
# ------------------------------------------------------------

process_labels <- function(proc_cfg) {
  labels <- proc_cfg$labels
  if (is.null(labels) || any(labels == "")) {
    stop("parameter$processes$labels must be specified.", call. = FALSE)
  }
  as.character(labels)
}

validate_vector <- function(x, n, name) {
  if (length(x) != n) {
    stop(sprintf("%s length (%d) != %d.", name, length(x), n), call. = FALSE)
  }
  invisible(x)
}

draw_or_use_values <- function(cfg, n, name) {
  if (!is.null(cfg$values)) {
    return(validate_vector(cfg$values, n, name))
  }

  if (is.null(cfg$distribution)) {
    stop(name, " must specify either values or distribution.", call. = FALSE)
  }

  args <- cfg$args %||% list()
  if (is.vector(args) && !is.list(args)) {
    args <- as.list(args)
  }

  do.call(
    match.fun(cfg$distribution),
    c(list(n = n), args)
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

# ---------------------------------------------------------------------
# Covariate utilites (needed here for computing marginal process rates)
# ---------------------------------------------------------------------

# Generate covariates for all observations
generate_covariate <- function(cov_cfg, n) {
  dist_fn <- match.fun(cov_cfg$distribution)
  covariate <- do.call(dist_fn, c(list(n = n), cov_cfg$args))
  return(covariate)
}

# Build design matrix compatible with stacked beta (kappa, gamma, delta_g, zeta_g)
get_X <- function(config, beta_0, n_per_process) {
  process_labels <- names(n_per_process)
  total_n <- sum(n_per_process)
  J <- length(n_per_process)
  cov_cfgs <- config$data$covariates
  row_idx <- rep(seq_len(J), times = n_per_process)
  X <- matrix(0, nrow = total_n, ncol = nrow(beta_0))
  colnames(X) <- rownames(beta_0)
  rownames(X) <- rep(names(n_per_process), times = n_per_process)

  for (symbol in colnames(X)) {
    if (grepl("α", symbol)) {
      g <- symbol |>
        stringr::str_sub(-1, -1) |>
        (\(g) which(LETTERS == g))()
      obs_idx <- which(row_idx == g)
      X[obs_idx, symbol] <- 1
    } else if (grepl("γ", symbol)) {
      cov_cfg <- Filter(
        \(c) c$symbol == symbol,
        cov_cfgs
      )[[1]]
      X[, symbol] <- generate_covariate(cov_cfg, total_n)
    } else if (grepl("ζ", symbol)) {
      g <- symbol |>
        stringr::str_sub(-1, -1) |>
        (\(g) which(LETTERS == g))()
      obs_idx <- which(row_idx == g)
      cov_cfg <- Filter(
        \(c) c$symbol == stringr::str_sub(symbol, 1, 2),
        cov_cfgs
      )[[1]]
      X[obs_idx, symbol] <- generate_covariate(
        cov_cfg,
        n_per_process[g]
      )
    }
  }
  return(X)
}

# ------------------------------------------------------------
# Core generators
# ------------------------------------------------------------

generate_process_weights <- function(param_cfg) {
  proc_cfg <- param_cfg$processes
  labels <- process_labels(proc_cfg)
  J <- length(labels)

  w_raw <- draw_or_use_values(param_cfg$weights, J, "weights")
  w <- normalize_weights(w_raw, param_cfg$weights)

  names(w) <- labels
  w
}

generate_beta_0 <- function(param_cfg) {
  proc_cfg <- param_cfg$processes
  labels <- process_labels(proc_cfg)
  J <- length(labels)

  beta_cfg <- param_cfg$beta

  # --------------------------------------------------
  # Intercepts (heterogeneous)
  # --------------------------------------------------

  alpha <- draw_or_use_values(
    beta_cfg$intercepts,
    J,
    "heterogeneous intercepts"
  )

  # --------------------------------------------------
  # Covariate effects
  # --------------------------------------------------

  cov_cfg <- beta_cfg$slopes

  homo_covs <- Filter(\(c) c$type == "homogeneous", cov_cfg)
  hetero_covs <- Filter(\(c) c$type == "heterogeneous", cov_cfg)

  # Homogeneous slopes (γ)
  gamma <- vapply(
    homo_covs,
    function(c) {
      draw_or_use_values(c$coefficient, 1, c$coefficient$symbol)
    },
    numeric(1)
  )

  # Heterogeneous slopes (ζ_g)
  zeta <- unlist(
    lapply(
      hetero_covs,
      function(c) {
        draw_or_use_values(c$coefficient, J, c$coefficient$symbol)
      }
    )
  )

  # --------------------------------------------------
  # Assemble β
  # --------------------------------------------------

  beta_vals <- c(alpha, gamma, zeta)

  beta_names <- c(
    paste0(beta_cfg$intercepts$symbol, "_", labels),
    vapply(homo_covs, \(c) c$coefficient$symbol, character(1)),
    unlist(lapply(
      hetero_covs,
      \(c) paste0(c$coefficient$symbol, "_", labels)
    ))
  )

  matrix(
    beta_vals,
    ncol = 1,
    dimnames = list(beta_names, NULL)
  )
}

generate_theta_0 <- function(config, beta_0) {
  param_cfg <- config$parameter
  proc_cfg <- param_cfg$processes
  labels <- proc_cfg$labels
  J <- length(labels)

  n_mc <- as.numeric(param_cfg$theta$n_mc)
  n_per_process <- setNames(rep(n_mc, J), labels)

  X_mc <- get_X(config, beta_0, n_per_process)

  eta <- drop(X_mc %*% beta_0)

  tibble::tibble(
    process = rep(labels, each = n_mc),
    eta = eta
  ) |>
    dplyr::group_by(process) |>
    dplyr::summarise(theta = mean(exp(eta)), .groups = "drop") |>
    tibble::deframe()
}

# ------------------------------------------------------------
# Factory
# ------------------------------------------------------------

make_parameter <- function(config) {
  param_cfg <- config$parameter
  if (is.null(param_cfg)) {
    stop("Config must contain a 'parameter' section.", call. = FALSE)
  }

  set.seed(param_cfg$seed)

  beta_0 <- generate_beta_0(param_cfg)

  theta_0 <- generate_theta_0(config, beta_0)

  weights <- generate_process_weights(param_cfg)

  parameter_spec(
    name = "Poisson fixed effect regression parameters",
    param_0 = beta_0,
    theta_0 = theta_0,
    weights = weights
  )
}
