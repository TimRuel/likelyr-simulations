# ============================================================
# likelihood.R — Likelihood factory
# ============================================================

# loglik <- function(param, data) {
#   X <- attr(data, "X")

#   eta <- drop(X %*% param)
#   mu <- data$t * exp(eta)

#   sum(data$Y * (log(data$t) + eta) - mu - lgamma(data$Y + 1))
# }

loglik <- function(param, data) {
  X <- attr(data, "X")
  eta <- drop(X %*% param)
  sum(data$Y * eta - data$t * exp(eta))
}

# ------------------------------------------------------------
# Formula construction
# ------------------------------------------------------------

make_formula <- function(config) {
  beta_cfg <- config$parameter$beta
  proc_cfg <- config$parameter$processes

  # Base: no global intercept, process-specific intercepts
  rhs_parts <- c("0", "process")

  cov_cfg <- beta_cfg$slopes

  homo_covs <- Filter(\(c) c$type == "homogeneous", cov_cfg)
  hetero_covs <- Filter(\(c) c$type == "heterogeneous", cov_cfg)

  # Homogeneous covariates → main effects
  if (length(homo_covs) > 0) {
    rhs_parts <- c(
      rhs_parts,
      vapply(homo_covs, \(c) c$covariate, character(1))
    )
  }

  # Heterogeneous covariates → interactions with process
  if (length(hetero_covs) > 0) {
    rhs_parts <- c(
      rhs_parts,
      paste0(
        vapply(hetero_covs, \(c) c$covariate, character(1)),
        ":process"
      )
    )
  }

  rhs <- paste(rhs_parts, collapse = " + ")
  as.formula(paste("Y ~", rhs))
}

# ------------------------------------------------------------
# Model fitting
# ------------------------------------------------------------

fit_model <- function(data) {
  formula <- attr(data, "formula")
  stopifnot(inherits(formula, "formula"))

  glm(
    formula,
    offset = log(t),
    family = poisson(),
    data = data
  )
}

# ------------------------------------------------------------
# Coefficient renaming (α / γ / ζ)
# ------------------------------------------------------------

rename_coefs <- function(coef_names, config) {
  beta_cfg <- config$parameter$beta
  proc_cfg <- config$parameter$processes
  labels <- proc_cfg$labels

  cov_cfg <- beta_cfg$slopes
  homo_covs <- Filter(\(c) c$type == "homogeneous", cov_cfg)
  hetero_covs <- Filter(\(c) c$type == "heterogeneous", cov_cfg)

  # Identify term types
  is_interaction <- grepl(":", coef_names)
  is_process_int <- grepl("^process", coef_names) & !is_interaction

  new_names <- character(length(coef_names))

  # --------------------------------------------------
  # Intercepts: process-specific
  # --------------------------------------------------
  intercept_terms <- coef_names[is_process_int]

  new_names[is_process_int] <- paste0(
    beta_cfg$intercepts$symbol,
    "_",
    sub("^process", "", intercept_terms)
  )

  # --------------------------------------------------
  # Homogeneous slopes (γ)
  # --------------------------------------------------
  if (length(homo_covs) > 0) {
    for (c in homo_covs) {
      idx <- coef_names == c$covariate
      new_names[idx] <- c$coefficient$symbol
    }
  }

  # --------------------------------------------------
  # Heterogeneous slopes (ζ_g)
  # --------------------------------------------------
  if (length(hetero_covs) > 0) {
    for (c in hetero_covs) {
      pattern <- paste0("^process([^:]+):", c$covariate, "$")
      idx <- grepl(pattern, coef_names)

      processes <- sub(pattern, "\\1", coef_names[idx])
      new_names[idx] <- paste0(c$coefficient$symbol, "_", processes)
    }
  }

  new_names
}

# ------------------------------------------------------------
# β̂_MLE extractor
# ------------------------------------------------------------

beta_mle_fn <- function(data) {
  model <- fit_model(data)
  coefs <- coef(model)

  beta_mle <- matrix(coefs, ncol = 1)
  rownames(beta_mle) <- data |>
    attr("X") |>
    colnames()

  beta_mle
}

# ------------------------------------------------------------
# Likelihood factory
# ------------------------------------------------------------

make_likelihood <- function(config) {
  cfg <- config$likelihood
  if (is.null(cfg)) {
    stop("Config must contain a 'likelihood' section.", call. = FALSE)
  }

  likelihood_spec(
    name = cfg$name %||% "Poisson fixed effects regression likelihood",
    loglik = loglik,
    param_mle_fn = beta_mle_fn
  )
}
