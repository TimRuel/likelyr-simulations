# ======================================================================
# Data Generation (Multinomial Logistic Regression with Random Effects)
#
# Generates clustered data for a multinomial logistic regression model
# with cluster-specific random intercepts. The model is the baseline-
# category logit random effects model of Hartzel, Agresti & Caffo (2001).
#
# Data structure:
#   n_clusters clusters, each contributing m observations.
#   For cluster i, observation t:
#     log P(Y_it = j | x_it, u_i) / P(Y_it = 1 | x_it, u_i)
#       = x_it^T beta_j + u_ij,  j = 2,...,J
#   u_i = (u_i2,...,u_iJ) ~ N(0, Sigma_0)  iid across clusters
#
# Parameterization convention:
#   Category 1 is the baseline. The free coefficient matrix is
#   B = [beta_2 | ... | beta_J] in R^{p x (J-1)}, stored as vec(B).
#   Sigma_0 = sigma2_u * I_{J-1} (diagonal, specified via config).
#
# The estimand is evaluated at x_0 = colMeans(X_design[1:n_obs,]) and
# u_i = 0 (median cluster). n_obs = n_clusters * m (pre-smoothing).
#
# Epsilon smoothing:
#   Pseudo-observations for zero-count categories are placed at the
#   observed covariate means and assigned to a new singleton cluster
#   (index n_clusters + 1, 2, ...) with u = 0. This avoids biasing
#   the within-cluster structure of the observed clusters.
#
# Config structure:
#   data:
#     n_clusters: 50
#     m: 5                    # observations per cluster (equal sizes)
#     formula: "Y ~ X1 + X2"
#     epsilon: 0.5
#     predictors:
#       - symbol: X1
#         distribution: {name: rnorm, args: [0.0, 1.0]}
#       - symbol: X2
#         distribution: {name: rnorm, args: [0.0, 1.0]}
#   parameter:
#     J: 5
#     sigma2_u: 0.5
# ======================================================================

# ── Design matrix helpers ───────────────────────────────────────────────
# (identical to fixed effects model — shared via sourcing)

get_X_design <- function(data) {
  trms <- attr(data, "terms")
  model_frame <- model.frame(
    trms,
    data = data[, setdiff(names(data), "cluster")]
  )
  X_design <- model.matrix(trms, data = model_frame)
  attr(X_design, "original_model_frame") <- model_frame
  attr(X_design, "terms") <- trms
  attr(X_design, "formula") <- formula(trms)
  X_design
}

get_Y_design <- function(data) {
  trms <- attr(data, "terms")
  response <- all.vars(trms)[attr(trms, "response")]
  y <- data[[response]]
  J <- attr(data, "J")
  y_int <- as.integer(as.character(y))
  non_baseline <- seq(2L, J)
  outer(y_int, non_baseline, `==`) * 1L
}

draw_from <- function(dist_cfg, n = 1L) {
  fn <- match.fun(dist_cfg$name)
  args <- c(list(n), dist_cfg$args)
  do.call(fn, args)
}

softmax_scalar <- function(x) {
  z <- x - max(x)
  exp(z) / sum(exp(z))
}

# ── Data generation ─────────────────────────────────────────────────────

#' Generate clustered multinomial logistic regression data
#'
#' @param config     Simulation config list.
#' @param parameter  Parameter spec with param_0 and extra$Sigma_0 set.
#' @return           Data frame with Y, cluster, and covariates, with
#'   attributes "terms", "J", "n_obs", "n_clusters", "m" set.
generate_data <- function(config, parameter) {
  data_cfg <- config$data
  param_cfg <- config$parameter
  n_clusters <- data_cfg$n_clusters
  m <- data_cfg$m
  n_total <- n_clusters * m
  epsilon <- data_cfg$epsilon %||% 0
  formula_str <- data_cfg$formula
  J <- param_cfg$J
  Sigma_0 <- matrix(parameter$extra$Sigma_0, nrow = J - 1L, ncol = J - 1L)

  # ── Draw observed covariates ──────────────────────────────────────────
  covariate_df <- lapply(data_cfg$predictors, \(pred) {
    vals <- draw_from(pred$distribution, n_total)
    setNames(data.frame(vals), pred$symbol)
  }) |>
    do.call(what = cbind)

  # ── Build design matrix ───────────────────────────────────────────────
  tmp_data <- covariate_df
  tmp_data[["Y"]] <- factor(rep(1L, n_total), levels = seq_len(J))
  attr(tmp_data, "terms") <- terms(as.formula(formula_str), data = tmp_data)
  attr(tmp_data, "J") <- J

  X_design <- get_X_design(tmp_data)
  p <- ncol(X_design)

  # ── Draw random effects u_i ~ N(0, Sigma_0) ───────────────────────────
  U <- MASS::mvrnorm(n_clusters, mu = rep(0, J - 1L), Sigma = Sigma_0)
  # U is n_clusters x (J-1); expand to observation level
  cluster_id <- rep(seq_len(n_clusters), each = m)
  U_obs <- U[cluster_id, , drop = FALSE] # n_total x (J-1)

  # ── Draw categorical responses ────────────────────────────────────────
  # Linear predictor: x_it^T B + u_i (random intercept added to each logit)
  B_0 <- matrix(parameter$param_0, nrow = p, ncol = J - 1L)
  eta <- X_design %*% B_0 + U_obs # n_total x (J-1)
  probs <- t(apply(cbind(0, eta), 1, softmax_scalar))
  Y <- apply(probs, 1, \(p) sample.int(J, 1L, prob = p))

  # ── Epsilon smoothing ─────────────────────────────────────────────────
  # Pseudo-observations for absent categories are added as new singleton
  # clusters (u = 0) to avoid distorting within-cluster structure.
  if (epsilon > 0) {
    zero_cats <- setdiff(seq_len(J), unique(Y))
    if (length(zero_cats) > 0L) {
      n_pseudo <- length(zero_cats)
      obs_means <- lapply(covariate_df, mean)
      pseudo_covs <- as.data.frame(obs_means)[rep(1L, n_pseudo), , drop = FALSE]
      rownames(pseudo_covs) <- NULL
      covariate_df <- rbind(covariate_df, pseudo_covs)
      Y <- c(Y, zero_cats)
      # Assign each pseudo-obs to a new singleton cluster
      new_cluster_ids <- seq(n_clusters + 1L, n_clusters + n_pseudo)
      cluster_id <- c(cluster_id, new_cluster_ids)
    }
  }

  # ── Assemble data frame ───────────────────────────────────────────────
  Y_factor <- factor(Y, levels = seq_len(J))

  data <- data.frame(Y = Y_factor, cluster = cluster_id) |>
    cbind(covariate_df)

  attr(data, "terms") <- terms(as.formula(formula_str), data = data)
  attr(data, "J") <- J
  attr(data, "n_obs") <- n_total # pre-smoothing count
  attr(data, "n_clusters") <- n_clusters
  attr(data, "m") <- m

  data
}

# ── Data Spec Constructor ───────────────────────────────────────────────

make_data <- function(config) {
  if (is.null(config$data)) {
    stop("Config must contain a 'data' section.", call. = FALSE)
  }
  likelyr::data_spec(
    name = "Clustered multinomial logistic regression data",
    generate_data = generate_data
  )
}
