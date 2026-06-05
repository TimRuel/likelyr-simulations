# ======================================================================
# Multinomial Likelihood (Random Effects Parameterization)
#
# Implements the marginal log-likelihood and its gradient for the
# baseline-category logit random effects model. The marginal likelihood
# integrates out the cluster-specific random effects u_i ~ N(0, Sigma):
#
#   L(B; data, Sigma) = prod_i int [prod_t prod_j theta_j(x_it; B, u_i)^y_itj]
#                              phi(u_i; 0, Sigma) du_i
#
# The integral has no closed form. We use a Laplace approximation, which
# is accurate when the posterior of u_i given y_i and B is close to
# Gaussian -- a reasonable assumption when m (cluster size) is moderate.
#
# Laplace approximation for cluster i:
#   log L_i^LA(B, Sigma) = f_i(u_hat_i) - 1/2 log|Sigma|
#                        - 1/2 log|{Sigma_t W_t(u_hat_i) + Sigma^{-1}}|
#   where:
#     f_i(u) = sum_t sum_j y_itj log theta_j(x_it; B, u) - 1/2 u^T Sigma^{-1} u
#     u_hat_i = argmax_u f_i(u)     (cluster-level mode)
#     W_t = diag(probs_t) - probs_t probs_t^T   (multinomial covariance)
#
# Gradient w.r.t. B (Laplace gradient, ignoring d(u_hat)/dB):
#   d log L / d B ≈ sum_i sum_t X_it^T (Y_design_it - probs(x_it; B, u_hat_i))
#
# Sigma treatment (controlled by fix_Sigma in config):
#   fix_Sigma = TRUE  (default): Sigma is fixed at Sigma_hat from mblogit().
#     param = vec(B), length p*(J-1). Sigma is read from
#     attr(param_mle, "Sigma_hat") and closed over in make_branch_fns().
#     Branch optimizer moves only B.
#
#   fix_Sigma = FALSE: Sigma is jointly optimized in each branch.
#     param = c(vec(B), vech(chol(Sigma))), length p*(J-1) + (J-1)*J/2.
#     Sigma is parameterized via its lower Cholesky factor L (so
#     Sigma = L L^T is always positive definite). Branch optimizer
#     moves both B and L. Switch via likelihood.fix_Sigma in config.
#
# Branch objective:
#   The branch objective is the actual marginal log-likelihood. The
#   omega_hat enters only through the constraint imposed by the estimand
#   (on the B part of param only). Sigma enters only through the
#   likelihood, not the constraint.
# ======================================================================

# ── Shared helpers ─────────────────────────────────────────────────────

softmax <- function(eta) {
  eta_aug <- cbind(0, eta)
  row_max <- apply(eta_aug, 1, max)
  shifted <- eta_aug - row_max
  exp_eta <- exp(shifted)
  probs <- exp_eta / rowSums(exp_eta)
  probs[, -1L, drop = FALSE]
}

x_reference <- function(data) {
  n_obs <- attr(data, "n_obs") %||% nrow(data)
  X_design <- get_X_design(data)
  colMeans(X_design[seq_len(n_obs), , drop = FALSE])
}

make_B_hat <- function(omega_hat, B_mle, x_0) {
  eta_0 <- log(omega_hat[-1L]) - log(omega_hat[1L])
  eta_mle <- as.numeric(x_0 %*% B_mle)
  delta <- eta_0 - eta_mle
  x0_norm2 <- sum(x_0^2)
  B_mle + outer(x_0, delta) / x0_norm2
}

# ── Cholesky pack/unpack helpers ────────────────────────────────────────

#' Pack (B, Sigma) into a single parameter vector
#'
#' Sigma is parameterized via its lower Cholesky factor L (Sigma = L L^T)
#' to ensure positive definiteness. The packed vector is
#' c(vec(B), vech(L)) where vech extracts the lower triangle including
#' the diagonal.
#'
#' @param B      p x (J-1) coefficient matrix.
#' @param Sigma  (J-1) x (J-1) positive definite covariance matrix.
#' @return       Numeric vector of length p*(J-1) + (J-1)*J/2.
pack_param <- function(B, Sigma) {
  L <- t(chol(Sigma)) # lower triangular Cholesky factor
  c(as.numeric(B), L[lower.tri(L, diag = TRUE)])
}

#' Unpack parameter vector into (B, Sigma)
#'
#' @param param  Packed parameter vector from pack_param().
#' @param p      Number of rows in B (number of predictors).
#' @param J      Number of categories.
#' @return       List with $B (p x (J-1)) and $Sigma ((J-1) x (J-1)).
unpack_param <- function(param, p, J) {
  n_B <- p * (J - 1L)
  B <- matrix(param[seq_len(n_B)], nrow = p, ncol = J - 1L)
  vech <- param[seq(n_B + 1L, length(param))]
  L <- matrix(0, nrow = J - 1L, ncol = J - 1L)
  L[lower.tri(L, diag = TRUE)] <- vech
  list(B = B, Sigma = L %*% t(L))
}

#' Extract only B from a parameter vector (works for both fix_Sigma modes)
#'
#' @param param  Either vec(B) (fix_Sigma=TRUE) or packed param (fix_Sigma=FALSE).
#' @param p      Number of rows in B.
#' @param J      Number of categories.
#' @return       p x (J-1) coefficient matrix.
extract_B <- function(param, p, J) {
  matrix(param[seq_len(p * (J - 1L))], nrow = p, ncol = J - 1L)
}

#' omega_hat_from_param_mle — works for both fix_Sigma modes
omega_hat_from_param_mle <- function(param_mle, data) {
  J <- attr(data, "J")
  x_0 <- x_reference(data)
  p <- length(x_0)
  B_mle <- extract_B(param_mle, p, J)
  eta_0 <- as.numeric(x_0 %*% B_mle)
  exp_eta <- exp(c(0, eta_0) - max(c(0, eta_0)))
  exp_eta / sum(exp_eta)
}

# ── Cluster-level Laplace approximation ────────────────────────────────

#' Laplace log-likelihood contribution for cluster i
#'
#' @param Y_cluster     m x (J-1) indicator matrix.
#' @param X_cluster     m x p design matrix.
#' @param B             p x (J-1) coefficient matrix.
#' @param Sigma_inv     (J-1) x (J-1) precision matrix.
#' @param log_det_Sigma Precomputed log|Sigma|.
#' @param u_init        Warm start for mode finding. Default: zeros.
#' @return              Scalar Laplace log-likelihood for cluster i.
cluster_loglik_laplace <- function(
  Y_cluster,
  X_cluster,
  B,
  Sigma_inv,
  log_det_Sigma,
  u_init = NULL
) {
  J_minus_1 <- ncol(B)
  m <- nrow(X_cluster)
  u_init <- u_init %||% rep(0, J_minus_1)

  f_cluster <- function(u) {
    eta <- X_cluster %*% B + matrix(u, nrow = m, ncol = J_minus_1, byrow = TRUE)
    probs <- softmax(eta)
    probs_full <- cbind(pmax(1 - rowSums(probs), 1e-300), pmax(probs, 1e-300))
    Y_full <- cbind(1L - rowSums(Y_cluster), Y_cluster)
    sum(Y_full * log(probs_full)) - 0.5 * as.numeric(u %*% Sigma_inv %*% u)
  }

  grad_f_cluster <- function(u) {
    eta <- X_cluster %*% B + matrix(u, nrow = m, ncol = J_minus_1, byrow = TRUE)
    probs <- softmax(eta)
    as.numeric(colSums(Y_cluster - probs)) - as.numeric(Sigma_inv %*% u)
  }

  opt <- tryCatch(
    optim(
      par = u_init,
      fn = function(u) -f_cluster(u),
      gr = function(u) -grad_f_cluster(u),
      method = "BFGS",
      control = list(maxit = 200, reltol = 1e-8)
    ),
    error = function(e) list(par = u_init, convergence = 1)
  )
  u_hat <- opt$par

  eta_hat <- X_cluster %*%
    B +
    matrix(u_hat, nrow = m, ncol = J_minus_1, byrow = TRUE)
  probs_hat <- softmax(eta_hat)

  neg_H <- Sigma_inv
  for (t in seq_len(m)) {
    p_t <- as.numeric(probs_hat[t, ])
    neg_H <- neg_H + diag(p_t, nrow = J_minus_1) - outer(p_t, p_t)
  }

  ll_mode <- f_cluster(u_hat)
  chol_neg_H <- tryCatch(chol(neg_H), error = function(e) NULL)
  log_det_neg_H <- if (!is.null(chol_neg_H)) {
    2 * sum(log(diag(chol_neg_H)))
  } else {
    as.numeric(determinant(neg_H, logarithm = TRUE)$modulus)
  }

  ll_mode - 0.5 * log_det_Sigma - 0.5 * log_det_neg_H
}

# ── Core marginal log-likelihood (internal) ────────────────────────────

#' Laplace marginal log-likelihood given explicit B and Sigma
#'
#' @param B             p x (J-1) coefficient matrix.
#' @param Sigma         (J-1) x (J-1) covariance matrix.
#' @param cluster_fns   List of per-cluster {Y, X, m} precomputed submatrices.
#' @return              Scalar Laplace marginal log-likelihood.
.loglik_core <- function(B, Sigma, cluster_fns) {
  Sigma_inv <- solve(Sigma)
  log_det_Sigma <- as.numeric(determinant(Sigma, logarithm = TRUE)$modulus)
  ll <- 0
  for (i in seq_along(cluster_fns)) {
    ll <- ll +
      cluster_loglik_laplace(
        cluster_fns[[i]]$Y,
        cluster_fns[[i]]$X,
        B,
        Sigma_inv,
        log_det_Sigma
      )
  }
  ll
}

#' Laplace gradient w.r.t. vec(B) given explicit B, Sigma, cluster data
#'
#' @param B           p x (J-1) coefficient matrix.
#' @param Sigma_inv   (J-1) x (J-1) precision matrix.
#' @param cluster_fns List of per-cluster {Y, X, m}.
#' @return            p x (J-1) gradient matrix.
.grad_B_core <- function(B, Sigma_inv, cluster_fns) {
  J_minus_1 <- ncol(B)
  p <- nrow(B)
  grad <- matrix(0, nrow = p, ncol = J_minus_1)

  for (i in seq_along(cluster_fns)) {
    Y_c <- cluster_fns[[i]]$Y
    X_c <- cluster_fns[[i]]$X
    m_i <- cluster_fns[[i]]$m

    f_i <- function(u) {
      eta <- X_c %*% B + matrix(u, nrow = m_i, ncol = J_minus_1, byrow = TRUE)
      probs <- softmax(eta)
      probs_full <- cbind(pmax(1 - rowSums(probs), 1e-300), pmax(probs, 1e-300))
      Y_full <- cbind(1L - rowSums(Y_c), Y_c)
      sum(Y_full * log(probs_full)) - 0.5 * as.numeric(u %*% Sigma_inv %*% u)
    }
    gr_i <- function(u) {
      eta <- X_c %*% B + matrix(u, nrow = m_i, ncol = J_minus_1, byrow = TRUE)
      probs <- softmax(eta)
      as.numeric(colSums(Y_c - probs)) - as.numeric(Sigma_inv %*% u)
    }

    opt <- tryCatch(
      optim(
        rep(0, J_minus_1),
        function(u) -f_i(u),
        function(u) -gr_i(u),
        method = "BFGS",
        control = list(maxit = 200)
      ),
      error = function(e) list(par = rep(0, J_minus_1))
    )
    u_hat <- opt$par
    eta_hat <- X_c %*%
      B +
      matrix(u_hat, nrow = m_i, ncol = J_minus_1, byrow = TRUE)
    probs_hat <- softmax(eta_hat)
    grad <- grad + t(X_c) %*% (Y_c - probs_hat)
  }
  grad
}

# ── Public log-likelihood (diagnostic use) ─────────────────────────────

#' Laplace-approximated marginal log-likelihood
#'
#' For diagnostic use. When Sigma is provided explicitly, param = vec(B)
#' (fix_Sigma = TRUE convention). When Sigma is NULL, Sigma is unpacked
#' from param = c(vec(B), vech(chol(Sigma))) (fix_Sigma = FALSE convention).
#'
#' Examples:
#'   loglik(param_B, data, Sigma = fit$VarCov$cluster)   # Sigma fixed
#'   loglik(c(param_B, vech_chol_Sigma), data)            # Sigma in param
#'
#' @param param  Numeric vector: vec(B) or c(vec(B), vech(chol(Sigma))).
#' @param data   Data frame with required attributes.
#' @param Sigma  (J-1) x (J-1) covariance matrix, or NULL to unpack from param.
#' @return       Scalar log-likelihood value.
loglik <- function(param, data, Sigma = NULL) {
  J <- attr(data, "J")
  n_clusters <- attr(data, "n_clusters")
  n_obs <- attr(data, "n_obs") %||% (n_clusters * attr(data, "m"))
  X_design <- get_X_design(data)
  Y_design <- get_Y_design(data)
  p <- ncol(X_design)
  cluster_ids <- as.integer(data$cluster[seq_len(n_obs)])
  cluster_fns <- lapply(seq_len(n_clusters), function(i) {
    idx <- which(cluster_ids == i)
    list(
      Y = Y_design[idx, , drop = FALSE],
      X = X_design[idx, , drop = FALSE],
      m = length(idx)
    )
  })

  if (!is.null(Sigma)) {
    # Sigma provided explicitly — param = vec(B)
    B <- extract_B(param, p, J)
  } else {
    # Sigma packed in param — param = c(vec(B), vech(chol(Sigma)))
    up <- unpack_param(param, p, J)
    B <- up$B
    Sigma <- up$Sigma
  }

  .loglik_core(B, Sigma, cluster_fns)
}

# ── Expected log-likelihood (branch objective) ──────────────────────────

#' Branch objective for the random effects model
#'
#' Reads Sigma from attr(param_mle, "Sigma_hat") (fix_Sigma = TRUE) or
#' unpacks it from param (fix_Sigma = FALSE). In both cases the objective
#' is the marginal log-likelihood; omega_hat enters only via the constraint.
#'
#' @param param      Numeric vector: vec(B) or c(vec(B), vech(chol(Sigma))).
#' @param omega_hat  Numeric vector of length J (constraint only).
#' @param data       Data frame with required attributes.
#' @param param_mle  Numeric vector with attr "Sigma_hat" and "fix_Sigma".
#' @return           Scalar marginal log-likelihood value.
E_loglik <- function(param, omega_hat, data, param_mle) {
  fix_Sigma <- attr(param_mle, "fix_Sigma") %||% TRUE

  J <- attr(data, "J")
  n_clusters <- attr(data, "n_clusters")
  n_obs <- attr(data, "n_obs") %||% (n_clusters * attr(data, "m"))
  X_design <- get_X_design(data)
  Y_design <- get_Y_design(data)
  p <- ncol(X_design)
  cluster_ids <- as.integer(data$cluster[seq_len(n_obs)])
  cluster_fns <- lapply(seq_len(n_clusters), function(i) {
    idx <- which(cluster_ids == i)
    list(
      Y = Y_design[idx, , drop = FALSE],
      X = X_design[idx, , drop = FALSE],
      m = length(idx)
    )
  })

  if (fix_Sigma) {
    Sigma <- attr(param_mle, "Sigma_hat")
    B <- extract_B(param, p, J)
  } else {
    up <- unpack_param(param, p, J)
    B <- up$B
    Sigma <- up$Sigma
  }

  .loglik_core(B, Sigma, cluster_fns)
}

# ── Gradient of E_loglik ───────────────────────────────────────────────

#' Laplace gradient of marginal log-likelihood
#'
#' For fix_Sigma = TRUE: returns gradient w.r.t. vec(B) only.
#' For fix_Sigma = FALSE: returns gradient w.r.t. c(vec(B), vech(chol(Sigma))),
#'   with the Sigma part computed via numerical differentiation.
#'
#' @param param      Numeric vector (vec(B) or packed).
#' @param omega_hat  Numeric vector of length J (unused).
#' @param data       Data frame with required attributes.
#' @param param_mle  Numeric vector with attr "Sigma_hat" and "fix_Sigma".
#' @return           Numeric gradient vector.
E_loglik_grad <- function(param, omega_hat, data, param_mle) {
  fix_Sigma <- attr(param_mle, "fix_Sigma") %||% TRUE

  J <- attr(data, "J")
  n_clusters <- attr(data, "n_clusters")
  n_obs <- attr(data, "n_obs") %||% (n_clusters * attr(data, "m"))
  X_design <- get_X_design(data)
  Y_design <- get_Y_design(data)
  p <- ncol(X_design)
  cluster_ids <- as.integer(data$cluster[seq_len(n_obs)])
  cluster_fns <- lapply(seq_len(n_clusters), function(i) {
    idx <- which(cluster_ids == i)
    list(
      Y = Y_design[idx, , drop = FALSE],
      X = X_design[idx, , drop = FALSE],
      m = length(idx)
    )
  })

  if (fix_Sigma) {
    Sigma <- attr(param_mle, "Sigma_hat")
    B <- extract_B(param, p, J)
    Sigma_inv <- solve(Sigma)
    gr_B <- .grad_B_core(B, Sigma_inv, cluster_fns)
    as.numeric(gr_B)
  } else {
    up <- unpack_param(param, p, J)
    B <- up$B
    Sigma <- up$Sigma
    Sigma_inv <- solve(Sigma)
    n_B <- p * (J - 1L)

    # Analytical gradient for B part
    gr_B <- as.numeric(.grad_B_core(B, Sigma_inv, cluster_fns))

    # Numerical gradient for Sigma part (vech of Cholesky factor)
    gr_Sigma <- numDeriv::grad(
      func = function(s) {
        up2 <- unpack_param(c(param[seq_len(n_B)], s), p, J)
        .loglik_core(up2$B, up2$Sigma, cluster_fns)
      },
      x = param[seq(n_B + 1L, length(param))]
    )

    c(gr_B, gr_Sigma)
  }
}

# ── Optimized branch objective factory ─────────────────────────────────

#' Build optimized fn/gr closures for the auglag inner loop
#'
#' Reads fix_Sigma from attr(param_mle, "fix_Sigma"). Precomputes
#' cluster submatrices and, when fix_Sigma = TRUE, Sigma_inv and
#' log_det_Sigma once at construction time.
#'
#' @param data       Data frame with required attributes.
#' @param param_mle  Numeric vector with attr "Sigma_hat" and "fix_Sigma".
#' @return           function(omega_hat) -> list(fn, gr)
make_branch_fns <- function(data, param_mle) {
  fix_Sigma <- attr(param_mle, "fix_Sigma") %||% TRUE
  Sigma_hat <- attr(param_mle, "Sigma_hat")

  if (is.null(Sigma_hat)) {
    stop(
      "attr(param_mle, 'Sigma_hat') not found. Was beta_mle_fn() run?",
      call. = FALSE
    )
  }

  J <- attr(data, "J")
  n_clusters <- attr(data, "n_clusters")
  n_obs <- attr(data, "n_obs") %||% (n_clusters * attr(data, "m"))
  X_design <- get_X_design(data)
  Y_design <- get_Y_design(data)
  p <- ncol(X_design)
  cluster_ids <- as.integer(data$cluster[seq_len(n_obs)])

  # Precompute per-cluster submatrices once
  cluster_fns <- lapply(seq_len(n_clusters), function(i) {
    idx <- which(cluster_ids == i)
    list(
      Y = Y_design[idx, , drop = FALSE],
      X = X_design[idx, , drop = FALSE],
      m = length(idx)
    )
  })

  if (fix_Sigma) {
    # Close over fixed Sigma_inv and log_det_Sigma
    Sigma_inv <- solve(Sigma_hat)
    log_det_Sigma <- as.numeric(
      determinant(Sigma_hat, logarithm = TRUE)$modulus
    )

    function(omega_hat) {
      list(
        fn = function(param) {
          B <- extract_B(param, p, J)
          ll <- 0
          for (i in seq_along(cluster_fns)) {
            ll <- ll +
              cluster_loglik_laplace(
                cluster_fns[[i]]$Y,
                cluster_fns[[i]]$X,
                B,
                Sigma_inv,
                log_det_Sigma
              )
          }
          -ll
        },
        gr = function(param) {
          B <- extract_B(param, p, J)
          -as.numeric(.grad_B_core(B, Sigma_inv, cluster_fns))
        }
      )
    }
  } else {
    # param = c(vec(B), vech(chol(Sigma))); Sigma is jointly optimized
    function(omega_hat) {
      list(
        fn = function(param) {
          up <- unpack_param(param, p, J)
          -.loglik_core(up$B, up$Sigma, cluster_fns)
        },
        gr = function(param) {
          up <- unpack_param(param, p, J)
          Sigma_inv <- solve(up$Sigma)
          n_B <- p * (J - 1L)
          gr_B <- -as.numeric(.grad_B_core(up$B, Sigma_inv, cluster_fns))
          gr_Sigma <- -numDeriv::grad(
            func = function(s) {
              up2 <- unpack_param(c(param[seq_len(n_B)], s), p, J)
              .loglik_core(up2$B, up2$Sigma, cluster_fns)
            },
            x = param[seq(n_B + 1L, length(param))]
          )
          c(gr_B, gr_Sigma)
        }
      )
    }
  }
}

# ── Likelihood Spec Constructor ─────────────────────────────────────────

make_likelihood <- function(config) {
  cfg <- config$likelihood
  if (is.null(cfg)) {
    stop("Config must contain a 'likelihood' section.", call. = FALSE)
  }
  likelyr::likelihood_spec(
    name = cfg$name %||% "Marginal multinomial likelihood (Laplace)",
    loglik = loglik,
    E_loglik = E_loglik,
    E_loglik_grad = E_loglik_grad,
    needs_param_mle = TRUE,
    omega_hat_from_param_mle = omega_hat_from_param_mle,
    make_branch_fns = make_branch_fns
  )
}
