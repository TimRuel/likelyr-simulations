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
# Laplace approximation for cluster i (actual likelihood):
#   log L_i^LA(B, Sigma) = g_i(u_hat_i) - 1/2 log|Sigma|
#                        - 1/2 log|{sum_t W_t(u_hat_i) + Sigma^{-1}}|
#   where:
#     g_i(u) = sum_t sum_j y_itj log theta_j(x_it; B, u) - 1/2 u^T Sigma^{-1} u
#     u_hat_i = argmax_u g_i(u)     (cluster-level posterior mode)
#     W_t = diag(probs_t) - probs_t probs_t^T   (multinomial covariance)
#
# Branch objective -- ZSE expected complete-data log-likelihood (Severini):
#   The integrated likelihood requires maximizing E_omega[ell_c(B, Sigma)]
#   over B subject to theta(x_0; B, 0) = omega, where the expectation
#   is over the data distribution induced by omega. This ensures the
#   zero-score-expectation (ZSE) property:
#     E_omega[d ell_c / d B] = 0  at the branch mode B*(omega).
#
#   The Laplace approximation to E_omega[ell_c] replaces observed
#   responses y_itj with soft responses q_itj = theta_j(x_it; B_hat, u)
#   where B_hat = make_B_hat(omega, B_mle, x_0):
#
#   g_i*(u) = sum_t sum_j q_tj(B_hat, u) log theta_j(x_it; B, u)
#             - 1/2 u^T Sigma^{-1} u
#
#   Gradient of E_omega[ell_c^LA] w.r.t. B (Laplace gradient approximation,
#   d u_hat*/d B = 0):
#     nabla_B E_omega[ell_c^LA] ~= sum_i sum_t X_it^T (q_it - p_it)
#   where q_it = softmax(x_it; B_hat, u_hat_i*) and
#         p_it = softmax(x_it; B, u_hat_i*).
#
# Cache design in make_branch_fns:
#   fn and gr share a cache environment (reference semantics, no global
#   assignment). ensure_modes(param) recomputes cluster modes only when
#   param changes. fn pays the cost on first call; gr reuses for free.
#   numDeriv::hessian for the Laplace correction is called inside fn only.
#
# Sigma treatment (controlled by fix_Sigma in config):
#   fix_Sigma = TRUE  (default): Sigma fixed at Sigma_hat from mblogit().
#   fix_Sigma = FALSE: Sigma jointly optimized in each branch via Cholesky.
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

pack_param <- function(B, Sigma) {
  L <- t(chol(Sigma))
  c(as.numeric(B), L[lower.tri(L, diag = TRUE)])
}

unpack_param <- function(param, p, J) {
  n_B <- p * (J - 1L)
  B <- matrix(param[seq_len(n_B)], nrow = p, ncol = J - 1L)
  vech <- param[seq(n_B + 1L, length(param))]
  L <- matrix(0, nrow = J - 1L, ncol = J - 1L)
  L[lower.tri(L, diag = TRUE)] <- vech
  list(B = B, Sigma = L %*% t(L))
}

extract_B <- function(param, p, J) {
  matrix(param[seq_len(p * (J - 1L))], nrow = p, ncol = J - 1L)
}

omega_hat_from_param_mle <- function(param_mle, data) {
  J <- attr(data, "J")
  x_0 <- x_reference(data)
  p <- length(x_0)
  B_mle <- extract_B(param_mle, p, J)
  eta_0 <- as.numeric(x_0 %*% B_mle)
  exp_eta <- exp(c(0, eta_0) - max(c(0, eta_0)))
  exp_eta / sum(exp_eta)
}

# ── Cluster-level helpers (shared across both objectives) ───────────────

#' Build g_star and its analytical gradient for one cluster.
#' Closed over X_c, m_i, B, B_hat, Sigma_inv.
make_cluster_g_star <- function(X_c, m_i, B, B_hat, Sigma_inv) {
  J_minus_1 <- ncol(B)

  g_star <- function(u) {
    eta_B <- X_c %*% B + matrix(u, nrow = m_i, ncol = J_minus_1, byrow = TRUE)
    eta_Bhat <- X_c %*%
      B_hat +
      matrix(u, nrow = m_i, ncol = J_minus_1, byrow = TRUE)
    p_t <- softmax(eta_B)
    q_t <- softmax(eta_Bhat)
    p_full <- cbind(pmax(1 - rowSums(p_t), 1e-300), pmax(p_t, 1e-300))
    q_full <- cbind(pmax(1 - rowSums(q_t), 1e-300), pmax(q_t, 1e-300))
    sum(q_full * log(p_full)) - 0.5 * as.numeric(u %*% Sigma_inv %*% u)
  }

  # Analytical gradient:
  # d g_i*/d u_k = sum_t [q_tk (log p_tk - E_q[log p_t]) + (q_tk - p_tk)]
  #                - (Sigma^{-1} u)_k
  grad_g_star <- function(u) {
    eta_B <- X_c %*% B + matrix(u, nrow = m_i, ncol = J_minus_1, byrow = TRUE)
    eta_Bhat <- X_c %*%
      B_hat +
      matrix(u, nrow = m_i, ncol = J_minus_1, byrow = TRUE)
    p_t <- softmax(eta_B)
    q_t <- softmax(eta_Bhat)
    p_full <- cbind(pmax(1 - rowSums(p_t), 1e-300), pmax(p_t, 1e-300))
    q_full <- cbind(pmax(1 - rowSums(q_t), 1e-300), pmax(q_t, 1e-300))
    Eq_logp <- rowSums(q_full * log(p_full))
    as.numeric(
      colSums(q_t * (log(pmax(p_t, 1e-300)) - Eq_logp) + (q_t - p_t))
    ) -
      as.numeric(Sigma_inv %*% u)
  }

  list(fn = g_star, gr = grad_g_star)
}

#' Find the mode of g_star for one cluster via BFGS.
find_cluster_mode <- function(g_star_fns, J_minus_1) {
  opt <- tryCatch(
    optim(
      rep(0, J_minus_1),
      function(u) -g_star_fns$fn(u),
      function(u) -g_star_fns$gr(u),
      method = "BFGS",
      control = list(maxit = 200, reltol = 1e-8)
    ),
    error = function(e) list(par = rep(0, J_minus_1))
  )
  opt$par
}

# ── Cluster-level Laplace (actual likelihood) ───────────────────────────

#' Laplace log-likelihood contribution for cluster i (observed responses)
#'
#' @param Y_cluster     m x (J-1) indicator matrix.
#' @param X_cluster     m x p design matrix.
#' @param B             p x (J-1) coefficient matrix.
#' @param Sigma_inv     (J-1) x (J-1) precision matrix.
#' @param log_det_Sigma Precomputed log|Sigma|.
#' @param u_init        Warm start. Default: zeros.
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

  g_i <- function(u) {
    eta <- X_cluster %*% B + matrix(u, nrow = m, ncol = J_minus_1, byrow = TRUE)
    probs <- softmax(eta)
    probs_full <- cbind(pmax(1 - rowSums(probs), 1e-300), pmax(probs, 1e-300))
    Y_full <- cbind(1L - rowSums(Y_cluster), Y_cluster)
    sum(Y_full * log(probs_full)) - 0.5 * as.numeric(u %*% Sigma_inv %*% u)
  }
  grad_g_i <- function(u) {
    eta <- X_cluster %*% B + matrix(u, nrow = m, ncol = J_minus_1, byrow = TRUE)
    probs <- softmax(eta)
    as.numeric(colSums(Y_cluster - probs)) - as.numeric(Sigma_inv %*% u)
  }

  opt <- tryCatch(
    optim(
      u_init,
      function(u) -g_i(u),
      function(u) -grad_g_i(u),
      method = "BFGS",
      control = list(maxit = 200, reltol = 1e-8)
    ),
    error = function(e) list(par = u_init)
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

  ll_mode <- g_i(u_hat)
  chol_neg_H <- tryCatch(chol(neg_H), error = function(e) NULL)
  log_det_neg_H <- if (!is.null(chol_neg_H)) {
    2 * sum(log(diag(chol_neg_H)))
  } else {
    as.numeric(determinant(neg_H, logarithm = TRUE)$modulus)
  }

  ll_mode - 0.5 * log_det_Sigma - 0.5 * log_det_neg_H
}

# ── Core functions (internal) ───────────────────────────────────────────

#' Laplace marginal log-likelihood (actual likelihood, observed responses)
loglik_core <- function(B, Sigma, cluster_fns) {
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

#' Laplace approximation to E_omega[ell_c^LA] (ZSE branch objective)
#'
#' @param B           p x (J-1) coefficient matrix.
#' @param B_hat       p x (J-1) matrix from make_B_hat(omega_hat).
#' @param Sigma       (J-1) x (J-1) covariance matrix.
#' @param cluster_fns List of per-cluster {X, m}.
eloglik_core <- function(B, B_hat, Sigma, cluster_fns) {
  Sigma_inv <- solve(Sigma)
  log_det_Sigma <- as.numeric(determinant(Sigma, logarithm = TRUE)$modulus)
  J_minus_1 <- ncol(B)
  ll <- 0
  for (i in seq_along(cluster_fns)) {
    X_c <- cluster_fns[[i]]$X
    m_i <- cluster_fns[[i]]$m
    fns <- make_cluster_g_star(X_c, m_i, B, B_hat, Sigma_inv)
    u_hat <- find_cluster_mode(fns, J_minus_1)
    neg_H <- tryCatch(
      {
        H <- numDeriv::hessian(fns$fn, u_hat)
        S <- -(H + t(H)) / 2
        S
      },
      error = function(e) Sigma_inv + diag(J_minus_1)
    )
    chol_neg_H <- tryCatch(chol(neg_H), error = function(e) NULL)
    log_det_neg_H <- if (!is.null(chol_neg_H)) {
      2 * sum(log(diag(chol_neg_H)))
    } else {
      as.numeric(determinant(neg_H, logarithm = TRUE)$modulus)
    }
    ll <- ll + fns$fn(u_hat) - 0.5 * log_det_Sigma - 0.5 * log_det_neg_H
  }
  ll
}

#' Laplace gradient of actual log-likelihood w.r.t. vec(B)
grad_B_core <- function(B, Sigma_inv, cluster_fns) {
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

#' Laplace gradient of E_omega[ell_c^LA] w.r.t. vec(B)
#'
#' Gradient = sum_i sum_t X_it^T (q_it - p_it) at u_hat_i*,
#' using cached modes when available.
#'
#' @param B           p x (J-1) coefficient matrix.
#' @param B_hat       p x (J-1) matrix from make_B_hat(omega_hat).
#' @param Sigma_inv   (J-1) x (J-1) precision matrix.
#' @param cluster_fns List of per-cluster {X, m}.
#' @param modes       Optional list of precomputed u_hat per cluster.
grad_B_eloglik_core <- function(
  B,
  B_hat,
  Sigma_inv,
  cluster_fns,
  modes = NULL
) {
  J_minus_1 <- ncol(B)
  p <- nrow(B)
  grad <- matrix(0, nrow = p, ncol = J_minus_1)
  for (i in seq_along(cluster_fns)) {
    X_c <- cluster_fns[[i]]$X
    m_i <- cluster_fns[[i]]$m
    u_hat <- if (!is.null(modes)) {
      modes[[i]]$u_hat
    } else {
      fns <- make_cluster_g_star(X_c, m_i, B, B_hat, Sigma_inv)
      find_cluster_mode(fns, J_minus_1)
    }
    eta_B <- X_c %*%
      B +
      matrix(u_hat, nrow = m_i, ncol = J_minus_1, byrow = TRUE)
    eta_Bhat <- X_c %*%
      B_hat +
      matrix(u_hat, nrow = m_i, ncol = J_minus_1, byrow = TRUE)
    p_hat <- softmax(eta_B)
    q_hat <- softmax(eta_Bhat)
    grad <- grad + t(X_c) %*% (q_hat - p_hat)
  }
  grad
}

# ── Public log-likelihood (diagnostic use) ─────────────────────────────

#' Laplace-approximated marginal log-likelihood (diagnostic use)
#'
#' When Sigma is provided explicitly, param = vec(B).
#' When Sigma is NULL, Sigma is unpacked from param.
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
    B <- extract_B(param, p, J)
  } else {
    up <- unpack_param(param, p, J)
    B <- up$B
    Sigma <- up$Sigma
  }
  loglik_core(B, Sigma, cluster_fns)
}

# ── Expected log-likelihood (ZSE branch objective) ─────────────────────

#' ZSE branch objective: Laplace approximation to E_omega[ell_c^LA]
E_loglik <- function(param, omega_hat, data, param_mle) {
  fix_Sigma <- attr(param_mle, "fix_Sigma") %||% TRUE
  J <- attr(data, "J")
  n_clusters <- attr(data, "n_clusters")
  n_obs <- attr(data, "n_obs") %||% (n_clusters * attr(data, "m"))
  X_design <- get_X_design(data)
  p <- ncol(X_design)
  cluster_ids <- as.integer(data$cluster[seq_len(n_obs)])
  cluster_fns <- lapply(seq_len(n_clusters), function(i) {
    idx <- which(cluster_ids == i)
    list(X = X_design[idx, , drop = FALSE], m = length(idx))
  })
  if (fix_Sigma) {
    Sigma <- attr(param_mle, "Sigma_hat")
    B <- extract_B(param, p, J)
    B_mle <- extract_B(param_mle, p, J)
  } else {
    up <- unpack_param(param, p, J)
    B <- up$B
    Sigma <- up$Sigma
    B_mle <- extract_B(param_mle, p, J)
  }
  B_hat <- make_B_hat(omega_hat, B_mle, x_reference(data))
  eloglik_core(B, B_hat, Sigma, cluster_fns)
}

# ── Gradient of E_loglik ───────────────────────────────────────────────

#' Gradient of ZSE branch objective w.r.t. param
E_loglik_grad <- function(param, omega_hat, data, param_mle) {
  fix_Sigma <- attr(param_mle, "fix_Sigma") %||% TRUE
  J <- attr(data, "J")
  n_clusters <- attr(data, "n_clusters")
  n_obs <- attr(data, "n_obs") %||% (n_clusters * attr(data, "m"))
  X_design <- get_X_design(data)
  p <- ncol(X_design)
  cluster_ids <- as.integer(data$cluster[seq_len(n_obs)])
  cluster_fns <- lapply(seq_len(n_clusters), function(i) {
    idx <- which(cluster_ids == i)
    list(X = X_design[idx, , drop = FALSE], m = length(idx))
  })
  B_mle <- extract_B(param_mle, p, J)
  B_hat <- make_B_hat(omega_hat, B_mle, x_reference(data))

  if (fix_Sigma) {
    Sigma <- attr(param_mle, "Sigma_hat")
    B <- extract_B(param, p, J)
    Sigma_inv <- solve(Sigma)
    as.numeric(grad_B_eloglik_core(B, B_hat, Sigma_inv, cluster_fns))
  } else {
    up <- unpack_param(param, p, J)
    B <- up$B
    Sigma <- up$Sigma
    Sigma_inv <- solve(Sigma)
    n_B <- p * (J - 1L)
    gr_B <- as.numeric(grad_B_eloglik_core(B, B_hat, Sigma_inv, cluster_fns))
    gr_Sigma <- numDeriv::grad(
      func = function(s) {
        up2 <- unpack_param(c(param[seq_len(n_B)], s), p, J)
        eloglik_core(up2$B, B_hat, up2$Sigma, cluster_fns)
      },
      x = param[seq(n_B + 1L, length(param))]
    )
    c(gr_B, gr_Sigma)
  }
}

# ── Optimized branch objective factory ─────────────────────────────────

#' Build fn/gr closures for the auglag inner loop
#'
#' fn and gr share a cache environment (reference semantics, no global
#' assignment operators). ensure_modes(param) finds cluster modes via
#' BFGS and stores them; fn additionally computes the numerical Hessian
#' for the Laplace correction. gr reuses cached modes at no extra cost.
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
  J_minus_1 <- J - 1L
  n_clusters <- attr(data, "n_clusters")
  n_obs <- attr(data, "n_obs") %||% (n_clusters * attr(data, "m"))
  X_design <- get_X_design(data)
  p <- ncol(X_design)
  cluster_ids <- as.integer(data$cluster[seq_len(n_obs)])
  x_0 <- x_reference(data)
  B_mle <- extract_B(param_mle, p, J)

  cluster_fns <- lapply(seq_len(n_clusters), function(i) {
    idx <- which(cluster_ids == i)
    list(X = X_design[idx, , drop = FALSE], m = length(idx))
  })

  if (fix_Sigma) {
    Sigma_inv <- solve(Sigma_hat)
    log_det_Sigma <- as.numeric(
      determinant(Sigma_hat, logarithm = TRUE)$modulus
    )

    function(omega_hat) {
      B_hat <- make_B_hat(omega_hat, B_mle, x_0)

      cache <- new.env(parent = emptyenv())
      cache$param <- NULL
      cache$modes <- NULL

      compute_modes <- function(param) {
        B <- extract_B(param, p, J)
        modes <- vector("list", length(cluster_fns))
        for (i in seq_along(cluster_fns)) {
          X_c <- cluster_fns[[i]]$X
          m_i <- cluster_fns[[i]]$m
          fns <- make_cluster_g_star(X_c, m_i, B, B_hat, Sigma_inv)
          u_hat <- find_cluster_mode(fns, J_minus_1)
          modes[[i]] <- list(u_hat = u_hat, g_star = fns$fn)
        }
        modes
      }

      ensure_modes <- function(param) {
        if (!identical(param, cache$param)) {
          cache$param <- param
          cache$modes <- compute_modes(param)
        }
      }

      list(
        fn = function(param) {
          ensure_modes(param)
          ll <- 0
          for (i in seq_along(cluster_fns)) {
            u_hat <- cache$modes[[i]]$u_hat
            g_star_fn <- cache$modes[[i]]$g_star
            ll_mode <- g_star_fn(u_hat)
            neg_H <- tryCatch(
              {
                H <- numDeriv::hessian(g_star_fn, u_hat)
                S <- -(H + t(H)) / 2
                S
              },
              error = function(e) Sigma_inv + diag(J_minus_1)
            )
            chol_neg_H <- tryCatch(chol(neg_H), error = function(e) NULL)
            log_det_neg_H <- if (!is.null(chol_neg_H)) {
              2 * sum(log(diag(chol_neg_H)))
            } else {
              as.numeric(determinant(neg_H, logarithm = TRUE)$modulus)
            }
            ll <- ll + ll_mode - 0.5 * log_det_Sigma - 0.5 * log_det_neg_H
          }
          -ll
        },
        gr = function(param) {
          ensure_modes(param)
          B <- extract_B(param, p, J)
          grad <- matrix(0, nrow = p, ncol = J_minus_1)
          for (i in seq_along(cluster_fns)) {
            X_c <- cluster_fns[[i]]$X
            m_i <- cluster_fns[[i]]$m
            u_hat <- cache$modes[[i]]$u_hat
            eta_B <- X_c %*%
              B +
              matrix(u_hat, nrow = m_i, ncol = J_minus_1, byrow = TRUE)
            eta_Bhat <- X_c %*%
              B_hat +
              matrix(u_hat, nrow = m_i, ncol = J_minus_1, byrow = TRUE)
            p_hat <- softmax(eta_B)
            q_hat <- softmax(eta_Bhat)
            grad <- grad + t(X_c) %*% (q_hat - p_hat)
          }
          -as.numeric(grad)
        }
      )
    }
  } else {
    function(omega_hat) {
      B_hat <- make_B_hat(omega_hat, B_mle, x_0)

      cache <- new.env(parent = emptyenv())
      cache$param <- NULL
      cache$modes <- NULL

      compute_modes <- function(param) {
        up <- unpack_param(param, p, J)
        B <- up$B
        Sigma_inv <- solve(up$Sigma)
        modes <- vector("list", length(cluster_fns))
        for (i in seq_along(cluster_fns)) {
          X_c <- cluster_fns[[i]]$X
          m_i <- cluster_fns[[i]]$m
          fns <- make_cluster_g_star(X_c, m_i, B, B_hat, Sigma_inv)
          u_hat <- find_cluster_mode(fns, J_minus_1)
          modes[[i]] <- list(
            u_hat = u_hat,
            g_star = fns$fn,
            Sigma_inv = Sigma_inv
          )
        }
        modes
      }

      ensure_modes <- function(param) {
        if (!identical(param, cache$param)) {
          cache$param <- param
          cache$modes <- compute_modes(param)
        }
      }

      list(
        fn = function(param) {
          ensure_modes(param)
          up <- unpack_param(param, p, J)
          log_det_Sigma <- as.numeric(
            determinant(up$Sigma, logarithm = TRUE)$modulus
          )
          ll <- 0
          for (i in seq_along(cluster_fns)) {
            u_hat <- cache$modes[[i]]$u_hat
            g_star_fn <- cache$modes[[i]]$g_star
            Sigma_inv <- cache$modes[[i]]$Sigma_inv
            ll_mode <- g_star_fn(u_hat)
            neg_H <- tryCatch(
              {
                H <- numDeriv::hessian(g_star_fn, u_hat)
                S <- -(H + t(H)) / 2
                S
              },
              error = function(e) Sigma_inv + diag(J_minus_1)
            )
            chol_neg_H <- tryCatch(chol(neg_H), error = function(e) NULL)
            log_det_neg_H <- if (!is.null(chol_neg_H)) {
              2 * sum(log(diag(chol_neg_H)))
            } else {
              as.numeric(determinant(neg_H, logarithm = TRUE)$modulus)
            }
            ll <- ll + ll_mode - 0.5 * log_det_Sigma - 0.5 * log_det_neg_H
          }
          -ll
        },
        gr = function(param) {
          ensure_modes(param)
          up <- unpack_param(param, p, J)
          B <- up$B
          Sigma_inv <- cache$modes[[1L]]$Sigma_inv
          n_B <- p * J_minus_1

          grad_B <- matrix(0, nrow = p, ncol = J_minus_1)
          for (i in seq_along(cluster_fns)) {
            X_c <- cluster_fns[[i]]$X
            m_i <- cluster_fns[[i]]$m
            u_hat <- cache$modes[[i]]$u_hat
            eta_B <- X_c %*%
              B +
              matrix(u_hat, nrow = m_i, ncol = J_minus_1, byrow = TRUE)
            eta_Bhat <- X_c %*%
              B_hat +
              matrix(u_hat, nrow = m_i, ncol = J_minus_1, byrow = TRUE)
            p_hat <- softmax(eta_B)
            q_hat <- softmax(eta_Bhat)
            grad_B <- grad_B + t(X_c) %*% (q_hat - p_hat)
          }

          gr_Sigma <- numDeriv::grad(
            func = function(s) {
              up2 <- unpack_param(c(param[seq_len(n_B)], s), p, J)
              eloglik_core(up2$B, B_hat, up2$Sigma, cluster_fns)
            },
            x = param[seq(n_B + 1L, length(param))]
          )

          c(-as.numeric(grad_B), -gr_Sigma)
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
    name = cfg$name %||% "Marginal multinomial likelihood (Laplace, ZSE)",
    loglik = loglik,
    E_loglik = E_loglik,
    E_loglik_grad = E_loglik_grad,
    needs_param_mle = TRUE,
    omega_hat_from_param_mle = omega_hat_from_param_mle,
    make_branch_fns = make_branch_fns
  )
}
