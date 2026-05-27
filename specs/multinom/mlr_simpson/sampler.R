# ======================================================================
# Sampler Specification (Multinomial Logistic Regression Parameterization)
# Target: Simpson's Index D(theta(x_0; B)) = sum(theta_j(x_0; B)^2)
#
# Samples omega_hat as a probability vector from the level set
#
#   Omega_psi_hat = { theta ∈ Delta^{J-1} : D(theta) = psi_hat }
#
# This set has the same sphere geometry as in the simple logit case:
# a (J-2)-sphere of radius r = sqrt(psi_hat - 1/J) centered at
# c = (1/J)1 within the affine hyperplane Pi = {v : sum(v) = 1}.
#
# Sampling is exact and cheap — no optimization required. The candidate
# is a length-J probability vector interpreted as the conditional
# category distribution at the reference covariate x_0 = colMeans(X).
# E_loglik uses omega_hat to construct B_hat via a rank-1 adjustment
# of B_mle, preserving the full covariate structure in the objective.
#
#   Disconnected regime (psi_hat >= 1/(J-1)):
#     Omega_psi_hat breaks into J disjoint spherical caps. Cap selection
#     is weighted by empirical category frequencies. data$Y uses natural
#     factor ordering (levels = 1:J), so table(data$Y) returns counts
#     in natural category order and p_emp[j] is the frequency of
#     category j.
#
#   Connected regime (psi_hat < 1/(J-1)):
#     Omega_psi_hat is a full (J-2)-sphere. Draws are taken uniformly
#     by projecting a Gaussian onto the tangent space of Pi.
#
# Returns function(history = NULL) -> list(candidate, diag):
#   $candidate      — numeric vector of length J (omega-hat as a
#                     probability vector in Delta^{J-1})
#   $diag$cap              — integer cap index; NA in connected regime
#   $diag$is_dominant_cap  — logical; NA in connected regime
# ======================================================================

# ======================================================================
# 1. Sphere sampler constructor
# ======================================================================

simpson_sampler_fn <- function(param_dim, psi_mle, data, ...) {
  J <- attr(data, "J")
  r <- sqrt(psi_mle - 1 / J)
  c_p <- rep(1 / J, J)
  disconnected <- psi_mle >= 1 / (J - 1)

  # data$Y has natural factor ordering (levels = 1:J), so table() returns
  # counts in natural category order: p_emp[j] is the frequency of category j.
  counts <- table(data$Y)
  p_emp <- as.numeric(counts) / sum(counts)
  dominant_cap <- which.max(p_emp)

  if (disconnected) {
    # Cap axes: unit vector from c toward e_j within Pi, for all j
    n_caps <- lapply(seq_len(J), function(j) {
      ej_minus_c <- rep(-1 / J, J)
      ej_minus_c[j] <- (J - 1) / J
      ej_minus_c / sqrt(sum(ej_minus_c^2))
    })

    cos_alpha <- {
      num <- 1 + J * sqrt((J - 2) * ((J - 1) * psi_mle - 1))
      denom <- (J - 1) * sqrt((J - 1) * (J * psi_mle - 1))
      num / denom
    }
    alpha <- acos(cos_alpha)
    sin_alpha <- sin(alpha)

    draw_h <- if (J == 2L) {
      function() 1
    } else {
      a <- (J - 2) / 2
      p_alpha <- pbeta(sin_alpha^2, a, 0.5)
      function() {
        s <- qbeta(runif(1) * p_alpha, a, 0.5)
        sqrt(1 - s)
      }
    }

    function(history = NULL) {
      j <- sample.int(J, size = 1L, prob = p_emp)
      n_j <- n_caps[[j]]
      h <- draw_h()
      w <- rnorm(J)
      w <- w - mean(w)
      w <- w - sum(w * n_j) * n_j
      norm_w <- sqrt(sum(w * w))

      v <- if (norm_w < 1e-10) {
        r * n_j
      } else {
        u <- w / norm_w
        r * (h * n_j + sqrt(max(0, 1 - h^2)) * u)
      }

      x <- c_p + v
      list(
        candidate = x,
        diag = list(
          cap = j,
          is_dominant_cap = j == dominant_cap
        )
      )
    }
  } else {
    function(history = NULL) {
      repeat {
        v <- rnorm(J)
        v <- v - mean(v)
        norm_v <- sqrt(sum(v * v))
        if (norm_v >= 1e-10) break
      }
      v <- v / norm_v * r
      x <- c_p + v
      list(
        candidate = x,
        diag = list(
          cap = NA_integer_,
          is_dominant_cap = NA
        )
      )
    }
  }
}

# ======================================================================
# 2. Spec constructor
# ======================================================================

#' Build a sampler_spec for Simpson's index under the MLR model
#'
#' Draws omega_hat as a probability vector from Omega_psi_hat in
#' Delta^{J-1} using the sphere geometry. omega_hat is interpreted as
#' the conditional category distribution at x_0 = colMeans(X_design).
#' E_loglik uses it to construct an observation-specific reference
#' B_hat via a rank-1 adjustment of B_mle, preserving the full
#' covariate structure in the branch objective.
#'
#' @param config  Simulation config list. Must contain a 'sampler' section.
#' @return        A \code{sampler_spec} object.
make_sampler <- function(config) {
  cfg <- config$sampler

  if (is.null(cfg)) {
    stop("Config must contain a 'sampler' section.", call. = FALSE)
  }

  likelyr::sampler_spec(
    sampler_fn = simpson_sampler_fn,
    min_branches = cfg$min_branches,
    branch_buffer = cfg$branch_buffer %||% 0L,
    name = "Simpson's index sphere sampler (MLR)"
  )
}
