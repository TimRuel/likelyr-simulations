# ======================================================================
# Sampler Specification (Logit Parameterization)
# Target: Simpson's Index D = sum(p_j^2)
#
# Exploits the geometry of Omega_psi_hat for Simpson's index:
#
#   L_psi_hat is a (J-2)-sphere of radius r = sqrt(psi_hat - 1/J)
#   centered at c = (1/J)1 within the affine plane Pi.
#
#   Disconnected regime (psi_hat >= 1/(J-1)):
#     Omega_psi_hat = J disjoint spherical caps, one near each vertex.
#     On each draw, a cap index j is sampled with probability
#     proportional to the empirical frequency p_j = n_j / n, restricted
#     to caps with p_j > 1/J (above-average categories). This excludes
#     rare categories whose omega-hats consistently produce branch modes
#     at the psi boundary, while recovering uniform selection over the
#     eligible caps when counts are balanced.
#
#     The polar angle gamma has marginal density proportional to
#     sin(gamma)^(J-3) on [0, alpha] (from the surface element of the
#     (J-2)-sphere). With the substitution s = sin^2(gamma), the CDF
#     of gamma is the regularized incomplete beta function with
#     parameters ((J-2)/2, 1/2), evaluated at sin^2(alpha). Exact
#     inversion via qbeta() gives h = cos(gamma) with no rejection.
#     All cap-independent quantities (alpha, draw_h) are precomputed
#     once; cap axes n_j are precomputed for all J caps.
#
#   Connected regime (psi_hat < 1/(J-1)):
#     L_psi_hat lies entirely within Delta^{J-1}. Base draws are taken
#     uniformly from the full (J-2)-sphere by projecting a Gaussian
#     onto the tangent space of Pi and normalizing. No rejection needed.
# ======================================================================

# ======================================================================
# 1. Sphere sampler constructor
#
# Returns function(history = NULL) -> list(candidate, diag):
#   $candidate      — numeric vector (omega-hat in logit space)
#   $diag$cap              — integer index j of the cap drawn from;
#                            NA in the connected regime
#   $diag$is_dominant_cap  — logical; TRUE if cap j matches the
#                            data-dominant category (argmax of counts);
#                            NA in the connected regime
#   $diag$n_eligible_caps  — number of above-average caps available
#                            for selection; NA in the connected regime
#
# history is accepted but ignored since this sampler draws directly
# from the geometry rather than adapting to past draws.
# ======================================================================

simpson_sampler_fn <- function(param_dim, psi_mle, counts, ...) {
  J <- param_dim + 1L
  r <- sqrt(psi_mle - 1 / J)
  c_p <- rep(1 / J, J)
  disconnected <- psi_mle >= 1 / (J - 1)

  # Cap selection: restrict to above-average categories (p_j > 1/J),
  # then weight proportional to empirical frequencies. This excludes
  # rare categories whose omega-hats produce boundary branch modes.
  p_emp <- counts / sum(counts)
  eligible <- which(p_emp > 1 / J)
  p_eligible <- p_emp[eligible]
  dominant_cap <- which.max(counts)

  if (disconnected) {
    # ------------------------------------------------------------------
    # Disconnected regime: exact uniform draw from cap near e_j, where
    # j is sampled proportional to p_emp on each call.
    #
    # Any point on the cap can be written as:
    #   x = c + v,  where  v = r * (h * n_j + sqrt(1 - h^2) * u)
    #
    # n_j  = unit vector along cap axis (from c toward e_j) in Pi
    # h    = cos(gamma) in [cos(alpha), 1], the height coordinate
    # u    = unit vector in the (J-3)-dimensional subspace of Pi
    #        orthogonal to n_j
    #
    # The surface element of the (J-2)-sphere decomposes as:
    #   dA \propto sin(gamma)^(J-3) * r * d(gamma) * d(sigma_{J-3})
    # so the marginal density of gamma is:
    #   f(gamma) \propto sin(gamma)^(J-3),  gamma in [0, alpha]
    #
    # With s = sin^2(gamma), the CDF of gamma is:
    #   F(gamma) = I_{sin^2(gamma)}((J-2)/2, 1/2)
    #              / I_{sin^2(alpha)}((J-2)/2, 1/2)
    # where I_x(a, b) is the regularized incomplete beta function.
    # Inversion: draw U ~ Uniform(0,1), then
    #   s = qbeta(U * pbeta(sin^2(alpha), (J-2)/2, 1/2), (J-2)/2, 1/2)
    #   h = cos(gamma) = sqrt(1 - s)
    # This is exact for all J >= 3 with no rejection.
    # ------------------------------------------------------------------

    # Cap axes: unit vector from c toward e_j within Pi, for all j
    n_caps <- lapply(seq_len(J), function(j) {
      ej_minus_c <- rep(-1 / J, J)
      ej_minus_c[j] <- (J - 1) / J
      ej_minus_c / sqrt(sum(ej_minus_c^2))
    })

    # Angular radius alpha (derived in dissertation, eq. alpha-psi-hat-general)
    # Depends only on psi_mle and J, not on which cap is selected
    cos_alpha <- {
      num <- 1 + J * sqrt((J - 2) * ((J - 1) * psi_mle - 1))
      denom <- (J - 1) * sqrt((J - 1) * (J * psi_mle - 1))
      num / denom
    }
    alpha <- acos(cos_alpha)
    sin_alpha <- sin(alpha)

    # Height sampler: exact inversion via regularized incomplete beta
    draw_h <- if (J == 2L) {
      # Degenerate: 0-sphere, cap collapses to a single point
      function() 1
    } else {
      # J >= 3: exact inversion of f(gamma) \propto sin(gamma)^(J-3)
      # via the substitution s = sin^2(gamma), which maps the CDF to
      # a regularized incomplete beta with parameters ((J-2)/2, 1/2).
      # Special cases:
      #   J = 3: Beta(1/2, 1/2) — arcsine distribution; reduces to
      #          gamma = U * alpha, h = cos(U * alpha)
      #   J = 4: Beta(1, 1/2); reduces to h uniform on [cos_alpha, 1]
      #   J >= 5: no simple closed form; qbeta() inverts numerically
      a <- (J - 2) / 2
      p_alpha <- pbeta(sin_alpha^2, a, 0.5)
      function() {
        s <- qbeta(runif(1) * p_alpha, a, 0.5)
        sqrt(1 - s)
      }
    }

    function(history = NULL) {
      # Select cap from eligible (above-average) categories,
      # weighted by empirical frequency
      j <- eligible[sample.int(length(eligible), size = 1L, prob = p_eligible)]
      n_j <- n_caps[[j]]

      h <- draw_h()

      # Sample unit vector u in subspace of Pi orthogonal to n_j:
      # generate Gaussian, project out the 1 direction (to stay in Pi),
      # then project out the n_j direction, then normalize.
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
        candidate = log(x[seq_len(J - 1L)]) - log(x[J]),
        diag = list(
          cap = j,
          is_dominant_cap = j == dominant_cap,
          n_eligible_caps = length(eligible)
        )
      )
    }
  } else {
    # ------------------------------------------------------------------
    # Connected regime: L_psi_hat lies entirely within Delta^{J-1}.
    # Draw uniformly from the full (J-2)-sphere by projecting a
    # Gaussian onto the tangent space of Pi and normalizing.
    # No rejection needed.
    # ------------------------------------------------------------------
    function(history = NULL) {
      repeat {
        v <- rnorm(J)
        v <- v - mean(v)
        norm_v <- sqrt(sum(v * v))
        if (norm_v < 1e-10) {
          next
        }
        break
      }
      v <- v / norm_v * r
      x <- c_p + v
      list(
        candidate = log(x[seq_len(J - 1L)]) - log(x[J]),
        diag = list(
          cap = NA_integer_,
          is_dominant_cap = NA,
          n_eligible_caps = NA_integer_
        )
      )
    }
  }
}

# ======================================================================
# 2. Spec constructor
# ======================================================================

make_sampler <- function(config) {
  cfg <- config$sampler
  if (is.null(cfg)) {
    stop("Config must contain a 'sampler' section.", call. = FALSE)
  }

  likelyr::sampler_spec(
    sampler_fn = simpson_sampler_fn,
    min_branches = cfg$min_branches,
    branch_buffer = cfg$branch_buffer %||% 0L,
    name = "Simpson's index geometric sampler"
  )
}
