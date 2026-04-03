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
#     Base draws are taken exactly from the cap near e_1 by decomposing
#     each point into a height component along the cap axis n_1 and a
#     uniform component in the orthogonal subspace of Pi.
#
#     The height coordinate h = cos(theta) is sampled via inversion of
#     the marginal CDF of theta ~ sin(theta)^(J-3) on [0, alpha]:
#       theta = arcsin(U^(1/(J-2)) * sin(alpha)),  U ~ Uniform(0,1)
#     This is exact and efficient regardless of how narrow the cap is.
#     No simplex boundary rejection is needed. Orbit expansion covers
#     all J caps.
#
#   Connected regime (psi_hat < 1/(J-1)):
#     L_psi_hat lies entirely within Delta^{J-1}. Base draws are taken
#     uniformly from the full (J-2)-sphere by projecting a Gaussian
#     onto the tangent space of Pi and normalizing. No rejection needed.
#     Orbit expansion applies but all members lie on the same component.
# ======================================================================

# ======================================================================
# 1. Sphere sampler constructor
#
# Returns function(history = NULL) -> numeric vector (omega-hat in
# logit space). history is accepted but ignored since this sampler
# draws directly from the geometry rather than adapting to past draws.
# ======================================================================

simpson_sampler_fn <- function(param_dim, psi_mle, ...) {
  J <- param_dim + 1L
  r <- sqrt(psi_mle - 1 / J)
  c_p <- rep(1 / J, J)
  disconnected <- psi_mle >= 1 / (J - 1)

  if (disconnected) {
    # ------------------------------------------------------------------
    # Disconnected regime: exact uniform draw from cap near e_1.
    #
    # Any point on the (J-2)-sphere can be written as:
    #   v = r * (h * n_1 + sqrt(1 - h^2) * u)
    # where:
    #   n_1  = unit vector along cap axis (from c toward e_1)
    #   h    = cos(theta) in [cos(alpha), 1], the height coordinate
    #   u    = unit vector in the (J-3)-dimensional subspace of the
    #          tangent space of Pi orthogonal to n_1
    #
    # theta is sampled via exact inversion of its marginal CDF:
    #   theta ~ sin(theta)^(J-3) on [0, alpha]
    #   theta = arcsin(U^(1/(J-2)) * sin(alpha)),  U ~ Uniform(0,1)
    # This is exact and efficient for any cap width.
    # ------------------------------------------------------------------

    # Cap axis: unit vector from c toward e_1 within Pi
    e1_minus_c <- c((J - 1) / J, rep(-1 / J, J - 1L))
    n_1 <- e1_minus_c / sqrt(sum(e1_minus_c^2))

    # Angular radius alpha (derived in dissertation, eq. alpha-psi-hat-general)
    cos_alpha <- {
      num <- 1 + J * sqrt((J - 2) * ((J - 1) * psi_mle - 1))
      denom <- (J - 1) * sqrt((J - 1) * (J * psi_mle - 1))
      num / denom
    }
    alpha <- acos(cos_alpha)
    sin_alpha <- sin(alpha)

    # Exact height sampler via inversion
    draw_h <- if (J == 2L) {
      # Degenerate: 0-sphere, only the cap center
      function() 1
    } else if (J == 3L) {
      # 1-sphere: theta uniform on [0, alpha], so h uniform on [cos_alpha, 1]
      function() runif(1, cos_alpha, 1)
    } else {
      # J >= 4: exact inversion of sin(theta)^(J-3) on [0, alpha]
      #   theta = arcsin(U^(1/(J-2)) * sin(alpha)),  U ~ Uniform(0,1)
      inv_dim <- 1 / (J - 2L)
      function() {
        theta <- asin(runif(1)^inv_dim * sin_alpha)
        cos(theta)
      }
    }

    function(history = NULL) {
      h <- draw_h()

      # Sample unit vector u in subspace of Pi orthogonal to n_1:
      # generate Gaussian, project out 1 direction then n_1 direction
      w <- rnorm(J)
      w <- w - mean(w) # project onto tangent space of Pi
      w <- w - sum(w * n_1) * n_1 # project out n_1 component
      norm_w <- sqrt(sum(w * w))

      v <- if (norm_w < 1e-10) {
        r * n_1 # degenerate: snap to cap center
      } else {
        u <- w / norm_w
        r * (h * n_1 + sqrt(max(0, 1 - h^2)) * u)
      }

      x <- c_p + v
      log(x[seq_len(J - 1L)]) - log(x[J])
    }
  } else {
    # ------------------------------------------------------------------
    # Connected regime: L_psi_hat lies entirely within Delta^{J-1}.
    # Draw uniformly from the full (J-2)-sphere — no rejection needed.
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
      log(x[seq_len(J - 1L)]) - log(x[J])
    }
  }
}

# ======================================================================
# 2. Orbit expander constructor
#
# Returns function(omega_hat) -> list of numeric vectors.
# Each element is a permuted variant of omega_hat obtained by applying
# a random permutation of category labels in full J-space and
# re-baselining to the last category.
# ======================================================================

simpson_orbit_expander_fn <- function(param_dim, orbit_size = NULL, ...) {
  J <- param_dim + 1L

  function(omega_hat) {
    K <- orbit_size %||% factorial(J)

    eta <- as.numeric(omega_hat)

    lapply(seq_len(K), function(i) {
      perm <- sample.int(J)
      eta_full <- c(eta, 0.0)
      eta_perm <- eta_full[perm]
      eta_perm <- eta_perm - eta_perm[J]
      eta_perm[seq_len(J - 1L)]
    })
  }
}

# ======================================================================
# 3. Spec constructor
# ======================================================================

make_sampler <- function(config) {
  cfg <- config$sampler
  if (is.null(cfg)) {
    stop("Config must contain a 'sampler' section.", call. = FALSE)
  }

  sampler_spec(
    sampler_fn = simpson_sampler_fn,
    orbit_expander_fn = simpson_orbit_expander_fn,
    orbit_size = cfg$orbit_size %||% NULL,
    min_branches = cfg$min_branches,
    branch_buffer = cfg$branch_buffer %||% 0L,
    name = "Simpson's index geometric sampler"
  )
}
