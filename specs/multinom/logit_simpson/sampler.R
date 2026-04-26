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
#     On each draw, a cap index j is sampled proportional to empirical
#     frequencies p_j = n_j / n, restricted to above-average categories
#     (p_j > 1/J). If fewer than ceil(J/2) caps are eligible — meaning
#     the data are too concentrated for frequency-weighted selection to
#     be reliable — cap selection falls back to uniform over all J caps.
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
#   $diag$cap_selection    — "weighted" | "uniform_fallback" | NA
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
  # weighted by empirical frequencies. If fewer than ceil(J/2) caps
  # are eligible, fall back to uniform selection over all J caps.
  p_emp <- counts / sum(counts)
  eligible <- which(p_emp > 1 / J)
  p_eligible <- p_emp[eligible]
  dominant_cap <- which.max(counts)

  min_eligible <- ceiling(J / 2)
  use_weighted <- length(eligible) >= min_eligible

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
      if (use_weighted) {
        j <- eligible[
          sample.int(length(eligible), size = 1L, prob = p_eligible)
        ]
        cap_selection <- "weighted"
      } else {
        j <- sample.int(J, size = 1L)
        cap_selection <- "uniform_fallback"
      }

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
        candidate = log(x[seq_len(J - 1L)]) - log(x[J]),
        diag = list(
          cap = j,
          is_dominant_cap = j == dominant_cap,
          n_eligible_caps = length(eligible),
          cap_selection = cap_selection
        )
      )
    }
  } else {
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
          n_eligible_caps = NA_integer_,
          cap_selection = NA_character_
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
