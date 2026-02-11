# ============================================================
# util.s — Utility functions for running simulations
# ============================================================

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

load_spec_env <- function(dir) {
  env <- new.env(parent = globalenv())
  for (f in c(
    "parameter.R",
    "likelihood.R",
    "estimand.R",
    "nuisance.R",
    "optimizer.R",
    "execution.R"
  )) {
    source(file.path(dir, f), local = env)
  }
  for (nm in ls(env, pattern = "^make_")) {
    environment(env[[nm]]) <- asNamespace("likelyr")
  }
  env
}

# ============================================================
# Likelihood-shape diagnostics
# ============================================================

#' Extract likelihood shape diagnostics for a pseudolikelihood
#'
#' @description
#' Computes local curvature, likelihood width at Δℓ = 0.5,
#' and a simple skewness proxy for the log-likelihood curve
#' of the parameter of interest ψ.
#'
#' This function is intentionally conservative: if required
#' components are missing or ill-conditioned, it returns NA
#' values rather than erroring.
#'
#' @param model A calibrated `likelyr` model after infer() + compare()
#' @param pseudolikelihood Character scalar identifying the
#'   pseudolikelihood (e.g. "Integrated", "Profile")
#'
#' @return Named list with elements:
#'   • curvature
#'   • width_05
#'   • skewness
#'
#' @keywords internal
extract_likelihood_shape <- function(model, pseudolikelihood) {
  # ----------------------------------------------------------
  # Locate likelihood curve
  # ----------------------------------------------------------
  psi_ll_df <- tryCatch(
    {
      model$workspace[[tolower(pseudolikelihood)]]$psi_ll_df
    },
    error = function(e) NULL
  )

  if (
    is.null(psi_ll_df) ||
      !all(c("psi", "loglik") %in% names(psi_ll_df)) ||
      nrow(psi_ll_df) < 5
  ) {
    return(list(
      curvature = NA_real_,
      width_05 = NA_real_,
      skewness = NA_real_
    ))
  }

  # Ensure ordering
  psi_ll_df <- psi_ll_df[order(psi_ll_df$psi), , drop = FALSE]

  # ----------------------------------------------------------
  # Identify mode
  # ----------------------------------------------------------
  i_max <- which.max(psi_ll_df$loglik)
  psi_hat <- psi_ll_df$psi[i_max]
  ll_max <- psi_ll_df$loglik[i_max]

  # ----------------------------------------------------------
  # Local curvature via quadratic fit
  # ----------------------------------------------------------
  idx <- max(1L, i_max - 2L):min(nrow(psi_ll_df), i_max + 2L)

  curvature <- tryCatch(
    {
      fit <- lm(
        loglik ~ poly(psi, 2, raw = TRUE),
        data = psi_ll_df[idx, , drop = FALSE]
      )
      coef2 <- coef(fit)[3] |>
        unname()
      if (is.na(coef2) || coef2 >= 0) NA_real_ else -2 * coef2
    },
    error = function(e) NA_real_
  )

  # ----------------------------------------------------------
  # Width at Δℓ = 0.5
  # ----------------------------------------------------------
  cutoff <- ll_max - 0.5

  psi_in <- psi_ll_df$psi[psi_ll_df$loglik >= cutoff]

  width_05 <- if (length(psi_in) >= 2) {
    diff(range(psi_in))
  } else {
    NA_real_
  }

  # ----------------------------------------------------------
  # Skewness proxy (relative mass left vs right of ψ̂)
  # ----------------------------------------------------------
  skewness <- tryCatch(
    {
      w <- exp(psi_ll_df$loglik - ll_max)
      left <- sum(w[psi_ll_df$psi < psi_hat])
      right <- sum(w[psi_ll_df$psi > psi_hat])
      if ((left + right) > 0) {
        (right - left) / (right + left)
      } else {
        NA_real_
      }
    },
    error = function(e) NA_real_
  )

  list(
    curvature = curvature,
    width_05 = width_05,
    skewness = skewness
  )
}
