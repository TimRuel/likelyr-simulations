#!/usr/bin/env Rscript

# ============================================================
# analyze_app.R
#
# Application-experiment analyzer — the truth-unknown counterpart to
# R/analyze_sim.R.
#
# Both analyzers consume the same directory layout
# (<sim_dir>/iterations/iter_XXXX/model.rds) because applications and
# simulations share the exp_vX / sim_XX naming convention. What differs
# is what can legitimately be computed:
#
#   simulation   psi_0 is the known data-generating value, so bias,
#                squared error and coverage are meaningful and the
#                interesting axis is aggregation ACROSS iterations
#                (frequency properties of the estimators).
#
#   application  psi_0 does not exist. The estimand spec still carries a
#                psi_0 because likelyr's estimand_spec() requires one
#                (see applications/multinom/ne_entropy/estimand.R, which
#                sets the placeholder log(J)/2), but every quantity
#                derived from it — error, contains_truth, and hence
#                bias / sq_error / covered — is meaningless here and is
#                deliberately NOT propagated. Each sim is one real
#                dataset (e.g. one dune meadow site) with a single
#                iteration, so the interesting axis is ACROSS sims.
#
# Consequently this script keeps what analyze_sim.R throws away: the
# actual CI endpoints (lower/upper). For a simulation those are only
# needed transiently to compute coverage and length; for an application
# they ARE the result, since each site is plotted as a pointrange.
#
# Writes to <sim_dir>/analysis/:
#   app_estimates.rds  one row per pseudolikelihood x confidence level
#   app_curves.rds     the psi/loglik grids underlying both curves
#   app_context.rds    one row of per-dataset context (N, J, psi_mle, ...)
#
# Unlike analyze_sim.R this does NOT delete <sim_dir>/model — an
# application experiment is cheap to re-analyze but the calibrated
# skeleton is the record of what was actually fitted to the real data.
#
# Usage:
#   Rscript R/analyze_app.R <exp_dir>/sim_XX
# ============================================================

suppressPackageStartupMessages({
  library(likelyr)
  library(fs)
  library(dplyr)
})

`%||%` <- function(x, y) if (is.null(x)) y else x

# ============================================================
# Parse CLI arguments
# ============================================================
args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 1L) {
  stop(
    "Usage: Rscript analyze_app.R <experiments/.../sim_k>",
    call. = FALSE
  )
}

sim_dir <- path_abs(args[[1]])

if (!dir_exists(sim_dir)) {
  stop("Simulation directory not found: ", sim_dir, call. = FALSE)
}

sim_id <- path_file(sim_dir)

exp_dir <- path_dir(sim_dir)
exp_id <- path_file(exp_dir)

# ============================================================
# Locate iterations
#
# An application experiment is configured with iterations: 1, so
# normally there is exactly one iter_0001. More than one is not an
# error (a re-run, or a sampler-seed sensitivity check), but it is
# worth flagging, and the iteration id is carried through so the
# downstream consumer can tell them apart.
# ============================================================
iter_root <- path(sim_dir, "iterations")

if (!dir_exists(iter_root)) {
  stop("No iterations directory found for ", sim_id, call. = FALSE)
}

iter_dirs <- dir_ls(iter_root, type = "directory")

if (length(iter_dirs) == 0L) {
  stop("No iteration folders found for ", sim_id, call. = FALSE)
}

if (length(iter_dirs) > 1L) {
  message(
    "ℹ ",
    sim_id,
    " has ",
    length(iter_dirs),
    " iterations; an application experiment normally has exactly one."
  )
}

estimate_results <- list()
curve_results <- list()
context_results <- list()

# ============================================================
# Extract the psi/loglik grid for one pseudolikelihood
#
# Both workspace$profile and workspace$integrated carry a
# psi_loglik_df with columns psi, loglik, rel_loglik and above_crit
# (the integrated one additionally carries n_support). rel_loglik and
# above_crit are kept because they are what a curve plot actually
# needs: rel_loglik puts both curves on a common scale with max 0, and
# above_crit marks the region the CI cutoff is drawn from. Columns are
# selected defensively so a missing extra does not abort the analysis.
# ============================================================
extract_curve <- function(workspace_result, label) {
  df <- workspace_result$psi_loglik_df

  if (is.null(df) || nrow(df) == 0L) {
    return(NULL)
  }

  keep <- intersect(
    c("psi", "loglik", "rel_loglik", "above_crit", "n_support"),
    names(df)
  )

  df |>
    as_tibble() |>
    select(all_of(keep)) |>
    mutate(pseudolikelihood = label, .before = 1)
}

# ============================================================
# Total sample size for one fitted dataset
#
# parameter_spec() carries an n_obs slot, but the data-driven parameter
# modes used by the applications leave it NA (the count vector is not
# known until calibrate time), so the data itself is the primary source
# and n_obs is the fallback.
# ============================================================
extract_n_obs <- function(model) {
  data <- model$data

  if (is.data.frame(data) && "count" %in% names(data)) {
    return(sum(data$count, na.rm = TRUE))
  }

  if (is.numeric(data)) {
    return(sum(data, na.rm = TRUE))
  }

  n_obs <- model$parameter$n_obs

  if (is.numeric(n_obs) && length(n_obs) == 1L) {
    return(n_obs)
  }

  NA_real_
}

# ============================================================
# Main traversal: iteration
# ============================================================
for (iter_dir in iter_dirs) {
  iter_id <- path_file(iter_dir)

  model_path <- path(iter_dir, "model.rds")

  if (!file_exists(model_path)) {
    warning("Missing model.rds: ", iter_id)
    next
  }

  model <- readRDS(model_path)

  model <- tryCatch(
    model |>
      infer() |>
      compare(),
    error = function(e) {
      warning("infer/compare failed for ", iter_id, ": ", e$message)
      NULL
    }
  )

  if (is.null(model)) {
    next
  }

  point_df <- model$workspace$comparison$point_estimates_df
  interval_df <- model$workspace$comparison$interval_estimates_df

  if (is.null(point_df) || is.null(interval_df)) {
    warning("Missing estimates: ", iter_id)
    next
  }

  # ----------------------------------------------------------
  # Estimates
  #
  # interval_estimates_df carries the formatted interval plus level;
  # the raw endpoints live in the interval_estimates_raw attribute
  # keyed by (pseudolikelihood, alpha) with a lowercase
  # pseudolikelihood label. Recovering lower/upper therefore means
  # rebuilding the alpha -> level map and joining, exactly as
  # analyze_sim.R does — the difference is that here the endpoints are
  # kept rather than dropped before saving.
  #
  # psi_0, error (point) and contains_truth (interval) are dropped:
  # all three are computed against the placeholder psi_0.
  # ----------------------------------------------------------
  interval_raw <- attr(interval_df, "interval_estimates_raw") |>
    mutate(pseudolikelihood = tools::toTitleCase(pseudolikelihood))

  alpha_to_level <- interval_raw |>
    distinct(alpha) |>
    mutate(level = paste0(round((1 - alpha) * 100), "%"))

  interval_raw <- interval_raw |>
    left_join(alpha_to_level, by = "alpha")

  point_estimates <- point_df |>
    select(pseudolikelihood, psi_hat, se_psi_hat)

  estimates <- interval_raw |>
    select(pseudolikelihood, level, alpha, lower, upper) |>
    right_join(
      interval_df |> select(pseudolikelihood, level, length),
      by = c("pseudolikelihood", "level")
    ) |>
    left_join(point_estimates, by = "pseudolikelihood") |>
    mutate(
      experiment = exp_id,
      simulation = sim_id,
      iteration = iter_id,
      valid_ci = is.finite(lower) & is.finite(upper),
      ci_length = ifelse(valid_ci, upper - lower, NA_real_),
      .before = 1
    ) |>
    select(
      experiment,
      simulation,
      iteration,
      pseudolikelihood,
      level,
      alpha,
      psi_hat,
      se_psi_hat,
      lower,
      upper,
      ci_length,
      valid_ci
    ) |>
    arrange(pseudolikelihood, alpha)

  # ----------------------------------------------------------
  # Curves
  # ----------------------------------------------------------
  curves <- bind_rows(
    extract_curve(model$workspace$profile, "Profile"),
    extract_curve(model$workspace$integrated, "Integrated")
  )

  if (!is.null(curves) && nrow(curves) > 0L) {
    curves <- curves |>
      mutate(
        experiment = exp_id,
        simulation = sim_id,
        iteration = iter_id,
        .before = 1
      )
  }

  # ----------------------------------------------------------
  # Context
  #
  # This is what makes the downstream plots self-contained. N is what
  # the dissertation figures order sites by; shipping it here means the
  # plotting script never has to reload the source dataset just to
  # recover a sample size. psi_mle / psi_lower / psi_upper give the
  # curve plots their reference lines and support bounds.
  # ----------------------------------------------------------
  psi_interval <- model$estimand$psi_interval

  context <- tibble(
    experiment = exp_id,
    simulation = sim_id,
    iteration = iter_id,
    n_obs = extract_n_obs(model),
    J = model$parameter$J %||% NA_integer_,
    param_dim = length(model$parameter$param_mle %||% numeric(0)),
    psi_mle = model$estimand$psi_mle %||% NA_real_,
    psi_lower = if (is.null(psi_interval)) NA_real_ else min(psi_interval),
    psi_upper = if (is.null(psi_interval)) NA_real_ else max(psi_interval),
    estimand_name = model$estimand$name %||% NA_character_
  )

  estimate_results[[length(estimate_results) + 1]] <- estimates
  curve_results[[length(curve_results) + 1]] <- curves
  context_results[[length(context_results) + 1]] <- context
}

# ============================================================
# Bind
# ============================================================
app_estimates <- bind_rows(estimate_results)
app_curves <- bind_rows(curve_results)
app_context <- bind_rows(context_results)

if (nrow(app_estimates) == 0L) {
  stop("No valid iteration results processed for ", sim_id, call. = FALSE)
}

# ============================================================
# Save
# ============================================================
analysis_dir <- path(sim_dir, "analysis")
dir_create(analysis_dir)

saveRDS(app_estimates, path(analysis_dir, "app_estimates.rds"))
saveRDS(app_curves, path(analysis_dir, "app_curves.rds"))
saveRDS(app_context, path(analysis_dir, "app_context.rds"))

n_invalid <- sum(!app_estimates$valid_ci)

message("✔ Analysis complete for ", sim_id)
message("✔ Saved:")
message(
  "  • analysis/app_estimates.rds  (",
  nrow(app_estimates),
  " rows, ",
  n_invalid,
  " with an invalid CI)"
)
message("  • analysis/app_curves.rds     (", nrow(app_curves), " grid points)")
message("  • analysis/app_context.rds    (", nrow(app_context), " rows)")
