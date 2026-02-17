#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(likelyr)
  library(here)
  library(fs)
  library(dplyr)
})

# ============================================================
# Parse CLI arguments
#   analyze_simulations.R <experiment_dir>
# ============================================================
args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 1L) {
  stop(
    "Usage: Rscript analyze_sims.R <experiments/<experiment>>",
    call. = FALSE
  )
}

exp_dir <- path_abs(args[[1]])

if (!dir_exists(exp_dir)) {
  stop("Experiment directory not found: ", exp_dir, call. = FALSE)
}

exp_id <- path_file(exp_dir)

# ============================================================
# Anchor project root + utilities
# ============================================================
root <- here()

source(
  file.path(root, "scripts", "utils.R"),
  local = TRUE
)

# ============================================================
# Locate simulations
# ============================================================
sim_dirs <- dir_ls(exp_dir, type = "directory")

if (length(sim_dirs) == 0L) {
  stop("No simulations found in experiment.", call. = FALSE)
}

results <- list()

# ============================================================
# Main traversal: simulation → iteration
# ============================================================
for (sim_dir in sim_dirs) {
  sim_id <- path_file(sim_dir)
  iter_root <- path(sim_dir, "iterations")

  if (!dir_exists(iter_root)) {
    warning("No iterations found for ", sim_id)
    next
  }

  iter_dirs <- dir_ls(iter_root, type = "directory")

  for (iter_dir in iter_dirs) {
    iter_id <- path_file(iter_dir)

    model_path <- path(iter_dir, "model.rds")
    runtime_path <- path(iter_dir, "runtime.rds")

    if (!file_exists(model_path)) {
      warning("Missing model.rds: ", iter_id)
      next
    }

    model <- readRDS(model_path)

    runtime_df <- if (file_exists(runtime_path)) {
      readRDS(runtime_path)
    } else {
      NULL
    }

    # --------------------------------------------------------
    # infer + compare (purely post-processing)
    # --------------------------------------------------------
    model <- model |>
      infer() |>
      compare()

    point_df <- model$workspace$comparison$point_estimates_df
    interval_df <- model$workspace$comparison$interval_estimates_df

    if (is.null(point_df) || is.null(interval_df)) {
      warning("Missing estimates: ", iter_id)
      next
    }

    psi_0 <- unique(point_df$psi_0)[1]

    # --------------------------------------------------------
    # Point metrics (per iteration)
    # --------------------------------------------------------
    point_metrics <- lapply(
      split(point_df, point_df$pseudolikelihood),
      function(d) {
        data.frame(
          experiment = exp_id,
          simulation = sim_id,
          iteration = iter_id,
          pseudolikelihood = d$pseudolikelihood,

          psi_hat = d$psi_hat,
          psi_0 = psi_0,

          bias = d$psi_hat - psi_0,
          sq_error = (d$psi_hat - psi_0)^2,

          stringsAsFactors = FALSE
        )
      }
    ) |>
      bind_rows()

    # --------------------------------------------------------
    # Interval metrics (unchanged logic)
    # --------------------------------------------------------
    interval_raw <- attr(interval_df, "interval_estimates_raw")

    alpha_to_level <- interval_raw |>
      distinct(alpha) |>
      mutate(level = paste0(round((1 - alpha) * 100), "%"))

    interval_raw <- interval_raw |>
      left_join(alpha_to_level, by = "alpha")

    interval_df2 <- interval_df |>
      left_join(
        interval_raw |>
          select(pseudolikelihood, level, alpha, lower, upper),
        by = c("pseudolikelihood", "level")
      ) |>
      mutate(valid_ci = is.finite(lower) & is.finite(upper))

    interval_metrics <- lapply(
      split(
        interval_df2,
        list(interval_df2$pseudolikelihood, interval_df2$level),
        drop = TRUE
      ),
      function(d) {
        data.frame(
          experiment = exp_id,
          simulation = sim_id,
          iteration = iter_id,
          pseudolikelihood = d$pseudolikelihood,
          level = d$level,
          alpha = d$alpha,

          valid_ci = d$valid_ci,
          ci_length = ifelse(d$valid_ci, d$upper - d$lower, NA),
          covered = ifelse(d$valid_ci, psi_0 >= d$lower & psi_0 <= d$upper, NA),

          stringsAsFactors = FALSE
        )
      }
    ) |>
      bind_rows()

    # --------------------------------------------------------
    # Shape diagnostics
    # --------------------------------------------------------
    shape_metrics <- lapply(
      unique(point_df$pseudolikelihood),
      function(p) {
        s <- extract_likelihood_shape(model, p)
        data.frame(
          experiment = exp_id,
          simulation = sim_id,
          iteration = iter_id,
          pseudolikelihood = p,
          curvature = s$curvature,
          width_05 = s$width_05,
          skewness = s$skewness,
          stringsAsFactors = FALSE
        )
      }
    ) |>
      bind_rows()

    # --------------------------------------------------------
    # Combine iteration-level results
    # --------------------------------------------------------
    iter_df <- point_metrics |>
      full_join(
        interval_metrics,
        by = c("experiment", "simulation", "iteration", "pseudolikelihood")
      ) |>
      full_join(
        shape_metrics,
        by = c("experiment", "simulation", "iteration", "pseudolikelihood")
      )

    if (!is.null(runtime_df)) {
      iter_df <- full_join(
        iter_df,
        runtime_df,
        by = "pseudolikelihood"
      )
    }

    results[[length(results) + 1]] <- iter_df
  }
}

# ============================================================
# Bind iteration-level results
# ============================================================
analysis_iter_df <- bind_rows(results)

if (nrow(analysis_iter_df) == 0L) {
  stop("No valid iteration results processed.", call. = FALSE)
}

# ============================================================
# Save artifacts
# ============================================================
analysis_dir <- path(exp_dir, "analysis")
dir_create(analysis_dir)

saveRDS(
  analysis_iter_df,
  path(analysis_dir, "metrics_iteration.rds")
)

message("✔ Analysis complete")
message("✔ Saved:")
message("  • metrics_iteration.rds")
