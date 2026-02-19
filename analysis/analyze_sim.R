#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(likelyr)
  library(here)
  library(fs)
  library(dplyr)
})

# ============================================================
# Parse CLI arguments
#   analyze_sim.R <sim_dir>
# ============================================================
args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 1L) {
  stop(
    "Usage: Rscript analyze_sim.R <experiments/.../sim_k>",
    call. = FALSE
  )
}

sim_dir <- path_abs(args[[1]])

if (!dir_exists(sim_dir)) {
  stop("Simulation directory not found: ", sim_dir, call. = FALSE)
}

sim_id <- path_file(sim_dir)

# experiment id = parent directory name
exp_dir <- path_dir(sim_dir)
exp_id <- path_file(exp_dir)

# ============================================================
# Anchor project root + utilities
# ============================================================
root <- here()

source(
  file.path(root, "analysis", "utils.R"),
  local = TRUE
)

# ============================================================
# Locate iterations
# ============================================================
iter_root <- path(sim_dir, "iterations")

if (!dir_exists(iter_root)) {
  stop("No iterations directory found for ", sim_id, call. = FALSE)
}

iter_dirs <- dir_ls(iter_root, type = "directory")

if (length(iter_dirs) == 0L) {
  stop("No iteration folders found for ", sim_id, call. = FALSE)
}

point_results <- list()
interval_results <- list()

# ============================================================
# Main traversal: iteration
# ============================================================
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

  # ----------------------------------------------------------
  # infer + compare (post-processing only)
  # ----------------------------------------------------------
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

  psi_0 <- unique(point_df$psi_0)[1]

  # ----------------------------------------------------------
  # Point metrics (NO level duplication)
  # ----------------------------------------------------------
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

  # ----------------------------------------------------------
  # Shape diagnostics (point-level)
  # ----------------------------------------------------------
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

  point_metrics <- point_metrics |>
    full_join(
      shape_metrics,
      by = c("experiment", "simulation", "iteration", "pseudolikelihood")
    )

  if (!is.null(runtime_df)) {
    point_metrics <- point_metrics |>
      full_join(runtime_df, by = "pseudolikelihood")
  }

  # ----------------------------------------------------------
  # Interval metrics (LEVEL-SPECIFIC)
  # ----------------------------------------------------------
  interval_raw <- attr(interval_df, "interval_estimates_raw") |>
    mutate(
      pseudolikelihood = recode_values(
        pseudolikelihood,
        "integrate" ~ "Integrated",
        "profile" ~ "Profile"
      )
    )

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
        covered = ifelse(
          d$valid_ci,
          psi_0 >= d$lower & psi_0 <= d$upper,
          NA
        ),

        stringsAsFactors = FALSE
      )
    }
  ) |>
    bind_rows()

  # ----------------------------------------------------------
  # Collect
  # ----------------------------------------------------------
  point_results[[length(point_results) + 1]] <- point_metrics
  interval_results[[length(interval_results) + 1]] <- interval_metrics
}

# ============================================================
# Bind + save
# ============================================================
sim_point_df <- bind_rows(point_results)
sim_interval_df <- bind_rows(interval_results)

if (nrow(sim_point_df) == 0L && nrow(sim_interval_df) == 0L) {
  stop("No valid iteration results processed for ", sim_id, call. = FALSE)
}

analysis_dir <- path(sim_dir, "analysis")
dir_create(analysis_dir)

saveRDS(
  sim_point_df,
  path(analysis_dir, "sim_point_metrics.rds")
)

saveRDS(
  sim_interval_df,
  path(analysis_dir, "sim_interval_metrics.rds")
)

message("✔ Analysis complete for ", sim_id)
message("✔ Saved:")
message("  • analysis/sim_point_metrics.rds")
message("  • analysis/sim_interval_metrics.rds")
