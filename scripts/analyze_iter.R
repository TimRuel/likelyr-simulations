#!/usr/bin/env Rscript

# ============================================================
# analyze_iter.R  (batch/programmatic version)
#
# Contract:
#   • Accepts <path/to/sim_XX/> as CLI argument
#   • Loops over all iterations/iter_XXXX/ directories
#   • Loads each model.rds, runs infer() and compare()
#   • Extracts point and interval estimate metrics
#   • Saves combined metrics to sim_dir/analysis/
#
# Output:
#   <sim_dir>/analysis/metrics_point_iteration.rds
#   <sim_dir>/analysis/metrics_interval_iteration.rds
#
# Skips iterations missing model.rds (failed or incomplete).
# ============================================================

suppressPackageStartupMessages({
  library(likelyr)
  library(here)
  library(fs)
  library(yaml)
  library(dplyr)
  library(purrr)
})

# ============================================================
# Parse CLI arguments
# ============================================================
args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 1L) {
  stop(
    "Usage: Rscript scripts/analyze_iter.R <path/to/sim_dir>",
    call. = FALSE
  )
}

sim_dir <- path_abs(args[[1]])

if (!dir_exists(sim_dir)) {
  stop("Simulation directory not found: ", sim_dir, call. = FALSE)
}

sim_id <- path_file(sim_dir)
iter_dir <- path(sim_dir, "iterations")
analysis_dir <- path(sim_dir, "analysis")

dir_create(analysis_dir, recurse = TRUE)

if (!dir_exists(iter_dir)) {
  stop("No iterations directory found in: ", sim_dir, call. = FALSE)
}

# ============================================================
# Discover completed iterations
# ============================================================
iter_dirs <- dir_ls(iter_dir, type = "directory", regexp = "iter_\\d+$")

if (length(iter_dirs) == 0L) {
  stop("No iter_* directories found in: ", iter_dir, call. = FALSE)
}

model_paths <- path(iter_dirs, "model.rds")
complete <- file_exists(model_paths)

n_total <- length(iter_dirs)
n_complete <- sum(complete)

message("▶ ", sim_id, ": ", n_complete, "/", n_total, " iterations complete")

if (n_complete == 0L) {
  stop("No completed iterations found.", call. = FALSE)
}

# ============================================================
# Extract metrics from each completed iteration
# ============================================================
extract_metrics <- function(model_path, iter_index) {
  model <- tryCatch(readRDS(model_path), error = function(e) NULL)
  if (is.null(model)) {
    message("  ⚠ Could not load: ", model_path)
    return(NULL)
  }

  model <- tryCatch(
    model |> infer() |> compare(),
    error = function(e) {
      message(
        "  ⚠ infer()/compare() failed for iter ",
        iter_index,
        ": ",
        e$message
      )
      NULL
    }
  )
  if (is.null(model)) {
    return(NULL)
  }

  # ------------------------------------------------------------------
  # Point estimates
  # ------------------------------------------------------------------
  point_rows <- list()

  for (pl in c("profile", "integrated")) {
    res <- model$workspace[[pl]]
    if (is.null(res) || is.null(res$inference)) {
      next
    }
    df <- res$inference$point_estimate_df
    if (is.null(df)) {
      next
    }
    point_rows[[pl]] <- df |>
      mutate(
        iter_index = iter_index,
        pseudolikelihood = pl,
        .before = everything()
      )
  }

  point_df <- if (length(point_rows) > 0) bind_rows(point_rows) else NULL

  # ------------------------------------------------------------------
  # Interval estimates
  # ------------------------------------------------------------------
  interval_rows <- list()

  for (pl in c("profile", "integrated")) {
    res <- model$workspace[[pl]]
    if (is.null(res) || is.null(res$inference)) {
      next
    }
    df <- res$inference$interval_estimate_df
    if (is.null(df)) {
      next
    }
    interval_rows[[pl]] <- df |>
      mutate(
        iter_index = iter_index,
        pseudolikelihood = pl,
        .before = everything()
      )
  }

  interval_df <- if (length(interval_rows) > 0) {
    bind_rows(interval_rows)
  } else {
    NULL
  }

  list(point = point_df, interval = interval_df)
}

results <- map2(
  model_paths[complete],
  which(complete),
  extract_metrics
)

results <- Filter(Negate(is.null), results)

if (length(results) == 0L) {
  stop("No metrics could be extracted from any iteration.", call. = FALSE)
}

# ============================================================
# Combine and save
# ============================================================
point_df <- bind_rows(map(results, "point"))
interval_df <- bind_rows(map(results, "interval"))

saveRDS(point_df, path(analysis_dir, "metrics_point_iteration.rds"))
saveRDS(interval_df, path(analysis_dir, "metrics_interval_iteration.rds"))

message(
  "✔ Saved metrics for ",
  nrow(point_df) / n_distinct(point_df$pseudolikelihood),
  " iterations to ",
  analysis_dir
)
