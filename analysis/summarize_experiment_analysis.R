# ============================================================
# summarize_experiment_analysis.R
#
# Summarize frequency properties across simulations
# ============================================================

suppressPackageStartupMessages({
  library(fs)
  library(yaml)
  library(dplyr)
  library(tidyr)
})

# ------------------------------------------------------------
# USER INPUT
# ------------------------------------------------------------
exp_dir <- "experiments/multinom/baseline_logit"

exp_dir <- path_abs(exp_dir)

if (!dir_exists(exp_dir)) {
  stop("Experiment directory not found: ", exp_dir, call. = FALSE)
}

# ------------------------------------------------------------
# Locate simulation folders
# ------------------------------------------------------------
sim_dirs <- dir_ls(exp_dir, type = "directory")
sim_dirs <- sim_dirs[startsWith(path_file(sim_dirs), "sim_")]

if (length(sim_dirs) == 0L) {
  stop("No sim_* folders found in experiment.", call. = FALSE)
}

# ------------------------------------------------------------
# Helper: extract design factors from simulation.yml
# ------------------------------------------------------------
extract_design_factors <- function(sim_dir) {
  cfg_path <- path(sim_dir, "simulation.yml")

  if (!file_exists(cfg_path)) {
    stop("Missing simulation.yml in ", sim_dir)
  }

  cfg <- read_yaml(cfg_path)

  tibble(
    simulation = path_file(sim_dir),
    J = cfg$parameter$J,
    entropy_target_frac = cfg$parameter$entropy_target_frac,
    n_obs = cfg$data$n_obs
  )
}

# ------------------------------------------------------------
# Helper: load + summarize one simulation
# ------------------------------------------------------------
summarize_sim <- function(sim_dir) {
  point_path <- path(sim_dir, "analysis", "sim_point_metrics.rds")
  interval_path <- path(sim_dir, "analysis", "sim_interval_metrics.rds")

  if (!file_exists(point_path) || !file_exists(interval_path)) {
    warning("Missing analysis artifacts in ", sim_dir)
    return(NULL)
  }

  point_metrics <- readRDS(point_path)
  interval_metrics <- readRDS(interval_path)

  # ---- point estimator summaries ----
  point_summary <- point_metrics |>
    group_by(pseudolikelihood) |>
    summarise(
      bias = mean(bias, na.rm = TRUE),
      rmse = sqrt(mean(sq_error, na.rm = TRUE)),
      mc_sd = sd(psi_hat, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- interval summaries (by level) ----
  interval_summary <- interval_metrics |>
    filter(!is.na(valid_ci)) |>
    group_by(pseudolikelihood, level) |>
    summarise(
      coverage = mean(covered, na.rm = TRUE),
      valid_rate = mean(valid_ci, na.rm = TRUE),
      mean_ci_length = mean(ci_length, na.rm = TRUE),
      .groups = "drop"
    )

  list(
    point = point_summary,
    interval = interval_summary
  )
}

# ------------------------------------------------------------
# Main loop over simulations
# ------------------------------------------------------------
point_rows <- list()
interval_rows <- list()

for (sim_dir in sim_dirs) {
  sim_id <- path_file(sim_dir)

  design_df <- extract_design_factors(sim_dir)
  sim_sum <- summarize_sim(sim_dir)

  if (is.null(sim_sum)) {
    next
  }

  # attach design factors
  point_rows[[sim_id]] <- sim_sum$point |>
    mutate(simulation = sim_id) |>
    left_join(design_df, by = "simulation")

  interval_rows[[sim_id]] <- sim_sum$interval |>
    mutate(simulation = sim_id) |>
    left_join(design_df, by = "simulation")
}

# ------------------------------------------------------------
# Bind experiment-level summaries
# ------------------------------------------------------------
point_exp_df <- bind_rows(point_rows)
interval_exp_df <- bind_rows(interval_rows)

# ------------------------------------------------------------
# Save experiment-level artifacts
# ------------------------------------------------------------
analysis_dir <- path(exp_dir, "analysis")
dir_create(analysis_dir)

saveRDS(
  point_exp_df,
  path(analysis_dir, "point_exp_df.rds")
)

saveRDS(
  interval_exp_df,
  path(analysis_dir, "interval_exp_df.rds")
)

message("✔ Experiment summaries saved")
message("  • analysis/point_exp_df.rds")
message("  • analysis/interval_exp_df.rds")
