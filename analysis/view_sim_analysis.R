# ============================================================
# view_sim_analysis.R
#
# Interactive helper for inspecting analyze_sim.R output
# ============================================================

suppressPackageStartupMessages({
  library(fs)
  library(dplyr)
  library(yaml)
  library(likelyr)
})

# ------------------------------------------------------------
# USER INPUT (edit this line only)
# ------------------------------------------------------------
sim_dir <- "experiments/multinom/baseline_logit/sim_08"

sim_config <- read_yaml(file.path(sim_dir, "simulation.yml"))

# ------------------------------------------------------------
# Resolve paths
# ------------------------------------------------------------
sim_dir <- path_abs(sim_dir)

analysis_path <- path(sim_dir, "analysis", "metrics_iteration.rds")

if (!file_exists(analysis_path)) {
  stop(
    "Analysis file not found:\n  ",
    analysis_path,
    "\n\nDid you run analyze_sim.R for this simulation?",
    call. = FALSE
  )
}

# ------------------------------------------------------------
# Load results
# ------------------------------------------------------------
metrics <- readRDS(analysis_path)

message("✔ Loaded metrics for: ", path_file(sim_dir))
message("✔ Rows: ", nrow(metrics))

# ------------------------------------------------------------
# Attach to environment (convenience)
# ------------------------------------------------------------
analysis_iter_df <- metrics

# ------------------------------------------------------------
# Common quick views (optional but useful)
# ------------------------------------------------------------

# 1. Coverage by method and level
coverage_summary <- analysis_iter_df |>
  filter(!is.na(covered)) |>
  group_by(level, pseudolikelihood) |>
  summarise(
    coverage = mean(covered),
    n_valid = sum(valid_ci),
    .groups = "drop"
  )

# 2. Interval width summary
interval_width_summary <- analysis_iter_df |>
  filter(valid_ci) |>
  group_by(level, pseudolikelihood) |>
  summarise(
    mean_width = mean(ci_length, na.rm = TRUE),
    median_width = median(ci_length, na.rm = TRUE),
    .groups = "drop"
  )

# 3. Point estimator performance
point_summary <- analysis_iter_df |>
  group_by(pseudolikelihood) |>
  summarise(
    bias = mean(bias),
    rmse = sqrt(mean(sq_error)),
    sd = sd(psi_hat),
    .groups = "drop"
  )

message("✔ Objects available in environment:")
message("  • analysis_iter_df")
message("  • coverage_summary")
message("  • interval_width_summary")
message("  • point_summary")

# ------------------------------------------------------------
# Optional: auto-print summaries
# ------------------------------------------------------------
print(sim_config$design$overrides)
print(coverage_summary)
print(interval_width_summary)
print(point_summary)

# model <- readRDS(file.path(sim_dir, "iterations", "iter_0001", "model.rds"))

# model$data

model$workspace$integrate$omega_draws |>
  purrr::map(model$estimand$psi_fn)


model$estimand$psi_mle
