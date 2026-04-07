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
sim_dir <- "experiments/multinom/logit_simpson/exp_v1/sim_05"

# ------------------------------------------------------------
# Load sim config
# ------------------------------------------------------------
sim_id <- path_file(sim_dir)
sim_config <- read_yaml(path(sim_dir, paste0(sim_id, ".yml")))

# ------------------------------------------------------------
# Resolve paths
# ------------------------------------------------------------
analysis_dir <- path(sim_dir, "analysis")
point_path <- path(analysis_dir, "sim_point_metrics.rds")
interval_path <- path(analysis_dir, "sim_interval_metrics.rds")

if (!file_exists(point_path) || !file_exists(interval_path)) {
  stop(
    "Analysis files not found in:\n  ",
    analysis_dir,
    "\n\nDid you run: make analyze SIM_CONFIG=",
    path(sim_dir, paste0(sim_id, ".yml")),
    "?",
    call. = FALSE
  )
}

# ------------------------------------------------------------
# Load results
# ------------------------------------------------------------
point_df <- readRDS(point_path)
interval_df <- readRDS(interval_path)

message("✔ Loaded metrics for: ", sim_id)
message("  • point rows:    ", nrow(point_df))
message("  • interval rows: ", nrow(interval_df))

# ------------------------------------------------------------
# Common quick views
# ------------------------------------------------------------

# 1. Coverage by method and level
coverage_summary <- interval_df |>
  filter(!is.na(covered)) |>
  group_by(level, pseudolikelihood) |>
  summarise(
    coverage = mean(covered),
    n_valid = sum(valid_ci),
    .groups = "drop"
  )

# 2. Interval width summary
width_summary <- interval_df |>
  filter(valid_ci) |>
  group_by(level, pseudolikelihood) |>
  summarise(
    mean_width = mean(ci_length, na.rm = TRUE),
    median_width = median(ci_length, na.rm = TRUE),
    .groups = "drop"
  )

# 3. Point estimator performance
point_summary <- point_df |>
  group_by(pseudolikelihood) |>
  summarise(
    bias = mean(bias),
    rmse = sqrt(mean(sq_error)),
    sd = sd(psi_hat),
    .groups = "drop"
  )

message("✔ Objects available in environment:")
message("  • point_df")
message("  • interval_df")
message("  • coverage_summary")
message("  • width_summary")
message("  • point_summary")

# ------------------------------------------------------------
# Auto-print summaries
# ------------------------------------------------------------
print(sim_config$design$overrides)
print(point_summary)
print(coverage_summary)
print(width_summary)
