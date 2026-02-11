# scripts/analyze_simulations.R
#!/usr/bin/env Rscript

# ============================================================
# Analyze simulation results for a single experiment
#
# Usage:
#   Rscript analyze_simulations.R <path/to/experiment.yml>
#
# Outputs:
#   • analysis_metrics_simulation.rds  (per-simulation, long)
#   • analysis_metrics_summary.rds     (aggregated across simulations)
#
# Notes:
#   • Interval endpoints live in attr(interval_df, "interval_estimates_raw")
#   • Interval validity is defined as: lower and upper are both finite (non-NA)
#   • Interval summaries:
#       - valid_rate (% success) computed over ALL sims
#       - other interval metrics computed ONLY among valid intervals
#       - includes mean and median CI width among valid intervals
# ============================================================

suppressPackageStartupMessages({
  library(likelyr)
  library(here)
  library(fs)
  library(yaml)
  library(dplyr)
})

# ============================================================
# Parse CLI arguments
# ============================================================
args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 1L) {
  stop("Usage: Rscript analyze_simulations.R <config.yml>", call. = FALSE)
}

config_path <- args[[1]]
if (!file.exists(config_path)) {
  stop("Config file not found: ", config_path, call. = FALSE)
}

# ============================================================
# Anchor project root + utilities
# ============================================================
root <- here()

source(
  file.path(root, "scripts", "utils.R"),
  local = TRUE
)

# ============================================================
# Load config
# ============================================================
config <- read_yaml(config_path)

exp_id <- config$experiment$id
if (is.null(exp_id) || !nzchar(exp_id)) {
  stop("experiment$id missing from config.", call. = FALSE)
}

# ============================================================
# Locate simulation directories
# ============================================================
sim_root <- file.path(root, "experiments", exp_id, "simulations")

if (!dir_exists(sim_root)) {
  stop("Simulation directory not found: ", sim_root, call. = FALSE)
}

sim_dirs <- dir_ls(sim_root, type = "directory")
if (length(sim_dirs) == 0L) {
  stop("No sim_* directories found.", call. = FALSE)
}

# ============================================================
# Main loop: extract per-simulation metrics
# ============================================================
results <- vector("list", length(sim_dirs))

for (i in seq_along(sim_dirs)) {
  sim_dir <- sim_dirs[[i]]
  sim_id <- path_file(sim_dir)
  model_path <- file.path(sim_dir, "model.rds")

  if (!file.exists(model_path)) {
    warning("Skipping ", sim_id, ": model.rds not found.")
    next
  }

  model <- readRDS(model_path)

  # Run infer + compare
  model <- model |>
    infer() |>
    compare()

  point_df <- model$workspace$comparison$point_estimates_df
  interval_df <- model$workspace$comparison$interval_estimates_df

  if (is.null(point_df) || is.null(interval_df)) {
    warning("Missing estimates for ", sim_id)
    next
  }

  # True value (shared across pseudolikelihoods)
  psi_0 <- unique(point_df$psi_0)
  if (length(psi_0) != 1L) {
    psi_0 <- psi_0[1]
  }

  # ----------------------------------------------------------
  # Point-estimate metrics (per simulation)
  # ----------------------------------------------------------
  point_metrics <- lapply(
    split(point_df, point_df$pseudolikelihood),
    function(d) {
      psi_hat <- d$psi_hat

      data.frame(
        sim_id = sim_id,
        pseudolikelihood = d$pseudolikelihood,

        psi_hat = psi_hat,
        psi_0 = psi_0,

        bias = psi_hat - psi_0,
        sq_error = (psi_hat - psi_0)^2,

        stringsAsFactors = FALSE
      )
    }
  ) |>
    bind_rows()

  # ----------------------------------------------------------
  # Interval-estimate metrics (per simulation, per level)
  #   - attach raw endpoints
  #   - compute validity flag based on endpoints
  #   - keep level/alpha so we can summarize across sims by level
  # ----------------------------------------------------------
  interval_raw <- attr(interval_df, "interval_estimates_raw") |>
    dplyr::mutate(
      pseudolikelihood = dplyr::recode_values(
        pseudolikelihood,
        "integrate" ~ "Integrated",
        "profile" ~ "Profile"
      )
    )

  if (is.null(interval_raw)) {
    stop("interval_estimates_raw attribute missing.", call. = FALSE)
  }

  # Map alpha → level (must match compare()'s formatting)
  alpha_to_level <- interval_raw |>
    distinct(alpha) |>
    mutate(level = paste0(round((1 - alpha) * 100), "%"))

  interval_raw <- interval_raw |>
    left_join(alpha_to_level, by = "alpha")

  # Attach endpoints to the main interval df
  interval_df2 <- interval_df |>
    left_join(
      interval_raw |>
        select(pseudolikelihood, level, alpha, lower, upper),
      by = c("pseudolikelihood", "level")
    ) |>
    mutate(
      # Valid CI iff both endpoints exist; this captures “curve didn’t extend far enough”
      valid_ci = is.finite(lower) & is.finite(upper)
    )

  interval_metrics <- lapply(
    split(
      interval_df2,
      list(interval_df2$pseudolikelihood, interval_df2$level),
      drop = TRUE
    ),
    function(d) {
      L <- d$lower
      U <- d$upper
      valid <- d$valid_ci

      data.frame(
        sim_id = sim_id,
        pseudolikelihood = d$pseudolikelihood,
        level = d$level,
        alpha = d$alpha,

        valid_ci = valid,

        # Width from authoritative endpoints (still equals d$length when valid)
        ci_length = ifelse(valid, U - L, NA_real_),

        # Coverage only meaningful when interval exists
        covered = ifelse(valid, (psi_0 >= L & psi_0 <= U), NA),

        # Miss decomposition only meaningful when interval exists
        lower_miss = ifelse(valid, psi_0 < L, NA),
        upper_miss = ifelse(valid, psi_0 > U, NA),

        # Endpoint bias only meaningful when interval exists
        lower_bias = ifelse(valid, L - psi_0, NA_real_),
        upper_bias = ifelse(valid, U - psi_0, NA_real_),

        stringsAsFactors = FALSE
      )
    }
  ) |>
    bind_rows()

  # ----------------------------------------------------------
  # Likelihood-shape diagnostics (per simulation)
  # ----------------------------------------------------------
  shape_metrics <- point_df |>
    dplyr::pull(pseudolikelihood) |>
    dplyr::recode_values("Integrated" ~ "integrate", "Profile" ~ "profile") |>
    unique() |>
    lapply(function(p) {
      shape <- extract_likelihood_shape(
        model = model,
        pseudolikelihood = p
      )

      data.frame(
        sim_id = sim_id,
        pseudolikelihood = p,

        curvature = shape$curvature,
        width_05 = shape$width_05,
        skewness = shape$skewness,

        stringsAsFactors = FALSE
      )
    }) |>
    bind_rows() |>
    dplyr::mutate(
      pseudolikelihood = dplyr::recode_values(
        pseudolikelihood,
        "integrate" ~ "Integrated",
        "profile" ~ "Profile"
      )
    )

  # ----------------------------------------------------------
  # Combine per-simulation metrics
  #   Note: interval_metrics has multiple rows per pseudolikelihood
  #         (one per level). This is expected and desired.
  # ----------------------------------------------------------
  results[[i]] <- point_metrics |>
    full_join(interval_metrics, by = c("sim_id", "pseudolikelihood")) |>
    full_join(shape_metrics, by = c("sim_id", "pseudolikelihood"))
}

# ============================================================
# Bind per-simulation results
# ============================================================
analysis_sim_df <- bind_rows(results)

if (nrow(analysis_sim_df) == 0L) {
  stop("No valid simulation results were processed.", call. = FALSE)
}

# Derived indicator (point-estimate outliers; meaningful only on point rows)
analysis_sim_df <- analysis_sim_df |>
  group_by(pseudolikelihood) |>
  mutate(
    outlier = ifelse(
      !is.na(psi_hat),
      abs(bias) > 3 * sd(psi_hat, na.rm = TRUE),
      NA
    )
  ) |>
  ungroup()

# ============================================================
# Cross-simulation summaries
# ============================================================

# Point-estimate summary (across sims)
point_summary_df <- analysis_sim_df |>
  filter(!is.na(psi_hat)) |>
  group_by(pseudolikelihood) |>
  summarise(
    n_sim = n() / length(unique(level)),

    bias = mean(bias, na.rm = TRUE),
    sd = sd(psi_hat, na.rm = TRUE),
    rmse = sqrt(mean(sq_error, na.rm = TRUE)),

    outlier_rate = mean(outlier, na.rm = TRUE),

    mean_curvature = mean(curvature, na.rm = TRUE),
    mean_width_05 = mean(width_05, na.rm = TRUE),
    mean_skewness = mean(skewness, na.rm = TRUE),

    .groups = "drop"
  )

# Interval-estimate summary (across sims, by pseudolikelihood × level)
#   - valid_rate computed over ALL sims
#   - other metrics computed among valid only
interval_summary_df <- analysis_sim_df |>
  filter(!is.na(level)) |>
  group_by(pseudolikelihood, level, alpha) |>
  summarise(
    n_sim = n() / length(unique(level)),

    # % success: curve extended far enough to yield both endpoints
    valid_rate = mean(valid_ci, na.rm = TRUE),
    valid_num = valid_rate * n_sim,

    # Summaries among successful intervals only
    mean_ci_length = mean(ci_length[valid_ci], na.rm = TRUE),
    median_ci_length = stats::median(ci_length[valid_ci], na.rm = TRUE),
    sd_ci_length = stats::sd(ci_length[valid_ci], na.rm = TRUE),

    coverage = mean(covered[valid_ci], na.rm = TRUE),
    under_coverage = mean(lower_miss[valid_ci], na.rm = TRUE),
    over_coverage = mean(upper_miss[valid_ci], na.rm = TRUE),

    avg_lower_bias = mean(lower_bias[valid_ci], na.rm = TRUE),
    avg_upper_bias = mean(upper_bias[valid_ci], na.rm = TRUE),

    .groups = "drop"
  )

# ============================================================
# Save artifacts
# ============================================================
analysis_dir <- file.path(
  root,
  "experiments",
  exp_id,
  "analysis"
)

# Create analysis directory if needed
if (!dir.exists(analysis_dir)) {
  dir.create(analysis_dir, recursive = TRUE, showWarnings = FALSE)
}

saveRDS(
  analysis_sim_df,
  file.path(
    analysis_dir,
    "metrics_simulation.rds"
  )
)

saveRDS(
  list(
    point = point_summary_df,
    interval = interval_summary_df
  ),
  file.path(
    analysis_dir,
    "metrics_summary.rds"
  )
)

message("✔ Analysis complete")
message("✔ Saved:")
message("  • metrics_simulation.rds")
message("  • metrics_summary.rds")
