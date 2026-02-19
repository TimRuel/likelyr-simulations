# ============================================================
# regress_frequency_properties.R
#
# Regression analysis of frequency properties
# (factorial-design aware)
# ============================================================

suppressPackageStartupMessages({
  library(fs)
  library(dplyr)
  library(tidyr)
  library(broom)
})

# ------------------------------------------------------------
# USER INPUT
# ------------------------------------------------------------
exp_dir <- "experiments/multinom/baseline_logit"

exp_dir <- path_abs(exp_dir)
analysis_dir <- path(exp_dir, "analysis")

# ------------------------------------------------------------
# Load experiment-level summaries
# ------------------------------------------------------------
point_path <- path(analysis_dir, "point_exp_df.rds")
interval_path <- path(analysis_dir, "interval_exp_df.rds")

if (!file_exists(point_path) || !file_exists(interval_path)) {
  stop(
    "Missing analysis artifacts in:\n  ",
    analysis_dir,
    "\n\nDid you run summarize_experiment_analysis.R?",
    call. = FALSE
  )
}

point_df <- readRDS(point_path)
interval_df <- readRDS(interval_path)

# ------------------------------------------------------------
# Preprocessing / factor encoding
# ------------------------------------------------------------

point_df <- point_df |>
  mutate(
    pseudolikelihood = factor(
      pseudolikelihood,
      levels = c("Profile", "Integrated")
    ),
    J = factor(J),
    entropy_target_frac = factor(entropy_target_frac),
    n_obs = factor(n_obs)
  )

interval_df <- interval_df |>
  mutate(
    pseudolikelihood = factor(
      pseudolikelihood,
      levels = c("Profile", "Integrated")
    ),
    level = factor(level, levels = c("90%", "95%", "99%")),
    J = factor(J),
    entropy_target_frac = factor(entropy_target_frac),
    n_obs = factor(n_obs)
  )

# ------------------------------------------------------------
# 1. Point estimator regressions
# ------------------------------------------------------------

# Bias
lm_bias <- lm(
  bias ~ pseudolikelihood *
    entropy_target_frac +
    pseudolikelihood * J +
    pseudolikelihood * n_obs,
  data = point_df
)

# RMSE
lm_rmse <- lm(
  rmse ~ pseudolikelihood *
    entropy_target_frac +
    pseudolikelihood * J +
    pseudolikelihood * n_obs,
  data = point_df
)

# Monte Carlo SD
lm_mc_sd <- lm(
  mc_sd ~ pseudolikelihood *
    entropy_target_frac +
    pseudolikelihood * J +
    pseudolikelihood * n_obs,
  data = point_df
)

# ------------------------------------------------------------
# 2. Interval performance regressions
# ------------------------------------------------------------

# Coverage
lm_coverage <- lm(
  coverage ~ pseudolikelihood *
    level +
    pseudolikelihood * entropy_target_frac +
    pseudolikelihood * J +
    pseudolikelihood * n_obs,
  data = interval_df
)

# Valid CI rate
lm_valid_rate <- lm(
  valid_rate ~ pseudolikelihood *
    level +
    pseudolikelihood * entropy_target_frac +
    pseudolikelihood * J +
    pseudolikelihood * n_obs,
  data = interval_df
)

# Mean CI length
lm_ci_length <- lm(
  log(mean_ci_length) ~ pseudolikelihood *
    level +
    pseudolikelihood * entropy_target_frac +
    pseudolikelihood * J +
    pseudolikelihood * n_obs,
  data = interval_df
)

# ------------------------------------------------------------
# Collect tidy summaries
# ------------------------------------------------------------
regression_summaries <- list(
  bias = tidy(lm_bias),
  rmse = tidy(lm_rmse),
  mc_sd = tidy(lm_mc_sd),
  coverage = tidy(lm_coverage),
  valid_rate = tidy(lm_valid_rate),
  ci_length = tidy(lm_ci_length)
)

# ------------------------------------------------------------
# Export to environment for inspection
# ------------------------------------------------------------
assign("lm_bias", lm_bias, envir = .GlobalEnv)
assign("lm_rmse", lm_rmse, envir = .GlobalEnv)
assign("lm_mc_sd", lm_mc_sd, envir = .GlobalEnv)
assign("lm_coverage", lm_coverage, envir = .GlobalEnv)
assign("lm_valid_rate", lm_valid_rate, envir = .GlobalEnv)
assign("lm_ci_length", lm_ci_length, envir = .GlobalEnv)

assign("regression_summaries", regression_summaries, envir = .GlobalEnv)

message("✔ Regression analysis complete")
message("✔ Models available:")
message("  • lm_bias")
message("  • lm_rmse")
message("  • lm_mc_sd")
message("  • lm_coverage")
message("  • lm_valid_rate")
message("  • lm_ci_length")
message("✔ Tidy coefficients in: regression_summaries")

regression_summaries
