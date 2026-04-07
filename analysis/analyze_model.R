#!/usr/bin/env Rscript

# ============================================================
# analyze_model.R  (interactive version)
#
# Purpose:
#   • Load a single model.rds and run infer() + compare()
#   • No CLI — edit the USER INPUT section below
#
# Paths:
#   test:  experiments/<path>/<version>/sim_XX/test_iteration/model/model.rds
#   slurm: experiments/<path>/<version>/sim_XX/iterations/iter_XXXX/model.rds
# ============================================================

suppressPackageStartupMessages({
  library(likelyr)
  library(here)
  library(fs)
  library(dplyr)
})

# ============================================================
# USER INPUT (edit this)
# ============================================================

model_path <- "experiments/multinom/logit_simpson/exp_v1/sim_05/iterations/iter_0001/model.rds"

# ============================================================
# Load model
# ============================================================
model_path <- path_abs(model_path)

if (!file_exists(model_path)) {
  stop("model.rds not found: ", model_path, call. = FALSE)
}

model <- readRDS(model_path)

iter_id <- path_file(path_dir(model_path))
sim_id <- path_file(path_dir(path_dir(path_dir(model_path))))

message("✔ Loaded: ", sim_id, " / ", iter_id)

# ============================================================
# infer + compare
# ============================================================
model <- model |>
  infer() |>
  compare()

point_df <- model$workspace$comparison$point_estimates_df
interval_df <- model$workspace$comparison$interval_estimates_df

psi_0 <- unique(point_df$psi_0)[1]

# ============================================================
# Point metrics
# ============================================================
point_metrics <- lapply(
  split(point_df, point_df$pseudolikelihood),
  function(d) {
    data.frame(
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

# ============================================================
# Interval metrics
# ============================================================
interval_raw <- attr(interval_df, "interval_estimates_raw")

alpha_to_level <- interval_raw |>
  distinct(alpha) |>
  mutate(level = paste0(round((1 - alpha) * 100), "%"))

interval_raw <- interval_raw |>
  left_join(alpha_to_level, by = "alpha")

interval_df2 <- interval_raw |>
  select(pseudolikelihood, level, alpha, lower, upper) |>
  mutate(pseudolikelihood = tools::toTitleCase(pseudolikelihood)) |>
  right_join(
    interval_df,
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

# ============================================================
# Print
# ============================================================
message("✔ Objects available in environment:")
message("  • model")
message("  • point_metrics")
message("  • interval_metrics")

print(point_metrics)
print(interval_metrics)

# ============================================================
# Plots
# ============================================================
model$workspace$integrated |> plot()
model$workspace$profile |> plot()
model$workspace$comparison |> plot()
model$workspace$comparison |> view()
