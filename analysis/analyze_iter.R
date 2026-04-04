#!/usr/bin/env Rscript

# ============================================================
# analyze_iter.R  (interactive version)
#
# Purpose:
#   • Load a single iteration artifact for interactive exploration
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
  library(ggplot2)
})

# ============================================================
# USER INPUT (edit these)
# ============================================================

sim_dir <- "experiments/multinom/logit_simpson/exp_v1/sim_01"
iter_index <- 1L # only used in slurm mode
mode <- "test" # "test" or "slurm"

# ============================================================
# Resolve model path
# ============================================================
sim_dir <- path_abs(sim_dir)

if (!dir_exists(sim_dir)) {
  stop("Simulation directory not found: ", sim_dir, call. = FALSE)
}

sim_id <- path_file(sim_dir)

model_path <- if (mode == "test") {
  path(sim_dir, "test_iteration", "model", "model.rds")
} else {
  iter_id <- sprintf("iter_%04d", iter_index)
  path(sim_dir, "iterations", iter_id, "model.rds")
}

if (!file_exists(model_path)) {
  stop("model.rds not found: ", model_path, call. = FALSE)
}

# ============================================================
# Load model
# ============================================================
model <- readRDS(model_path)

# ============================================================
# Diagnostics + inference + comparison
# ============================================================
model <- model |>
  diagnose() |>
  infer() |>
  compare()

# ============================================================
# Plots
# ============================================================
model$workspace$integrated |> plot()
model$workspace$profile |> plot()

model$workspace$comparison |> plot()
model$workspace$comparison |> view()

model$workspace$integrated$diagnostics |> plot()
