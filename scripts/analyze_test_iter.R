#!/usr/bin/env Rscript

# ============================================================
# analyze_test_iter.R
#
# Purpose:
#   • Load a single local test iteration artifact
#   • Intended for interactive / exploratory use
#   • No CLI — user edits paths below
#
# Expects:
#   experiments/<exp>/<sim>/test_runs/test_XXXX/model.rds
# ============================================================

suppressPackageStartupMessages({
  library(likelyr)
  library(here)
  library(fs)
})

# ============================================================
# USER INPUT (edit these)
# ============================================================

# Path to simulation.yml (authoritative)
sim_config_path <- "experiments/multinom/baseline_logit/sim_01/simulation.yml"

# Test iteration index (default = 1)
iter_index <- 1L

# ============================================================
# Validate inputs
# ============================================================

sim_yml <- path_abs(sim_config_path)

if (!file_exists(sim_yml)) {
  stop("simulation.yml not found: ", sim_yml, call. = FALSE)
}

if (!is.numeric(iter_index) || length(iter_index) != 1L || iter_index < 1L) {
  stop("iter_index must be a positive integer.", call. = FALSE)
}

# ============================================================
# Resolve project root
# ============================================================

root <- here()

# ============================================================
# Resolve test iteration directory
# ============================================================

sim_dir <- path_dir(sim_yml)
sim_id <- path_file(sim_dir)

iter_id <- sprintf("test_%04d", iter_index)
iter_dir <- path(sim_dir, "test_runs", iter_id)

if (!dir_exists(iter_dir)) {
  stop("Test iteration directory not found: ", iter_dir, call. = FALSE)
}

# ============================================================
# Load model artifact
# ============================================================

model_path <- path(iter_dir, "model.rds")

if (!file_exists(model_path)) {
  stop(
    "model.rds not found in test iteration directory: ",
    model_path,
    call. = FALSE
  )
}

model <- readRDS(model_path)

model <- model |>
  infer() |>
  compare()

model$workspace$integrate |> plot()
model$workspace$profile |> plot()

model$workspace$integrate$inference |> plot()
model$workspace$profile$inference |> plot()

model$workspace$comparison |> plot()

model$workspace$comparison |> view()
