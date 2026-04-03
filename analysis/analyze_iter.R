#!/usr/bin/env Rscript

# ============================================================
# analyze_iter.R  (interactive version)
#
# Purpose:
#   • Load a single iteration artifact for interactive exploration
#   • No CLI — edit the USER INPUT section below
#
# Expects:
#   experiments/<path>/<version>/test_sim_XX/iterations/iter_XXXX/model.rds  (test)
#   experiments/<path>/<version>/sim_XX/iterations/iter_XXXX/model.rds       (slurm)
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
iter_index <- 1L
mode <- "slurm" # "test" or "slurm"

# ============================================================
# Resolve iteration directory
# ============================================================
sim_dir <- path_abs(sim_dir)

if (!dir_exists(sim_dir)) {
  stop("Simulation directory not found: ", sim_dir, call. = FALSE)
}

sim_id <- path_file(sim_dir)

iter_id <- if (mode == "test") {
  sprintf("test_%04d", iter_index)
} else {
  sprintf("iter_%04d", iter_index)
}

iter_root <- if (mode == "test") {
  path(sim_dir, "test_runs")
} else {
  path(sim_dir, "iterations")
}

iter_dir <- path(iter_root, iter_id)

if (!dir_exists(iter_dir)) {
  stop("Iteration directory not found: ", iter_dir, call. = FALSE)
}

# ============================================================
# Load model
# ============================================================
model_path <- path(iter_dir, "model.rds")

if (!file_exists(model_path)) {
  stop("model.rds not found: ", model_path, call. = FALSE)
}

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

# ============================================================
# Scratch / exploratory
# ============================================================
model$data

branch_mat <- model$workspace$integrated$branch_mat
n_branches <- ncol(branch_mat)

psi_vals <- model$workspace$integrated$diagnostics$plot_data$omega_branches$psi
omega_draws <- model$workspace$integrated$cache$branch_seeds |>
  purrr::map(\(s) s$omega_hat)

psi_jac <- model$estimand$psi_jac
param_mle <- model$parameter$param_mle

dist_eta <- function(omega_hat, param_mle) {
  sqrt(sum((omega_hat - param_mle)^2))
}

min_prob <- function(omega_hat) {
  min(softmax_from_eta(omega_hat))
}

psi_grad_norm <- function(omega_hat, psi_jac) {
  g <- psi_jac(omega_hat)
  sqrt(sum(g^2))
}

# Branch-by-branch inspection
for (i in seq_len(min(50L, n_branches))) {
  plot(x = psi_vals, y = branch_mat[, i])
  title(psi_grad_norm(omega_draws[[i]], psi_jac))
}

# Order branches by minimum simplex probability
ord <- order(sapply(omega_draws, min_prob))

for (i in ord[seq_len(min(50L, n_branches))]) {
  plot(psi_vals, branch_mat[, i])
  title(sprintf(
    "min(p)=%.2e | dist=%.2f",
    min_prob(omega_draws[[i]]),
    dist_eta(omega_draws[[i]], param_mle)
  ))
}
