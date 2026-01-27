# scripts/run_simulation.R

#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(likelyr)
  library(here)
  library(fs)
})

# -------------------------------
# ✅ Anchor project root
# -------------------------------
suppressMessages(i_am("scripts/run_simulation.R"))


root <- here::here()
exp_id <- Sys.getenv("EXPERIMENT_ID")
sim_id <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))

exp_dir <- file.path(root, "experiments", exp_id)
sim_dir <- file.path(exp_dir, "simulations", sprintf("sim_%04d", sim_id))

dir.create(sim_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# Load invariant objects
# ------------------------------------------------------------
config <- yaml::read_yaml(file.path(exp_dir, "experiment.yml"))
true_params <- readRDS(file.path(exp_dir, "true_params", "true_params.rds"))
model_ready <- readRDS(file.path(exp_dir, "model", "model_ready.rds"))

# ------------------------------------------------------------
# Generate data (stochastic)
# ------------------------------------------------------------
set.seed(100000 + sim_id)

data <- generate_data(true_params, config)
saveRDS(data, file.path(sim_dir, "data.rds"))

# ------------------------------------------------------------
# Calibrate to data
# ------------------------------------------------------------
model_data <- calibrate(model_ready, data)

# ------------------------------------------------------------
# Run inference
# ------------------------------------------------------------
res_int <- integrate(model_data, config$optimization)
res_pro <- profile(model_data, config$optimization)

saveRDS(res_int, file.path(sim_dir, "integrate.rds"))
saveRDS(res_pro, file.path(sim_dir, "profile.rds"))
