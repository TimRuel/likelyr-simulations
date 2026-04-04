#!/usr/bin/env Rscript

# ============================================================
# run_iter.R
#
# Contract:
#   • Runs ONE iteration of ONE simulation
#   • Execution mode determined by env var:
#       LIKELYR_EXEC_MODE = "slurm" | "test"
#   • Input:
#       slurm: <path/to/sim_XX/sim_XX.yml>
#       test:  <path/to/sim_XX/test_iteration/test_sim.yml>
#   • Output:
#       slurm: <sim_dir>/iterations/iter_XXXX/model.rds
#       test:  <test_iteration>/model/model.rds  (overwrites build output)
#
# Environment variables:
#   LIKELYR_EXEC_MODE   "slurm" | "test"       (default: "slurm")
#   LIKELYR_VERBOSE     "TRUE"  | "FALSE"       (default: "TRUE")
#   LIKELYR_SIM_DIR     in test mode: path to test_iteration/
#                       provides model/model.rds and is the output dir
#
# Seed hierarchy:
#   experiment.seed_base                              — exp-level anchor
#   simulation.seed_base = exp_seed_base + i*100000  — sim-level space
#   parameter.seed       = sim_seed_base             — sim-level (setup time)
#   sampler.seed_base    = sim_seed_base + 10000     — iter: + iter_index
#   data.seed_base       = sim_seed_base + 20000     — iter: + iter_index
# ============================================================

suppressPackageStartupMessages({
  library(likelyr)
  library(here)
  library(fs)
  library(future)
  library(yaml)
})

# ============================================================
# Parse CLI arguments
# ============================================================
args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 1L) {
  stop(
    "Usage: Rscript R/run_iter.R <path/to/sim_XX/sim_XX.yml>",
    call. = FALSE
  )
}

sim_yml <- path_abs(args[[1]])

if (!file_exists(sim_yml)) {
  stop("sim yaml not found: ", sim_yml, call. = FALSE)
}

# ============================================================
# Resolve project root + utilities
# ============================================================
root <- here()

source(
  file.path(root, "R", "utils.R"),
  local = TRUE
)

# ============================================================
# Execution configuration
# ============================================================
exec_mode <- Sys.getenv("LIKELYR_EXEC_MODE", "slurm")
verbose <- isTRUE(as.logical(Sys.getenv("LIKELYR_VERBOSE", "TRUE")))

if (!exec_mode %in% c("slurm", "test")) {
  stop("Invalid LIKELYR_EXEC_MODE: ", exec_mode, call. = FALSE)
}

# ============================================================
# Resolve simulation directory and iteration paths
#
# test:  LIKELYR_SIM_DIR = test_iteration/
#        model/model.rds is loaded from there and overwritten
#        iter_index fixed at 1 for seed derivation
#
# slurm: sim_dir from yaml path
#        iter_dir = sim_dir/iterations/iter_XXXX/
# ============================================================
if (exec_mode == "test") {
  sim_dir_env <- Sys.getenv("LIKELYR_SIM_DIR", "")
  if (!nzchar(sim_dir_env)) {
    stop("LIKELYR_SIM_DIR must be set in test mode.", call. = FALSE)
  }
  sim_dir <- path_abs(sim_dir_env) # test_iteration/
  iter_id <- "test_iteration"
  iter_index <- 1L
} else {
  sim_dir <- path_dir(sim_yml)
  iter_index <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID", NA)) + 1L
  if (is.na(iter_index) || iter_index < 1L) {
    stop("SLURM_ARRAY_TASK_ID not set or invalid.", call. = FALSE)
  }
  iter_id <- sprintf("iter_%04d", iter_index)
}

sim_id <- path_file(sim_dir)

message("🚀 Starting iteration")
message("  Mode:        ", exec_mode)
message("  Simulation:  ", sim_id)
message("  Iteration:   ", iter_id)
message("  Verbose:     ", verbose)

# ============================================================
# Load simulation config
# ============================================================
config <- read_yaml(sim_yml)

# ============================================================
# Load model skeleton
# In test mode: test_iteration/model/model.rds (built from test_sim.yml)
# In slurm mode: sim_XX/model/model.rds
# ============================================================
model_path <- path(sim_dir, "model", "model.rds")

if (!file_exists(model_path)) {
  stop("Simulation model not found: ", model_path, call. = FALSE)
}

model <- readRDS(model_path)

# ============================================================
# Seeding
# ============================================================
data_seed <- as.integer(config$data$seed_base) + iter_index
sampler_seed <- as.integer(config$sampler$seed_base) + iter_index

# ============================================================
# Generate iteration data (data seed)
# ============================================================
set.seed(data_seed)

spec_dir <- path(root, config$experiment$spec_path)
data_spec_env <- load_data_env(spec_dir)

data <- data_spec_env$generate_data(
  config = config,
  parameter = model$parameter
)

# ============================================================
# Parallel execution setup
# ============================================================
available_cpus <- max(1L, as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "1")))
exec <- model$execution
use_parallel <- isTRUE(exec$mode == "parallel")

if (use_parallel) {
  requested_workers <- as.integer(exec$num_workers)

  if (is.na(requested_workers) || requested_workers < 1L) {
    stop("execution$num_workers must be a positive integer.", call. = FALSE)
  }

  if (requested_workers > available_cpus) {
    stop(
      sprintf(
        "execution$num_workers=%d exceeds available cores=%d",
        requested_workers,
        available_cpus
      ),
      call. = FALSE
    )
  }

  plan(multisession, workers = requested_workers)
  on.exit(plan(sequential), add = TRUE)
}

# ============================================================
# Calibrate, preprocess, integrate (sampler seed)
# ============================================================
set.seed(sampler_seed)

model <- tryCatch(
  {
    model |>
      calibrate(data) |>
      preprocess(verbose = verbose) |>
      integrate(verbose = verbose)
  },
  error = function(e) e
)

# ============================================================
# Save model or error record
#
# test:  overwrite test_iteration/model/model.rds
# slurm: write to iterations/iter_XXXX/model.rds
# ============================================================
if (inherits(model, "error")) {
  error_record <- list(
    sim_id = sim_id,
    iter_id = iter_id,
    iter_index = iter_index,
    data_seed = data_seed,
    sampler_seed = sampler_seed,
    message = conditionMessage(model),
    call = deparse(conditionCall(model))
  )

  error_path <- if (exec_mode == "test") {
    path(sim_dir, "model", "error.rds")
  } else {
    iter_dir <- path(sim_dir, "iterations", iter_id)
    dir_create(iter_dir, recurse = TRUE)
    path(iter_dir, "error.rds")
  }

  saveRDS(error_record, error_path)
  message("❌ Iteration failed: ", conditionMessage(model))
  message("   Error record saved: ", error_path)
  quit(status = 1L, save = "no")
}

model_out_path <- if (exec_mode == "test") {
  path(sim_dir, "model", "model.rds") # overwrite build output
} else {
  iter_dir <- path(sim_dir, "iterations", iter_id)
  dir_create(iter_dir, recurse = TRUE)
  path(iter_dir, "model.rds")
}

saveRDS(model, model_out_path)
message("💾 Model saved: ", model_out_path)

message("✅ Iteration complete: ", sim_id, " / ", iter_id)
