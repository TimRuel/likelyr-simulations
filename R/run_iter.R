#!/usr/bin/env Rscript

# ============================================================
# run_iter.R
#
# Contract:
#   • Runs ONE iteration of ONE simulation
#   • Execution mode determined by env var:
#       LIKELYR_EXEC_MODE = "slurm" | "test"
#   • Input:
#       slurm: <path/to/config/.../sim_XX.yml>
#       test:  <path/to/.../test_sim/test_sim.yml>
#   • Derives sim_dir from experiment$exp_dir + simulation$sim_id
#     in both modes — no path derivation from yaml location
#   • Output:
#       slurm: <sim_dir>/iterations/iter_XXXX/model.rds
#       test:  <sim_dir>/iterations/iter_XXXX/model.rds  (same structure)
#
# Environment variables:
#   LIKELYR_EXEC_MODE   "slurm" | "test"   (default: "slurm")
#   LIKELYR_VERBOSE     "TRUE"  | "FALSE"  (default: "TRUE")
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
    "Usage: Rscript R/run_iter.R <path/to/sim_XX.yml>",
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
# Load simulation config
# ============================================================
config <- read_yaml(sim_yml)

# ============================================================
# Resolve simulation directory from config
# ============================================================
exp_dir <- config$experiment$exp_dir
sim_id <- config$simulation$sim_id

if (is.null(exp_dir) || !nzchar(exp_dir)) {
  stop("experiment$exp_dir must be defined in the sim yaml.", call. = FALSE)
}
if (is.null(sim_id) || !nzchar(sim_id)) {
  stop("simulation$sim_id must be defined in the sim yaml.", call. = FALSE)
}

sim_dir <- path(exp_dir, sim_id)

# ============================================================
# Resolve iteration index and iter_id
# In test mode SLURM_ARRAY_TASK_ID is set by test_sim.sh (default 0).
# In slurm mode it is set by the Slurm scheduler.
# ============================================================
if (exec_mode == "test") {
  iter_index <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID", "0")) + 1L
  iter_id <- sprintf("iter_%04d", iter_index)
} else {
  iter_index <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID", NA))
  if (is.na(iter_index) || iter_index < 1L) {
    stop("SLURM_ARRAY_TASK_ID not set or invalid.", call. = FALSE)
  }
  iter_id <- sprintf("iter_%04d", iter_index)
}

message("🚀 Starting iteration")
message("  Mode:        ", exec_mode)
message("  Simulation:  ", sim_id)
message("  Iteration:   ", iter_id)
message("  Verbose:     ", verbose)

# ============================================================
# Load model skeleton from sim_dir/model/model.rds
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
# Generate iteration data
# ============================================================
set.seed(data_seed)

specs_dir <- config$experiment$specs_dir

if (is.null(specs_dir) || !nzchar(specs_dir)) {
  stop("experiment$specs_dir must be defined in the sim yaml.", call. = FALSE)
}

data_spec_env <- load_data_env(specs_dir)

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
# Calibrate, preprocess, integrate
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
# Resolve output path — same structure in both modes:
#   <sim_dir>/iterations/iter_XXXX/model.rds
# ============================================================
out_dir <- path(sim_dir, "iterations", iter_id)
out_path <- path(out_dir, "model.rds")

dir_create(out_dir, recurse = TRUE)

# ============================================================
# Save model or error record
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

  error_path <- path(out_dir, "error.rds")
  saveRDS(error_record, error_path)
  message("❌ Iteration failed: ", conditionMessage(model))
  message("   Error record saved: ", error_path)
  quit(status = 1L, save = "no")
}

saveRDS(model, out_path)
message("💾 Model saved: ", out_path)
message("✅ Iteration complete: ", sim_id, " / ", iter_id)
