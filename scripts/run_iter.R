#!/usr/bin/env Rscript

# ============================================================
# run_iter.R
#
# Contract:
#   • Runs ONE iteration of ONE simulation
#   • Execution mode determined by env var:
#       LIKELYR_EXEC_MODE = "slurm" | "test"
#   • Input:
#       - <path/to/simulation.yml>
#       - SLURM_ARRAY_TASK_ID   (slurm mode)
#       - LIKELYR_TEST_ITER     (test mode, optional)
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
    "Usage: Rscript run_iter.R <path/to/simulation.yml>",
    call. = FALSE
  )
}

sim_yml <- path_abs(args[[1]])

if (!file_exists(sim_yml)) {
  stop("simulation.yml not found: ", sim_yml, call. = FALSE)
}

# ============================================================
# Resolve project root + utilities
# ============================================================
root <- here()

source(
  file.path(root, "scripts", "utils.R"),
  local = TRUE
)

# ============================================================
# Resolve simulation context
# ============================================================
sim_dir <- path_dir(sim_yml)
sim_id <- path_file(sim_dir)

# ============================================================
# Determine execution mode
# ============================================================
exec_mode <- Sys.getenv("LIKELYR_EXEC_MODE", "slurm")

if (!exec_mode %in% c("slurm", "test")) {
  stop("Invalid LIKELYR_EXEC_MODE: ", exec_mode, call. = FALSE)
}

# ============================================================
# Iteration bookkeeping (MODE-AWARE)
# ============================================================
if (exec_mode == "slurm") {
  iter_index <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID", NA)) + 1L
  if (is.na(iter_index) || iter_index < 1L) {
    stop("SLURM_ARRAY_TASK_ID not set or invalid.", call. = FALSE)
  }

  iter_root <- path(sim_dir, "iterations")
  iter_id <- sprintf("iter_%04d", iter_index)
} else {
  iter_index <- as.integer(Sys.getenv("LIKELYR_TEST_ITER", "1"))
  if (is.na(iter_index) || iter_index < 1L) {
    stop("LIKELYR_TEST_ITER must be a positive integer.", call. = FALSE)
  }

  iter_root <- path(sim_dir, "test_runs")
  iter_id <- sprintf("test_%04d", iter_index)
}

iter_dir <- path(iter_root, iter_id)
dir_create(iter_dir, recurse = TRUE)

message("🚀 Starting iteration")
message("  Mode:        ", exec_mode)
message("  Simulation:  ", sim_id)
message("  Iteration:   ", iter_id)

# ============================================================
# Load simulation config snapshot
# ============================================================
config <- read_yaml(sim_yml)

# ============================================================
# Load shared model skeleton
# ============================================================
model_path <- path(sim_dir, "model", "model.rds")

if (!file_exists(model_path)) {
  stop("Shared simulation model not found: ", model_path, call. = FALSE)
}

model <- readRDS(model_path)

# ============================================================
# Seeding (iteration-specific)
# ============================================================
if (!is.null(config$execution$seed)) {
  set.seed(config$execution$seed + iter_index)
} else {
  set.seed(100000 + iter_index)
}

# ============================================================
# Load data generation spec
# ============================================================
spec_path <- config$experiment$spec_path

if (is.null(spec_path)) {
  stop("experiment$spec_path must be defined.", call. = FALSE)
}

spec_dir <- path(root, spec_path)

if (!dir_exists(spec_dir)) {
  stop("Spec directory not found: ", spec_dir, call. = FALSE)
}

spec_env <- load_spec_env(spec_dir)

data_spec_file <- path(spec_dir, "data.R")
if (!file_exists(data_spec_file)) {
  stop("data.R not found in spec directory: ", data_spec_file, call. = FALSE)
}

source(data_spec_file, local = spec_env)

if (!exists("generate_data", envir = spec_env, inherits = FALSE)) {
  stop("data.R must define generate_data(config, parameter).", call. = FALSE)
}

data <- spec_env$generate_data(
  config = config,
  parameter = model$parameter
)

# ============================================================
# Calibrate model
# ============================================================
model <- calibrate(model, data)

# ============================================================
# Parallel execution (if requested)
# ============================================================
available_cpus <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "1"))
available_cpus <- max(1L, available_cpus)

exec <- model$execution
use_parallel <- exec$mode == "parallel"

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
}

# ============================================================
# Inference + runtimes
# ============================================================
t_integrate <- system.time({
  model <- integrate(model)
})["elapsed"]

plan(sequential)

t_profile <- system.time({
  model <- profile(model)
})["elapsed"]

runtime <- data.frame(
  pseudolikelihood = c("Integrated", "Profile"),
  runtime_sec = c(
    as.numeric(t_integrate),
    as.numeric(t_profile)
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# Save iteration artifacts
# ============================================================
saveRDS(runtime, path(iter_dir, "runtime.rds"))
saveRDS(model, path(iter_dir, "model.rds"))

message("✅ Iteration complete: ", sim_id, " / ", iter_id)
