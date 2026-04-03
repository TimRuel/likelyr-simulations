#!/usr/bin/env Rscript

# ============================================================
# run_iter.R
#
# Contract:
#   • Runs ONE iteration of ONE simulation
#   • Execution mode determined by env var:
#       LIKELYR_EXEC_MODE = "slurm" | "test"
#   • Input:
#       - <path/to/sim_XX/sim_XX.yml>
#       - SLURM_ARRAY_TASK_ID   (slurm mode)
#       - LIKELYR_TEST_ITER     (test mode, optional)
#   • Output:
#       - <iter_dir>/model.rds        (success)
#       - <iter_dir>/error.rds        (failure)
#
# Environment variables:
#   LIKELYR_EXEC_MODE   "slurm" | "test"       (default: "slurm")
#   LIKELYR_VERBOSE     "TRUE"  | "FALSE"       (default: "TRUE")
#   LIKELYR_SIM_DIR     override sim directory  (optional; used by test_iter.R
#                       to redirect output to a test_sim_* directory)
#
# Seed hierarchy:
#   experiment.seed_base                              — exp-level anchor
#   simulation.seed_base = exp_seed_base + i*100000  — sim-level space
#   parameter.seed       = sim_seed_base             — sim-level (setup time)
#   data.seed_base       = sim_seed_base             — iter: + iter_index
#   sampler.seed_base    = sim_seed_base + 10000     — iter: + iter_index
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
    "Usage: Rscript run_iter.R <path/to/sim_XX/sim_XX.yml>",
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
  file.path(root, "scripts", "utils.R"),
  local = TRUE
)

# ============================================================
# Resolve simulation context
# LIKELYR_SIM_DIR allows test_iter.R to redirect output to a
# test_sim_* directory while still reading the original sim config.
# ============================================================
sim_dir_override <- Sys.getenv("LIKELYR_SIM_DIR", "")
sim_dir <- if (nzchar(sim_dir_override)) {
  path_abs(sim_dir_override)
} else {
  path_dir(sim_yml)
}
sim_id <- path_file(sim_dir)

# ============================================================
# Execution configuration
# ============================================================
exec_mode <- Sys.getenv("LIKELYR_EXEC_MODE", "slurm")
verbose <- isTRUE(as.logical(Sys.getenv("LIKELYR_VERBOSE", "TRUE")))

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
message("  Verbose:     ", verbose)

# ============================================================
# Load simulation config
# ============================================================
config <- read_yaml(sim_yml)

# ============================================================
# Load shared model skeleton
# (true parameter already baked in from build_model_spec.R)
# ============================================================
model_path <- path(sim_dir, "model", "model.rds")

if (!file_exists(model_path)) {
  stop("Shared simulation model not found: ", model_path, call. = FALSE)
}

model <- readRDS(model_path)

# ============================================================
# Seeding
#   data.seed_base and sampler.seed_base are stored in the sim
#   yaml and derived from simulation.seed_base at expand time.
#   Final iter-level seeds are seed_base + iter_index.
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
  error_path <- path(iter_dir, "error.rds")
  saveRDS(error_record, error_path)
  message("❌ Iteration failed: ", conditionMessage(model))
  message("   Error record saved: ", error_path)
  quit(status = 1L, save = "no")
}

model_out_path <- path(iter_dir, "model.rds")
saveRDS(model, model_out_path)
message("💾 Model saved: ", model_out_path)

message("✅ Iteration complete: ", sim_id, " / ", iter_id)
