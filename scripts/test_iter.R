#!/usr/bin/env Rscript

# ============================================================
# test_iter.R
#
# R-based local test harness for run_iter.R.
#
# Derives a test_sim_* directory from the given sim_* path,
# initializes it if needed (copying model.rds and simulation.yml),
# and redirects run_iter.R output there via LIKELYR_SIM_DIR.
# Iterations are labeled normally (iter_0001, etc.) and saved to:
#   experiments/<exp>/test_sim_XX/iterations/iter_XXXX/model.rds
#
# This is distinct from test_iter.sh, which uses LIKELYR_EXEC_MODE=test
# and saves to test_runs/ instead. Use this script from R or RStudio;
# use test_iter.sh (via `make test-iter`) for command-line testing.
#
# Usage:
#   Rscript scripts/test_iter.R <path/to/sim_XX/simulation.yml> [iter_index]
# ============================================================

suppressPackageStartupMessages({
  library(fs)
})

# ------------------------------------------------------------
# Parse CLI arguments
# ------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1L || length(args) > 2L) {
  stop(
    "Usage: Rscript test_iter.R <path/to/simulation.yml> [iter_index]",
    call. = FALSE
  )
}

sim_yml <- path_abs(args[[1]])

if (!file_exists(sim_yml)) {
  stop("simulation.yml not found: ", sim_yml, call. = FALSE)
}

iter_index <- if (length(args) == 2L) as.integer(args[[2]]) else 1L

if (is.na(iter_index) || iter_index < 1L) {
  stop("iter_index must be a positive integer.", call. = FALSE)
}

# ------------------------------------------------------------
# Derive test_sim_* directory from the sim_* path
# e.g. experiments/<exp>/sim_01 → experiments/<exp>/test_sim_01
# ------------------------------------------------------------
original_sim_dir <- path_dir(sim_yml)
original_sim_id <- path_file(original_sim_dir)
exp_dir <- path_dir(original_sim_dir)

if (!grepl("^sim_", original_sim_id)) {
  stop(
    "SIM_CONFIG must point to a sim_* directory. Got: ",
    original_sim_id,
    call. = FALSE
  )
}

test_sim_id <- sub("^sim_", "test_sim_", original_sim_id)
test_sim_dir <- path(exp_dir, test_sim_id)

# ------------------------------------------------------------
# Initialize test_sim_* directory if needed
# ------------------------------------------------------------
if (!dir_exists(test_sim_dir)) {
  message("▶ Initializing ", test_sim_id, " from ", original_sim_id)

  dir_create(path(test_sim_dir, "model"), recurse = TRUE)
  dir_create(path(test_sim_dir, "iterations"), recurse = TRUE)
  dir_create(path(test_sim_dir, "analysis"), recurse = TRUE)

  file_copy(
    path(original_sim_dir, "model", "model.rds"),
    path(test_sim_dir, "model", "model.rds")
  )
  file_copy(sim_yml, path(test_sim_dir, "simulation.yml"))

  message("✔ ", test_sim_id, " ready: ", test_sim_dir)
}

# ------------------------------------------------------------
# Emulate SLURM environment (slurm mode in run_iter.R)
# ------------------------------------------------------------
Sys.setenv(SLURM_ARRAY_TASK_ID = as.character(iter_index - 1L))
Sys.setenv(SLURM_CPUS_PER_TASK = Sys.getenv("SLURM_CPUS_PER_TASK", "1"))

# Redirect run_iter.R output to test_sim_* directory
Sys.setenv(LIKELYR_SIM_DIR = as.character(test_sim_dir))

# ------------------------------------------------------------
# Locate and delegate to run_iter.R
# ------------------------------------------------------------
run_iter_path <- fs::path("scripts", "run_iter.R")

if (!file_exists(run_iter_path)) {
  stop("Could not find scripts/run_iter.R", call. = FALSE)
}

message("🧪 Local test iteration")
message("  Source sim:  ", original_sim_id)
message("  Test sim:    ", test_sim_id)
message("  Iteration:   iter_", sprintf("%04d", iter_index))

cmd <- sprintf('Rscript "%s" "%s"', run_iter_path, sim_yml)
status <- system(cmd)

if (status != 0L) {
  stop("run_iter.R failed with status ", status, call. = FALSE)
}

message("✅ Local test iteration complete")
