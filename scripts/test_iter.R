#!/usr/bin/env Rscript

# ============================================================
# test_iter.R
#
# Local test harness for run_iter.R.
#
# Contract:
#   • Accepts the SAME argument as run_iter.R:
#       <path/to/simulation.yml>
#   • Emulates the Slurm execution environment
#   • Delegates ALL computation to run_iter.R
#
# Usage:
#   Rscript scripts/test_iter.R <path/to/simulation.yml> [iter_index]
#
# Notes:
#   • iter_index is optional (1-based, default = 1)
#   • No logic duplication with run_iter.R
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

iter_index <- if (length(args) == 2L) {
  as.integer(args[[2]])
} else {
  1L
}

if (is.na(iter_index) || iter_index < 1L) {
  stop("iter_index must be a positive integer.", call. = FALSE)
}

# ------------------------------------------------------------
# Emulate Slurm environment
# ------------------------------------------------------------
# SLURM_ARRAY_TASK_ID is 0-based
Sys.setenv(SLURM_ARRAY_TASK_ID = as.character(iter_index - 1))

# Default to single core locally
Sys.setenv(SLURM_CPUS_PER_TASK = Sys.getenv("SLURM_CPUS_PER_TASK", "1"))

# ------------------------------------------------------------
# Locate run_iter.R
# ------------------------------------------------------------
run_iter_path <- fs::path("scripts", "run_iter.R")

if (!file_exists(run_iter_path)) {
  stop("Could not find scripts/run_iter.R", call. = FALSE)
}

message("🧪 Local test iteration")
message("  Simulation: ", sim_yml)
message("  Iteration:  iter_", sprintf("%04d", iter_index))
message("  Using:      ", run_iter_path)

# ------------------------------------------------------------
# Delegate to run_iter.R
# ------------------------------------------------------------
cmd <- sprintf(
  'Rscript "%s" "%s"',
  run_iter_path,
  sim_yml
)

status <- system(cmd)

if (status != 0) {
  stop("run_iter.R failed with status ", status, call. = FALSE)
}

message("✅ Local test iteration complete")
