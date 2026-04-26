#!/usr/bin/env Rscript

# ============================================================
# test_sim.R
#
# Creates test_sim.yml by applying the test: overrides from
# the sim yaml to a copy of the sim config, then redirecting
# exp_dir and sim_id so that build_model_spec.R and run_iter.R
# save outputs under <exp_dir>/sim_XX/test_sim/.
#
# Output:
#   <exp_dir>/sim_XX/test_sim/test_sim.yml
#
# Usage:
#   Rscript R/test_sim.R <path/to/config/.../sim_XX.yml>
# ============================================================

suppressPackageStartupMessages({
  library(fs)
  library(yaml)
})

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------
set_by_path <- function(x, path, value) {
  tokens <- strsplit(path, "\\.", fixed = FALSE)[[1]]

  set_rec <- function(obj, toks, val) {
    key <- toks[[1]]
    if (length(toks) == 1L) {
      obj[[key]] <- val
      return(obj)
    }
    if (is.null(obj[[key]])) {
      obj[[key]] <- list()
    }
    obj[[key]] <- set_rec(obj[[key]], toks[-1], val)
    obj
  }

  set_rec(x, tokens, value)
}

# ------------------------------------------------------------
# Parse CLI arguments
# ------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 1L) {
  stop(
    "Usage: Rscript R/test_sim.R <path/to/sim_XX.yml>",
    call. = FALSE
  )
}

sim_yml <- path_abs(args[[1]])

if (!file_exists(sim_yml)) {
  stop("sim yaml not found: ", sim_yml, call. = FALSE)
}

# ------------------------------------------------------------
# Read sim config
# ------------------------------------------------------------
config <- read_yaml(sim_yml)

exp_dir <- config$experiment$exp_dir
sim_id <- config$simulation$sim_id

if (is.null(exp_dir) || !nzchar(exp_dir)) {
  stop("experiment$exp_dir must be defined in the sim yaml.", call. = FALSE)
}
if (is.null(sim_id) || !nzchar(sim_id)) {
  stop("simulation$sim_id must be defined in the sim yaml.", call. = FALSE)
}

# ------------------------------------------------------------
# Apply test overrides
# ------------------------------------------------------------
test_block <- config$test

if (is.null(test_block) || length(test_block) == 0L) {
  message("ℹ No test: overrides found — using sim config as-is")
} else {
  message("▶ Applying test overrides:")
  for (k in names(test_block)) {
    message("    ", k, ": ", test_block[[k]])
    config <- set_by_path(config, k, test_block[[k]])
  }
}

# ------------------------------------------------------------
# Redirect exp_dir and sim_id so that build_model_spec.R and
# run_iter.R write into <exp_dir>/test_sim/, mirroring the
# production sim structure exactly.
# ------------------------------------------------------------
config$experiment$exp_dir <- path(exp_dir, sim_id)
config$simulation$sim_id <- "test_sim"

# ------------------------------------------------------------
# Write test_sim.yml inside the test_sim folder
# ------------------------------------------------------------
test_dir <- path(exp_dir, sim_id, "test_sim")
dir_create(test_dir, recurse = TRUE)

test_yml <- path(test_dir, "test_sim.yml")
write_yaml(config, test_yml)

message("✔ test_sim.yml written to: ", test_dir)
