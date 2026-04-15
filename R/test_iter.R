#!/usr/bin/env Rscript

# ============================================================
# test_iter.R
#
# Creates test_iter.yml by applying the test: overrides from
# the sim yaml to a copy of the sim config, then redirecting
# exp_dir and sim_id so that build_model_spec.R and run_iter.R
# save outputs under the test_iter directory.
#
# Output:
#   <exp_dir>/sim_XX/iterations/test_iter/test_iter.yml
#
# Usage:
#   Rscript R/test_iter.R <path/to/config/.../sim_XX.yml>
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
    "Usage: Rscript R/test_iter.R <path/to/sim_XX.yml>",
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
# Redirect exp_dir and sim_id to test_iter location so that
# build_model_spec.R and run_iter.R save outputs correctly
# ------------------------------------------------------------
iter_dir <- path(exp_dir, sim_id, "iterations")
config$experiment$exp_dir <- iter_dir
config$simulation$sim_id <- "test_iter"

# ------------------------------------------------------------
# Write test_iter.yml into test_iter/
# ------------------------------------------------------------
test_dir <- path(iter_dir, "test_iter")
dir_create(test_dir, recurse = TRUE)

test_yml <- path(test_dir, "test_iter.yml")
write_yaml(config, test_yml)

message("✔ test_iter.yml written to: ", test_dir)
