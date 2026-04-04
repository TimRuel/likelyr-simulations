#!/usr/bin/env Rscript

# ============================================================
# test_iter.R
#
# Creates test_iteration/test_sim.yml by applying the test:
# overrides from the sim yaml to a copy of the sim config.
# Does not run the iteration — that is handled by test_sim.sh.
#
# Output:
#   sim_XX/test_iteration/test_sim.yml
#
# Usage:
#   Rscript R/test_iter.R <path/to/sim_XX/sim_XX.yml>
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
    "Usage: Rscript R/test_iter.R <path/to/sim_XX/sim_XX.yml>",
    call. = FALSE
  )
}

sim_yml <- path_abs(args[[1]])

if (!file_exists(sim_yml)) {
  stop("sim yaml not found: ", sim_yml, call. = FALSE)
}

sim_dir <- path_dir(sim_yml)

# ------------------------------------------------------------
# Read sim config and apply test overrides
# ------------------------------------------------------------
config <- read_yaml(sim_yml)
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
# Write test_sim.yml into test_iteration/
# ------------------------------------------------------------
test_dir <- path(sim_dir, "test_iteration")
dir_create(test_dir, recurse = TRUE)

test_yml <- path(test_dir, "test_sim.yml")
write_yaml(config, test_yml)

message("✔ test_sim.yml written to: ", test_dir)
