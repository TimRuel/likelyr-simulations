#!/usr/bin/env Rscript

# =============================================================================
# expand_design.R
#
# Expand a declarative exp_vX.yml into concrete sim_XX.yml files.
#
# Reads from (manual, never written to):
#   config/<path>/exp_vX.yml
#
# Writes to:
#   config/<path>/exp_vX/sim_01.yml
#   config/<path>/exp_vX/sim_02.yml
#   ...
#
# Also creates empty simulation data directories:
#   <experiment$exp_dir>/sim_01/
#   <experiment$exp_dir>/sim_02/
#   ...
#
# The output directory is read directly from experiment$exp_dir in the
# config YAML — no path derivation from config/ location.
#
# The top-level experiment: block is automatically propagated into each sim
# yaml's experiment: section — it does not need to be repeated in
# base_simulation:.
#
# The top-level test: block (if present) is copied verbatim onto each sim
# yaml. test_iter.R reads it and applies the overrides when running in
# test mode, without affecting production iterations.
#
# Each generated sim yaml has the following top-level structure (in order):
#   experiment:   version, distribution, model, estimand, specs_dir,
#                 logs_dir, exp_dir, seed_base
#   simulation:   sim_id, seed_base, iterations
#   design:       design_type, overrides
#   test:         dotted-path overrides applied only in test mode (optional)
#   parameter:    ...  (seed: = sim_seed_base, sim-level)
#   likelihood:   ...
#   sampler:      ...  (seed_base: = sim_seed_base + 10000, iter-level)
#   traversal:    ...
#   solver:       ...
#   execution:    ...
#   data:         ...  (seed_base: = sim_seed_base + 20000, iter-level)
#
# Seed hierarchy:
#   experiment.seed_base                              — exp-level anchor
#   simulation.seed_base = exp_seed_base + i*100000  — sim-level space
#   parameter.seed       = sim_seed_base             — sim-level (setup time)
#   sampler.seed_base    = sim_seed_base + 10000     — iter: + iter_index
#   data.seed_base       = sim_seed_base + 20000     — iter: + iter_index
#
# Usage:
#   Rscript R/expand_design.R config/<path>/exp_vX.yml
# =============================================================================

suppressPackageStartupMessages({
  library(yaml)
  library(fs)
  library(tools)
})

`%||%` <- function(x, y) if (is.null(x)) y else x

# -----------------------------------------------------------------------------
# CLI
# -----------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) {
  stop("Usage: Rscript expand_design.R <path/to/exp_vX.yml>", call. = FALSE)
}

exp_yml <- fs::path_abs(args[[1]])
if (!fs::file_exists(exp_yml)) {
  stop("Experiment config not found: ", exp_yml, call. = FALSE)
}

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------

path_tokens <- function(p) {
  if (!is.character(p) || length(p) != 1L || !nzchar(p)) {
    stop("Invalid factor path: must be a non-empty string.", call. = FALSE)
  }
  strsplit(p, "\\.", fixed = FALSE)[[1]]
}

has_path <- function(x, tokens) {
  cur <- x
  for (t in tokens) {
    if (!is.list(cur) || !(t %in% names(cur))) {
      return(FALSE)
    }
    cur <- cur[[t]]
  }
  TRUE
}

set_by_path <- function(x, path, value, require_existing = TRUE) {
  tokens <- path_tokens(path)
  if (require_existing && !has_path(x, tokens)) {
    stop("Override path not found in base_simulation: ", path, call. = FALSE)
  }

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

normalize_scalar <- function(v) {
  if (is.numeric(v) && length(v) == 1L && is.finite(v)) {
    if (v == as.integer(v)) {
      return(as.integer(v))
    }
    return(as.numeric(v))
  }
  if (is.logical(v) && length(v) == 1L) {
    return(as.logical(v))
  }
  if (is.character(v) && length(v) == 1L) {
    return(as.character(v))
  }
  v
}

expand_grid_design <- function(factors) {
  if (!is.list(factors) || length(factors) == 0L) {
    stop("design$factors must be a non-empty mapping.", call. = FALSE)
  }
  factor_names <- sort(names(factors))
  if (any(!nzchar(factor_names))) {
    stop("All design$factors keys must be non-empty strings.", call. = FALSE)
  }
  values_list <- lapply(factor_names, function(nm) {
    v <- factors[[nm]]
    if (is.null(v)) {
      stop("design$factors contains NULL values.", call. = FALSE)
    }
    if (!is.atomic(v) && !is.list(v)) {
      stop("design$factors values must be atomic vectors.", call. = FALSE)
    }
    unlist(v, recursive = TRUE, use.names = FALSE)
  })
  names(values_list) <- factor_names
  grid <- expand.grid(
    values_list,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  lapply(seq_len(nrow(grid)), function(i) as.list(grid[i, , drop = FALSE]))
}

expand_points_design <- function(points) {
  if (!is.list(points) || length(points) == 0L) {
    stop("design$points must be a non-empty list.", call. = FALSE)
  }
  for (i in seq_along(points)) {
    if (
      !is.list(points[[i]]) ||
        is.null(names(points[[i]])) ||
        any(names(points[[i]]) == "")
    ) {
      stop(
        "Each design$points[[i]] must be a named mapping of path -> value.",
        call. = FALSE
      )
    }
  }
  points
}

# -----------------------------------------------------------------------------
# Load experiment config
# -----------------------------------------------------------------------------
exp_cfg <- yaml::read_yaml(exp_yml)

if (is.null(exp_cfg$base_simulation) || !is.list(exp_cfg$base_simulation)) {
  stop(
    "experiment config must define `base_simulation:` as a nested YAML mapping.",
    call. = FALSE
  )
}

base <- exp_cfg$base_simulation

# Experiment metadata
exp_meta <- exp_cfg$experiment %||% list()
exp_name <- exp_meta$name %||% exp_meta$id %||% "experiment"
exp_version <- exp_meta$version

if (is.null(exp_version) || !nzchar(exp_version)) {
  stop("experiment$version must be defined (e.g., 'exp_v1').", call. = FALSE)
}

# Read output directory directly from config
exp_run_dir <- exp_meta$exp_dir

if (is.null(exp_run_dir) || !nzchar(exp_run_dir)) {
  stop(
    "experiment$exp_dir must be defined with the full path to the results directory.",
    call. = FALSE
  )
}

# Sim yamls are written to the same directory as the exp yaml
config_sim_dir <- fs::path_dir(exp_yml)

# Propagate full experiment block into each sim yaml
sim_experiment_block <- exp_meta[setdiff(names(exp_meta), "name")]

# Test overrides block (optional) — copied verbatim to each sim yaml
test_block <- exp_cfg$test %||% NULL

# Design block
design <- exp_cfg$design
if (is.null(design) || !is.list(design)) {
  stop("experiment config must define a `design:` block.", call. = FALSE)
}

design_type <- design$type %||% "grid"
if (!is.character(design_type) || length(design_type) != 1L) {
  stop("design$type must be a character scalar.", call. = FALSE)
}

require_existing_paths <- design$require_existing_paths %||% TRUE
if (
  !is.logical(require_existing_paths) || length(require_existing_paths) != 1L
) {
  stop("design$require_existing_paths must be TRUE/FALSE.", call. = FALSE)
}

# Seed policy
seed_base <- exp_meta$seed_base
if (is.null(seed_base)) {
  stop(
    "experiment$seed_base must be defined in the top-level experiment: block.",
    call. = FALSE
  )
}
if (!is.numeric(seed_base) || length(seed_base) != 1L || seed_base < 0) {
  stop(
    "experiment$seed_base must be a single non-negative number.",
    call. = FALSE
  )
}
seed_base <- as.integer(seed_base)

# Iterations
iterations <- base$iterations
if (is.null(iterations) || !is.numeric(iterations) || iterations < 1L) {
  stop("base_simulation$iterations must be a positive integer.", call. = FALSE)
}
iterations <- as.integer(iterations)

# -----------------------------------------------------------------------------
# Expand design
# -----------------------------------------------------------------------------
points <- switch(
  tolower(design_type),
  "grid" = expand_grid_design(design$factors),
  "points" = expand_points_design(design$points),
  stop("Unsupported design$type: ", design_type, call. = FALSE)
)

if (length(points) == 0L) {
  stop("Design expansion produced 0 simulation points.", call. = FALSE)
}

# -----------------------------------------------------------------------------
# Write sim configs
# -----------------------------------------------------------------------------
pad_width <- max(2L, nchar(as.character(length(points))))

overwrite <- design$overwrite %||% TRUE
if (!is.logical(overwrite) || length(overwrite) != 1L) {
  stop("design$overwrite must be TRUE/FALSE.", call. = FALSE)
}

existing_sim_ymls <- if (fs::dir_exists(config_sim_dir)) {
  fs::dir_ls(
    config_sim_dir,
    type = "file",
    regexp = "sim_\\d+\\.yml$",
    fail = FALSE
  )
} else {
  character(0)
}

if (length(existing_sim_ymls) > 0L && !overwrite) {
  stop(
    "sim_*.yml files already exist in ",
    config_sim_dir,
    " and design$overwrite is FALSE.",
    call. = FALSE
  )
}
if (length(existing_sim_ymls) > 0L && overwrite) {
  fs::file_delete(existing_sim_ymls)
}

message("▶ Expanding design into ", length(points), " simulation config(s)")
message("   Experiment: ", exp_name, " (", exp_version, ")")
message("   Design type: ", design_type)
message("   Sim yamls:   ", config_sim_dir)
message("   Data dir:    ", exp_run_dir)
if (!is.null(test_block)) {
  message("   Test overrides: ", length(test_block), " field(s)")
}

body_keys <- setdiff(
  names(base),
  c("iterations", "experiment", "simulation", "design")
)

for (i in seq_along(points)) {
  sim_id <- sprintf(paste0("sim_%0", pad_width, "d"), i)
  sim_seed_base <- as.integer(seed_base + i * 100000L)

  sim <- base
  pt <- points[[i]]
  for (k in names(pt)) {
    sim <- set_by_path(
      sim,
      path = k,
      value = normalize_scalar(pt[[k]]),
      require_existing = require_existing_paths
    )
  }

  sim$parameter$seed <- sim_seed_base
  sim$sampler$seed_base <- sim_seed_base + 10000L
  sim$data$seed_base <- sim_seed_base + 20000L

  # Build output in desired key order
  out <- list()
  out$experiment <- sim_experiment_block
  out$simulation <- list(
    sim_id = sim_id,
    seed_base = sim_seed_base,
    iterations = iterations
  )
  out$design <- list(
    design_type = design_type,
    overrides = lapply(pt, normalize_scalar)
  )

  if (!is.null(test_block)) {
    out$test <- test_block
  }

  for (k in body_keys) {
    out[[k]] <- sim[[k]]
  }

  sim_out_dir <- fs::path(exp_run_dir, sim_id)
  fs::dir_create(sim_out_dir, recurse = TRUE)
  fs::dir_create(config_sim_dir, recurse = TRUE)
  yaml::write_yaml(out, fs::path(config_sim_dir, paste0(sim_id, ".yml")))
}

message("✔ Done. Wrote sim yamls to: ", config_sim_dir)
message("        Created data dirs in: ", exp_run_dir)
