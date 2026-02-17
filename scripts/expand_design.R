#!/usr/bin/env Rscript

# =============================================================================
# expand_design.R
#
# Expand a declarative experiment.yml into concrete sim_XX.yml files.
#
# Takes a single experiment.yml that contains:
#   • base_simulation: a full simulation template (nested YAML)
#   • design: how simulations differ (grid or explicit points)
#
# Writes:
#   config/<experiment>/sim_01.yml, sim_02.yml, ...
#
# Usage:
#   Rscript scripts/expand_design.R config/<experiment>/experiment.yml
#
# Notes:
#   • Overrides are specified via dotted paths (e.g., "parameter.J").
#   • For each design point, base_simulation is deep-copied and overridden.
#   • Each sim config receives attached design metadata for provenance.
# =============================================================================

suppressPackageStartupMessages({
  library(yaml)
  library(fs)
  library(tools)
})

# -----------------------------------------------------------------------------
# Minimal %||% operator (local)
# -----------------------------------------------------------------------------
`%||%` <- function(x, y) if (is.null(x)) y else x

# -----------------------------------------------------------------------------
# CLI
# -----------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) {
  stop(
    "Usage: Rscript expand_design.R <path/to/experiment.yml>",
    call. = FALSE
  )
}

exp_yml <- fs::path_abs(args[[1]])
if (!fs::file_exists(exp_yml)) {
  stop("Experiment config not found: ", exp_yml, call. = FALSE)
}

exp_dir <- fs::path_dir(exp_yml)

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------

# Split dotted path into tokens
path_tokens <- function(p) {
  if (!is.character(p) || length(p) != 1L || !nzchar(p)) {
    stop("Invalid factor path: must be a non-empty string.", call. = FALSE)
  }
  strsplit(p, "\\.", fixed = FALSE)[[1]]
}

# Safe check whether a path exists in a nested list
has_path <- function(x, tokens) {
  cur <- x
  for (t in tokens) {
    if (!is.list(cur) || is.null(cur[[t]])) {
      return(FALSE)
    }
    cur <- cur[[t]]
  }
  TRUE
}

# Set a nested value by dotted path
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

# Normalize scalars for YAML
normalize_scalar <- function(v) {
  if (is.numeric(v) && length(v) == 1L && is.finite(v)) {
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

# Expand a grid design
expand_grid_design <- function(factors) {
  if (!is.list(factors) || length(factors) == 0L) {
    stop("design$factors must be a non-empty mapping.", call. = FALSE)
  }

  factor_names <- sort(names(factors))
  if (any(!nzchar(factor_names))) {
    stop("All design$factors keys must be non-empty strings.", call. = FALSE)
  }

  values_list <- lapply(factor_names, function(nm) factors[[nm]])
  names(values_list) <- factor_names

  values_list <- lapply(values_list, function(v) {
    if (is.null(v)) {
      stop("design$factors contains NULL values.", call. = FALSE)
    }
    if (!is.atomic(v) && !is.list(v)) {
      stop("design$factors values must be atomic vectors.", call. = FALSE)
    }
    unlist(v, recursive = TRUE, use.names = FALSE)
  })

  grid <- expand.grid(
    values_list,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  lapply(seq_len(nrow(grid)), function(i) {
    as.list(grid[i, , drop = FALSE])
  })
}

# Expand explicit points design
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
# Load experiment.yml
# -----------------------------------------------------------------------------
exp_cfg <- yaml::read_yaml(exp_yml)

if (is.null(exp_cfg$base_simulation) || !is.list(exp_cfg$base_simulation)) {
  stop(
    "experiment.yml must define `base_simulation:` as a nested YAML mapping.",
    call. = FALSE
  )
}

base <- exp_cfg$base_simulation

# Optional experiment metadata
exp_meta <- exp_cfg$experiment %||% list()
exp_name <- exp_meta$name %||% exp_meta$id %||% fs::path_file(exp_dir)

# Design block
design <- exp_cfg$design
if (is.null(design) || !is.list(design)) {
  stop("experiment.yml must define a `design:` block.", call. = FALSE)
}

design_type <- design$type %||% "grid"
if (!is.character(design_type) || length(design_type) != 1L) {
  stop("design$type must be a character scalar.", call. = FALSE)
}

# Optional simulation-wide controls
sim_controls <- exp_cfg$simulation
if (!is.null(sim_controls) && !is.list(sim_controls)) {
  stop("simulation: (if present) must be a mapping.", call. = FALSE)
}

# Require override paths to exist (recommended)
require_existing_paths <- design$require_existing_paths %||% TRUE
if (
  !is.logical(require_existing_paths) || length(require_existing_paths) != 1L
) {
  stop("design$require_existing_paths must be TRUE/FALSE.", call. = FALSE)
}

# -----------------------------------------------------------------------------
# Seed policy (experiment-wide)
# -----------------------------------------------------------------------------
seed_base <- base$experiment$seed_base
if (is.null(seed_base)) {
  stop(
    "experiment$seed_base must be defined (e.g., 4000).",
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

# -----------------------------------------------------------------------------
# Expand design → list of points
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

existing <- fs::dir_ls(exp_dir, glob = "sim_*.yml", type = "file", fail = FALSE)
if (length(existing) > 0L && !overwrite) {
  stop(
    "sim_*.yml already exist in ",
    exp_dir,
    " and design$overwrite is FALSE.",
    call. = FALSE
  )
}
if (length(existing) > 0L && overwrite) {
  fs::file_delete(existing)
}

message("▶ Expanding design into ", length(points), " simulation config(s)")
message("   Experiment: ", exp_name)
message("   Design type: ", design_type)

for (i in seq_along(points)) {
  sim <- base

  # parameter seed → data-generating randomness
  # execution seed → algorithmic randomness
  sim_seed <- seed_base + i
  sim$parameter$seed <- sim_seed
  sim$execution$seed <- sim_seed + 10000

  pt <- points[[i]]
  for (k in names(pt)) {
    sim <- set_by_path(
      sim,
      path = k,
      value = normalize_scalar(pt[[k]]),
      require_existing = require_existing_paths
    )
  }

  sim$design <- list(
    experiment = exp_name,
    design_type = design_type,
    sim_index = i,
    sim_seed = sim_seed,
    overrides = lapply(pt, normalize_scalar)
  )

  if (!is.null(sim_controls)) {
    sim$simulation <- sim_controls
  }

  sim_id <- sprintf(paste0("sim_%0", pad_width, "d"), i)
  sim$simulation$sim_id <- sim_id

  out_path <- fs::path(exp_dir, paste0(sim_id, ".yml"))
  yaml::write_yaml(sim, out_path)
}

message("✔ Done. Wrote sim_*.yml files to: ", exp_dir)
