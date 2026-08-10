#!/usr/bin/env Rscript

# ============================================================
# bundle_exp.R
#
# Collapse an experiment's per-simulation analysis outputs into a single
# <exp_dir>/analysis/bundle.rds.
#
# Why bundle at all: the per-sim analysis outputs are already small, but
# there are 3 of them per sim and up to 48 sims per experiment, so a
# download is ~150 files and the consumer then has to walk the tree,
# readRDS each one and bind_rows before it can plot anything. Across the
# dozen-plus experiment versions per estimand that adds up to thousands
# of files whose only purpose is to be immediately concatenated.
#
# Bundling does that concatenation once, on the machine that already has
# the data, and turns the wire transfer and the downstream read into a
# single file each. The experiment and simulation ids are already columns
# on every table (both analyzers write them), so nothing is lost by
# flattening the directory structure away.
#
# Works for both experiment kinds — it bundles whichever analysis files
# are present rather than being told the kind:
#   simulation   sim_point_metrics / sim_interval_metrics / invalid_ci_index
#   application  app_estimates / app_curves / app_context
#
# Output structure (a named list):
#   $meta                  experiment id, kind, sim counts, timestamp
#   $point_metrics         \
#   $interval_metrics       > simulation experiments
#   $invalid_ci_index      /
#   $estimates             \
#   $curves                 > application experiments
#   $context               /
#
# Absent tables are omitted, so names(bundle) is itself a reliable
# indicator of the experiment kind.
#
# Usage:
#   Rscript R/bundle_exp.R <exp_dir>
# ============================================================

suppressPackageStartupMessages({
  library(fs)
  library(dplyr)
})

# ============================================================
# Parse CLI arguments
# ============================================================
args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 1L) {
  stop("Usage: Rscript bundle_exp.R <exp_dir>", call. = FALSE)
}

exp_dir <- path_abs(args[[1]])

if (!dir_exists(exp_dir)) {
  stop("Experiment directory not found: ", exp_dir, call. = FALSE)
}

exp_id <- path_file(exp_dir)

# ============================================================
# Table registry
#
# Maps the on-disk filename written by each analyzer to the slot it
# occupies in the bundle, and records which kind it belongs to so the
# kind can be inferred from what was actually found.
# ============================================================
TABLES <- list(
  list(file = "sim_point_metrics.rds",    slot = "point_metrics",    kind = "simulation"),
  list(file = "sim_interval_metrics.rds", slot = "interval_metrics", kind = "simulation"),
  list(file = "invalid_ci_index.rds",     slot = "invalid_ci_index", kind = "simulation"),
  list(file = "app_estimates.rds",        slot = "estimates",        kind = "application"),
  list(file = "app_curves.rds",           slot = "curves",           kind = "application"),
  list(file = "app_context.rds",          slot = "context",          kind = "application")
)

# ============================================================
# Discover analyzed simulations
# ============================================================
sim_dirs <- dir_ls(
  exp_dir,
  type = "directory",
  regexp = "sim_\\d+$",
  fail = FALSE
) |>
  sort()

if (length(sim_dirs) == 0L) {
  stop("No sim_* directories found in: ", exp_dir, call. = FALSE)
}

analysis_dirs <- path(sim_dirs, "analysis")
analysis_dirs <- analysis_dirs[dir_exists(analysis_dirs)]

if (length(analysis_dirs) == 0L) {
  stop(
    "No sim_*/analysis directories found in: ",
    exp_dir,
    "\nRun `make analyze-exp EXP_CONFIG=...` first.",
    call. = FALSE
  )
}

message("📦 Bundling experiment: ", exp_id)
message(
  "   Analyzed simulations: ",
  length(analysis_dirs),
  " of ",
  length(sim_dirs)
)

# ============================================================
# Read and concatenate
#
# invalid_ci_index is the one table analyze_sim.R writes WITHOUT an
# experiment/simulation column, so the sim id is stamped on during the
# read rather than assumed to be present. Doing it for every table is
# harmless and keeps the loop uniform: if the column already exists the
# stamp is a no-op because the coalesce prefers the existing value.
# ============================================================
read_stamped <- function(analysis_dir, file) {
  p <- path(analysis_dir, file)

  if (!file_exists(p)) {
    return(NULL)
  }

  df <- readRDS(p)

  if (!is.data.frame(df) || nrow(df) == 0L) {
    return(NULL)
  }

  sim_id <- path_file(path_dir(analysis_dir))

  if (!"simulation" %in% names(df)) {
    df <- df |> mutate(simulation = sim_id, .before = 1)
  }

  if (!"experiment" %in% names(df)) {
    df <- df |> mutate(experiment = exp_id, .before = 1)
  }

  df
}

bundle <- list()
kinds_found <- character(0)

for (spec in TABLES) {
  parts <- lapply(analysis_dirs, read_stamped, file = spec$file)
  parts <- parts[!vapply(parts, is.null, logical(1))]

  if (length(parts) == 0L) {
    next
  }

  combined <- bind_rows(parts)

  bundle[[spec$slot]] <- combined
  kinds_found <- c(kinds_found, spec$kind)

  message(
    "   • ",
    format(spec$slot, width = 17),
    format(nrow(combined), big.mark = ",", width = 9),
    " rows  from ",
    length(parts),
    " sim(s)"
  )
}

if (length(bundle) == 0L) {
  stop(
    "No recognized analysis files found under ",
    exp_dir,
    "\nExpected one of: ",
    paste(vapply(TABLES, `[[`, character(1), "file"), collapse = ", "),
    call. = FALSE
  )
}

# ============================================================
# Infer kind from what was found
#
# A well-formed experiment yields exactly one kind. Both showing up means
# the directory has been analyzed by both analyzers — worth surfacing
# rather than silently picking one, since it usually means experiment$kind
# was changed after the fact and the stale outputs were never cleared.
# ============================================================
kinds_found <- unique(kinds_found)

if (length(kinds_found) > 1L) {
  warning(
    "Found analysis outputs of BOTH kinds (",
    paste(kinds_found, collapse = ", "),
    ") under ",
    exp_dir,
    ". Was experiment$kind changed without clearing sim_*/analysis?",
    call. = FALSE
  )
}

n_sims_by_table <- vapply(
  bundle,
  function(df) length(unique(df$simulation)),
  integer(1)
)

bundle$meta <- list(
  experiment = exp_id,
  kind = if (length(kinds_found) == 1L) kinds_found else kinds_found,
  exp_dir = as.character(exp_dir),
  n_sims_total = length(sim_dirs),
  n_sims_analyzed = max(n_sims_by_table),
  tables = setdiff(names(bundle), "meta"),
  bundled_at = Sys.time()
)

# ============================================================
# Save
# ============================================================
out_dir <- path(exp_dir, "analysis")
dir_create(out_dir)

out_path <- path(out_dir, "bundle.rds")

saveRDS(bundle, out_path)

message(
  "✔ Bundle written: ",
  out_path,
  "  (",
  format(
    round(as.numeric(file_info(out_path)$size) / 1024, 1),
    nsmall = 1
  ),
  " KB)"
)
