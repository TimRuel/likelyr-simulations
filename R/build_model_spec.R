#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(likelyr)
  library(here)
  library(fs)
  library(yaml)
})

# ============================================================
# build_model_spec.R
#
# Build and persist a model specification for a single simulation.
#
# Contract:
#   • Called once per simulation
#   • Receives path to:
#       experiments/<experiment>/<simulation>/simulation.yml
#   • Assumes directory structure already exists
#   • Builds and saves:
#       experiments/<experiment>/<simulation>/model/model.rds
#   • NO data generation
#   • NO calibration
#   • Refuses to overwrite an existing model spec
# ============================================================

# ============================================================
# 1. Anchor project root
# ============================================================
root <- here()

# ============================================================
# 2. Load local utilities
# ============================================================
source(
  file.path(root, "R", "utils.R"),
  local = TRUE
)

# ============================================================
# 3. Parse CLI arguments
# ============================================================
args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 1L) {
  stop(
    "Usage: Rscript build_model_spec.R <path/to/simulation.yml>",
    call. = FALSE
  )
}

sim_config_path <- path_abs(args[[1]])

if (!file_exists(sim_config_path)) {
  stop("Simulation config not found: ", sim_config_path, call. = FALSE)
}

# ============================================================
# 4. Resolve simulation directory (authoritative)
# ============================================================
sim_dir <- path_dir(sim_config_path)
model_path <- path(sim_dir, "model", "model.rds")

if (!dir_exists(sim_dir)) {
  stop("Simulation directory does not exist: ", sim_dir, call. = FALSE)
}

# Model specs are immutable once built
if (file_exists(model_path)) {
  stop("Model already exists: ", model_path, call. = FALSE)
}

message("🧩 Building model specification for:")
message("   ", sim_dir)

# ============================================================
# 5. Read simulation config snapshot
# ============================================================
config <- read_yaml(sim_config_path)

# ============================================================
# 6. Resolve spec directory
# ============================================================
spec_path <- config$experiment$spec_path

if (is.null(spec_path)) {
  stop("experiment$spec_path must be defined.", call. = FALSE)
}

spec_dir <- path(root, spec_path)

if (!dir_exists(spec_dir)) {
  stop("Spec directory not found: ", spec_dir, call. = FALSE)
}

required_files <- c(
  "parameter.R",
  "likelihood.R",
  "estimand.R",
  "sampler.R",
  "traversal.R",
  "solver.R",
  "execution.R"
)

missing_files <- required_files[
  !file_exists(path(spec_dir, required_files))
]

if (length(missing_files)) {
  stop(
    "Spec directory is missing required file(s): ",
    paste(missing_files, collapse = ", "),
    call. = FALSE
  )
}

# ============================================================
# 7. Source model specs into isolated environment
# ============================================================
spec_env <- load_spec_env(spec_dir)

# ============================================================
# 8. Validate required factory functions
# ============================================================
required_fns <- c(
  "make_parameter",
  "make_likelihood",
  "make_estimand",
  "make_sampler",
  "make_traversal",
  "make_solver",
  "make_execution"
)

missing_fns <- required_fns[
  !vapply(
    required_fns,
    exists,
    logical(1),
    envir = spec_env,
    inherits = FALSE
  )
]

if (length(missing_fns)) {
  stop(
    "Specs did not define required factory function(s): ",
    paste(missing_fns, collapse = ", "),
    call. = FALSE
  )
}

# ============================================================
# 9. Build spec objects
# ============================================================
parameter <- spec_env$make_parameter(config)
likelihood <- spec_env$make_likelihood(config)
estimand <- spec_env$make_estimand(config)
sampler <- spec_env$make_sampler(config)
traversal <- spec_env$make_traversal(config)
solver <- spec_env$make_solver(config)
execution <- spec_env$make_execution(config)

# ============================================================
# 10. Assemble model_spec (STRUCTURE ONLY)
# ============================================================
model <- model_spec(
  name = sprintf(
    "%s — %s / %s",
    config$experiment$distribution,
    config$experiment$model,
    config$experiment$estimand
  )
) |>
  add(parameter) |>
  add(likelihood) |>
  add(estimand) |>
  add(sampler) |>
  add(traversal) |>
  add(solver) |>
  add(execution)

# ============================================================
# 11. Save model specification
# ============================================================
saveRDS(model, model_path)

message("✅ Model specification built:")
message("   ", model_path)
