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
#       config/<path>/exp_vX/sim_XX.yml
#   • Reads experiment$exp_dir and simulation$sim_id from the yaml
#     to determine the output location
#   • Assumes directory structure already exists
#   • Builds and saves:
#       <exp_dir>/sim_XX/model/model.rds
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
    "Usage: Rscript build_model_spec.R <path/to/sim_XX.yml>",
    call. = FALSE
  )
}

sim_config_path <- path_abs(args[[1]])

if (!file_exists(sim_config_path)) {
  stop("Simulation config not found: ", sim_config_path, call. = FALSE)
}

# ============================================================
# 4. Read simulation config
# ============================================================
config <- read_yaml(sim_config_path)

# ============================================================
# 5. Resolve simulation data directory from config
# ============================================================
exp_dir <- config$experiment$exp_dir
sim_id <- config$simulation$sim_id

if (is.null(exp_dir) || !nzchar(exp_dir)) {
  stop("experiment$exp_dir must be defined in the sim yaml.", call. = FALSE)
}

if (is.null(sim_id) || !nzchar(sim_id)) {
  stop("simulation$sim_id must be defined in the sim yaml.", call. = FALSE)
}

sim_dir <- path(exp_dir, sim_id)
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
# 6. Resolve spec directory
# ============================================================
specs_dir <- config$experiment$specs_dir

if (is.null(specs_dir) || !nzchar(specs_dir)) {
  stop("experiment$specs_dir must be defined.", call. = FALSE)
}

if (!dir_exists(specs_dir)) {
  stop("Spec directory not found: ", specs_dir, call. = FALSE)
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
  !file_exists(path(specs_dir, required_files))
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
spec_env <- load_spec_env(specs_dir)

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
