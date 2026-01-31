# scripts/run_simulation.R
#!/usr/bin/env Rscript

# ============================================================
# Bootstrap: packages + startup hygiene
# ============================================================
suppressPackageStartupMessages({
  library(likelyr)
  library(here)
  library(fs)
  library(future)
  library(yaml)
})

# ============================================================
# Parse CLI arguments (authoritative input)
# ============================================================
args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 1L) {
  stop(
    "Usage: Rscript run_simulation.R <config.yml>",
    call. = FALSE
  )
}

config_path <- args[[1]]

if (!file.exists(config_path)) {
  stop(
    sprintf("Config file not found: %s", config_path),
    call. = FALSE
  )
}

# ============================================================
# Anchor project root + shared utilities
# ============================================================
root <- here()

source(
  file.path(root, "scripts", "utils.R"),
  local = TRUE
)

# ============================================================
# Load config (single source of truth)
# ============================================================
config <- read_yaml(config_path)

exp_id <- config$experiment$id
if (is.null(exp_id) || !nzchar(exp_id)) {
  stop("experiment$id missing from config.", call. = FALSE)
}

# ============================================================
# Resolve Slurm execution context
# ============================================================
sim_num <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID", NA))
if (is.na(sim_num)) {
  stop("SLURM_ARRAY_TASK_ID not set.", call. = FALSE)
}

sim_id <- sim_num + 1L

available_cpus <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "1"))
if (is.na(available_cpus) || available_cpus < 1L) {
  available_cpus <- 1L
}

# ============================================================
# Resolve experiment + simulation directories
# ============================================================
exp_dir <- file.path(root, "experiments", exp_id)

sim_dir <- file.path(
  exp_dir,
  "simulations",
  sprintf("sim_%04d", sim_id)
)

dir_create(sim_dir, recurse = TRUE)

# ============================================================
# Load invariant experiment objects
# ============================================================
model_path <- file.path(exp_dir, "model", "model.rds")

if (!file.exists(model_path)) {
  stop(
    sprintf("Model not found: %s", model_path),
    call. = FALSE
  )
}

model <- readRDS(model_path)

# ============================================================
# Stochastic data generation (spec-driven)
# ============================================================
if (!is.null(config$execution$seed)) {
  set.seed(config$execution$seed + sim_id)
} else {
  set.seed(100000 + sim_id)
}

spec_path <- config$experiment$spec_path
if (is.null(spec_path)) {
  stop("experiment$spec_path must be defined.", call. = FALSE)
}

spec_dir <- path(root, spec_path)

if (!dir_exists(spec_dir)) {
  stop(
    sprintf("Spec directory not found: %s", spec_dir),
    call. = FALSE
  )
}

# ------------------------------------------------------------
# Load spec environment (mirrors setup_experiment.R)
# ------------------------------------------------------------
spec_env <- load_spec_env(spec_dir)

data_spec_file <- path(spec_dir, "data.R")
if (!file.exists(data_spec_file)) {
  stop(
    sprintf("data.R not found in spec directory: %s", spec_dir),
    call. = FALSE
  )
}

source(data_spec_file, local = spec_env)

if (!exists("generate_data", envir = spec_env, inherits = FALSE)) {
  stop(
    "data.R must define generate_data(config, parameter).",
    call. = FALSE
  )
}

data <- spec_env$generate_data(
  config = config,
  parameter = model$parameter
)

# ============================================================
# Calibrate model to generated data
# ============================================================
model <- model |>
  calibrate(data)

# ============================================================
# Configure parallel execution (if requested)
# ============================================================
exec <- model$execution
use_parallel <- exec$mode == "parallel"

if (use_parallel) {
  requested_workers <- as.integer(exec$num_workers)

  if (is.na(requested_workers) || requested_workers < 1L) {
    stop("execution$num_workers must be a positive integer.", call. = FALSE)
  }

  if (requested_workers > available_cpus) {
    stop(
      sprintf(
        "Execution spec requests %d workers, but SLURM_CPUS_PER_TASK = %d.",
        requested_workers,
        available_cpus
      ),
      call. = FALSE
    )
  }

  message(
    sprintf(
      "Parallel integrate(): using %d workers (requested=%d, available=%d).",
      requested_workers,
      requested_workers,
      available_cpus
    )
  )

  plan(multisession, workers = requested_workers)
}

# ============================================================
# Run inference
# ============================================================
model <- model |>
  integrate()

plan(sequential)

model <- model |>
  profile()

# ============================================================
# Persist results
# ============================================================
saveRDS(
  model,
  file.path(sim_dir, "model_cip.rds")
)
