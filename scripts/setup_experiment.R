#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(likelyr)
  library(here)
  library(fs)
  library(yaml)
})

# ============================================================
# 1. Anchor project root
# ============================================================
root <- here()

# -------------------------------
# 2. Load local utilities
# -------------------------------
source(file.path(root, "scripts", "utils.R"))

# ============================================================
# 3. Read config
# ============================================================
args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 1) {
  stop(
    "Usage: Rscript setup_experiment.R <path/to/experiment/config.yml>",
    call. = FALSE
  )
}

config_path <- path_abs(args[1])

if (!file_exists(config_path)) {
  stop("Config file not found: ", config_path, call. = FALSE)
}

config <- read_yaml(config_path)

# ============================================================
# 4. Resolve spec directory
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
  "nuisance.R",
  "optimizer.R",
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
# 6. Create experiment directory structure
# ============================================================
exp <- config$experiment
if (is.null(exp$id)) {
  stop("experiment$id must be defined in config", call. = FALSE)
}

exp_id <- exp$id

exp_dir <- path(root, "experiments", exp_id)

dir_create(exp_dir)
dir_create(path(exp_dir, "model"))
dir_create(path(exp_dir, "simulations"))

# ============================================================
# 7. Freeze config
# ============================================================
write_yaml(config, path(exp_dir, "experiment.yml"))

# ============================================================
# 8. Source model specs into isolated environment
# ============================================================
spec_env <- load_spec_env(spec_dir)

source(path(spec_dir, "parameter.R"), local = spec_env)
source(path(spec_dir, "likelihood.R"), local = spec_env)
source(path(spec_dir, "estimand.R"), local = spec_env)
source(path(spec_dir, "nuisance.R"), local = spec_env)
source(path(spec_dir, "optimizer.R"), local = spec_env)
source(path(spec_dir, "execution.R"), local = spec_env)

# ============================================================
# 9. Validate required factories
# ============================================================
required_fns <- c(
  "make_parameter",
  "make_likelihood",
  "make_estimand",
  "make_nuisance",
  "make_optimizer",
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
# 10. Build spec objects via factories
# ============================================================
parameter <- spec_env$make_parameter(config)
likelihood <- spec_env$make_likelihood(config)
estimand <- spec_env$make_estimand(config)
nuisance <- spec_env$make_nuisance(config)
optimizer <- spec_env$make_optimizer(config)
execution <- spec_env$make_execution(config)

# ============================================================
# 11. Assemble model_spec (NO DATA)
# ============================================================
model <- model_spec(
  name = sprintf(
    "%s — %s / %s",
    exp$distribution,
    exp$model,
    exp$estimand
  )
) |>
  add(parameter) |>
  add(likelihood) |>
  add(estimand) |>
  add(nuisance) |>
  add(optimizer) |>
  add(execution)

# ============================================================
# 12. Save model ready for calibration
# ============================================================
saveRDS(model, path(exp_dir, "model", "model_ready.rds"))

message("✅ Experiment setup complete: ", exp_id)
