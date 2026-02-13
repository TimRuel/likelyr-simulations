# =============================================================================
# 1. Package Setup
# =============================================================================

suppressPackageStartupMessages({
  library(likelyr)
  library(here)
  library(fs)
  library(future)
  library(yaml)
})

root <- here()

# =============================================================================
# 2. Load Utilities
# =============================================================================

source(
  file.path(root, "scripts", "utils.R"),
  local = TRUE
)

# =============================================================================
# 3. Load Configuration
# =============================================================================

config_path <- "config/multinom/exp_multinom_lr_entropy_test.yml"
config <- read_yaml(config_path)

spec_path <- config$experiment$spec_path
spec_dir <- path(root, spec_path)

# =============================================================================
# 4. Load Specification Environment
# =============================================================================

spec_env <- load_spec_env(spec_dir)

# --- Load all spec components into isolated environment ---------------------

source(path(spec_dir, "parameter.R"), local = spec_env)
source(path(spec_dir, "likelihood.R"), local = spec_env)
source(path(spec_dir, "estimand.R"), local = spec_env)
source(path(spec_dir, "nuisance.R"), local = spec_env)
source(path(spec_dir, "optimizer.R"), local = spec_env)
source(path(spec_dir, "execution.R"), local = spec_env)
source(path(spec_dir, "data.R"), local = spec_env)

# --- Optional sanity check ---------------------------------------------------

required_fns <- c(
  "make_parameter",
  "make_likelihood",
  "make_estimand",
  "make_nuisance",
  "make_optimizer",
  "make_execution",
  "generate_data"
)

missing_fns <- required_fns[
  !vapply(required_fns, exists, logical(1), envir = spec_env)
]

if (length(missing_fns) > 0) {
  stop("Missing required spec functions: ", paste(missing_fns, collapse = ", "))
}

# =============================================================================
# 5. Instantiate Model Components
# =============================================================================

parameter <- spec_env$make_parameter(config)
likelihood <- spec_env$make_likelihood(config)
estimand <- spec_env$make_estimand(config)
nuisance <- spec_env$make_nuisance(config)
optimizer <- spec_env$make_optimizer(config)
execution <- spec_env$make_execution(config)

# =============================================================================
# 6. Build Model Specification
# =============================================================================

model <- model_spec() |>
  add(parameter) |>
  add(likelihood) |>
  add(estimand) |>
  add(nuisance) |>
  add(optimizer) |>
  add(execution)

# =============================================================================
# 7. Generate Data
# =============================================================================

data <- spec_env$generate_data(
  config = config,
  parameter = model$parameter
)

# =============================================================================
# 8. Calibrate Model
# =============================================================================

model <- model |>
  calibrate(data)

# =============================================================================
# 9. Likelihood Construction
# =============================================================================

model <- model |>
  integrate()

model <- model |>
  profile()

# =============================================================================
# 10. Inference & Comparison
# =============================================================================

model_ic <- model |>
  infer() |>
  compare()

model_ic$workspace$comparison |> view()

# =============================================================================
# End of Script
# =============================================================================
