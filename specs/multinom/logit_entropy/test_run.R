suppressPackageStartupMessages({
  library(likelyr)
  library(here)
  library(fs)
  library(yaml)
})

root <- here()

source(
  file.path(root, "scripts", "utils.R"),
  local = TRUE
)

sim_config_path <- "experiments/multinom/logit_simpson/sim_01/simulation.yml"

sim_dir <- path_dir(sim_config_path)

config <- read_yaml(sim_config_path)

spec_path <- config$experiment$spec_path

spec_dir <- path(root, spec_path)

spec_env <- load_spec_env(spec_dir)

source(path(spec_dir, "parameter.R"), local = spec_env)
source(path(spec_dir, "likelihood.R"), local = spec_env)
source(path(spec_dir, "estimand.R"), local = spec_env)
source(path(spec_dir, "nuisance.R"), local = spec_env)
source(path(spec_dir, "optimizer.R"), local = spec_env)
source(path(spec_dir, "execution.R"), local = spec_env)

parameter <- spec_env$make_parameter(config)
likelihood <- spec_env$make_likelihood(config)
estimand <- spec_env$make_estimand(config)
nuisance <- spec_env$make_nuisance(config)
optimizer <- spec_env$make_optimizer(config)
execution <- spec_env$make_execution(config)

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
  add(nuisance) |>
  add(optimizer) |>
  add(execution)

data_spec_file <- path(spec_dir, "data.R")

source(data_spec_file, local = spec_env)

data <- spec_env$generate_data(
  config = config,
  parameter = model$parameter
)

model <- model |>
  calibrate(data)

probe_res <- model |>
  probe()

probe_res |> plot()

model$execution$R <- 10

model$estimand$increment <- 0.05

model <- model |>
  integrate()

model <- model |>
  profile()

model <- model |>
  infer() |>
  compare()

model$workspace$integrate |> plot()
model$workspace$profile |> plot()

model$workspace$integrate$inference |> plot()
model$workspace$profile$inference |> plot()

model$workspace$comparison |> plot()

model$workspace$comparison |> view()
