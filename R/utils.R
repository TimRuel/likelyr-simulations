# ============================================================
# utils.R — Utility functions for running simulations
# ============================================================

#' Load all spec factory functions into an isolated environment.
#' Used by build_model_spec.R to construct the model_spec object.
#'
#' Each make_* function is given an environment that can see:
#'   1. All other definitions in the spec files (helpers, constants)
#'   2. likelyr exports (via parent)
#'   3. The global environment (via likelyr's parent)
#' This ensures spec-local helpers like generate_eta_0() are found
#' without polluting the global namespace.
load_spec_env <- function(dir) {
  env <- new.env(parent = globalenv())
  for (f in c(
    "parameter.R",
    "likelihood.R",
    "estimand.R",
    "sampler.R",
    "traversal.R",
    "solver.R",
    "execution.R"
  )) {
    source(file.path(dir, f), local = env)
  }

  # Build a search environment: spec contents + likelyr as parent
  spec_search_env <- new.env(parent = asNamespace("likelyr"))
  for (nm in ls(env)) {
    spec_search_env[[nm]] <- env[[nm]]
  }

  # Point each make_* function at this search environment
  for (nm in ls(env, pattern = "^make_")) {
    environment(env[[nm]]) <- spec_search_env
  }

  env
}

#' Load only data.R into an isolated environment.
#' Used by run_iter.R — lighter than load_spec_env since only
#' generate_data() is needed at iteration time.
load_data_env <- function(dir) {
  data_file <- file.path(dir, "data.R")

  if (!file.exists(data_file)) {
    stop("data.R not found in spec directory: ", data_file, call. = FALSE)
  }

  env <- new.env(parent = globalenv())
  source(data_file, local = env)

  if (!exists("generate_data", envir = env, inherits = FALSE)) {
    stop("data.R must define generate_data(config, parameter).", call. = FALSE)
  }

  env
}
