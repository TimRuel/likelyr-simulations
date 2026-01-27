# ============================================================
# util.s — Utility functions for running simulations
# ============================================================

load_spec_env <- function(dir) {
  env <- new.env(parent = globalenv())
  for (f in c(
    "parameter.R",
    "likelihood.R",
    "estimand.R",
    "nuisance.R",
    "optimizer.R",
    "execution.R"
  )) {
    source(file.path(dir, f), local = env)
  }
  for (nm in ls(env, pattern = "^make_")) {
    environment(env[[nm]]) <- asNamespace("likelyr")
  }
  env
}
