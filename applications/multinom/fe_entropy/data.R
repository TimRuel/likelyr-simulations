generate_data <- function(config, parameter) {
  data(dune, package = "vegan")
  data(dune.env, package = "vegan")

  counts <- as.matrix(dune)

  env <- dune.env
  env$Moisture <- factor(env$Moisture, ordered = FALSE)
  env$Use <- factor(env$Use, ordered = FALSE)
  env$Manure <- factor(env$Manure, ordered = FALSE)

  X <- model.matrix(
    ~ A1 + Moisture + Management + Use + Manure,
    data = env
  )

  list(
    counts = counts,
    X = X
  )
}
