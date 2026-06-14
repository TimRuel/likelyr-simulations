# ======================================================================
# Data (No Effects Multinomial — Dune Application)
# ======================================================================

generate_data <- function(config, parameter) {
  data(dune, package = "vegan")
  counts <- colSums(dune)
  data.frame(
    cell = names(counts),
    count = as.integer(counts),
    row.names = NULL
  )
}
