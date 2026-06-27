# ======================================================================
# Data (No Effects Multinomial — Dune Application)
# ======================================================================

generate_data <- function(config, parameter) {
  sim_id <- config$simulation$sim_id

  if (is.null(sim_id) || !nzchar(sim_id)) {
    stop("simulation$sim_id must be defined in the sim yaml.", call. = FALSE)
  }

  row_index <- as.integer(sub("sim_", "", sim_id))

  if (is.na(row_index) || row_index < 1L || row_index > 20L) {
    stop(
      sprintf("Could not parse valid row index from sim_id '%s'.", sim_id),
      call. = FALSE
    )
  }

  dune <- vegan::dune

  counts <- as.integer(dune[row_index, ])

  data.frame(
    cell  = colnames(dune),
    count = counts,
    row.names = NULL
  )
}