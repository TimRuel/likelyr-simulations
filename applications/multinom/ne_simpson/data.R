# ======================================================================
# Data (No Effects Multinomial — Dune Application)
#
# Returns the full count vector (all J = 30 species) for a single dune
# meadow site, including zero-count species (2026-08-03 — previously
# filtered to observed-only; see parameter.R for why this changed to
# match the entropy application's fixed-J=30 approach). Zero-count
# categories are numerically inert for Simpson's index (they contribute
# 0 to sum(p_j^2)), so this does not change psi_mle relative to
# filtering to observed species only. What it does change is
# psi_upper/psi_lower staying fixed (1.0 / 1/30) for every site rather
# than psi_lower = 1/J_obs drifting upward for species-poor sites.
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

  data("dune", package = "vegan", envir = environment())

  counts <- as.integer(dune[row_index, ])

  data.frame(
    cell  = colnames(dune),
    count = counts,
    row.names = NULL
  )
}