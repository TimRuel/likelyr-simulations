# ======================================================================
# Data (No Effects Multinomial — Dune Application)
#
# Returns the full count vector (all J = 30 species) for a single dune
# meadow site, including zero-count species. Zero-count categories are
# numerically inert in the entropy calculation (x log x -> 0), so this
# does not change psi_mle relative to filtering to observed species
# only. What it does change is psi_upper = log(J), which stays fixed at
# log(30) for every site rather than collapsing to log(J_obs) for
# species-poor or near-uniform sites. This gives the sampler and
# traversal genuine room between psi_mle and psi_upper, avoiding the
# boundary-clustering pathology seen when J was restricted to the
# observed support.
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