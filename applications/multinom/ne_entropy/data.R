# ======================================================================
# Data (No Effects Multinomial — Dune Application)
#
# Returns the count vector for a single dune meadow site, restricted
# to species with positive counts. Unobserved species are treated as
# absent rather than merely unsampled, consistent with the site-level
# analyses of Tiffeau-Mayer et al. (2024).
#
# The sampler internally augments the parameter space with a phantom
# "other" category to allow IL branches to explore entropy values above
# log(J_obs), without affecting the profile likelihood computation.
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
  observed <- counts > 0L

  data.frame(
    cell  = colnames(dune)[observed],
    count = counts[observed],
    row.names = NULL
  )
}