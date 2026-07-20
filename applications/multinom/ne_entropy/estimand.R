# ======================================================================
# Estimand Specification — Shannon Entropy
#   ψ(η) = H(θ(η)) = −Σ p_j log p_j
#
# J is now fixed at 30 for every site (see ne_data_dune.R / ne_parameter.R),
# so psi_upper = log(30) is constant and provides a large buffer above
# psi_mle for every site, rather than psi_upper = log(J_obs) collapsing
# toward psi_mle for species-poor or near-uniform sites.
# ======================================================================

psi_fn <- function(param, data = NULL) {
  p <- softmax_from_eta(param)
  -sum(p * log(p))
}

# ∂H/∂η_k = −p_k (log p_k + H)
psi_jac <- function(param, data = NULL) {
  p <- softmax_from_eta(param)
  H <- -sum(p * log(p))
  -p[-length(p)] * (log(p[-length(p)]) + H)
}

make_estimand <- function(config, parameter = NULL, ...) {
  cfg <- config$estimand

  if (is.null(cfg)) {
    stop("Config must contain an 'estimand' section.", call. = FALSE)
  }

  if (is.null(cfg$psi_lower)) {
    stop("Missing estimand tuning parameter: psi_lower.", call. = FALSE)
  }

  likelyr::estimand_spec(
    psi_fn = psi_fn,
    psi_jac = psi_jac,
    psi_lower = cfg$psi_lower,
    psi_upper = log(parameter$J),
    psi_0 = log(parameter$J) / 2,
    name = "Shannon entropy"
  )
}