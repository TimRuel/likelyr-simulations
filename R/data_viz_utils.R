# 1. Coverage summary (with optional blended IL/PL strategy)
#
# When point_df and J are supplied, adds a "Blended" pseudolikelihood row:
#   - iterations where psi_mle >= 1/(J-1): use Integrated covered
#   - iterations where psi_mle <  1/(J-1): use Profile covered
#
# psi_mle is proxied by psi_hat from the Profile row, which equals the
# MLE up to grid discretization.
#
# Also adds diagnostic rows "Integrated (connected)" and
# "Integrated (disconnected)" and "Profile (connected)" and
# "Profile (disconnected)" showing coverage broken out by regime,
# to verify whether the blended approach is working as expected and
# whether PL is actually recovering coverage in the connected regime.
get_coverage_summary <- function(interval_df, point_df = NULL, J = NULL) {
  standard <- interval_df |>
    filter(!is.na(covered)) |>
    group_by(level, pseudolikelihood) |>
    summarise(
      coverage = mean(covered),
      n_valid = sum(valid_ci),
      .groups = "drop"
    )

  if (is.null(point_df) || is.null(J)) {
    return(standard)
  }

  threshold <- 1 / (J - 1)

  # psi_mle proxy: Profile psi_hat equals the MLE up to grid discretization.
  # Using Profile (not Integrated) because the profile mode is anchored at
  # psi_mle by construction; the IL mode can drift.
  psi_mle_df <- point_df |>
    filter(pseudolikelihood == "Profile") |>
    select(experiment, simulation, iteration, psi_mle = psi_hat)

  interval_with_regime <- interval_df |>
    filter(!is.na(covered)) |>
    left_join(psi_mle_df, by = c("experiment", "simulation", "iteration")) |>
    mutate(regime = if_else(psi_mle >= threshold, "disconnected", "connected"))

  # Blended: IL when disconnected, PL when connected
  blended <- interval_with_regime |>
    pivot_wider(
      id_cols = c(
        experiment,
        simulation,
        iteration,
        level,
        alpha,
        psi_mle,
        regime
      ),
      names_from = pseudolikelihood,
      values_from = c(covered, valid_ci, ci_length)
    ) |>
    mutate(
      covered_blended = if_else(
        regime == "disconnected",
        covered_Integrated,
        covered_Profile
      ),
      valid_blended = if_else(
        regime == "disconnected",
        valid_ci_Integrated,
        valid_ci_Profile
      )
    ) |>
    filter(!is.na(covered_blended)) |>
    group_by(level) |>
    summarise(
      coverage = mean(covered_blended),
      n_valid = sum(valid_blended),
      .groups = "drop"
    ) |>
    mutate(pseudolikelihood = "Blended")

  # Coverage broken out by regime for each method
  by_regime <- interval_with_regime |>
    filter(!is.na(covered)) |>
    group_by(level, pseudolikelihood, regime) |>
    summarise(
      coverage = mean(covered),
      n_valid = sum(valid_ci),
      .groups = "drop"
    ) |>
    mutate(
      pseudolikelihood = paste0(pseudolikelihood, " (", regime, ")")
    ) |>
    select(-regime)

  bind_rows(standard, blended, by_regime)
}

# 2. Interval width summary
get_width_summary <- function(interval_df) {
  interval_df |>
    filter(valid_ci) |>
    group_by(level, pseudolikelihood) |>
    summarise(
      mean_width = mean(ci_length, na.rm = TRUE),
      median_width = median(ci_length, na.rm = TRUE),
      .groups = "drop"
    )
}

# 3. Point estimator performance
#
# psi_hat_below_threshold: fraction of iterations where the point estimate
# falls in the connected regime (psi_hat < 1/(J-1)). This uses psi_hat
# from each pseudolikelihood separately. To check the regime using psi_mle
# (the MLE, not the pseudolikelihood mode), filter to pseudolikelihood ==
# "Profile" before calling, since the profile mode equals psi_mle by
# construction.
get_point_summary <- function(point_df, J) {
  point_df |>
    group_by(pseudolikelihood) |>
    summarise(
      bias = mean(bias),
      rmse = sqrt(mean(sq_error)),
      sd = sd(psi_hat),
      psi_hat_below_threshold = mean(psi_hat < (1 / (J - 1))),
      .groups = "drop"
    )
}
