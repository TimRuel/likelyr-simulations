library(dplyr)

get_coverage_summary <- function(interval_df) {
  interval_df |>
    filter(!is.na(covered)) |>
    group_by(level, pseudolikelihood) |>
    summarise(
      coverage = mean(covered),
      n_valid = sum(valid_ci),
      .groups = "drop"
    )
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
get_point_summary <- function(point_df) {
  point_df |>
    group_by(pseudolikelihood) |>
    summarise(
      bias = mean(bias),
      rmse = sqrt(mean(sq_error)),
      sd = sd(psi_hat),
      .groups = "drop"
    )
}
