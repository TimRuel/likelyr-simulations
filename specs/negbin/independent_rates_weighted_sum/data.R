generate_data <- function(config, parameter) {
  n_per_process <- parameter$extra$n_per_process
  J <- length(n_per_process)

  # --- Split parameter vector
  p0 <- parameter$param_0
  theta_0 <- p0[seq_len(J)]
  phi_0 <- p0[J + seq_len(J)]

  # --- Sizes
  total_n <- sum(n_per_process)
  process_labels <- names(n_per_process)

  # --- Exposure
  expo <- match.fun(config$exposure$distribution)
  t <- do.call(expo, c(list(n = total_n), config$exposure$args))

  # --- Expand parameters
  theta_i <- rep(theta_0, times = n_per_process)
  phi_i <- rep(phi_0, times = n_per_process)

  # --- Simulate
  Y <- rnbinom(total_n, size = phi_i, mu = theta_i * t)

  # --- Assemble
  data <- tibble::tibble(
    process = factor(
      rep(process_labels, times = n_per_process),
      levels = process_labels
    ),
    t = t,
    Y = Y
  )

  structure(
    data,
    n_per_process = n_per_process,
    weights = parameter$extra$weights,
    formula = as.formula("Y ~ 0 + process + offset(log(t))")
  )
}
