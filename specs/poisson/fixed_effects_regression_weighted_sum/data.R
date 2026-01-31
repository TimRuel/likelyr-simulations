recover_original_covariates <- function(
  design_matrix,
  intercept_prefix = "α",
  homo_cov_prefix = "γ",
  hetero_cov_prefix = "ζ"
) {
  # design_matrix: data.frame or matrix
  # intercept_prefix: prefix for the one-hot process indicator columns
  # homo_cov_prefix: vector of names for covariates that are common across processes
  # hetero_cov_prefix: prefix for the process-specific covariate columns

  coefs <- colnames(design_matrix)

  homo_cov_coefs <- coefs[grepl(homo_cov_prefix, coefs)]

  num_homo_covs <- length(homo_cov_coefs)

  homo_covs <- design_matrix[, homo_cov_coefs, drop = FALSE]

  colnames(homo_covs) <- paste0("X", 1:num_homo_covs)

  J <- design_matrix |>
    rownames() |>
    unique() |>
    length()

  num_hetero_covs <- sum(grepl(hetero_cov_prefix, coefs)) / J

  hetero_cov_coefs <- coefs[grepl(hetero_cov_prefix, coefs)]

  hetero_cov_dummy_mat <- design_matrix[, hetero_cov_coefs, drop = FALSE]

  hetero_covs <- matrix(
    NA,
    nrow = nrow(hetero_cov_dummy_mat),
    ncol = num_hetero_covs
  )

  for (i in 1:num_hetero_covs) {
    hetero_covs[, i] <- rowSums(hetero_cov_dummy_mat[,
      i:(i + J - 1),
      drop = FALSE
    ])
  }

  colnames(hetero_covs) <- paste0(
    "X",
    (num_homo_covs + 1):(num_homo_covs + num_hetero_covs)
  )

  # 4. Combine into a single data.frame
  recovered <- cbind(homo_covs, hetero_covs) |>
    as.data.frame(row.names = 1:nrow(design_matrix))

  return(recovered)
}

sample_from_config <- function(dist_config, n = 1) {
  dist_fun <- match.fun(dist_config$name)
  args <- dist_config$args
  if (is.null(args)) {
    args <- list()
  }

  # Combine n with args; if args is a vector, convert to list
  if (is.vector(args) && !is.list(args)) {
    args <- as.list(args)
  }
  out <- do.call(dist_fun, c(list(n = n), args))

  return(out)
}

# Generate Poisson outcomes
generate_data <- function(config, parameter) {
  param_cfg <- config$parameter
  n_per_process <- param_cfg$processes$n_per_process
  process_labels <- param_cfg$processes$labels
  J <- length(n_per_process)
  total_n <- sum(n_per_process)
  process_id <- rep(process_labels, times = n_per_process)

  data_cfg <- config$data
  exposure_dist <- match.fun(data_cfg$exposure$distribution)
  exposure_args <- data_cfg$exposure$args
  t <- do.call(exposure_dist, c(list(n = total_n), exposure_args))

  beta_0 <- parameter$param_0

  names(n_per_process) <- process_labels
  X <- get_X(config, beta_0, n_per_process)
  eta <- X %*% beta_0
  mu <- exp(eta)
  Y <- rpois(total_n, t * mu)

  covariates <- recover_original_covariates(X)

  # Combine into final data frame
  data <- tibble::tibble(
    process = factor(process_id, levels = process_labels),
    t = t
  ) |>
    dplyr::bind_cols(covariates) |>
    tibble::add_column(Y = Y)

  attr(data, "X") <- X
  attr(data, "weights") <- parameter$extra$weights
  attr(data, "formula") <- make_formula(config)

  return(data)
}
