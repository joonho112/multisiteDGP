# nolint start: object_usage_linter
test_that("T1b Engine A2 Gaussian distribution clears KS binomial gate", {
  skip_if_not_slow()

  M <- default_m_replications
  seeds <- t_invariant_seed_stream(M, 820102L)
  args <- t_invariant_default_args()
  tau_values <- vector("list", M)
  ks_pvalues <- numeric(M)
  for (i in seq_along(seeds)) {
    df <- t_invariant_sim_default(seed = seeds[[i]])
    tau_values[[i]] <- df$tau_j
    ks_pvalues[[i]] <- suppressWarnings(
      stats::ks.test(df$tau_j, "pnorm", mean = args$tau, sd = args$sigma_tau)$p.value
    )
  }

  count_high <- sum(ks_pvalues > 0.05)
  expect_gte(count_high, stats::qbinom(0.005, M, 0.95))
  expect_lte(count_high, stats::qbinom(0.995, M, 0.95))

  tau_all <- unlist(tau_values, use.names = FALSE)
  mean_tolerance <- 4 * args$sigma_tau / sqrt(length(tau_all))
  var_tolerance <- 4 * args$sigma_tau^2 * sqrt(2 / (length(tau_all) - 1))
  expect_lt(abs(mean(tau_all) - args$tau), mean_tolerance)
  expect_lt(abs(stats::var(tau_all) - args$sigma_tau^2), var_tolerance)
})
# nolint end
