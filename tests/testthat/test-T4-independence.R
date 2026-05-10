# nolint start: object_usage_linter
test_that("T4 dependence none empirical Spearman stays within permutation envelope", {
  skip_if_not_slow()

  J <- 200L
  M <- default_m_replications
  seeds <- t_invariant_seed_stream(M, 820401L)
  actual <- vapply(seeds, function(seed) {
    df <- t_invariant_sim_default(seed = seed, J = J, dependence = "none")
    stats::cor(df$tau_j, df$se2_j, method = "spearman")
  }, numeric(1))
  envelope <- withr::with_seed(1820401L, replicate(M, {
    tau_j <- stats::rnorm(J)
    se2_j <- stats::rgamma(J, shape = 1)
    stats::cor(tau_j, sample(se2_j), method = "spearman")
  }))

  q95_actual <- unname(stats::quantile(abs(actual), 0.95))
  q95_envelope <- unname(stats::quantile(abs(envelope), 0.95))
  expect_lte(q95_actual, q95_envelope * permutation_envelope_x)
})
# nolint end
