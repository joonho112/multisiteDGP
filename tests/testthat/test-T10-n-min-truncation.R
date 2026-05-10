# nolint start: object_usage_linter
test_that("T10 nj_min truncation and multi-rep mean hold in wrapper output", {
  skip_if_not_slow()

  M <- default_m_replications
  seeds <- t_invariant_seed_stream(M, 821001L)
  mean_estimates <- vapply(seeds, function(seed) {
    df <- t_invariant_sim_default(seed = seed, nj_mean = 50, nj_min = 5L)
    expect_true(all(df$n_j >= 5L))
    mean(df$n_j)
  }, numeric(1))

  expect_lt(abs(mean(mean_estimates) - 50) / 50, 0.005)
})
# nolint end
