# nolint start: object_usage_linter
test_that("T9 default true_dist is Gaussian under Shapiro binomial gate", {
  skip_if_not_slow()

  M <- default_m_replications
  seeds <- t_invariant_seed_stream(M, 820901L)
  shapiro_pvalues <- vapply(seeds, function(seed) {
    df <- t_invariant_sim_default(seed = seed)
    stats::shapiro.test(df$tau_j[seq_len(min(50L, nrow(df)))])$p.value
  }, numeric(1))
  count_low <- sum(shapiro_pvalues <= 0.01)

  expect_gte(count_low, shapiro_p_leq_001_count_min)
  expect_lte(count_low, shapiro_p_leq_001_count_max)
})
# nolint end
