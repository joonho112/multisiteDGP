# nolint start: object_usage_linter
test_that("T2 default kappa equals four in the final wrapper output", {
  df <- t_invariant_sim_default(seed = 820201L)

  expect_equal(compute_kappa(), 4, tolerance = tol_deterministic)
  expect_equal(df$se2_j * df$n_j, rep(4, nrow(df)), tolerance = tol_deterministic)
})
# nolint end
