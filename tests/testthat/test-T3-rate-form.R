# nolint start: object_usage_linter
test_that("T3 final wrapper preserves se2_j times n_j rate form across CV grid", {
  for (cv in c(0, 0.25, 0.50, 0.75)) {
    df <- t_invariant_sim_default(seed = 820300L + as.integer(cv * 100), cv = cv)
    products <- df$se2_j * df$n_j

    expect_lt(stats::var(products), tol_deterministic)
    expect_equal(products, rep(compute_kappa(), length(products)), tolerance = tol_deterministic)
  }
})
# nolint end
