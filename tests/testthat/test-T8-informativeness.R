# nolint start: object_usage_linter
test_that("T8 informativeness uses geometric mean of se2_j, not arithmetic mean", {
  se2_j <- c(0.01, 0.05, 0.1, 0.5, 1.0)
  sigma_tau <- 0.5
  expected <- sigma_tau^2 / (sigma_tau^2 + exp(mean(log(se2_j))))
  arithmetic <- sigma_tau^2 / (sigma_tau^2 + mean(se2_j))

  actual <- compute_I(se2_j, sigma_tau)

  expect_equal(actual, expected, tolerance = tol_deterministic)
  expect_false(isTRUE(all.equal(actual, arithmetic, tolerance = 1e-3)))
})
# nolint end
