# nolint start: object_usage_linter
test_that("T13 ALD standardization matches Yu-Zhang moment contract", {
  skip_if_not_slow()
  testthat::skip_if_not_installed("LaplacesDemon")

  for (rho in c(0.25, 0.5, 0.7)) {
    z_j <- withr::with_seed(220L + round(100 * rho), gen_effects_ald(
      J = default_n_property_large,
      rho = rho
    )$z_j)

    expect_lt(abs(mean(z_j)), 0.005)
    expect_lt(abs(stats::var(z_j) - 1), 0.005)
  }
})
# nolint end
