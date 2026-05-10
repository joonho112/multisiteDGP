# nolint start: object_usage_linter
test_that("T16 Skew-Normal standardization and skewness sign hold", {
  skip_if_not_slow()
  testthat::skip_if_not_installed("sn")
  testthat::skip_if_not_installed("moments")

  slants <- c(-4, -1, 0, 1, 4)
  for (slant in slants) {
    z_j <- withr::with_seed(831600L + match(slant, slants), suppressWarnings(
      gen_effects_skewn(J = default_n_property_large, slant = slant)
    )$z_j)
    skew <- moments::skewness(z_j)

    expect_lt(abs(mean(z_j)), 0.005)
    expect_lt(abs(stats::var(z_j) - 1), 0.005)
    if (identical(slant, 0)) {
      expect_lt(abs(skew), 0.01)
    } else {
      expect_identical(sign(skew), sign(slant))
    }
  }
})
# nolint end
