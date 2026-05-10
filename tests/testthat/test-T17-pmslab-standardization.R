# nolint start: object_usage_linter
test_that("T17 PMSlab standardization and point-mass fraction hold", {
  skip_if_not_slow()

  for (pi0_value in c(0.2, 0.5, 0.8)) {
    for (slab_shape in c("Gaussian", "Laplace")) {
      z_j <- withr::with_seed(
        831700L + round(pi0_value * 100) + nchar(slab_shape),
        gen_effects_pmslab(
          J = default_n_property_large,
          pi0 = pi0_value,
          slab_shape = slab_shape
        )$z_j
      )
      slab_values <- z_j[abs(z_j) >= 1e-12]

      expect_lt(abs(mean(z_j)), 0.005)
      expect_lt(abs(stats::var(z_j) - 1), 0.01)
      expect_lt(abs(mean(abs(z_j) < 1e-12) - pi0_value), 0.002)
      expect_lt(abs(mean(slab_values)), 0.02)
    }
  }
})
# nolint end
