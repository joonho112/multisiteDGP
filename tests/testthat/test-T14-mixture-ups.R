# nolint start: object_usage_linter
test_that("T14a legacy raw mixture variance equals 7.15", {
  skip_if_not_slow()

  for (seed in c(42L, 1L, 2024L)) {
    raw <- .legacy_mixture_raw_draw(
      delta = 5,
      eps = 0.3,
      ups = 2,
      J = default_n_property_large,
      seed = seed
    )

    expect_equal(stats::var(raw), 7.15, tolerance = 0.05)
  }
})

test_that("T14b standardized Mixture output has unit variance", {
  skip_if_not_slow()

  for (seed in c(42L, 1L, 2024L)) {
    out <- withr::with_seed(seed, gen_effects(
      J = default_n_property_large,
      true_dist = "Mixture",
      theta_G = list(delta = 5, eps = 0.3, ups = 2)
    ))

    expect_lt(abs(mean(out$z_j)), 0.005)
    expect_lt(abs(stats::var(out$z_j) - 1), 0.01)
  }
})
# nolint end
