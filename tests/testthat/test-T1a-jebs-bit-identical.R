# nolint start: object_usage_linter
test_that("T1a Engine A1 Mixture matches JEBS appendix golden fixtures", {
  for (seed in c(42L, 1L, 2024L, 12345L)) {
    actual <- t_invariant_jebs_plain_frame(sim_multisite(preset_jebs_strict(), seed = seed))
    ref <- readRDS(t_invariant_jebs_seed_file(seed))

    for (column in names(ref)) {
      expect_identical(
        actual[[column]],
        ref[[column]],
        info = sprintf("T1a raw column %s, seed = %d", column, seed)
      )
    }
    expect_identical(actual, ref, info = sprintf("T1a raw frame, seed = %d", seed))
    expect_identical(
      canonical_hash(actual),
      canonical_hash(ref),
      info = sprintf("T1a canonical payload, seed = %d", seed)
    )
  }
})

test_that("T1a floor-active authority enforces the paper lower limit n_j = 5", {
  actual <- t_invariant_jebs_plain_frame(sim_multisite(
    preset_jebs_strict(J = 300L, nj_mean = 10, cv = 0.75),
    seed = 42L
  ))
  ref <- readRDS(t_invariant_jebs_floor_file())

  expect_identical(min(actual$n_j), 5L)
  expect_gt(sum(actual$n_j == 5L), 0L)
  for (column in names(ref)) {
    expect_identical(
      actual[[column]],
      ref[[column]],
      info = sprintf("T1a floor-active raw column %s", column)
    )
  }
  expect_identical(actual, ref)
  expect_identical(canonical_hash(actual), canonical_hash(ref))
})
# nolint end
