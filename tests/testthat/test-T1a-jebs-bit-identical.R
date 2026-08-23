# nolint start: object_usage_linter
test_that("T1a Engine A1 Mixture matches JEBS appendix golden fixtures", {
  for (seed in c(42L, 1L, 2024L, 12345L)) {
    actual <- t_invariant_jebs_plain_frame(sim_multisite(preset_jebs_strict(), seed = seed))
    ref <- readRDS(t_invariant_jebs_seed_file(seed))

    # canonical_hash is the portable authority and it holds on every CI cell.
    # Raw column identity does not: the reference .rds was serialized on one
    # platform, and raw doubles differ there by one or two ULP from any other.
    # Asserting it made this test claim the opposite of the contract it exists
    # to protect, and it passed only on the machine that built the fixture
    # (D-060). Integer columns are still compared exactly.
    expect_identical(
      canonical_hash(actual),
      canonical_hash(ref),
      info = sprintf("T1a canonical payload, seed = %d", seed)
    )
    expect_identical(names(actual), names(ref), info = sprintf("seed = %d", seed))
    expect_identical(actual$site_index, ref$site_index, info = sprintf("seed = %d", seed))
    expect_identical(actual$n_j, ref$n_j, info = sprintf("seed = %d", seed))
    expect_equal(actual, ref, tolerance = 1e-12,
                 info = sprintf("T1a values, seed = %d", seed))
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
  expect_identical(canonical_hash(actual), canonical_hash(ref))
  expect_identical(names(actual), names(ref))
  expect_identical(actual$n_j, ref$n_j)
  expect_equal(actual, ref, tolerance = 1e-12)
})
# nolint end
