# nolint start: object_usage_linter
test_that("T1a Engine A1 Mixture matches JEBS appendix golden fixtures", {
  # No platform gate. The v0.2.0 contract is that the same design and seed give
  # the same canonical_hash() everywhere, so this has to run everywhere or the
  # contract is untested on four of the five CI cells. Verified locally on
  # aarch64-apple-darwin23 against fixtures generated elsewhere.
  for (seed in c(42L, 1L, 2024L, 12345L)) {
    actual <- t_invariant_jebs_plain_frame(sim_multisite(preset_jebs_strict(), seed = seed))
    ref <- readRDS(t_invariant_jebs_seed_file(seed))

    expect_identical(
      canonical_hash(actual),
      canonical_hash(ref),
      info = sprintf("T1a seed = %d", seed)
    )
  }
})
# nolint end
