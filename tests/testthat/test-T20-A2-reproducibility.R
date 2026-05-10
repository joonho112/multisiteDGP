# nolint start: object_usage_linter
test_that("T20 Engine A2 wrapper is hash-stable at the same seed", {
  hashes <- vapply(seq_len(5), function(i) {
    canonical_hash(t_invariant_sim_default(seed = 20L, engine = "A2_modern"))
  }, character(1))

  expect_identical(length(unique(hashes)), 1L)
})

test_that("T20 Engine A2 active-RNG path is reproducible from the same RNG state", {
  hashes <- vapply(seq_len(5), function(i) {
    withr::with_seed(
      20L,
      canonical_hash(t_invariant_sim_default(seed = NULL, engine = "A2_modern"))
    )
  }, character(1))

  expect_identical(length(unique(hashes)), 1L)
})

test_that("T20 Engine A2 seed wrapper preserves caller RNG state", {
  set.seed(832001L)
  before <- .Random.seed
  invisible(t_invariant_sim_default(seed = 20L, engine = "A2_modern"))
  after <- .Random.seed

  expect_identical(after, before)
})
# nolint end
