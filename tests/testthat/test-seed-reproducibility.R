# What a seed guarantees: the same data on every call, whatever random number
# generator the caller has set, and an untouched generator in the caller's
# workspace afterwards.

local_rng_state <- function(env = parent.frame()) {
  kind <- RNGkind()
  has_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  seed <- if (has_seed) get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) else NULL
  withr::defer(
    {
      do.call(RNGkind, as.list(kind))
      if (has_seed) {
        assign(".Random.seed", seed, envir = .GlobalEnv)
      } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    },
    envir = env
  )
}

test_that("the same seed gives the same data", {
  runs <- lapply(1:3, function(i) as.data.frame(sim_multisite(J = 25L, seed = 6501L)))
  expect_identical(runs[[2]], runs[[1]])
  expect_identical(runs[[3]], runs[[1]])

  runs <- lapply(1:3, function(i) as.data.frame(sim_meta(J = 25L, I = 0.4, R = 3, seed = 6502L)))
  expect_identical(runs[[2]], runs[[1]])
  expect_identical(runs[[3]], runs[[1]])

  different <- as.data.frame(sim_multisite(J = 25L, seed = 6503L))
  expect_false(identical(different$tau_j, runs[[1]]$tau_j))
})

test_that("a seed stored in the design is used, and the seed argument overrides it", {
  design <- multisitedgp_design(J = 25L, seed = 11L)
  from_design <- sim_multisite(design)
  from_argument <- sim_multisite(multisitedgp_design(J = 25L), seed = 11L)
  overridden <- sim_multisite(design, seed = 12L)

  expect_identical(as.data.frame(from_design), as.data.frame(from_argument))
  expect_identical(attr(from_design, "design")$seed, 11L)
  expect_identical(attr(overridden, "design")$seed, 12L)
  expect_false(identical(overridden$tau_j, from_design$tau_j))
})

test_that("a seeded call leaves the caller's generator untouched", {
  local_rng_state()

  set.seed(6504L)
  kind <- RNGkind()
  before <- .Random.seed
  invisible(sim_multisite(J = 25L, seed = 6505L))
  invisible(sim_meta(J = 25L, I = 0.4, R = 3, seed = 6506L))

  expect_identical(RNGkind(), kind)
  expect_identical(.Random.seed, before)
})

test_that("a seeded call gives the same data under any generator kind", {
  local_rng_state()

  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(7103L)
  reference <- as.data.frame(sim_multisite(J = 20L, seed = 7102L))

  for (kind in c("L'Ecuyer-CMRG", "Wichmann-Hill")) {
    RNGkind(kind, "Inversion", "Rejection")
    set.seed(7101L)
    caller_kind <- RNGkind()
    caller_state <- .Random.seed
    out <- as.data.frame(sim_multisite(J = 20L, seed = 7102L))

    expect_identical(out, reference, info = kind)
    expect_identical(RNGkind(), caller_kind, info = kind)
    expect_identical(.Random.seed, caller_state, info = kind)
  }
})

test_that("a seeded call does not create a seed where none existed", {
  local_rng_state()

  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    rm(".Random.seed", envir = .GlobalEnv)
  }
  kind <- RNGkind()
  invisible(sim_meta(J = 10L, I = 0.4, R = 2, seed = 7104L))

  expect_identical(RNGkind(), kind)
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})

test_that("without a seed the call draws from the caller's stream", {
  local_rng_state()

  set.seed(20L)
  before <- .Random.seed
  first <- as.data.frame(sim_multisite(J = 25L))
  second <- as.data.frame(sim_multisite(J = 25L))

  expect_false(identical(.Random.seed, before))
  expect_false(identical(second$tau_j, first$tau_j))
  expect_null(attr(sim_multisite(J = 25L), "design")$seed)

  set.seed(20L)
  again <- as.data.frame(sim_multisite(J = 25L))
  expect_identical(again, first)
})

test_that("a seed must be a non-negative integer", {
  expect_multisitedgp_error(
    multisitedgp_design(seed = -1L)
  )
})

test_that("the seeds given to grid cells are fixed by seed_root and leave the caller's generator untouched", {
  local_seed_stream <- multisitedgp_internal(".local_seed_stream")

  set.seed(99L)
  before <- .Random.seed
  seeds_one <- local_seed_stream(5L, 123L)
  after <- .Random.seed
  seeds_two <- local_seed_stream(5L, 123L)

  expect_identical(before, after)
  expect_identical(seeds_one, seeds_two)
  expect_type(seeds_one, "integer")
  expect_length(seeds_one, 5L)

  expect_multisitedgp_error(
    local_seed_stream(0L, 123L)
  )
  expect_multisitedgp_error(
    local_seed_stream(2L, NULL)
  )
})
