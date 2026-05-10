# nolint start: object_usage_linter
legacy_a1_engine <- function(...) {
  multisitedgp_internal("engine_legacy_jebs_censor_round")(...)
}

reference_a1_rgamma_stream_n_j <- function(J, nj_mean, cv, nj_min) {
  if (identical(cv, 0) || isTRUE(all.equal(cv, 0))) {
    return(rep(as.integer(nj_mean), J))
  }
  shape <- 1 / cv^2
  rate <- 1 / (cv^2 * nj_mean)
  as.integer(round(pmax(nj_min, stats::rgamma(n = J, shape = shape, rate = rate)), 0))
}

consume_jebs_strict_mixture_stream <- function() {
  gen_effects_mixture(
    J = 100L,
    tau = 0,
    sigma_tau = 0.15,
    delta = 5,
    eps = 0.3,
    ups = 2
  )
  invisible(NULL)
}

test_that("Engine A1 matches JEBS manifest hash after strict mixture stream", {
  manifest_file <- test_path("../../tools/jebs-golden-fixtures/jebs-golden-fixture-manifest.csv")
  skip_if_not(file.exists(manifest_file), "Step 4.1 JEBS fixture manifest is not shipped in the package tarball.")
  manifest <- read.csv(manifest_file, stringsAsFactors = FALSE)

  for (seed in c(42L, 1L, 2024L, 12345L)) {
    actual <- withr::with_seed(seed, {
      consume_jebs_strict_mixture_stream()
      legacy_a1_engine(J = 100L, nj_mean = 80, cv = 0.50, nj_min = 4L)$n_j
    })

    actual_frame <- data.frame(
      n_j = as.numeric(actual),
      se2_j = compute_kappa() / actual
    )
    manifest_row <- manifest[manifest$seed == seed, ]
    expect_equal(nrow(manifest_row), 1L)
    expect_identical(canonical_hash(actual_frame), manifest_row$component_site_size_hash)
  }
})

test_that("Engine A1 performs lower-censor then round and preserves rate form inputs", {
  out <- withr::with_seed(701L, legacy_a1_engine(J = 100L, nj_mean = 80, cv = 0.50, nj_min = 4L))
  se2_j <- compute_kappa() / out$n_j

  expect_s3_class(out, "tbl_df")
  expect_identical(names(out), "n_j")
  expect_true(is.integer(out$n_j))
  expect_true(all(out$n_j >= 4L))
  expect_equal(se2_j * out$n_j, rep(compute_kappa(), nrow(out)), tolerance = tol_deterministic)
})

test_that("Engine A1 cv zero path is deterministic and RNG neutral", {
  set.seed(702L)
  before <- .Random.seed
  out <- legacy_a1_engine(J = 25L, nj_mean = 50, cv = 0, nj_min = 5L)
  after <- .Random.seed

  expect_identical(out$n_j, rep(50L, 25L))
  expect_identical(before, after)
})

test_that("Engine A1 consumes exactly one rgamma stream when cv is positive", {
  engine_seed <- withr::with_seed(703L, {
    legacy_a1_engine(J = 25L, nj_mean = 50, cv = 0.5, nj_min = 5L)
    .Random.seed
  })
  reference_seed <- withr::with_seed(703L, {
    reference_a1_rgamma_stream_n_j(J = 25L, nj_mean = 50, cv = 0.5, nj_min = 5L)
    .Random.seed
  })

  expect_identical(engine_seed, reference_seed)
})

test_that("Engine A1 validates legacy site-size inputs", {
  expect_multisitedgp_error(
    legacy_a1_engine(J = 5L, nj_mean = 50, cv = 0.5, nj_min = 5L),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    legacy_a1_engine(J = 25L, nj_mean = 5, cv = 0.5, nj_min = 5L),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    legacy_a1_engine(J = 25L, nj_mean = 50, cv = -0.5, nj_min = 5L),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    legacy_a1_engine(J = 25L, nj_mean = 50.5, cv = 0, nj_min = 5L),
    "multisitedgp_arg_error"
  )
})
# nolint end
