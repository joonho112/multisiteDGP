# nolint start: object_usage_linter
site_size_upstream <- function(J = 25L) {
  tibble::tibble(
    site_index = seq_len(J),
    z_j = rep(0, J),
    tau_j = rep(0, J)
  )
}

reference_a1_public_n_j <- function(J, nj_mean, cv, nj_min) {
  if (identical(cv, 0) || isTRUE(all.equal(cv, 0))) {
    return(rep(as.integer(nj_mean), J))
  }
  shape <- 1 / cv^2
  rate <- 1 / (cv^2 * nj_mean)
  as.integer(round(pmax(nj_min, stats::rgamma(n = J, shape = shape, rate = rate)), 0))
}

test_that("gen_site_sizes is exported and appends the L2 schema", {
  expect_true("gen_site_sizes" %in% getNamespaceExports("multisiteDGP"))

  upstream <- site_size_upstream()
  out <- withr::with_seed(
    801L,
    gen_site_sizes(upstream, J = 25L, nj_mean = 50, cv = 0.5, nj_min = 5L)
  )

  expect_s3_class(out, "tbl_df")
  expect_named(out, c("site_index", "z_j", "tau_j", "n_j", "se2_j", "se_j"))
  expect_true(is.integer(out$n_j))
  expect_true(is.numeric(out$se2_j))
  expect_true(is.numeric(out$se_j))
  expect_true(all(out$n_j >= 5L))
  expect_equal(out$se_j, sqrt(out$se2_j), tolerance = tol_deterministic)
  expect_identical(attr(out, "engine", exact = TRUE), "A2_modern")
  expect_identical(attr(out, "kappa", exact = TRUE), compute_kappa())
})

test_that("gen_site_sizes preserves kappa rate form for both engines", {
  upstream <- site_size_upstream()

  a2 <- withr::with_seed(
    802L,
    gen_site_sizes(upstream, J = 25L, nj_mean = 50, cv = 0.5, nj_min = 5L, engine = "A2_modern")
  )
  a1 <- withr::with_seed(
    802L,
    gen_site_sizes(upstream, J = 25L, nj_mean = 50, cv = 0.5, nj_min = 5L, engine = "A1_legacy")
  )

  expect_equal(a2$se2_j * a2$n_j, rep(compute_kappa(), 25L), tolerance = tol_deterministic)
  expect_equal(a1$se2_j * a1$n_j, rep(compute_kappa(), 25L), tolerance = tol_deterministic)
  expect_true(all(a2$n_j >= 5L))
  expect_true(all(a1$n_j >= 5L))
})

test_that("gen_site_sizes A1 path preserves legacy JEBS site-size stream", {
  upstream <- site_size_upstream()

  actual <- withr::with_seed(
    803L,
    gen_site_sizes(
      upstream,
      J = 25L,
      nj_mean = 50,
      cv = 0.5,
      nj_min = 5L,
      engine = "A1_legacy"
    )$n_j
  )
  expected <- withr::with_seed(803L, reference_a1_public_n_j(25L, 50, 0.5, 5L))

  expect_identical(actual, expected)
})

test_that("gen_site_sizes computes kappa from p, R2, and outcome variance", {
  upstream <- site_size_upstream()
  out <- withr::with_seed(
    804L,
    gen_site_sizes(
      upstream,
      J = 25L,
      nj_mean = 50,
      cv = 0.5,
      nj_min = 5L,
      p = 0.25,
      R2 = 0.2,
      var_outcome = 2,
      engine = "A1_legacy"
    )
  )
  kappa <- compute_kappa(p = 0.25, R2 = 0.2, var_outcome = 2)

  expect_identical(attr(out, "kappa", exact = TRUE), kappa)
  expect_equal(out$se2_j * out$n_j, rep(kappa, 25L), tolerance = tol_deterministic)
})

test_that("gen_site_sizes A2 path is reproducible under fixed seed", {
  upstream <- site_size_upstream()

  out_1 <- withr::with_seed(
    805L,
    gen_site_sizes(upstream, J = 25L, nj_mean = 50, cv = 0.5, nj_min = 5L, engine = "A2_modern")
  )
  out_2 <- withr::with_seed(
    805L,
    gen_site_sizes(upstream, J = 25L, nj_mean = 50, cv = 0.5, nj_min = 5L, engine = "A2_modern")
  )

  expect_identical(out_1, out_2)
  expect_identical(canonical_hash(out_1), canonical_hash(out_2))
})

test_that("gen_site_sizes A2 stochastic integerization preserves mean in Monte Carlo smoke", {
  upstream <- site_size_upstream(100000L)

  out <- withr::with_seed(
    806L,
    suppressWarnings(gen_site_sizes(
      upstream,
      J = 100000L,
      nj_mean = 50,
      cv = 0.5,
      nj_min = 5L,
      engine = "A2_modern"
    ))
  )

  expect_lt(abs(mean(out$n_j) - 50) / 50, tol_mc_moment_n1e5)
  expect_true(all(out$n_j >= 5L))
  expect_true(is.integer(out$n_j))
})

test_that("gen_site_sizes CV zero path is deterministic and RNG neutral", {
  upstream <- site_size_upstream()

  set.seed(807L)
  before <- .Random.seed
  out <- gen_site_sizes(upstream, J = 25L, nj_mean = 50, cv = 0, nj_min = 5L, engine = "A2_modern")
  after <- .Random.seed

  expect_identical(out$n_j, rep(50L, 25L))
  expect_equal(out$se2_j * out$n_j, rep(compute_kappa(), 25L), tolerance = tol_deterministic)
  expect_identical(before, after)
})

test_that("stochastic rounding RNG policy is explicit", {
  stochastic_round <- multisitedgp_internal(".stochastic_round_site_sizes")

  set.seed(808L)
  before <- .Random.seed
  expect_identical(stochastic_round(c(10, 20, 30)), c(10L, 20L, 30L))
  after_integer <- .Random.seed
  expect_identical(after_integer, before)

  set.seed(809L)
  out <- stochastic_round(c(10.25, 20, 30.75))
  after_round <- .Random.seed
  set.seed(809L)
  uniforms <- stats::runif(2L)
  after_two_uniforms <- .Random.seed
  expected <- c(
    10L + as.integer(uniforms[[1L]] < 0.25),
    20L,
    30L + as.integer(uniforms[[2L]] < 0.75)
  )

  expect_identical(out, expected)
  expect_identical(after_round, after_two_uniforms)
})

test_that("gen_site_sizes validates public L2 inputs", {
  upstream <- site_size_upstream()

  expect_multisitedgp_error(
    gen_site_sizes(J = 25L),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    gen_site_sizes(upstream),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    gen_site_sizes(1:25, J = 25L),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    gen_site_sizes(upstream, J = 24L),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    gen_site_sizes(upstream["site_index"], J = 25L),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    gen_site_sizes(dplyr::mutate(upstream, se_j = 1), J = 25L),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    gen_site_sizes(upstream, J = 25L, engine = "bad"),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    gen_site_sizes(upstream, J = 25L, cv = -0.5),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    gen_site_sizes(upstream, J = 25L, nj_mean = 50.5, cv = 0),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    gen_site_sizes(upstream, J = 25L, p = 0),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    gen_site_sizes(upstream, J = 25L, R2 = 1),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    gen_site_sizes(upstream, J = 25L, var_outcome = 0),
    "multisitedgp_arg_error"
  )

  materialize_n_j <- multisitedgp_internal(".materialize_public_n_j")
  expect_multisitedgp_error(
    materialize_n_j(c(10, NA), nj_min = 5L, engine = "A2_modern"),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    suppressWarnings(materialize_n_j(c(10, .Machine$integer.max * 2), nj_min = 5L, engine = "A2_modern")),
    "multisitedgp_arg_error"
  )
})

test_that("gen_site_sizes propagates A2 solver errors", {
  upstream <- site_size_upstream()

  expect_multisitedgp_error(
    suppressWarnings(gen_site_sizes(
      upstream,
      J = 25L,
      nj_mean = 6,
      cv = 1.8,
      nj_min = 5L,
      engine = "A2_modern"
    )),
    "multisitedgp_solver_error"
  )
})
# nolint end
