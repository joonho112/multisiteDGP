direct_upstream <- function(J = 25L) {
  tibble::tibble(
    site_index = seq_len(J),
    z_j = rep(0, J),
    tau_j = rep(0, J)
  )
}

direct_ratio <- function(x) {
  max(x) / min(x)
}

se_direct <- function(se_fn, se_args = list(), J = 10L) {
  upstream <- withr::with_seed(905L, gen_effects(J = J, sigma_tau = 0.2))
  gen_se_direct(upstream, J = J, I = 0.5, R = 2, se_fn = se_fn, se_args = se_args)
}

test_that("gen_se_direct is exported and appends se2_j, se_j and an NA n_j", {
  expect_true("gen_se_direct" %in% getNamespaceExports("multisiteDGP"))

  upstream <- direct_upstream()
  out <- gen_se_direct(upstream, J = 25L, I = 0.5, R = 4, shuffle = FALSE, sigma_tau = 0.20)

  expect_s3_class(out, "tbl_df")
  expect_named(out, c("site_index", "z_j", "tau_j", "n_j", "se2_j", "se_j"))
  expect_identical(out$n_j, rep(NA_integer_, 25L))
  expect_true(is.numeric(out$se2_j))
  expect_true(all(out$se2_j > 0))
  expect_equal(out$se_j, sqrt(out$se2_j), tolerance = tol_deterministic)
  expect_identical(attr(out, "engine", exact = TRUE), "direct")
  expect_identical(attr(out, "I", exact = TRUE), 0.5)
  expect_identical(attr(out, "R", exact = TRUE), 4)
})

test_that("gen_se_direct requires upstream, J, and I", {
  expect_multisitedgp_error(
    gen_se_direct()
  )
  expect_multisitedgp_error(
    gen_se_direct(direct_upstream(), I = 0.5)
  )
  expect_multisitedgp_error(
    gen_se_direct(direct_upstream(), J = 25L)
  )
})

test_that("gen_se_direct exactly recovers target I and R", {
  upstream <- direct_upstream(50L)
  out <- gen_se_direct(upstream, J = 50L, I = 0.3, R = 10, shuffle = FALSE, sigma_tau = 0.20)

  gm <- 0.20^2 * (1 - 0.3) / 0.3
  expect_equal(compute_I(out$se2_j, sigma_tau = 0.20), 0.3, tolerance = tol_deterministic)
  expect_equal(direct_ratio(out$se2_j), 10, tolerance = tol_deterministic)
  expect_equal(exp(mean(log(out$se2_j))), gm, tolerance = tol_deterministic)
  expect_equal(min(out$se2_j), gm / sqrt(10), tolerance = tol_deterministic)
  expect_equal(max(out$se2_j), gm * sqrt(10), tolerance = tol_deterministic)
})

test_that("gen_se_direct short-circuits homogeneous R equals one", {
  upstream <- direct_upstream()
  out <- gen_se_direct(upstream, J = 25L, I = 0.4, R = 1, shuffle = TRUE, sigma_tau = 0.20)
  gm <- 0.20^2 * (1 - 0.4) / 0.4

  expect_equal(out$se2_j, rep(gm, 25L), tolerance = tol_deterministic)
  expect_equal(compute_I(out$se2_j, sigma_tau = 0.20), 0.4, tolerance = tol_deterministic)
  expect_equal(direct_ratio(out$se2_j), 1, tolerance = tol_deterministic)
})

test_that("gen_se_direct shuffle permutes the same deterministic grid reproducibly", {
  upstream <- direct_upstream()

  sorted <- gen_se_direct(upstream, J = 25L, I = 0.5, R = 6, shuffle = FALSE, sigma_tau = 0.20)
  shuffled_1 <- withr::with_seed(
    901L,
    gen_se_direct(upstream, J = 25L, I = 0.5, R = 6, shuffle = TRUE, sigma_tau = 0.20)
  )
  shuffled_2 <- withr::with_seed(
    901L,
    gen_se_direct(upstream, J = 25L, I = 0.5, R = 6, shuffle = TRUE, sigma_tau = 0.20)
  )

  expect_identical(shuffled_1, shuffled_2)
  expect_equal(sort(shuffled_1$se2_j), sorted$se2_j, tolerance = tol_deterministic)
  expect_equal(compute_I(shuffled_1$se2_j, sigma_tau = 0.20), 0.5, tolerance = tol_deterministic)
  expect_equal(direct_ratio(shuffled_1$se2_j), 6, tolerance = tol_deterministic)
  expect_false(isTRUE(all.equal(shuffled_1$se2_j, sorted$se2_j, tolerance = tol_deterministic)))
})

test_that("gen_se_direct shuffle-free and homogeneous paths are RNG neutral", {
  upstream <- direct_upstream()

  set.seed(902L)
  before <- .Random.seed
  gen_se_direct(upstream, J = 25L, I = 0.5, R = 6, shuffle = FALSE, sigma_tau = 0.20)
  after <- .Random.seed
  expect_identical(before, after)

  set.seed(903L)
  before <- .Random.seed
  gen_se_direct(upstream, J = 25L, I = 0.5, R = 1, shuffle = TRUE, sigma_tau = 0.20)
  after <- .Random.seed
  expect_identical(before, after)
})

test_that("gen_se_direct shuffle path follows active sample RNG policy", {
  upstream <- direct_upstream()

  sorted <- gen_se_direct(upstream, J = 25L, I = 0.5, R = 6, shuffle = FALSE, sigma_tau = 0.20)
  set.seed(904L)
  expected_idx <- sample(seq_along(sorted$se2_j), size = length(sorted$se2_j), replace = FALSE)
  expected <- sorted$se2_j[expected_idx]
  expected_after <- .Random.seed

  set.seed(904L)
  shuffled <- gen_se_direct(upstream, J = 25L, I = 0.5, R = 6, shuffle = TRUE, sigma_tau = 0.20)
  actual_after <- .Random.seed

  expect_identical(shuffled$se2_j, expected)
  expect_identical(actual_after, expected_after)
})

test_that("gen_se_direct conditioning warnings are soft guidance", {
  upstream <- direct_upstream()

  expect_warning(
    gen_se_direct(upstream, J = 25L, I = 0.005, R = 1, shuffle = FALSE, sigma_tau = 0.20),
    "`I` is below 0.01"
  )
  expect_warning(
    gen_se_direct(upstream, J = 25L, I = 0.97, R = 1, shuffle = FALSE, sigma_tau = 0.20),
    "`I` is above 0.95"
  )
  expect_warning(
    gen_se_direct(upstream, J = 25L, I = 0.5, R = 101, shuffle = FALSE, sigma_tau = 0.20),
    "`R` is above 100"
  )
})

test_that("gen_se_direct validates its inputs", {
  upstream <- direct_upstream()

  expect_multisitedgp_error(
    gen_se_direct(upstream, J = 24L, I = 0.5)
  )
  expect_multisitedgp_error(
    gen_se_direct(upstream["site_index"], J = 25L, I = 0.5)
  )
  expect_multisitedgp_error(
    gen_se_direct(dplyr::mutate(upstream, se2_j = 1), J = 25L, I = 0.5)
  )
  for (bad_i in c(0, 1, -0.1, 1.1)) {
    expect_multisitedgp_error(
      gen_se_direct(upstream, J = 25L, I = bad_i)
    )
  }
  expect_multisitedgp_error(
    gen_se_direct(upstream, J = 25L, I = 0.5, R = 0.9)
  )
  expect_multisitedgp_error(
    gen_se_direct(upstream, J = 25L, I = 0.5, shuffle = NA)
  )
  expect_multisitedgp_error(
    gen_se_direct(upstream, J = 25L, I = 0.5, sigma_tau = 0)
  )
})

test_that("gen_se_direct accepts a user-supplied se_fn", {
  upstream <- direct_upstream(20L)
  se_fn <- function(J, scale = 0.2) {
    list(
      se2_j = rep(scale^2, J),
      n_j = seq_len(J) + 50L
    )
  }
  out <- gen_se_direct(
    upstream,
    J = 20L,
    I = 0.5,
    R = 1,
    se_fn = se_fn,
    se_args = list(scale = 0.3)
  )

  expect_identical(out$n_j, as.integer(seq_len(20L) + 50L))
  expect_equal(out$se2_j, rep(0.09, 20L), tolerance = tol_deterministic)
  expect_identical(attr(out, "engine", exact = TRUE), "direct_custom")
  expect_identical(attr(out, "direct_se_diagnostics", exact = TRUE)$method, "custom")

  null_n_fn <- function(J) list(se2_j = seq(0.01, 0.05, length.out = J), n_j = NULL)
  null_n <- gen_se_direct(upstream, J = 20L, I = 0.5, R = 1, se_fn = null_n_fn)
  expect_identical(null_n$n_j, rep(NA_integer_, 20L))

  bad_length_fn <- function(J) list(se2_j = rep(0.1, J - 1L))
  expect_multisitedgp_error(
    gen_se_direct(upstream, J = 20L, I = 0.5, se_fn = bad_length_fn)
  )
  expect_multisitedgp_error(
    gen_se_direct(upstream, J = 20L, I = 0.5, se_fn = se_fn, se_args = list(J = 20L))
  )
})

test_that("se_fn must return something carrying se2_j", {
  bad_returns <- list(
    function(J) numeric(J),                    # bare vector, not a named list
    function(J) list(numeric(J)),              # list without names
    function(J) list(variance = numeric(J))    # named, but not se2_j
  )
  for (fn in bad_returns) {
    expect_multisitedgp_error(se_direct(fn))
  }
})

test_that("a data frame returned by se_fn is accepted and read column-wise", {
  # A data frame is converted to a list before the return value is checked, so
  # a data frame with an se2_j column is a valid se_fn return.
  out <- se_direct(function(J) data.frame(se2_j = rep(0.04, J)))
  expect_true(is_multisitedgp_data(out) || is.data.frame(out))
  expect_equal(unique(out$se2_j), 0.04)
})

test_that("se_fn must return exactly one se2_j per site", {
  expect_multisitedgp_error(
    se_direct(function(J) list(se2_j = rep(0.04, J + 3L)))
  )
})

test_that("se_fn's n_j must be integer-like and one per site, or NULL", {
  ok <- se_direct(function(J) list(se2_j = rep(0.04, J), n_j = NULL))
  expect_true(all(is.na(ok$n_j)))

  bad <- list(
    function(J) list(se2_j = rep(0.04, J), n_j = rep(10.5, J)),   # not integer-like
    function(J) list(se2_j = rep(0.04, J), n_j = rep(10L, J - 1L)),
    function(J) list(se2_j = rep(0.04, J), n_j = rep(NA_real_, J))
  )
  for (fn in bad) {
    expect_multisitedgp_error(se_direct(fn))
  }
})

test_that("se_fn's n_j must be positive and within the integer range", {
  for (n in c(-5, 0, 2^31, 2^40)) {
    se_fn <- function(J) list(se2_j = rep(0.04, J), n_j = rep(n, J))
    expect_rng_neutral_error(
      gen_se_direct(direct_upstream(20L), J = 20L, I = 0.5, se_fn = se_fn),
      regexp = "se_fn.*n_j"
    )
    expect_multisitedgp_error(
      sim_meta(J = 10L, I = 0.5, R = 2, se_fn = se_fn, seed = 33L),
      regexp = "se_fn.*n_j"
    )
  }
  for (n in c(1, .Machine$integer.max)) {
    out <- gen_se_direct(
      direct_upstream(20L), J = 20L, I = 0.5,
      se_fn = function(J) list(se2_j = rep(0.04, J), n_j = rep(n, J))
    )
    expect_identical(out$n_j, rep(as.integer(n), 20L))
  }
})

test_that("gen_se_direct stops when the built-in variances are not positive finite numbers", {
  upstream <- direct_upstream(20L)
  cases <- list(
    list(I = 1e-310, R = 1, sigma_tau = 0.2),
    list(I = 1e-310, R = 2, sigma_tau = 0.2),
    list(I = 0.5, R = 2, sigma_tau = 1e160),
    list(I = 0.5, R = 2, sigma_tau = 1e-200),
    list(I = 0.5, R = 1e308, sigma_tau = 1e154)
  )
  for (args in cases) {
    expect_rng_neutral_error(
      suppressWarnings(do.call(gen_se_direct, c(list(upstream = upstream, J = 20L), args))),
      regexp = "I.*R.*sigma_tau"
    )
  }
  # A very small sigma_tau whose variances can still be represented is accepted.
  out <- gen_se_direct(upstream, J = 20L, I = 0.5, R = 1, sigma_tau = 1e-160)
  expect_true(all(is.finite(out$se2_j) & out$se2_j > 0))
})

test_that("se_args must be named and must not smuggle in J", {
  fn <- function(J, scale = 1) list(se2_j = rep(0.04 * scale, J))

  expect_multisitedgp_error(se_direct(fn, se_args = list(0.5)))
  expect_multisitedgp_error(se_direct(fn, se_args = list(J = 99L)))
  expect_no_error(se_direct(fn, se_args = list(scale = 0.5)))
})
