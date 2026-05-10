# nolint start: object_usage_linter
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

test_that("gen_se_direct is exported and appends Paradigm B schema", {
  expect_true("gen_se_direct" %in% getNamespaceExports("multisiteDGP"))

  upstream <- direct_upstream()
  out <- gen_se_direct(upstream, J = 25L, I = 0.5, R = 4, shuffle = FALSE, sigma_tau = 0.20)

  expect_s3_class(out, "tbl_df")
  expect_named(out, c("site_index", "z_j", "tau_j", "n_j", "se2_j", "se_j"))
  expect_identical(out$n_j, rep(NA_integer_, 25L))
  expect_true(is.numeric(out$se2_j))
  expect_true(all(out$se2_j > 0))
  expect_equal(out$se_j, sqrt(out$se2_j), tolerance = tol_deterministic)
  expect_identical(attr(out, "engine", exact = TRUE), "paradigm_B_deterministic")
  expect_identical(attr(out, "I", exact = TRUE), 0.5)
  expect_identical(attr(out, "R", exact = TRUE), 4)
})

test_that("gen_se_direct requires upstream, J, and I", {
  expect_multisitedgp_error(
    gen_se_direct(),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    gen_se_direct(direct_upstream(), I = 0.5),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    gen_se_direct(direct_upstream(), J = 25L),
    "multisitedgp_arg_error"
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
    "very low informativeness"
  )
  expect_warning(
    gen_se_direct(upstream, J = 25L, I = 0.97, R = 1, shuffle = FALSE, sigma_tau = 0.20),
    "very high informativeness"
  )
  expect_warning(
    gen_se_direct(upstream, J = 25L, I = 0.5, R = 101, shuffle = FALSE, sigma_tau = 0.20),
    "very large heterogeneity"
  )
})

test_that("gen_se_direct validates public Paradigm B inputs", {
  upstream <- direct_upstream()

  expect_multisitedgp_error(
    gen_se_direct(upstream, J = 24L, I = 0.5),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    gen_se_direct(upstream["site_index"], J = 25L, I = 0.5),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    gen_se_direct(dplyr::mutate(upstream, se2_j = 1), J = 25L, I = 0.5),
    "multisitedgp_arg_error"
  )
  for (bad_i in c(0, 1, -0.1, 1.1)) {
    expect_multisitedgp_error(
      gen_se_direct(upstream, J = 25L, I = bad_i),
      "multisitedgp_arg_error"
    )
  }
  expect_multisitedgp_error(
    gen_se_direct(upstream, J = 25L, I = 0.5, R = 0.9),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    gen_se_direct(upstream, J = 25L, I = 0.5, shuffle = NA),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    gen_se_direct(upstream, J = 25L, I = 0.5, sigma_tau = 0),
    "multisitedgp_arg_error"
  )
})

test_that("gen_se_direct supports direct custom se_fn hooks", {
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
  expect_identical(attr(out, "engine", exact = TRUE), "paradigm_B_custom")
  expect_identical(attr(out, "direct_se_diagnostics", exact = TRUE)$method, "custom")

  null_n_fn <- function(J) list(se2_j = seq(0.01, 0.05, length.out = J), n_j = NULL)
  null_n <- gen_se_direct(upstream, J = 20L, I = 0.5, R = 1, se_fn = null_n_fn)
  expect_identical(null_n$n_j, rep(NA_integer_, 20L))

  bad_length_fn <- function(J) list(se2_j = rep(0.1, J - 1L))
  expect_multisitedgp_error(
    gen_se_direct(upstream, J = 20L, I = 0.5, se_fn = bad_length_fn),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    gen_se_direct(upstream, J = 20L, I = 0.5, se_fn = se_fn, se_args = list(J = 20L)),
    "multisitedgp_arg_error"
  )
})
# nolint end
