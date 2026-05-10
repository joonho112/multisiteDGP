# nolint start: object_usage_linter
test_that("Gaussian generator returns canonical L1 schema and exact RNG stream", {
  out <- withr::with_seed(101L, gen_effects_gaussian(J = 10L, tau = 0.5, sigma_tau = 0.2))
  expected_z <- withr::with_seed(101L, stats::rnorm(10L))

  expect_s3_class(out, "tbl_df")
  expect_identical(names(out), c("site_index", "z_j", "tau_j"))
  expect_identical(out$site_index, seq_len(10L))
  expect_equal(out$z_j, expected_z, tolerance = tol_deterministic)
  expect_equal(out$tau_j, 0.5 + 0.2 * expected_z, tolerance = tol_deterministic)
})

test_that("Gaussian generator enforces unit-variance residual contract", {
  expect_multisitedgp_error(
    gen_effects_gaussian(J = 10L, variance = 2),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    gen_effects_gaussian(J = 10L, variance = 0),
    "multisitedgp_arg_error"
  )
})

test_that("Gaussian generator preserves covariate decomposition", {
  data_x <- tibble::tibble(x = seq(-1, 1, length.out = 10L))
  out <- withr::with_seed(
    102L,
    gen_effects_gaussian(
      J = 10L,
      tau = -0.1,
      sigma_tau = 0.4,
      formula = ~ x,
      beta = 1.25,
      data = data_x
    )
  )
  x_beta <- stats::model.matrix(~ x, data_x) %*% c(0, 1.25)

  expect_named(out, c("site_index", "z_j", "tau_j", "x"))
  expect_equal(out$tau_j - (-0.1) - as.numeric(x_beta), 0.4 * out$z_j, tolerance = 1e-12)
})

test_that("invalid shared Layer 1 arguments do not advance caller RNG", {
  data_x <- tibble::tibble(x = seq_len(10L))
  short_data_x <- tibble::tibble(x = seq_len(9L))

  expect_rng_neutral_error(
    gen_effects_gaussian(J = 10L, tau = NA_real_),
    "multisitedgp_arg_error"
  )
  expect_rng_neutral_error(
    gen_effects_gaussian(J = 10L, sigma_tau = 0),
    "multisitedgp_arg_error"
  )
  expect_rng_neutral_error(
    gen_effects_gaussian(J = 10L, formula = ~ x, beta = 1),
    "multisitedgp_arg_error"
  )
  expect_rng_neutral_error(
    gen_effects_gaussian(J = 10L, formula = ~ x, data = data_x),
    "multisitedgp_arg_error"
  )
  expect_rng_neutral_error(
    gen_effects_gaussian(J = 10L, formula = ~ x, beta = 1, data = short_data_x),
    "multisitedgp_arg_error"
  )
  expect_rng_neutral_error(
    gen_effects_gaussian(
      J = 10L,
      formula = ~ site_index,
      beta = 1,
      data = tibble::tibble(site_index = seq_len(10L))
    ),
    "multisitedgp_arg_error"
  )
})

test_that("Student-t generator standardizes by sqrt((nu - 2) / nu)", {
  nu <- 5
  out <- withr::with_seed(103L, gen_effects_studentt(J = 10L, nu = nu))
  expected_z <- withr::with_seed(103L, stats::rt(10L, df = nu) * sqrt((nu - 2) / nu))

  expect_identical(names(out), c("site_index", "z_j", "tau_j"))
  expect_equal(out$z_j, expected_z, tolerance = tol_deterministic)
  expect_equal(out$tau_j, 0.20 * expected_z, tolerance = tol_deterministic)
})

test_that("Student-t generator validates nu and warns for non-finite kurtosis", {
  expect_multisitedgp_error(
    gen_effects_studentt(J = 10L),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    gen_effects_studentt(J = 10L, nu = 2),
    "multisitedgp_arg_error"
  )
  expect_warning(
    out <- gen_effects_studentt(J = 10L, nu = 3),
    "non-finite kurtosis"
  )
  expect_s3_class(out, "tbl_df")
})

test_that("Gaussian and Student-t large-sample z_j moments match standardization contract", {
  skip_if_not_slow()

  gaussian <- withr::with_seed(104L, gen_effects_gaussian(J = 100000L))

  expect_lt(abs(mean(gaussian$z_j)), tol_mc_moment_n1e5)
  expect_lt(abs(stats::var(gaussian$z_j) - 1), tol_mc_moment_n1e5)

  for (nu in c(3, 5, 10)) {
    studentt <- withr::with_seed(105L + nu, suppressWarnings(
      gen_effects_studentt(J = default_n_property_large, nu = nu)
    ))
    var_tolerance <- if (nu < 4) tol_studentt_inf_kurt_var else tol_mc_moment_n1e5
    expect_lt(abs(mean(studentt$z_j)), tol_mc_moment_n1e6)
    expect_lt(abs(stats::var(studentt$z_j) - 1), var_tolerance)
  }
})

test_that("gen_effects dispatcher routes Gaussian and Student-t shapes", {
  expect_true(all(
    c("gen_effects", "gen_effects_gaussian", "gen_effects_studentt") %in%
      getNamespaceExports("multisiteDGP")
  ))

  gaussian <- withr::with_seed(106L, gen_effects(J = 10L))
  direct_gaussian <- withr::with_seed(106L, gen_effects_gaussian(J = 10L))
  studentt <- withr::with_seed(107L, gen_effects(J = 10L, true_dist = "StudentT", theta_G = list(nu = 5)))
  direct_studentt <- withr::with_seed(107L, gen_effects_studentt(J = 10L, nu = 5))

  expect_equal(gaussian, direct_gaussian, tolerance = tol_deterministic)
  expect_equal(studentt, direct_studentt, tolerance = tol_deterministic)
})
# nolint end
