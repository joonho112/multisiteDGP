# nolint start: object_usage_linter
test_that("SkewN generator returns canonical schema and preserves decomposition", {
  testthat::skip_if_not_installed("sn")
  data_x <- tibble::tibble(x = seq(-1, 1, length.out = 10L))
  out <- withr::with_seed(
    201L,
    gen_effects_skewn(
      J = 10L,
      tau = 0.25,
      sigma_tau = 0.3,
      slant = 2,
      formula = ~ x,
      beta = 0.75,
      data = data_x
    )
  )
  x_beta <- stats::model.matrix(~ x, data_x) %*% c(0, 0.75)

  expect_named(out, c("site_index", "z_j", "tau_j", "x"))
  expect_equal(out$tau_j - 0.25 - as.numeric(x_beta), 0.3 * out$z_j, tolerance = 1e-12)
})

test_that("ALD generator returns canonical schema and preserves decomposition", {
  testthat::skip_if_not_installed("LaplacesDemon")
  data_x <- tibble::tibble(x = seq(-1, 1, length.out = 10L))
  out <- withr::with_seed(
    202L,
    gen_effects_ald(
      J = 10L,
      tau = -0.25,
      sigma_tau = 0.4,
      rho = 0.3,
      formula = ~ x,
      beta = 1.25,
      data = data_x
    )
  )
  x_beta <- stats::model.matrix(~ x, data_x) %*% c(0, 1.25)

  expect_named(out, c("site_index", "z_j", "tau_j", "x"))
  expect_equal(out$tau_j - (-0.25) - as.numeric(x_beta), 0.4 * out$z_j, tolerance = 1e-12)
})

test_that("SkewN standardization and skewness sign match T16", {
  skip_if_not_slow()
  testthat::skip_if_not_installed("sn")
  testthat::skip_if_not_installed("moments")

  for (slant in c(-4, -1, 0, 1, 4)) {
    z_j <- withr::with_seed(210L + slant, suppressWarnings(
      gen_effects_skewn(J = default_n_property_large, slant = slant)$z_j
    ))
    expect_lt(abs(mean(z_j)), tol_mc_moment_n1e6)
    expect_lt(abs(stats::var(z_j) - 1), tol_mc_moment_n1e6)

    skew <- moments::skewness(z_j)
    if (slant == 0) {
      expect_lt(abs(skew), 0.01)
    } else {
      expect_identical(sign(skew), sign(slant))
    }
  }
})

test_that("ALD standardization matches T13 with Yu-Zhang parameter conversion", {
  skip_if_not_slow()
  testthat::skip_if_not_installed("LaplacesDemon")

  for (rho in c(0.25, 0.5, 0.7)) {
    z_j <- withr::with_seed(220L + round(100 * rho), gen_effects_ald(
      J = default_n_property_large,
      rho = rho
    )$z_j)
    expect_lt(abs(mean(z_j)), tol_mc_moment_n1e6)
    expect_lt(abs(stats::var(z_j) - 1), tol_mc_moment_n1e6)
  }
})

test_that("SkewN validation warns and aborts with classed errors", {
  testthat::skip_if_not_installed("sn")

  expect_multisitedgp_error(
    gen_effects_skewn(J = 10L),
    "multisitedgp_arg_error"
  )
  expect_warning(
    out_zero <- gen_effects_skewn(J = 10L, slant = 0),
    "exactly Gaussian"
  )
  expect_s3_class(out_zero, "tbl_df")
  expect_warning(
    out_large <- gen_effects_skewn(J = 10L, slant = 31),
    "half-normal"
  )
  expect_s3_class(out_large, "tbl_df")
})

test_that("ALD validation rejects boundary rho values", {
  testthat::skip_if_not_installed("LaplacesDemon")

  expect_multisitedgp_error(
    gen_effects_ald(J = 10L),
    "multisitedgp_arg_error"
  )
  for (rho in c(0.05, 0.95, 0, 1)) {
    expect_multisitedgp_error(
      gen_effects_ald(J = 10L, rho = rho),
      "multisitedgp_arg_error"
    )
  }
})

test_that("SkewN and ALD soft dependencies hard-reject when unavailable", {
  testthat::local_mocked_bindings(
    .require_namespace = function(package) FALSE,
    .package = "multisiteDGP"
  )

  expect_multisitedgp_error(
    gen_effects_skewn(J = 10L, slant = 1),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    gen_effects_ald(J = 10L, rho = 0.3),
    "multisitedgp_arg_error"
  )
})

test_that("gen_effects dispatcher routes SkewN and ALD", {
  testthat::skip_if_not_installed("sn")
  testthat::skip_if_not_installed("LaplacesDemon")
  expect_true(all(c("gen_effects_skewn", "gen_effects_ald") %in% getNamespaceExports("multisiteDGP")))

  skewn <- withr::with_seed(230L, gen_effects(J = 10L, true_dist = "SkewN", theta_G = list(slant = 2)))
  direct_skewn <- withr::with_seed(230L, gen_effects_skewn(J = 10L, slant = 2))
  ald <- withr::with_seed(231L, gen_effects(J = 10L, true_dist = "ALD", theta_G = list(rho = 0.3)))
  direct_ald <- withr::with_seed(231L, gen_effects_ald(J = 10L, rho = 0.3))

  expect_equal(skewn, direct_skewn, tolerance = tol_deterministic)
  expect_equal(ald, direct_ald, tolerance = tol_deterministic)
})

test_that("design validation checks SkewN and ALD theta boundaries", {
  expect_multisitedgp_error(
    multisitedgp_design(true_dist = "SkewN"),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    multisitedgp_design(true_dist = "ALD"),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    multisitedgp_design(true_dist = "ALD", theta_G = list(rho = 0.05)),
    "multisitedgp_arg_error"
  )

  expect_s3_class(
    multisitedgp_design(true_dist = "SkewN", theta_G = list(slant = 2)),
    "multisitedgp_design"
  )
  expect_s3_class(
    multisitedgp_design(true_dist = "ALD", theta_G = list(rho = 0.3)),
    "multisitedgp_design"
  )
})
# nolint end
