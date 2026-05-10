# nolint start: object_usage_linter
effect_frame <- function(z_j, ..., J = length(z_j)) {
  multisitedgp_internal(".new_effects_frame")(z_j = z_j, J = J, ...)
}

test_that("common L1 frame creates canonical effect columns", {
  z_j <- seq(-1, 1, length.out = 10)
  out <- effect_frame(z_j, tau = 0.5, sigma_tau = 0.2)

  expect_s3_class(out, "tbl_df")
  expect_identical(names(out), c("site_index", "z_j", "tau_j"))
  expect_type(out$site_index, "integer")
  expect_type(out$z_j, "double")
  expect_type(out$tau_j, "double")
  expect_identical(out$site_index, seq_len(10L))
  expect_equal(out$tau_j, 0.5 + 0.2 * z_j, tolerance = 1e-12)
})

test_that("covariate-off path matches no-formula path exactly", {
  z_j <- seq(-1, 1, length.out = 10)
  data_x <- tibble::tibble(x = rnorm(10), group = rep(c("A", "B"), 5))
  no_cov <- effect_frame(z_j, tau = 0.1, sigma_tau = 0.3)
  with_zero_beta <- effect_frame(
    z_j,
    tau = 0.1,
    sigma_tau = 0.3,
    formula = ~ x,
    beta = 0,
    data = data_x
  )

  expect_equal(no_cov$tau_j, with_zero_beta$tau_j, tolerance = 1e-12)
  expect_named(with_zero_beta, c("site_index", "z_j", "tau_j", "x", "group"))
})

test_that("covariate decomposition preserves public z_j identity", {
  z_j <- seq(-1, 1, length.out = 10)
  data_x <- tibble::tibble(x = seq(-2, 2, length.out = 10))
  out <- effect_frame(
    z_j,
    tau = -0.2,
    sigma_tau = 0.4,
    formula = ~ x,
    beta = 1.5,
    data = data_x
  )
  x_beta <- stats::model.matrix(~ x, data_x) %*% c(0, 1.5)

  expect_equal(
    out$tau_j - (-0.2) - as.numeric(x_beta),
    0.4 * out$z_j,
    tolerance = 1e-12
  )
})

test_that("named beta supports explicit intercept and model columns", {
  z_j <- rep(0, 10)
  data_x <- tibble::tibble(x = seq_len(10))
  out <- effect_frame(
    z_j,
    tau = 1,
    sigma_tau = 0.2,
    formula = ~ x,
    beta = c(intercept = 0.5, x = 2),
    data = data_x
  )

  expect_equal(out$tau_j, 1 + 0.5 + 2 * data_x$x, tolerance = 1e-12)
})

test_that("common L1 frame validates residual and covariate inputs", {
  z_j <- seq(-1, 1, length.out = 10)
  data_x <- tibble::tibble(x = seq_len(10))

  expect_multisitedgp_error(
    effect_frame(z_j[-1], J = 10L),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    effect_frame(z_j, formula = ~ x, beta = 1),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    effect_frame(z_j, formula = ~ x, data = data_x),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    effect_frame(z_j, formula = ~ x, beta = 1, data = data_x[-1, ]),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    effect_frame(
      z_j,
      formula = ~ site_index,
      beta = 1,
      data = tibble::tibble(site_index = seq_len(10))
    ),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    effect_frame(z_j, formula = ~ x, beta = c(1, 2, 3), data = data_x),
    "multisitedgp_arg_error"
  )
})
# nolint end
