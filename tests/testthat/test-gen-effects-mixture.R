# nolint start: object_usage_linter
test_that("Mixture generator returns schema, latent component, and exact JEBS stream", {
  delta <- 5
  eps <- 0.3
  ups <- 2
  scale <- sqrt((1 - eps) + eps * ups^2 + eps * (1 - eps) * delta^2)
  expected <- withr::with_seed(301L, {
    component <- as.integer(ifelse(stats::runif(10L) < (1 - eps), 1L, 2L))
    component_1 <- stats::rnorm(10L, mean = -eps * delta / scale, sd = 1 / scale)
    component_2 <- stats::rnorm(10L, mean = (1 - eps) * delta / scale, sd = ups / scale)
    z_j <- ifelse(component == 1L, component_1, component_2)
    list(component = component, z_j = z_j)
  })
  out <- withr::with_seed(
    301L,
    gen_effects_mixture(J = 10L, tau = 0.5, sigma_tau = 0.25, delta = delta, eps = eps, ups = ups)
  )

  expect_named(out, c("site_index", "z_j", "tau_j", "latent_component"))
  expect_identical(out$site_index, seq_len(10L))
  expect_identical(out$latent_component, expected$component)
  expect_equal(out$z_j, expected$z_j, tolerance = tol_deterministic)
  expect_equal(out$tau_j, 0.5 + 0.25 * expected$z_j, tolerance = tol_deterministic)
})

test_that("Mixture generator preserves covariate decomposition", {
  data_x <- tibble::tibble(x = seq(-1, 1, length.out = 10L))
  out <- withr::with_seed(
    302L,
    gen_effects_mixture(
      J = 10L,
      tau = -0.1,
      sigma_tau = 0.4,
      delta = 4,
      eps = 0.5,
      ups = 1,
      formula = ~ x,
      beta = 1.25,
      data = data_x
    )
  )
  x_beta <- stats::model.matrix(~ x, data_x) %*% c(0, 1.25)

  expect_named(out, c("site_index", "z_j", "tau_j", "latent_component", "x"))
  expect_equal(out$tau_j - (-0.1) - as.numeric(x_beta), 0.4 * out$z_j, tolerance = tol_deterministic)
  expect_true(all(out$latent_component %in% c(1L, 2L)))
})

test_that("T14a raw legacy mixture variance matches R2-F06 baseline", {
  skip_if_not_slow()

  for (seed in c(42L, 1L, 2024L)) {
    raw <- .legacy_mixture_raw_draw(delta = 5, eps = 0.3, ups = 2, J = default_n_property_large, seed = seed)
    expect_equal(
      stats::var(raw),
      7.15,
      tolerance = 0.05,
      info = sprintf("T14a seed = %d", seed)
    )
  }
})

test_that("T14b standardized mixture output has unit variance", {
  skip_if_not_slow()

  for (seed in c(42L, 1L, 2024L)) {
    out <- withr::with_seed(
      seed,
      gen_effects(J = default_n_property_large, true_dist = "Mixture", theta_G = list(delta = 5, eps = 0.3, ups = 2))
    )
    expect_true(abs(stats::var(out$z_j) - 1) < 0.01, info = sprintf("T14b seed = %d", seed))
    expect_true(abs(mean(out$z_j)) < tol_mc_moment_n1e6, info = sprintf("T14b mean seed = %d", seed))
    expect_true(all(out$latent_component %in% c(1L, 2L)))
    expect_lt(abs(mean(out$latent_component == 2L) - 0.3), 0.005)
  }
})

test_that("Mixture validation rejects missing and invalid parameters", {
  expect_multisitedgp_error(
    gen_effects_mixture(J = 10L, eps = 0.3, ups = 2),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    gen_effects_mixture(J = 10L, delta = 5, ups = 2),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    gen_effects_mixture(J = 10L, delta = 5, eps = 0.3),
    "multisitedgp_arg_error"
  )
  for (bad_delta in c(0, -1)) {
    expect_multisitedgp_error(
      gen_effects_mixture(J = 10L, delta = bad_delta, eps = 0.3, ups = 2),
      "multisitedgp_arg_error"
    )
  }
  for (bad_eps in c(0, 1, -0.1, 1.1)) {
    expect_multisitedgp_error(
      gen_effects_mixture(J = 10L, delta = 5, eps = bad_eps, ups = 2),
      "multisitedgp_arg_error"
    )
  }
  for (bad_ups in c(0, -1)) {
    expect_multisitedgp_error(
      gen_effects_mixture(J = 10L, delta = 5, eps = 0.3, ups = bad_ups),
      "multisitedgp_arg_error"
    )
  }
  expect_s3_class(
    gen_effects_mixture(J = 10L, delta = 5, eps = 0.3, ups = 0.5),
    "tbl_df"
  )
})

test_that("Mixture dispatcher and design validation are wired", {
  expect_true("gen_effects_mixture" %in% getNamespaceExports("multisiteDGP"))

  routed <- withr::with_seed(
    303L,
    gen_effects(J = 10L, true_dist = "Mixture", theta_G = list(delta = 5, eps = 0.3, ups = 2))
  )
  direct <- withr::with_seed(303L, gen_effects_mixture(J = 10L, delta = 5, eps = 0.3, ups = 2))

  expect_equal(routed, direct, tolerance = tol_deterministic)
  expect_s3_class(
    multisitedgp_design(J = 10L, true_dist = "Mixture", theta_G = list(delta = 5, eps = 0.3, ups = 2)),
    "multisitedgp_design"
  )
  expect_multisitedgp_error(
    multisitedgp_design(J = 10L, true_dist = "Mixture", theta_G = list(delta = 5, eps = 0.3)),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    gen_effects_mixture(
      J = 10L,
      delta = 5,
      eps = 0.3,
      ups = 2,
      formula = ~ latent_component,
      beta = 1,
      data = tibble::tibble(latent_component = seq_len(10L))
    ),
    "multisitedgp_arg_error"
  )
})
# nolint end
