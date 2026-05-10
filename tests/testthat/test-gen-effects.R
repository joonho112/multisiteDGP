# nolint start: object_usage_linter
test_that("Layer 1 dispatcher exports all catalog routes", {
  expected_exports <- c(
    "gen_effects",
    "gen_effects_gaussian",
    "gen_effects_studentt",
    "gen_effects_skewn",
    "gen_effects_ald",
    "gen_effects_mixture",
    "gen_effects_pmslab",
    "gen_effects_user",
    "gen_effects_dpm"
  )

  expect_true(all(expected_exports %in% getNamespaceExports("multisiteDGP")))
})

test_that("Layer 1 dispatcher returns the canonical schema across available routes", {
  standardized_g <- function(J) {
    z <- seq(-1, 1, length.out = J)
    (z - mean(z)) / stats::sd(z)
  }
  fixtures <- list(
    Gaussian = list(true_dist = "Gaussian", theta_G = list()),
    StudentT = list(true_dist = "StudentT", theta_G = list(nu = 5)),
    Mixture = list(true_dist = "Mixture", theta_G = list(delta = 5, eps = 0.3, ups = 2)),
    PointMassSlab = list(true_dist = "PointMassSlab", theta_G = list(pi0 = 0.5, slab_shape = "Gaussian")),
    User = list(true_dist = "User", theta_G = list(), g_fn = standardized_g),
    DPM = list(true_dist = "DPM", theta_G = list(), g_fn = standardized_g)
  )
  if (requireNamespace("sn", quietly = TRUE)) {
    fixtures$SkewN <- list(true_dist = "SkewN", theta_G = list(slant = 2))
  }
  if (requireNamespace("LaplacesDemon", quietly = TRUE)) {
    fixtures$ALD <- list(true_dist = "ALD", theta_G = list(rho = 0.5))
  }

  for (name in names(fixtures)) {
    fixture <- fixtures[[name]]
    out <- withr::with_seed(
      501L,
      gen_effects(
        J = 25L,
        true_dist = fixture$true_dist,
        theta_G = fixture$theta_G,
        g_fn = fixture$g_fn
      )
    )

    expect_s3_class(out, "tbl_df")
    expect_equal(nrow(out), 25L)
    expect_true(all(c("site_index", "z_j", "tau_j") %in% names(out)))
    expect_identical(out$site_index, seq_len(25L))
    expect_true(all(is.finite(out$z_j)))
    expect_true(all(is.finite(out$tau_j)))
  }
})

test_that("Layer 1 dispatcher preserves covariate decomposition across available routes", {
  standardized_g <- function(J, reverse) {
    z <- seq(-1, 1, length.out = J)
    z <- (z - mean(z)) / stats::sd(z)
    if (isTRUE(reverse)) {
      z <- -z
    }
    z
  }
  data_x <- tibble::tibble(x = seq(-1, 1, length.out = 20L))
  x_beta <- stats::model.matrix(~ x, data_x) %*% c(0, 0.75)
  fixtures <- list(
    Gaussian = list(true_dist = "Gaussian", theta_G = list()),
    StudentT = list(true_dist = "StudentT", theta_G = list(nu = 5)),
    Mixture = list(true_dist = "Mixture", theta_G = list(delta = 5, eps = 0.3, ups = 2)),
    PointMassSlab = list(true_dist = "PointMassSlab", theta_G = list(pi0 = 0.5, slab_shape = "Gaussian")),
    User = list(true_dist = "User", theta_G = list(reverse = TRUE), g_fn = standardized_g),
    DPM = list(true_dist = "DPM", theta_G = list(reverse = TRUE), g_fn = standardized_g)
  )
  if (requireNamespace("sn", quietly = TRUE)) {
    fixtures$SkewN <- list(true_dist = "SkewN", theta_G = list(slant = 2))
  }
  if (requireNamespace("LaplacesDemon", quietly = TRUE)) {
    fixtures$ALD <- list(true_dist = "ALD", theta_G = list(rho = 0.5))
  }

  for (name in names(fixtures)) {
    fixture <- fixtures[[name]]
    out <- withr::with_seed(
      503L,
      gen_effects(
        J = 20L,
        true_dist = fixture$true_dist,
        tau = -0.25,
        sigma_tau = 0.35,
        theta_G = fixture$theta_G,
        formula = ~ x,
        beta = 0.75,
        data = data_x,
        g_fn = fixture$g_fn
      )
    )

    expect_true("x" %in% names(out))
    expect_equal(out$x, data_x$x, tolerance = tol_deterministic)
    expect_equal(out$tau_j - (-0.25) - as.numeric(x_beta), 0.35 * out$z_j, tolerance = 1e-12)
  }
})

test_that("Layer 1 dispatcher preserves g_fn auto-User and DPM stub contracts", {
  standardized_g <- function(J) {
    z <- seq(-1, 1, length.out = J)
    (z - mean(z)) / stats::sd(z)
  }

  auto_user <- gen_effects(J = 25L, g_fn = standardized_g)
  explicit_user <- gen_effects(J = 25L, true_dist = "User", g_fn = standardized_g)
  expect_equal(auto_user, explicit_user, tolerance = tol_deterministic)

  expect_multisitedgp_error(
    gen_effects(J = 25L, true_dist = "DPM"),
    "multisitedgp_arg_error"
  )
})

test_that("Layer 1 dispatcher forwards covariate and callback contracts", {
  data_x <- tibble::tibble(x = seq(-1, 1, length.out = 10L))
  gaussian <- withr::with_seed(
    502L,
    gen_effects(
      J = 10L,
      true_dist = "Gaussian",
      tau = -0.1,
      sigma_tau = 0.4,
      formula = ~ x,
      beta = 1.25,
      data = data_x
    )
  )
  direct_gaussian <- withr::with_seed(
    502L,
    gen_effects_gaussian(
      J = 10L,
      tau = -0.1,
      sigma_tau = 0.4,
      formula = ~ x,
      beta = 1.25,
      data = data_x
    )
  )

  expect_equal(gaussian, direct_gaussian, tolerance = tol_deterministic)

  raw_g <- function(J, offset) {
    seq_len(J) / 10 + offset
  }
  user <- gen_effects(
    J = 10L,
    true_dist = "User",
    tau = 0.5,
    sigma_tau = 0.2,
    g_fn = raw_g,
    g_returns = "raw",
    theta_G = list(offset = 1)
  )
  direct_user <- gen_effects_user(
    J = 10L,
    tau = 0.5,
    sigma_tau = 0.2,
    g_fn = raw_g,
    g_returns = "raw",
    g_args = list(offset = 1)
  )

  expect_equal(user, direct_user, tolerance = tol_deterministic)
})

test_that("Layer 1 dispatcher preserves DPM bridge forwarding", {
  bridge_g <- function(J, shift) {
    z <- seq(-1, 1, length.out = J) + shift
    (z - mean(z)) / stats::sd(z)
  }

  dpm <- gen_effects(
    J = 10L,
    true_dist = "DPM",
    theta_G = list(shift = 0.3),
    g_fn = bridge_g
  )
  direct_user <- gen_effects_user(
    J = 10L,
    g_fn = bridge_g,
    g_args = list(shift = 0.3)
  )

  expect_equal(dpm, direct_user, tolerance = tol_deterministic)

  raw_g <- function(J, offset) {
    seq_len(J) / 10 + offset
  }
  raw_dpm <- gen_effects(
    J = 10L,
    true_dist = "DPM",
    tau = 0.5,
    sigma_tau = 0.2,
    theta_G = list(offset = 1),
    g_fn = raw_g,
    g_returns = "raw"
  )
  raw_user <- gen_effects_user(
    J = 10L,
    tau = 0.5,
    sigma_tau = 0.2,
    g_fn = raw_g,
    g_returns = "raw",
    g_args = list(offset = 1)
  )

  expect_equal(raw_dpm, raw_user, tolerance = tol_deterministic)
})

test_that("Layer 1 dispatcher enforces dispatcher-specific validation", {
  expect_multisitedgp_error(
    gen_effects(J = 10L, upstream = tibble::tibble(x = 1)),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    gen_effects(J = 10L, true_dist = "Gaussian", g_fn = function(J) stats::rnorm(J)),
    "multisitedgp_coherence_error"
  )
})
# nolint end
