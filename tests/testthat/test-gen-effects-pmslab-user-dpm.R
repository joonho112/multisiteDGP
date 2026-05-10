# nolint start: object_usage_linter
standardized_sequence_g <- function(J, reverse = FALSE) {
  z <- seq(-1, 1, length.out = J)
  z <- (z - mean(z)) / stats::sd(z)
  if (isTRUE(reverse)) {
    z <- -z
  }
  z
}

test_that("PMSlab generator returns schema and preserves decomposition", {
  data_x <- tibble::tibble(x = seq(-1, 1, length.out = 10L))
  out <- withr::with_seed(
    401L,
    gen_effects_pmslab(
      J = 10L,
      tau = 0.25,
      sigma_tau = 0.3,
      pi0 = 0.5,
      slab_shape = "Gaussian",
      formula = ~ x,
      beta = 0.75,
      data = data_x
    )
  )
  x_beta <- stats::model.matrix(~ x, data_x) %*% c(0, 0.75)

  expect_named(out, c("site_index", "z_j", "tau_j", "x"))
  expect_equal(out$tau_j - 0.25 - as.numeric(x_beta), 0.3 * out$z_j, tolerance = tol_deterministic)
  expect_true(any(out$z_j == 0))
})

test_that("T17 PMSlab standardization and zero mass match contract", {
  skip_if_not_slow()

  for (pi0_value in c(0.2, 0.5, 0.8)) {
    for (slab_shape in c("Gaussian", "Laplace")) {
      z_j <- withr::with_seed(
        410L + round(pi0_value * 100) + nchar(slab_shape),
        gen_effects_pmslab(
          J = default_n_property_large,
          pi0 = pi0_value,
          slab_shape = slab_shape
        )$z_j
      )
      expect_lt(abs(mean(z_j)), tol_mc_moment_n1e5)
      expect_lt(abs(stats::var(z_j) - 1), tol_mc_moment_n1e5)
      expect_lt(abs(mean(abs(z_j) < 1e-12) - pi0_value), 0.002)
    }
  }
})

test_that("PMSlab validation rejects invalid boundaries and accepts supported slabs", {
  expect_multisitedgp_error(
    gen_effects_pmslab(J = 10L),
    "multisitedgp_arg_error"
  )
  for (bad_pi0 in c(0, 1, -0.1, 1.1)) {
    expect_multisitedgp_error(
      gen_effects_pmslab(J = 10L, pi0 = bad_pi0),
      "multisitedgp_arg_error"
    )
  }
  expect_multisitedgp_error(
    gen_effects_pmslab(J = 10L, pi0 = 0.5, slab_shape = "Cauchy"),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    gen_effects_pmslab(J = 10L, pi0 = 0.5, sigma_slab = 0),
    "multisitedgp_arg_error"
  )
  expect_s3_class(gen_effects_pmslab(J = 10L, pi0 = 0.5, slab_shape = "Laplace"), "tbl_df")
  expect_s3_class(gen_effects_pmslab(J = 10L, pi0 = 0.5, slab_shape = "laplace"), "tbl_df")
})

test_that("PMSlab dispatcher and design validation are wired", {
  expect_true("gen_effects_pmslab" %in% getNamespaceExports("multisiteDGP"))

  routed <- withr::with_seed(
    402L,
    gen_effects(J = 10L, true_dist = "PointMassSlab", theta_G = list(pi0 = 0.5, slab_shape = "gaussian"))
  )
  direct <- withr::with_seed(402L, gen_effects_pmslab(J = 10L, pi0 = 0.5, slab_shape = "Gaussian"))

  expect_equal(routed, direct, tolerance = tol_deterministic)
  expect_s3_class(
    multisitedgp_design(J = 10L, true_dist = "PointMassSlab", theta_G = list(pi0 = 0.5)),
    "multisitedgp_design"
  )
  expect_multisitedgp_error(
    multisitedgp_design(J = 10L, true_dist = "PointMassSlab", theta_G = list(pi0 = 1)),
    "multisitedgp_arg_error"
  )
})

test_that("User generator standardized convention applies shared wrapper", {
  data_x <- tibble::tibble(x = seq(-1, 1, length.out = 10L))
  out <- gen_effects_user(
    J = 10L,
    tau = -0.2,
    sigma_tau = 0.4,
    g_fn = standardized_sequence_g,
    formula = ~ x,
    beta = 1.25,
    data = data_x
  )
  x_beta <- stats::model.matrix(~ x, data_x) %*% c(0, 1.25)

  expect_named(out, c("site_index", "z_j", "tau_j", "x"))
  expect_equal(out$tau_j - (-0.2) - as.numeric(x_beta), 0.4 * out$z_j, tolerance = tol_deterministic)
  expect_equal(mean(out$z_j), 0, tolerance = tol_deterministic)
})

test_that("User generator raw convention preserves final tau_j and derives z_j", {
  data_x <- tibble::tibble(x = seq(-1, 1, length.out = 10L))
  raw_fn <- function(J, tau, beta_x) {
    x <- seq(-1, 1, length.out = J)
    tau + beta_x * x + seq_len(J) / 100
  }
  out <- gen_effects_user(
    J = 10L,
    tau = 0.5,
    sigma_tau = 0.2,
    g_fn = raw_fn,
    g_returns = "raw",
    formula = ~ x,
    beta = 1.25,
    data = data_x,
    g_args = list(tau = 0.5, beta_x = 1.25)
  )
  expected_tau <- raw_fn(10L, tau = 0.5, beta_x = 1.25)
  x_beta <- stats::model.matrix(~ x, data_x) %*% c(0, 1.25)

  expect_equal(out$tau_j, expected_tau, tolerance = tol_deterministic)
  expect_equal(out$z_j, (expected_tau - 0.5 - as.numeric(x_beta)) / 0.2, tolerance = tol_deterministic)
})

test_that("User generator validates callback output and warns on audit drift", {
  expect_multisitedgp_error(
    gen_effects_user(J = 10L),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    gen_effects_user(J = 10L, g_fn = function(J) stats::rnorm(J - 1)),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    gen_effects_user(J = 10L, g_fn = function(J) c(Inf, stats::rnorm(J - 1))),
    "multisitedgp_arg_error"
  )
  expect_warning(
    out <- gen_effects_user(J = 10L, g_fn = function(J) stats::rnorm(J, sd = 5)),
    "audit"
  )
  expect_s3_class(out, "tbl_df")
})

test_that("User generator audit preserves caller RNG after the main callback draw", {
  rng_g <- function(J) stats::rnorm(J)

  set.seed(451L)
  out <- gen_effects_user(J = 25L, g_fn = rng_g, audit_g = TRUE)
  seed_after_audit <- .Random.seed

  set.seed(451L)
  expected_z <- rng_g(25L)
  seed_after_main_draw <- .Random.seed

  expect_identical(out$z_j, expected_z)
  expect_identical(seed_after_audit, seed_after_main_draw)

  out_audit <- withr::with_seed(452L, gen_effects_user(J = 25L, g_fn = rng_g, audit_g = TRUE))
  out_no_audit <- withr::with_seed(452L, gen_effects_user(J = 25L, g_fn = rng_g, audit_g = FALSE))
  expect_identical(out_audit$z_j, out_no_audit$z_j)
})

test_that("User generator audit still runs without advancing caller RNG", {
  seen_j <- integer()
  traced_g <- function(J) {
    seen_j <<- c(seen_j, J)
    stats::rnorm(J)
  }

  set.seed(454L)
  out <- gen_effects_user(J = 25L, g_fn = traced_g, audit_g = TRUE)
  seed_after_call <- .Random.seed

  set.seed(454L)
  expected_z <- stats::rnorm(25L)
  seed_after_main_draw <- .Random.seed

  expect_identical(seen_j, c(25L, 100000L))
  expect_identical(out$z_j, expected_z)
  expect_identical(seed_after_call, seed_after_main_draw)
})

test_that("User generator audit warning preserves caller RNG", {
  wide_g <- function(J) stats::rnorm(J, sd = 5)

  set.seed(455L)
  expect_warning(
    out <- gen_effects_user(J = 10L, g_fn = wide_g, audit_g = TRUE),
    "audit"
  )
  seed_after_call <- .Random.seed

  set.seed(455L)
  expected_z <- stats::rnorm(10L, sd = 5)
  seed_after_main_draw <- .Random.seed

  expect_identical(out$z_j, expected_z)
  expect_identical(seed_after_call, seed_after_main_draw)
})

test_that("User generator preflights shared arguments before callback invocation", {
  called <- FALSE
  traced_g <- function(J) {
    called <<- TRUE
    stats::rnorm(J)
  }

  expect_rng_neutral_error(
    gen_effects_user(
      J = 10L,
      g_fn = traced_g,
      formula = ~ x,
      beta = 1,
      data = tibble::tibble(x = seq_len(9L)),
      audit_g = FALSE
    ),
    "multisitedgp_arg_error"
  )
  expect_false(called)
})

test_that("User dispatcher uses theta_G as callback arguments", {
  expect_true("gen_effects_user" %in% getNamespaceExports("multisiteDGP"))

  routed <- gen_effects(
    J = 10L,
    true_dist = "User",
    g_fn = standardized_sequence_g,
    theta_G = list(reverse = TRUE)
  )
  direct <- gen_effects_user(
    J = 10L,
    g_fn = standardized_sequence_g,
    g_args = list(reverse = TRUE)
  )

  expect_equal(routed, direct, tolerance = tol_deterministic)
  expect_s3_class(
    multisitedgp_design(J = 10L, true_dist = "User", g_fn = standardized_sequence_g),
    "multisitedgp_design"
  )
  expect_multisitedgp_error(
    multisitedgp_design(J = 10L, true_dist = "User"),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    multisitedgp_design(J = 10L, true_dist = "Gaussian", g_fn = standardized_sequence_g),
    "multisitedgp_coherence_error"
  )
})

test_that("User dispatcher exposes audit_g passthrough", {
  rng_g <- function(J) stats::rnorm(J)

  set.seed(453L)
  routed <- gen_effects(J = 25L, g_fn = rng_g, audit_g = FALSE)
  seed_after_routed <- .Random.seed

  set.seed(453L)
  expected_z <- rng_g(25L)
  seed_after_main_draw <- .Random.seed

  expect_identical(routed$z_j, expected_z)
  expect_identical(seed_after_routed, seed_after_main_draw)
})

test_that("DPM bridge exposes audit_g passthrough", {
  rng_g <- function(J) stats::rnorm(J)

  set.seed(456L)
  bridged <- gen_effects(J = 25L, true_dist = "DPM", g_fn = rng_g, audit_g = FALSE)
  seed_after_bridged <- .Random.seed

  set.seed(456L)
  expected_z <- rng_g(25L)
  seed_after_main_draw <- .Random.seed

  expect_identical(bridged$z_j, expected_z)
  expect_identical(seed_after_bridged, seed_after_main_draw)
})

test_that("DPM stub hard-aborts unless bridged through g_fn", {
  expect_true("gen_effects_dpm" %in% getNamespaceExports("multisiteDGP"))

  expect_multisitedgp_error(
    gen_effects_dpm(J = 10L),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    gen_effects(J = 10L, true_dist = "DPM"),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    multisitedgp_design(J = 10L, true_dist = "DPM"),
    "multisitedgp_arg_error"
  )

  bridged <- gen_effects(
    J = 10L,
    true_dist = "DPM",
    g_fn = standardized_sequence_g,
    theta_G = list(reverse = TRUE)
  )
  user <- gen_effects_user(J = 10L, g_fn = standardized_sequence_g, g_args = list(reverse = TRUE))

  expect_equal(bridged, user, tolerance = tol_deterministic)
  expect_s3_class(
    multisitedgp_design(J = 10L, true_dist = "DPM", g_fn = standardized_sequence_g),
    "multisitedgp_design"
  )
})
# nolint end
