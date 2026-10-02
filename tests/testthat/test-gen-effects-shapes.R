standardized_sequence_g <- function(J, reverse = FALSE) {
  z <- seq(-1, 1, length.out = J)
  z <- (z - mean(z)) / stats::sd(z)
  if (isTRUE(reverse)) {
    z <- -z
  }
  z
}

# Sample size and tolerances of the Student-t moment checks. For nu > 4 they
# are 4 standard errors at n = 100000, with kurtosis 3 + 6 / (nu - 4). At
# nu = 3 there is no fourth moment, so the sample variance has no standard
# error to set a tolerance from: that case uses 1e6 draws and fixed tolerances.
# At nu = 5 one extreme draw can move the variance further than a normal
# approximation allows: about 2 seeds in 1000 exceed the 4 standard errors.
studentt_moment_check <- function(nu) {
  if (nu > 4) {
    n <- n_moment_check
    list(n = n, mean = 4 * se_mean(n), variance = 4 * se_variance(n, kurtosis = 3 + 6 / (nu - 4)))
  } else {
    list(n = default_n_property_large, mean = tol_mc_moment_n1e6, variance = tol_studentt_inf_kurt_var)
  }
}

# Kurtosis of the skew-normal: 3 at slant 0, rising to 3.63 at slant 4 or -4.
skewn_kurtosis <- function(slant) {
  delta_sq <- slant^2 / (1 + slant^2)
  3 + 2 * (pi - 3) * (2 * delta_sq / pi)^2 / (1 - 2 * delta_sq / pi)^2
}

# Kurtosis of the point mass and slab. The slab has variance 1 / (1 - pi0), so
# the fourth moment of z_j is the kurtosis of the slab (3 for a Gaussian, 6 for
# a Laplace) over 1 - pi0.
pmslab_kurtosis <- function(pi0, slab_shape) {
  c(Gaussian = 3, Laplace = 6)[[slab_shape]] / (1 - pi0)
}

test_that("there are six built-in effect distributions plus User", {
  builtins <- multisitedgp_internal(".builtin_g_shapes")()
  generated <- multisitedgp_internal(".generated_g_shapes")()

  expect_identical(
    builtins,
    c("Gaussian", "StudentT", "SkewN", "ALD", "Mixture", "PointMassSlab")
  )
  expect_identical(generated, c(builtins, "User"))
  expect_length(builtins, 6L)
  expect_length(generated, 7L)
})

test_that("the design constructor accepts every effect distribution", {
  for (shape in multisitedgp_internal(".generated_g_shapes")()) {
    if (identical(shape, "User")) {
      design <- multisitedgp_design(
        J = 10L,
        true_dist = shape,
        g_fn = function(J, ...) seq_len(J)
      )
    } else {
      theta <- switch(
        shape,
        StudentT = list(nu = 5),
        SkewN = list(slant = 2),
        ALD = list(rho = 0.3),
        Mixture = list(delta = 5, eps = 0.3, ups = 2),
        PointMassSlab = list(pi0 = 0.3),
        list()
      )
      design <- multisitedgp_design(J = 10L, true_dist = shape, theta_G = theta)
    }
    expect_identical(design$true_dist, shape)
  }
})

test_that("Gaussian generator returns the standard columns and z_j equal to rnorm(J) at the same seed", {
  out <- withr::with_seed(101L, gen_effects_gaussian(J = 10L, tau = 0.5, sigma_tau = 0.2))
  expected_z <- withr::with_seed(101L, stats::rnorm(10L))

  expect_s3_class(out, "tbl_df")
  expect_identical(names(out), c("site_index", "z_j", "tau_j"))
  expect_identical(out$site_index, seq_len(10L))
  expect_equal(out$z_j, expected_z, tolerance = tol_deterministic)
  expect_equal(out$tau_j, 0.5 + 0.2 * expected_z, tolerance = tol_deterministic)
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

test_that("invalid arguments shared by the generators do not advance the caller's RNG", {
  data_x <- tibble::tibble(x = seq_len(10L))
  short_data_x <- tibble::tibble(x = seq_len(9L))

  expect_rng_neutral_error(
    gen_effects_gaussian(J = 10L, tau = NA_real_)
  )
  expect_rng_neutral_error(
    gen_effects_gaussian(J = 10L, sigma_tau = 0)
  )
  expect_rng_neutral_error(
    gen_effects_gaussian(J = 10L, formula = ~ x, beta = 1)
  )
  expect_rng_neutral_error(
    gen_effects_gaussian(J = 10L, formula = ~ x, data = data_x)
  )
  expect_rng_neutral_error(
    gen_effects_gaussian(J = 10L, formula = ~ x, beta = 1, data = short_data_x)
  )
  expect_rng_neutral_error(
    gen_effects_gaussian(
      J = 10L,
      formula = ~ site_index,
      beta = 1,
      data = tibble::tibble(site_index = seq_len(10L))
    )
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
  withr::local_seed(108L)
  expect_multisitedgp_error(
    gen_effects_studentt(J = 10L)
  )
  expect_multisitedgp_error(
    gen_effects_studentt(J = 10L, nu = 2)
  )
  expect_warning(
    out <- gen_effects_studentt(J = 10L, nu = 3),
    "infinite kurtosis"
  )
  expect_s3_class(out, "tbl_df")
})

test_that("the kurtosis warning boundary is nu <= 4, not nu < 4", {
  withr::local_seed(109L)
  # Excess kurtosis of a t is 6 / (nu - 4), which diverges at nu = 4 as well
  # as below it.
  expect_warning(
    gen_effects_studentt(J = 10L, nu = 4),
    "infinite kurtosis"
  )
  expect_silent(gen_effects_studentt(J = 10L, nu = 4.5))

  # The suggested fix must not send the user back into the range that warns.
  w <- tryCatch(gen_effects_studentt(J = 10L, nu = 4), warning = function(w) w)
  expect_match(conditionMessage(w), "nu > 4", fixed = TRUE)
  expect_no_match(conditionMessage(w), "nu >= 4", fixed = TRUE)
})

test_that("Gaussian and Student-t z_j have mean 0 and variance 1 in large samples", {
  # The Gaussian tolerances are 4 standard errors at n = 100000, with kurtosis 3.
  n <- n_moment_check
  gaussian <- withr::with_seed(104L, gen_effects_gaussian(J = n))

  expect_lt(abs(mean(gaussian$z_j)), 4 * se_mean(n))
  expect_lt(abs(stats::var(gaussian$z_j) - 1), 4 * se_variance(n, kurtosis = 3))

  # The tolerances for nu = 5 and 10 are 4 standard errors at n = 100000.
  # studentt_moment_check() explains the case nu = 3.
  for (nu in c(3, 5, 10)) {
    check <- studentt_moment_check(nu)
    studentt <- withr::with_seed(105L + nu, suppressWarnings(
      gen_effects_studentt(J = check$n, nu = nu)
    ))
    expect_lt(abs(mean(studentt$z_j)), check$mean)
    expect_lt(abs(stats::var(studentt$z_j) - 1), check$variance)
  }
})

test_that("Student-t z_j has mean 0 and variance 1 for nu = 3, 5 and 10", {
  # The tolerances for nu = 5 and 10 are 4 standard errors at n = 100000.
  # studentt_moment_check() explains the case nu = 3.
  for (nu in c(3, 5, 10)) {
    check <- studentt_moment_check(nu)
    z_j <- withr::with_seed(831500L + nu, suppressWarnings(
      gen_effects_studentt(J = check$n, nu = nu)
    )$z_j)

    expect_lt(abs(mean(z_j)), check$mean)
    expect_lt(abs(stats::var(z_j) - 1), check$variance)
  }
})

test_that("gen_effects routes Gaussian and StudentT to their generators", {
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

test_that("SkewN generator returns the standard columns and preserves the covariate decomposition", {
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

test_that("ALD generator returns the standard columns and preserves the covariate decomposition", {
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

test_that("SkewN z_j has mean 0 and variance 1, and its skewness has the sign of slant", {
  # Tolerances are 4 standard errors at n = 100000. The skewness of a Gaussian
  # sample has standard error sqrt(6 / n).
  n <- n_moment_check
  for (slant in c(-4, -1, 0, 1, 4)) {
    z_j <- withr::with_seed(210L + slant, suppressWarnings(
      gen_effects_skewn(J = n, slant = slant)$z_j
    ))
    expect_lt(abs(mean(z_j)), 4 * se_mean(n))
    expect_lt(abs(stats::var(z_j) - 1), 4 * se_variance(n, skewn_kurtosis(slant)))

    skew <- sample_skewness(z_j)
    if (slant == 0) {
      expect_lt(abs(skew), 4 * sqrt(6 / n))
    } else {
      expect_identical(sign(skew), sign(slant))
    }
  }
})

test_that("SkewN z_j is standardized and skewed in the direction of slant, with a second set of seeds", {
  # Tolerances are 4 standard errors at n = 100000. The skewness of a Gaussian
  # sample has standard error sqrt(6 / n).
  n <- n_moment_check
  slants <- c(-4, -1, 0, 1, 4)
  for (slant in slants) {
    z_j <- withr::with_seed(831600L + match(slant, slants), suppressWarnings(
      gen_effects_skewn(J = n, slant = slant)
    )$z_j)
    skew <- sample_skewness(z_j)

    expect_lt(abs(mean(z_j)), 4 * se_mean(n))
    expect_lt(abs(stats::var(z_j) - 1), 4 * se_variance(n, skewn_kurtosis(slant)))
    if (identical(slant, 0)) {
      expect_lt(abs(skew), 4 * sqrt(6 / n))
    } else {
      expect_identical(sign(skew), sign(slant))
    }
  }
})

test_that("ALD z_j has mean 0 and variance 1, and is right-skewed for rho below 0.5", {
  # Tolerances are 4 standard errors at n = 100000. The ALD is a difference of
  # two exponentials, which gives the kurtosis below: 6 at rho = 0.5, 7.9 at
  # rho = 0.25.
  n <- n_moment_check
  for (rho in c(0.25, 0.5, 0.7)) {
    z_j <- withr::with_seed(220L + round(100 * rho), gen_effects_ald(
      J = n,
      rho = rho
    )$z_j)
    kurtosis <- 3 + 6 * (rho^4 + (1 - rho)^4) / (rho^2 + (1 - rho)^2)^2
    expect_lt(abs(mean(z_j)), 4 * se_mean(n))
    expect_lt(abs(stats::var(z_j) - 1), 4 * se_variance(n, kurtosis))
    skewness <- mean((z_j - mean(z_j))^3) / stats::sd(z_j)^3
    if (rho < 0.5) {
      expect_gt(skewness, 0)
    } else if (rho > 0.5) {
      expect_lt(skewness, 0)
    }
  }
})

test_that("SkewN validation warns and aborts with classed errors", {
  withr::local_seed(203L)
  expect_multisitedgp_error(
    gen_effects_skewn(J = 10L)
  )
  expect_warning(
    out_zero <- gen_effects_skewn(J = 10L, slant = 0),
    "`slant = 0` gives the normal distribution"
  )
  expect_s3_class(out_zero, "tbl_df")
  expect_warning(
    out_large <- gen_effects_skewn(J = 10L, slant = 31),
    "half-normal"
  )
  expect_s3_class(out_large, "tbl_df")
})

test_that("ALD validation rejects boundary rho values", {
  expect_multisitedgp_error(
    gen_effects_ald(J = 10L)
  )
  for (rho in c(0.05, 0.95, 0, 1)) {
    expect_multisitedgp_error(
      gen_effects_ald(J = 10L, rho = rho)
    )
  }
})

test_that("ALD and SkewN name the parameter they are missing", {
  expect_multisitedgp_error(gen_effects_ald(J = 10L))
  expect_multisitedgp_error(gen_effects_skewn(J = 10L))

  expect_match(
    tryCatch(gen_effects_ald(J = 10L), error = conditionMessage),
    "rho", fixed = TRUE
  )
  expect_match(
    tryCatch(gen_effects_skewn(J = 10L), error = conditionMessage),
    "slant", fixed = TRUE
  )
})

test_that("SkewN and ALD generators need no optional package", {
  withr::local_seed(204L)

  # A generator that needed another package would load it.
  loaded_before <- loadedNamespaces()
  skewn <- gen_effects_skewn(J = 10L, slant = 1)
  ald <- gen_effects_ald(J = 10L, rho = 0.3)
  loaded_after <- loadedNamespaces()

  expect_identical(setdiff(loaded_after, loaded_before), character())
  expect_s3_class(skewn, "tbl_df")
  expect_s3_class(ald, "tbl_df")
})

test_that("gen_effects routes SkewN and ALD to their generators", {
  expect_true(all(c("gen_effects_skewn", "gen_effects_ald") %in% getNamespaceExports("multisiteDGP")))

  skewn <- withr::with_seed(230L, gen_effects(J = 10L, true_dist = "SkewN", theta_G = list(slant = 2)))
  direct_skewn <- withr::with_seed(230L, gen_effects_skewn(J = 10L, slant = 2))
  ald <- withr::with_seed(231L, gen_effects(J = 10L, true_dist = "ALD", theta_G = list(rho = 0.3)))
  direct_ald <- withr::with_seed(231L, gen_effects_ald(J = 10L, rho = 0.3))

  expect_equal(skewn, direct_skewn, tolerance = tol_deterministic)
  expect_equal(ald, direct_ald, tolerance = tol_deterministic)
})

test_that("the design constructor checks the SkewN and ALD parameters", {
  expect_multisitedgp_error(
    multisitedgp_design(true_dist = "SkewN")
  )
  expect_multisitedgp_error(
    multisitedgp_design(true_dist = "ALD")
  )
  expect_multisitedgp_error(
    multisitedgp_design(true_dist = "ALD", theta_G = list(rho = 0.05))
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

test_that("Mixture generator returns the latent component and draws in the order of the JEBS code", {
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

test_that("Mixture z_j has mean 0 and variance 1, and the second component has weight eps", {
  # Tolerances are 4 standard errors at n = 100000. Before the division by
  # sqrt(7.15) the components have means -1.5 and 3.5 and variances 1 and 4,
  # which gives the kurtosis below, 3.18.
  n <- n_moment_check
  kurtosis <- (0.7 * (1.5^4 + 6 * 1.5^2 + 3) + 0.3 * (3.5^4 + 6 * 3.5^2 * 4 + 3 * 4^2)) / 7.15^2
  for (seed in c(42L, 1L, 2024L)) {
    out <- withr::with_seed(
      seed,
      gen_effects(J = n, true_dist = "Mixture", theta_G = list(delta = 5, eps = 0.3, ups = 2))
    )
    expect_true(abs(stats::var(out$z_j) - 1) < 4 * se_variance(n, kurtosis), info = sprintf("seed = %d", seed))
    expect_true(abs(mean(out$z_j)) < 4 * se_mean(n), info = sprintf("mean, seed = %d", seed))
    expect_true(all(out$latent_component %in% c(1L, 2L)))
    expect_lt(abs(mean(out$latent_component == 2L) - 0.3), 4 * se_proportion(n, 0.3))
  }
})

test_that("Mixture validation rejects missing and invalid parameters", {
  withr::local_seed(304L)
  expect_multisitedgp_error(
    gen_effects_mixture(J = 10L, eps = 0.3, ups = 2)
  )
  expect_multisitedgp_error(
    gen_effects_mixture(J = 10L, delta = 5, ups = 2)
  )
  expect_multisitedgp_error(
    gen_effects_mixture(J = 10L, delta = 5, eps = 0.3)
  )
  for (bad_delta in c(0, -1)) {
    expect_multisitedgp_error(
      gen_effects_mixture(J = 10L, delta = bad_delta, eps = 0.3, ups = 2)
    )
  }
  for (bad_eps in c(0, 1, -0.1, 1.1)) {
    expect_multisitedgp_error(
      gen_effects_mixture(J = 10L, delta = 5, eps = bad_eps, ups = 2)
    )
  }
  for (bad_ups in c(0, -1)) {
    expect_multisitedgp_error(
      gen_effects_mixture(J = 10L, delta = 5, eps = 0.3, ups = bad_ups)
    )
  }
  expect_s3_class(
    gen_effects_mixture(J = 10L, delta = 5, eps = 0.3, ups = 0.5),
    "tbl_df"
  )
})

test_that("gen_effects and the design constructor accept and check the Mixture parameters", {
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
    multisitedgp_design(J = 10L, true_dist = "Mixture", theta_G = list(delta = 5, eps = 0.3))
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
    )
  )
})

test_that("PMSlab generator returns the standard columns and preserves the covariate decomposition", {
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

test_that("PMSlab z_j has mean 0, variance 1 and a point mass of pi0 at zero", {
  # Tolerances are 4 standard errors at n = 100000: of the mean, of the
  # variance, and of the proportion of zeros.
  n <- n_moment_check
  for (pi0_value in c(0.2, 0.5, 0.8)) {
    for (slab_shape in c("Gaussian", "Laplace")) {
      z_j <- withr::with_seed(
        410L + round(pi0_value * 100) + nchar(slab_shape),
        gen_effects_pmslab(
          J = n,
          pi0 = pi0_value,
          slab_shape = slab_shape
        )$z_j
      )
      kurtosis <- pmslab_kurtosis(pi0_value, slab_shape)
      expect_lt(abs(mean(z_j)), 4 * se_mean(n))
      expect_lt(abs(stats::var(z_j) - 1), 4 * se_variance(n, kurtosis))
      expect_lt(abs(mean(abs(z_j) < 1e-12) - pi0_value), 4 * se_proportion(n, pi0_value))
    }
  }
})

test_that("PMSlab z_j is standardized, with point mass pi0 and a slab centered at zero", {
  # Tolerances are 4 standard errors at n = 100000. The mean of the slab values
  # has standard error 1 / sqrt(1 - pi0) over the root of their number. Its
  # tolerance is 0.02 where 4 standard errors are less than that.
  n <- n_moment_check
  for (pi0_value in c(0.2, 0.5, 0.8)) {
    for (slab_shape in c("Gaussian", "Laplace")) {
      z_j <- withr::with_seed(
        831700L + round(pi0_value * 100) + nchar(slab_shape),
        gen_effects_pmslab(
          J = n,
          pi0 = pi0_value,
          slab_shape = slab_shape
        )$z_j
      )
      slab_values <- z_j[abs(z_j) >= 1e-12]
      kurtosis <- pmslab_kurtosis(pi0_value, slab_shape)
      se_slab_mean <- 1 / sqrt((1 - pi0_value) * length(slab_values))

      expect_lt(abs(mean(z_j)), 4 * se_mean(n))
      expect_lt(abs(stats::var(z_j) - 1), 4 * se_variance(n, kurtosis))
      expect_lt(abs(mean(abs(z_j) < 1e-12) - pi0_value), 4 * se_proportion(n, pi0_value))
      expect_lt(abs(mean(slab_values)), max(0.02, 4 * se_slab_mean))
    }
  }
})

test_that("PMSlab validation rejects invalid boundaries and accepts supported slabs", {
  withr::local_seed(403L)
  expect_multisitedgp_error(
    gen_effects_pmslab(J = 10L)
  )
  for (bad_pi0 in c(0, 1, -0.1, 1.1)) {
    expect_multisitedgp_error(
      gen_effects_pmslab(J = 10L, pi0 = bad_pi0)
    )
  }
  expect_multisitedgp_error(
    gen_effects_pmslab(J = 10L, pi0 = 0.5, slab_shape = "Cauchy")
  )
  expect_multisitedgp_error(
    gen_effects_pmslab(J = 10L, pi0 = 0.5, sigma_slab = 0)
  )
  expect_s3_class(gen_effects_pmslab(J = 10L, pi0 = 0.5, slab_shape = "Laplace"), "tbl_df")
  expect_s3_class(gen_effects_pmslab(J = 10L, pi0 = 0.5, slab_shape = "laplace"), "tbl_df")
})

test_that("gen_effects and the design constructor accept and check the PointMassSlab parameters", {
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
    multisitedgp_design(J = 10L, true_dist = "PointMassSlab", theta_G = list(pi0 = 1))
  )
})

test_that("finite parameters that overflow or cancel are refused before any draw, by every function", {
  cases <- list(
    list(true_dist = "SkewN", theta_G = list(slant = 1e160), parameter = "slant"),
    list(true_dist = "SkewN", theta_G = list(slant = -1e160), parameter = "slant"),
    list(true_dist = "Mixture", theta_G = list(delta = 1e160, eps = 0.5, ups = 1), parameter = "delta"),
    list(true_dist = "Mixture", theta_G = list(delta = 5, eps = 0.3, ups = 1e160), parameter = "ups"),
    list(true_dist = "PointMassSlab", theta_G = list(pi0 = 0.3, mu_slab = 1e20), parameter = "sigma_slab"),
    list(
      true_dist = "PointMassSlab", parameter = "sigma_slab",
      theta_G = list(pi0 = 0.3, slab_shape = "Laplace", mu_slab = -1e20)
    ),
    list(
      true_dist = "PointMassSlab", parameter = "sigma_slab",
      theta_G = list(pi0 = 0.3, sigma_slab = .Machine$double.xmin / 2)
    )
  )
  generators <- list(SkewN = gen_effects_skewn, Mixture = gen_effects_mixture, PointMassSlab = gen_effects_pmslab)

  for (case in cases) {
    generator <- generators[[case$true_dist]]
    expect_rng_neutral_error(do.call(generator, c(list(J = 10L), case$theta_G)), regexp = case$parameter)
    by_name <- expect_rng_neutral_error(
      gen_effects(J = 10L, true_dist = case$true_dist, theta_G = case$theta_G),
      regexp = case$parameter
    )
    expect_match(conditionMessage(by_name), "theta_G$", fixed = TRUE)
    expect_rng_neutral_error(
      multisitedgp_design(J = 10L, true_dist = case$true_dist, theta_G = case$theta_G),
      regexp = case$parameter
    )
    # Without a seed, a draw before the error would move the caller's RNG.
    expect_rng_neutral_error(
      sim_multisite(J = 10L, true_dist = case$true_dist, theta_G = case$theta_G),
      regexp = case$parameter
    )
    expect_rng_neutral_error(
      sim_meta(J = 10L, I = 0.5, R = 2, true_dist = case$true_dist, theta_G = case$theta_G),
      regexp = case$parameter
    )
  }
})

test_that("a slab far from zero is accepted when its scale can be told apart from its location", {
  for (slab_shape in c("Gaussian", "Laplace")) {
    out <- withr::with_seed(
      17L,
      gen_effects_pmslab(J = 10L, pi0 = 0.3, slab_shape = slab_shape, mu_slab = 1e8, sigma_slab = 1)
    )
    expect_s3_class(out, "tbl_df")
    expect_true(all(is.finite(out$z_j)))
  }
})

test_that("a sample in which every site is in the point mass is returned as zeros", {
  for (slab_shape in c("Gaussian", "Laplace")) {
    out <- withr::with_seed(
      1L,
      gen_effects_pmslab(J = 10L, pi0 = 1 - .Machine$double.eps, slab_shape = slab_shape)
    )
    expect_identical(out$z_j, numeric(10L))
  }
})

test_that("User generator treats a standardized g_fn like a built-in distribution", {
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

test_that("User generator with raw g_fn output keeps tau_j and derives z_j", {
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

test_that("User generator validates the g_fn output and warns when the audit finds it is not standardized", {
  withr::local_seed(456L)
  expect_multisitedgp_error(
    gen_effects_user(J = 10L)
  )
  expect_multisitedgp_error(
    gen_effects_user(J = 10L, g_fn = function(J) stats::rnorm(J - 1))
  )
  expect_multisitedgp_error(
    gen_effects_user(J = 10L, g_fn = function(J) c(Inf, stats::rnorm(J - 1)))
  )
  expect_warning(
    out <- gen_effects_user(J = 10L, g_fn = function(J) stats::rnorm(J, sd = 5)),
    "does not seem to return draws with mean 0 and variance 1"
  )
  expect_s3_class(out, "tbl_df")
})

test_that("an error raised inside g_fn is reported with its own message", {
  err <- tryCatch(
    gen_effects(J = 10L, sigma_tau = 0.2,
                g_fn = function(J) stop("callback exploded")),
    error = conditionMessage
  )
  expect_match(err, "callback exploded", fixed = TRUE)
})

test_that("User generator audit leaves the caller's RNG where the main g_fn draw left it", {
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
    "does not seem to return draws with mean 0 and variance 1"
  )
  seed_after_call <- .Random.seed

  set.seed(455L)
  expected_z <- stats::rnorm(10L, sd = 5)
  seed_after_main_draw <- .Random.seed

  expect_identical(out$z_j, expected_z)
  expect_identical(seed_after_call, seed_after_main_draw)
})

test_that("User generator checks the shared arguments before it calls g_fn", {
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
    )
  )
  expect_false(called)
})

test_that("gen_effects passes theta_G to g_fn as its arguments", {
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
    regexp = "`g_fn` is required"
  )
  expect_multisitedgp_error(
    multisitedgp_design(J = 10L, true_dist = "Gaussian", g_fn = standardized_sequence_g),
    regexp = "were both supplied"
  )
})

test_that("gen_effects passes audit_g on to the User generator", {
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
