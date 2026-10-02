effect_frame <- function(z_j, ..., J = length(z_j)) {
  multisitedgp_internal(".new_effects_frame")(z_j = z_j, J = J, ...)
}

test_that("every site-effect generator is exported", {
  expected_exports <- c(
    "gen_effects",
    "gen_effects_gaussian",
    "gen_effects_studentt",
    "gen_effects_skewn",
    "gen_effects_ald",
    "gen_effects_mixture",
    "gen_effects_pmslab",
    "gen_effects_user"
  )

  expect_true(all(expected_exports %in% getNamespaceExports("multisiteDGP")))
})

test_that("gen_effects returns the standard columns for every distribution", {
  standardized_g <- function(J) {
    z <- seq(-1, 1, length.out = J)
    (z - mean(z)) / stats::sd(z)
  }
  shapes <- list(
    Gaussian = list(true_dist = "Gaussian", theta_G = list()),
    StudentT = list(true_dist = "StudentT", theta_G = list(nu = 5)),
    Mixture = list(true_dist = "Mixture", theta_G = list(delta = 5, eps = 0.3, ups = 2)),
    PointMassSlab = list(true_dist = "PointMassSlab", theta_G = list(pi0 = 0.5, slab_shape = "Gaussian")),
    User = list(true_dist = "User", theta_G = list(), g_fn = standardized_g),
    SkewN = list(true_dist = "SkewN", theta_G = list(slant = 2)),
    ALD = list(true_dist = "ALD", theta_G = list(rho = 0.5))
  )

  for (name in names(shapes)) {
    shape <- shapes[[name]]
    out <- withr::with_seed(
      501L,
      gen_effects(
        J = 25L,
        true_dist = shape$true_dist,
        theta_G = shape$theta_G,
        g_fn = shape$g_fn
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

test_that("gen_effects selects the User distribution when only g_fn is supplied", {
  standardized_g <- function(J) {
    z <- seq(-1, 1, length.out = J)
    (z - mean(z)) / stats::sd(z)
  }

  auto_user <- gen_effects(J = 25L, g_fn = standardized_g)
  explicit_user <- gen_effects(J = 25L, true_dist = "User", g_fn = standardized_g)
  expect_equal(auto_user, explicit_user, tolerance = tol_deterministic)
})

test_that("gen_effects passes covariate and g_fn arguments on to the generators", {
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

test_that("gen_effects rejects an invalid audit_g and a g_fn given with a named distribution", {
  expect_multisitedgp_error(
    gen_effects(J = 10L, audit_g = "yes"),
    regexp = "`audit_g` must be TRUE or FALSE"
  )
  expect_multisitedgp_error(
    gen_effects(J = 10L, true_dist = "Gaussian", g_fn = function(J) stats::rnorm(J)),
    regexp = "were both supplied"
  )
})

test_that("gen_effects keeps tau_j = tau + x beta + sigma_tau z_j for every distribution", {
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
  shapes <- list(
    Gaussian = list(true_dist = "Gaussian", theta_G = list()),
    StudentT = list(true_dist = "StudentT", theta_G = list(nu = 5)),
    Mixture = list(true_dist = "Mixture", theta_G = list(delta = 5, eps = 0.3, ups = 2)),
    PointMassSlab = list(true_dist = "PointMassSlab", theta_G = list(pi0 = 0.5, slab_shape = "Gaussian")),
    User = list(true_dist = "User", theta_G = list(reverse = TRUE), g_fn = standardized_g),
    SkewN = list(true_dist = "SkewN", theta_G = list(slant = 2)),
    ALD = list(true_dist = "ALD", theta_G = list(rho = 0.5))
  )

  for (name in names(shapes)) {
    shape <- shapes[[name]]
    out <- withr::with_seed(
      503L,
      gen_effects(
        J = 20L,
        true_dist = shape$true_dist,
        tau = -0.25,
        sigma_tau = 0.35,
        theta_G = shape$theta_G,
        formula = ~ x,
        beta = 0.75,
        data = data_x,
        g_fn = shape$g_fn
      )
    )

    expect_true("x" %in% names(out))
    expect_equal(out$x, data_x$x, tolerance = tol_deterministic)
    expect_equal(out$tau_j - (-0.25) - as.numeric(x_beta), 0.35 * out$z_j, tolerance = 1e-12)
  }
})

test_that("a formula without an intercept adds beta times the covariate to tau_j", {
  J <- 10L
  tau <- -0.08
  sigma_tau <- 0.2 * sqrt(2)
  beta <- 0.06
  data <- data.frame(x = seq_len(J))

  set.seed(123L)
  z <- stats::rnorm(J)
  set.seed(123L)
  out <- gen_effects_gaussian(
    J = J,
    tau = tau,
    sigma_tau = sigma_tau,
    formula = ~0 + x,
    beta = beta,
    data = data
  )

  expect_equal(out$tau_j, tau + beta * data$x + sigma_tau * z, tolerance = tol_deterministic)
})

test_that("with a formula without an intercept, a change in beta shifts tau_j by that change times the covariate", {
  J <- 10L
  data <- data.frame(x = seq_len(J))

  set.seed(321L)
  small_beta <- gen_effects_gaussian(
    J = J,
    sigma_tau = 0.2,
    formula = ~0 + x,
    beta = 0.06,
    data = data
  )
  set.seed(321L)
  large_beta <- gen_effects_gaussian(
    J = J,
    sigma_tau = 0.2,
    formula = ~0 + x,
    beta = 0.3,
    data = data
  )

  expect_equal(
    large_beta$tau_j - small_beta$tau_j,
    (0.3 - 0.06) * data$x,
    tolerance = tol_deterministic
  )
})

# `formula`, `beta` and `data` must agree with one another. The tests below
# cover the ways they can disagree and the forms of `beta` that are accepted.

test_that("beta or data without a formula is refused", {
  expect_multisitedgp_error(
    gen_effects(J = 20L, sigma_tau = 0.2, beta = 0.3)
  )
  expect_multisitedgp_error(
    gen_effects(J = 20L, sigma_tau = 0.2, data = school())
  )
})

test_that("formula must actually be a formula", {
  expect_multisitedgp_error(
    gen_effects(J = 20L, sigma_tau = 0.2, formula = "~ prior",
                beta = 0.3, data = school())
  )
})

test_that("a formula naming absent variables is refused with the modelling error", {
  expect_multisitedgp_error(
    gen_effects(J = 20L, sigma_tau = 0.2, formula = ~ no_such_column,
                beta = 0.3, data = school())
  )
})

test_that("beta must be finite numeric", {
  for (bad in list("0.3", c(0.3, NA), c(0.3, Inf), list(0.3))) {
    expect_multisitedgp_error(
      gen_effects(J = 20L, sigma_tau = 0.2, formula = ~ prior,
                  beta = bad, data = school())
    )
  }
})

test_that("a non-finite linear predictor is caught before it reaches the draw", {
  d <- school()
  d$prior[3L] <- Inf
  expect_multisitedgp_error(
    gen_effects(J = 20L, sigma_tau = 0.2, formula = ~ prior, beta = 0.3, data = d)
  )
})

test_that("named beta must name model-matrix columns", {
  expect_multisitedgp_error(
    gen_effects(J = 20L, sigma_tau = 0.2, formula = ~ prior,
                beta = c(nope = 0.3), data = school())
  )
  expect_multisitedgp_error(
    gen_effects(J = 20L, sigma_tau = 0.2, formula = ~ prior,
                beta = c(prior = 0.3, prior = 0.4), data = school())
  )
})

test_that("named beta must supply every non-intercept coefficient", {
  expect_multisitedgp_error(
    gen_effects(J = 20L, sigma_tau = 0.2, formula = ~ prior + urban,
                beta = c(prior = 0.3), data = school())
  )
})

test_that("a named beta is reordered into model-matrix order", {
  draw <- function(beta) {
    withr::with_seed(1L, gen_effects(J = 20L, sigma_tau = 0.2,
                                     formula = ~ prior + urban,
                                     beta = beta, data = school()))
  }
  expect_equal(draw(c(urban = 0.5, prior = 0.3))$tau_j,
               draw(c(0, 0.3, 0.5))$tau_j)
})

test_that("a named beta may omit the intercept, like an unnamed one", {
  draw <- function(beta) {
    withr::with_seed(1L, gen_effects(J = 20L, sigma_tau = 0.2, formula = ~ prior,
                                     beta = beta, data = school()))
  }
  expect_equal(draw(c(prior = 0.3))$tau_j, draw(0.3)$tau_j)
  expect_equal(draw(c(prior = 0.3))$tau_j, draw(c(0, 0.3))$tau_j)
})

test_that("an unnamed beta is accepted with or without the intercept slot", {
  draw <- function(beta) {
    withr::with_seed(1L, gen_effects(J = 20L, sigma_tau = 0.2, formula = ~ prior,
                                     beta = beta, data = school()))
  }
  expect_equal(draw(c(0, 0.3))$tau_j, draw(0.3)$tau_j)
})

test_that("the site-effects frame has site_index, z_j and tau_j", {
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

test_that("a zero beta gives the same tau_j as no formula", {
  z_j <- seq(-1, 1, length.out = 10)
  data_x <- tibble::tibble(x = withr::with_seed(504L, rnorm(10)), group = rep(c("A", "B"), 5))
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

test_that("the site-effects frame keeps tau_j = tau + x beta + sigma_tau z_j", {
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

test_that("the site-effects frame rejects mismatched z_j and covariate inputs", {
  z_j <- seq(-1, 1, length.out = 10)
  data_x <- tibble::tibble(x = seq_len(10))

  expect_multisitedgp_error(
    effect_frame(z_j[-1], J = 10L)
  )
  expect_multisitedgp_error(
    effect_frame(z_j, formula = ~ x, beta = 1)
  )
  expect_multisitedgp_error(
    effect_frame(z_j, formula = ~ x, data = data_x)
  )
  expect_multisitedgp_error(
    effect_frame(z_j, formula = ~ x, beta = 1, data = data_x[-1, ])
  )
  expect_multisitedgp_error(
    effect_frame(
      z_j,
      formula = ~ site_index,
      beta = 1,
      data = tibble::tibble(site_index = seq_len(10))
    )
  )
  expect_multisitedgp_error(
    effect_frame(z_j, formula = ~ x, beta = c(1, 2, 3), data = data_x)
  )
})

test_that("default site effects pass the Shapiro-Wilk test at the nominal rate", {
  # 4000 samples of 50 site effects, 200 from each of 20 simulations of 10000
  # sites. The count of p <= 0.01 is Binomial(4000, 0.01) with mean 40 and
  # standard error 6.3, so the band of 4 to 17 per 1000 samples is 3.8 standard
  # errors below the mean and 4.5 above it.
  n_sites <- 50L
  samples_per_simulation <- 200L
  seeds <- replication_seeds(20L, 820901L)
  shapiro_pvalues <- unlist(lapply(seeds, function(seed) {
    df <- sim_default(seed = seed, J = n_sites * samples_per_simulation)
    vapply(consecutive_samples(df$tau_j, n_sites), function(tau_j) {
      stats::shapiro.test(tau_j)$p.value
    }, numeric(1))
  }))
  count_low <- sum(shapiro_pvalues <= 0.01)
  # The band constants are counts per default_m_replications samples.
  band_scale <- length(shapiro_pvalues) / default_m_replications

  expect_gte(count_low, shapiro_p_leq_001_count_min * band_scale)
  expect_lte(count_low, shapiro_p_leq_001_count_max * band_scale)
})

test_that("default site effects pass the KS test against N(tau, sigma_tau^2) at the nominal rate", {
  # 1000 samples of 50 site effects, 50 from each of 20 simulations of 2500
  # sites. The count of p > 0.05 is Binomial(1000, 0.95) with standard error
  # 6.9, and the band is its central 99 percent interval. The moment tolerances
  # are 4 standard errors at the 50000 pooled effects.
  M <- default_m_replications
  n_sites <- 50L
  samples_per_simulation <- 50L
  seeds <- replication_seeds(M / samples_per_simulation, 820102L)
  args <- default_design_args()
  tau_values <- vector("list", length(seeds))
  ks_pvalues <- vector("list", length(seeds))
  for (i in seq_along(seeds)) {
    df <- sim_default(seed = seeds[[i]], J = n_sites * samples_per_simulation)
    tau_values[[i]] <- df$tau_j
    ks_pvalues[[i]] <- vapply(consecutive_samples(df$tau_j, n_sites), function(tau_j) {
      suppressWarnings(
        stats::ks.test(tau_j, "pnorm", mean = args$tau, sd = args$sigma_tau)$p.value
      )
    }, numeric(1))
  }
  ks_pvalues <- unlist(ks_pvalues)

  count_high <- sum(ks_pvalues > 0.05)
  expect_gte(count_high, stats::qbinom(0.005, M, 0.95))
  expect_lte(count_high, stats::qbinom(0.995, M, 0.95))

  tau_all <- unlist(tau_values, use.names = FALSE)
  mean_tolerance <- 4 * args$sigma_tau / sqrt(length(tau_all))
  var_tolerance <- 4 * args$sigma_tau^2 * sqrt(2 / (length(tau_all) - 1))
  expect_lt(abs(mean(tau_all) - args$tau), mean_tolerance)
  expect_lt(abs(stats::var(tau_all) - args$sigma_tau^2), var_tolerance)
})

test_that("`sim_multisite()` keeps tau_j = tau + x beta + sigma_tau z_j with a covariate", {
  data_x <- tibble::tibble(x = seq(-1, 1, length.out = 50L))
  args <- default_design_args(formula = ~ x, beta = 1.5, data = data_x)
  df <- do.call(sim_multisite, c(args, list(seed = 18L)))
  diagnostics <- attr(df, "diagnostics", exact = TRUE)
  x_beta <- stats::model.matrix(~ x, data_x) %*% c(0, 1.5)

  residual_lhs <- df$tau_j - args$tau - as.numeric(x_beta)
  residual_rhs <- args$sigma_tau * df$z_j

  expect_equal(residual_lhs, residual_rhs, tolerance = tol_deterministic)
  expect_true(is.finite(diagnostics$rho_S_residual))
  expect_true(is.finite(diagnostics$rho_S_marginal))
  expect_gt(abs(diagnostics$rho_S_marginal - diagnostics$rho_S_residual), 0)
})

test_that("`sim_multisite()` gives the same data with no formula and with a zero beta", {
  data_x <- tibble::tibble(x = seq(-1, 1, length.out = 50L))
  no_cov <- sim_default(seed = 820501L)
  beta_zero <- sim_default(seed = 820501L, formula = ~ x, beta = 0, data = data_x)

  for (column in names(no_cov)) {
    expect_equal(no_cov[[column]], beta_zero[[column]], tolerance = tol_deterministic)
  }
  expect_equal(beta_zero$x, data_x$x, tolerance = tol_deterministic)
})
