# nolint start: object_usage_linter
old_gen_priorG2_gaussian <- function(
  J,
  sigma_tau,
  tau,
  variance,
  formula = NULL,
  beta = NULL,
  data = NULL
) {
  site_means <- rep(tau, J)
  if (!is.null(formula)) {
    site_means <- site_means + as.vector(stats::model.matrix(formula, data) %*% beta)
  }
  sigma_tau * stats::rnorm(J, mean = site_means, sd = sqrt(variance))
}

test_that("siteBayes2 gen_priorG2 Gaussian scale conversion preserves raw effects", {
  J <- 10L
  sigma_old <- 0.2
  tau_old <- -0.4
  variance_old <- 2
  beta_old <- 0.3
  data <- data.frame(x = seq_len(J))

  set.seed(123L)
  old <- old_gen_priorG2_gaussian(
    J = J,
    sigma_tau = sigma_old,
    tau = tau_old,
    variance = variance_old,
    formula = ~0 + x,
    beta = beta_old,
    data = data
  )
  set.seed(123L)
  new <- gen_effects_gaussian(
    J = J,
    tau = sigma_old * tau_old,
    sigma_tau = sigma_old * sqrt(variance_old),
    formula = ~0 + x,
    beta = sigma_old * beta_old,
    data = data
  )

  expect_equal(new$tau_j, old, tolerance = tol_deterministic)
  expect_equal(beta_old * sigma_old, 0.06, tolerance = tol_deterministic)
})

test_that("copying old beta without conversion changes the migrated DGP", {
  J <- 10L
  sigma_old <- 0.2
  beta_old <- 0.3
  data <- data.frame(x = seq_len(J))

  set.seed(321L)
  converted <- gen_effects_gaussian(
    J = J,
    sigma_tau = sigma_old,
    formula = ~0 + x,
    beta = sigma_old * beta_old,
    data = data
  )
  set.seed(321L)
  copied <- gen_effects_gaussian(
    J = J,
    sigma_tau = sigma_old,
    formula = ~0 + x,
    beta = beta_old,
    data = data
  )

  expect_equal(
    copied$tau_j - converted$tau_j,
    (1 - sigma_old) * beta_old * data$x,
    tolerance = tol_deterministic
  )
})

test_that("siteBayes2 ALD kappa converts to the new Yu-Zhang quantile", {
  kappa_old <- c(0.5, 1, 2)
  rho_new <- kappa_old^2 / (1 + kappa_old^2)

  expect_equal(rho_new, c(0.2, 0.5, 0.8), tolerance = tol_deterministic)
  expect_equal(sqrt(rho_new / (1 - rho_new)), kappa_old, tolerance = tol_deterministic)
})

test_that("old FALSE shuffle parity requires the A1 legacy observation path", {
  effects <- withr::with_seed(455L, gen_effects_gaussian(J = 20L))
  a1 <- withr::with_seed(456L, gen_site_sizes(
    effects,
    J = 20L,
    nj_mean = 40,
    cv = 0.5,
    engine = "A1_legacy"
  ))

  set.seed(457L)
  observed <- gen_observations(a1)
  observed_after <- .Random.seed
  set.seed(457L)
  expected_perm <- sample.int(nrow(a1), size = nrow(a1))
  expected_hat <- stats::rnorm(
    nrow(a1),
    mean = a1$tau_j,
    sd = sqrt(a1$se2_j[expected_perm])
  )
  expected_after <- .Random.seed

  expect_identical(
    attr(observed, "observation_permutation_perm", exact = TRUE),
    expected_perm
  )
  expect_identical(observed$se2_j, a1$se2_j[expected_perm])
  expect_identical(observed$tau_j_hat, expected_hat)
  expect_identical(observed_after, expected_after)

  a2 <- withr::with_seed(458L, gen_site_sizes(
    effects,
    J = 20L,
    nj_mean = 40,
    cv = 0.5,
    engine = "A2_modern"
  ))
  set.seed(459L)
  modern <- gen_observations(a2)
  modern_after <- .Random.seed
  set.seed(459L)
  modern_expected <- stats::rnorm(nrow(a2), mean = a2$tau_j, sd = sqrt(a2$se2_j))

  expect_null(attr(modern, "observation_permutation_perm", exact = TRUE))
  expect_identical(modern$se2_j, a2$se2_j)
  expect_identical(modern$tau_j_hat, modern_expected)
  expect_identical(modern_after, .Random.seed)
})

test_that("M8 names real predecessor exports and explicit conversion rules", {
  path <- test_path("../../vignettes/m8-migration-from-siteBayes2.Rmd")
  skip_if_not(file.exists(path), "Source vignette is absent from the installed test context.")
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(text, "tau_new       <- sigma_old * tau_old", fixed = TRUE)
  expect_match(text, "beta_new      <- sigma_old * beta_old", fixed = TRUE)
  expect_match(text, "rho_old^2 / (1 + rho_old^2)", fixed = TRUE)
  expect_match(text, "build_standata_rubin_normal()", fixed = TRUE)
  expect_match(text, "prepare_prior_settings_rubin_normal()", fixed = TRUE)
  expect_false(grepl("data_prep_rubin_normal", text, fixed = TRUE))
  expect_false(grepl("pp_check_rubin_normal", text, fixed = TRUE))
  expect_false(grepl("loo_rubin_normal", text, fixed = TRUE))
})
# nolint end
