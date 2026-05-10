# nolint start: object_usage_linter
snapshot_internal <- function(name) {
  getFromNamespace(name, "multisiteDGP")
}

local_error_snapshot_output <- function() {
  testthat::local_reproducible_output(width = 80, unicode = FALSE)
  withr::local_options(list(cli.width = 80, width = 80))
}

snapshot_l1_upstream <- function(J = 25L) {
  tibble::tibble(
    site_index = seq_len(J),
    z_j = seq_len(J),
    tau_j = seq_len(J) / J
  )
}

snapshot_l2_upstream <- function(J = 25L) {
  tibble::tibble(
    site_index = seq_len(J),
    z_j = seq_len(J),
    tau_j = seq_len(J) / J,
    n_j = rep(20L, J),
    se2_j = rep(0.04, J),
    se_j = rep(0.20, J)
  )
}

snapshot_custom_dependence_upstream <- function() {
  se2_j <- c(0.04, 0.04, 0.09, 0.16)
  tibble::tibble(
    site_index = seq_along(se2_j),
    z_j = c(-2, -1, 1, 2),
    tau_j = c(2, 1, -1, -2),
    n_j = 101:104,
    se2_j = se2_j,
    se_j = sqrt(se2_j)
  )
}

snapshot_adapter_data <- function(seed = 8501L) {
  sim_multisite(J = 10L, seed = seed)
}

snapshot_solver_transform <- function(x) {
  gsub(
    "Maximum scaled residual was [^ ]+",
    "Maximum scaled residual was <residual>",
    x
  )
}

test_that("E01 snapshot records J lower-bound abort", {
  local_error_snapshot_output()
  expect_snapshot(multisitedgp_design(J = 5L), error = TRUE)
})

test_that("E02 snapshot records sigma_tau positivity abort", {
  local_error_snapshot_output()
  expect_snapshot(sim_multisite(sigma_tau = 0), error = TRUE)
})

test_that("E03 snapshot records nj_mean and nj_min relation abort", {
  local_error_snapshot_output()
  expect_snapshot(sim_multisite(nj_mean = 10, nj_min = 15L), error = TRUE)
})

test_that("E04 snapshot records Engine A1 dependence abort", {
  local_error_snapshot_output()
  expect_snapshot(
    multisitedgp_design(engine = "A1_legacy", dependence = "rank", rank_corr = 0.3),
    error = TRUE
  )
})

test_that("E05 snapshot records direct-design site-size argument abort", {
  local_error_snapshot_output()
  expect_snapshot(
    snapshot_internal(".validate_direct_args")(nj_mean = 40, cv = 0.5),
    error = TRUE
  )
})

test_that("E06 snapshot records g_fn and true_dist coherence abort", {
  local_error_snapshot_output()
  expect_snapshot(
    multisitedgp_design(true_dist = "Gaussian", g_fn = function(J) rep(0, J)),
    error = TRUE
  )
})

test_that("E07 snapshot records missing StudentT theta_G nu abort", {
  local_error_snapshot_output()
  expect_snapshot(
    multisitedgp_design(true_dist = "StudentT", theta_G = list(other = 1)),
    error = TRUE
  )
})

test_that("E08 snapshot records mutually exclusive correlation target abort", {
  local_error_snapshot_output()
  expect_snapshot(
    multisitedgp_design(rank_corr = 0.3, pearson_corr = 0.3),
    error = TRUE
  )
})

test_that("E09 snapshot records sim_meta wrong-door argument abort", {
  local_error_snapshot_output()
  expect_snapshot(sim_meta(nj_mean = 40, cv = 0.5), error = TRUE)
})

test_that("E10 snapshot records sim_multisite direct-argument abort", {
  local_error_snapshot_output()
  expect_snapshot(sim_multisite(I = 0.7), error = TRUE)
})

test_that("E11 snapshot records sim_meta missing-I abort", {
  local_error_snapshot_output()
  expect_snapshot(sim_meta(), error = TRUE)
})

test_that("E12 snapshot records ALD rho boundary abort", {
  local_error_snapshot_output()
  expect_snapshot(snapshot_internal(".validate_ald_rho")(0.99), error = TRUE)
})

test_that("E13 snapshot records Student-t nu boundary abort", {
  local_error_snapshot_output()
  expect_snapshot(gen_effects_studentt(J = 10L, nu = 1.5), error = TRUE)
})

test_that("E14 snapshot records user g_fn wrong-length abort", {
  local_error_snapshot_output()
  expect_snapshot(
    gen_effects_user(J = 10L, g_fn = function(J) stats::rnorm(J - 1L)),
    error = TRUE
  )
})

test_that("E15 snapshot records A2 truncated-Gamma solver abort", {
  local_error_snapshot_output()
  expect_snapshot(
    suppressWarnings(gen_site_sizes(
      snapshot_l1_upstream(),
      J = 25L,
      nj_mean = 6,
      cv = 1.8,
      nj_min = 5L,
      engine = "A2_modern"
    )),
    error = TRUE,
    transform = snapshot_solver_transform
  )
})

test_that("E16 snapshot records direct-I boundary abort", {
  local_error_snapshot_output()
  expect_snapshot(
    gen_se_direct(snapshot_l1_upstream(), J = 25L, I = 1.5),
    error = TRUE
  )
})

test_that("E17 snapshot records deterministic hill-climb sign-gate abort", {
  local_error_snapshot_output()
  expect_snapshot(
    snapshot_internal(".check_rank_fit")(
      list(achieved = 0.1, converged = FALSE),
      target = -0.3,
      tol = 0.02,
      J = 25L
    ),
    error = TRUE
  )
})

test_that("E18 snapshot records obs_fn wrong-length abort", {
  local_error_snapshot_output()
  expect_snapshot(
    gen_observations(
      snapshot_l2_upstream(),
      obs_fn = function(tau_j, se2_j) tau_j[-1L]
    ),
    error = TRUE
  )
})

test_that("E19 snapshot records missing sn package abort", {
  local_error_snapshot_output()
  testthat::local_mocked_bindings(
    .require_namespace = function(package) FALSE,
    .package = "multisiteDGP"
  )
  expect_snapshot(gen_effects_skewn(J = 10L, slant = 2), error = TRUE)
})

test_that("E20 snapshot records missing LaplacesDemon package abort", {
  local_error_snapshot_output()
  testthat::local_mocked_bindings(
    .require_namespace = function(package) FALSE,
    .package = "multisiteDGP"
  )
  expect_snapshot(gen_effects_ald(J = 10L, rho = 0.3), error = TRUE)
})

test_that("E21 snapshot records PointMassSlab pi0 boundary abort", {
  local_error_snapshot_output()
  expect_snapshot(gen_effects_pmslab(J = 10L, pi0 = 0), error = TRUE)
})

test_that("E22 snapshot records DPM v1 stub abort", {
  local_error_snapshot_output()
  expect_snapshot(gen_effects_dpm(J = 10L), error = TRUE)
})

test_that("E23 snapshot records target_marginal_rho v1 abort", {
  local_error_snapshot_output()
  expect_snapshot(multisitedgp_design(target_marginal_rho = 0.3), error = TRUE)
})

test_that("E24 snapshot records deferred PSD placeholder", {
  local_error_snapshot_output()
  expect_snapshot({
    cat("E24 PSD compatibility violation is deferred in v1; no active abort path.\n")
  })
})

test_that("E25 snapshot records custom dependence marginal violation abort", {
  local_error_snapshot_output()
  drift_hook <- function(z_j, se2_j, target) {
    list(se2_j = c(0.04, 0.09, 0.09, 0.16), perm = seq_along(se2_j))
  }
  expect_snapshot(
    align_rank_corr(
      snapshot_custom_dependence_upstream(),
      rank_corr = 0.3,
      dependence_fn = drift_hook
    ),
    error = TRUE
  )
})

test_that("E26 snapshot records missing metafor package abort", {
  local_error_snapshot_output()
  dat <- snapshot_adapter_data()
  testthat::local_mocked_bindings(
    .require_namespace = function(package) FALSE,
    .package = "multisiteDGP"
  )
  expect_snapshot(as_metafor(dat), error = TRUE)
})

test_that("E27 snapshot records missing baggr package abort", {
  local_error_snapshot_output()
  dat <- snapshot_adapter_data()
  testthat::local_mocked_bindings(
    .require_namespace = function(package) FALSE,
    .package = "multisiteDGP"
  )
  expect_snapshot(as_baggr(dat), error = TRUE)
})

test_that("E28 snapshot records feasibility warning-not-abort path", {
  local_error_snapshot_output()
  expect_snapshot(feasibility_index(rep(100, 10L), sigma_tau = 0.01, warn = TRUE))
})

test_that("E29 snapshot records site-size paradigm se_fn coherence abort", {
  local_error_snapshot_output()
  expect_snapshot(
    multisitedgp_design(se_fn = function(J) rep(0.1, J)),
    error = TRUE
  )
})

test_that("E30 snapshot records adapter reserved-name collision abort", {
  local_error_snapshot_output()
  dat <- sim_multisite(
    J = 10L,
    data = tibble::tibble(yi = seq_len(10L)),
    formula = ~yi,
    beta = 0,
    seed = 8502L
  )
  testthat::local_mocked_bindings(
    .require_namespace = function(package) TRUE,
    .package = "multisiteDGP"
  )
  expect_snapshot(as_metafor(dat), error = TRUE)
})
# nolint end
