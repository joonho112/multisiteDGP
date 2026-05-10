# nolint start: object_usage_linter
test_that("T19 PMSlab ties preserve Spearman rank target on A2 modern path", {
  df <- sim_multisite(
    J = 100L,
    true_dist = "PointMassSlab",
    theta_G = list(pi0 = 0.5, slab_shape = "Gaussian"),
    sigma_tau = 0.20,
    nj_mean = 80,
    cv = 0.50,
    nj_min = 5L,
    p = 0.5,
    R2 = 0,
    engine = "A2_modern",
    dependence = "rank",
    rank_corr = 0.3,
    seed = 19L
  )
  achieved <- stats::cor(df$z_j, df$se2_j, method = "spearman", use = "complete.obs")
  dependence_diag <- attr(df, "diagnostics", exact = TRUE)$dependence_diagnostics

  expect_true(any(duplicated(df$z_j)))
  expect_identical(dependence_diag$target_type, "residual_spearman")
  expect_identical(dependence_diag$converged, TRUE)
  expect_equal(dependence_diag$tol, tol_spearman_continuous, tolerance = tol_deterministic)
  expect_identical(sign(achieved), sign(0.3))
  expect_lt(abs(achieved - 0.3), tol_spearman_ties)
})
# nolint end
