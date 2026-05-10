# nolint start: object_usage_linter
test_that("T6 residual and marginal sigma_tau agree in covariate-off mode", {
  df <- t_invariant_sim_default(seed = 820601L, J = 300L)
  diagnostics <- attr(df, "diagnostics", exact = TRUE)

  expect_equal(diagnostics$sigma_tau_resid, diagnostics$sigma_tau_marg, tolerance = tol_deterministic)
  expect_equal(diagnostics$sigma_tau_resid, diagnostics$target_sigma_tau_resid, tolerance = 0.05)
})
# nolint end
