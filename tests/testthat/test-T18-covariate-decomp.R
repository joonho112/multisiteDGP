# nolint start: object_usage_linter
test_that("T18 covariate decomposition preserves public z_j identity", {
  data_x <- tibble::tibble(x = seq(-1, 1, length.out = 50L))
  fixture <- t_invariant_default_args(formula = ~ x, beta = 1.5, data = data_x)
  df <- do.call(sim_multisite, c(fixture, list(seed = 18L)))
  diagnostics <- attr(df, "diagnostics", exact = TRUE)
  x_beta <- stats::model.matrix(~ x, data_x) %*% c(0, 1.5)

  residual_lhs <- df$tau_j - fixture$tau - as.numeric(x_beta)
  residual_rhs <- fixture$sigma_tau * df$z_j

  expect_equal(residual_lhs, residual_rhs, tolerance = tol_deterministic)
  expect_true(is.finite(diagnostics$rho_S_residual))
  expect_true(is.finite(diagnostics$rho_S_marginal))
  expect_gt(abs(diagnostics$rho_S_marginal - diagnostics$rho_S_residual), 0)
})

test_that("T18 print and summary report residual and marginal dependence", {
  data_x <- tibble::tibble(x = seq(-1, 1, length.out = 50L))
  df <- sim_multisite(
    formula = ~ x,
    beta = 1.5,
    data = data_x,
    dependence = "rank",
    rank_corr = 0.3,
    seed = 18L
  )

  printed <- capture.output(print(df, n = 1L))
  summarized <- capture.output(summary(df))

  expect_true(any(grepl("rho_S:", printed, fixed = TRUE)))
  expect_true(any(grepl("rho_S_marg:", printed, fixed = TRUE)))
  expect_true(any(grepl("rank_corr residual", summarized, fixed = TRUE)))
  expect_true(any(grepl("rank_corr marginal", summarized, fixed = TRUE)))
  expect_true(any(grepl("pearson_corr residual", summarized, fixed = TRUE)))
  expect_true(any(grepl("pearson_corr marginal", summarized, fixed = TRUE)))
})
# nolint end
