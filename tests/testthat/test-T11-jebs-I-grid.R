# nolint start: object_usage_linter
test_that("T11 full JEBS Paradigm A grid spans the intended I envelope", {
  skip_if_not_slow()

  J_grid <- c(25L, 50L, 100L, 200L, 300L)
  nj_mean_grid <- c(10, 20, 40, 80, 160)
  cv_grid <- c(0, 0.25, 0.50, 0.75)
  sigma_grid <- c(0.05, 0.10, 0.15, 0.20, 0.25)
  I_values <- numeric()
  seeds <- t_invariant_seed_stream(500L, 820111L)
  i <- 0L

  for (J in J_grid) for (nbar in nj_mean_grid) for (cv in cv_grid) for (sigma_tau in sigma_grid) {
    i <- i + 1L
    df <- t_invariant_sim_default(
      seed = seeds[[i]],
      J = J,
      nj_mean = nbar,
      cv = cv,
      sigma_tau = sigma_tau,
      engine = "A1_legacy"
    )
    I_values <- c(I_values, attr(df, "diagnostics", exact = TRUE)$I_hat)
  }

  expect_length(I_values, 500L)
  expect_gte(min(I_values), 0.004)
  expect_lte(min(I_values), 0.01)
  expect_gte(max(I_values), 0.70)
  expect_lte(max(I_values), 0.75)
})
# nolint end
