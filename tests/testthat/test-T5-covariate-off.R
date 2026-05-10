# nolint start: object_usage_linter
test_that("T5 formula NULL matches beta zero covariate path in the wrapper", {
  data_x <- tibble::tibble(x = seq(-1, 1, length.out = 50L))
  no_cov <- t_invariant_sim_default(seed = 820501L)
  beta_zero <- t_invariant_sim_default(seed = 820501L, formula = ~ x, beta = 0, data = data_x)

  for (column in names(no_cov)) {
    expect_equal(no_cov[[column]], beta_zero[[column]], tolerance = tol_deterministic)
  }
  expect_equal(beta_zero$x, data_x$x, tolerance = tol_deterministic)
})
# nolint end
