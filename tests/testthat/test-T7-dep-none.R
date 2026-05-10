# nolint start: object_usage_linter
test_that("T7 dependence none records identity permutation contract", {
  df <- t_invariant_sim_default(seed = 820701L, J = 50L, dependence = "none")
  diagnostics <- attr(df, "diagnostics", exact = TRUE)

  expect_identical(df$site_index, seq_len(50L))
  expect_identical(diagnostics$dependence_method, "none")
  expect_identical(diagnostics$permutation, "identity")
  expect_null(attr(df, "permutation_perm", exact = TRUE))
})
# nolint end
