# nolint start: object_usage_linter
test_that("T15 Student-t standardization holds across nu grid", {
  skip_if_not_slow()

  for (nu in c(3, 5, 10)) {
    z_j <- withr::with_seed(831500L + nu, suppressWarnings(
      gen_effects_studentt(J = default_n_property_large, nu = nu)
    )$z_j)
    var_tolerance <- if (nu < 4) tol_studentt_inf_kurt_var else tol_mc_moment_n1e5

    expect_lt(abs(mean(z_j)), 0.005)
    expect_lt(abs(stats::var(z_j) - 1), var_tolerance)
  }
})
# nolint end
