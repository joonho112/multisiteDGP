# nolint start: object_usage_linter
test_that("G-shape catalog has six built-ins plus User and reserved DPM", {
  builtins <- multisitedgp_internal(".builtin_g_shapes")()
  generated <- multisitedgp_internal(".generated_g_shapes")()
  accepted <- multisitedgp_internal(".accepted_g_shape_values")()

  expect_identical(
    builtins,
    c("Gaussian", "StudentT", "SkewN", "ALD", "Mixture", "PointMassSlab")
  )
  expect_identical(generated, c(builtins, "User"))
  expect_identical(accepted, c(generated, "DPM"))
  expect_length(builtins, 6L)
  expect_length(generated, 7L)
  expect_false("DPM" %in% generated)
})

test_that("constructor and dispatcher accept the same catalog values", {
  for (shape in multisitedgp_internal(".accepted_g_shape_values")()) {
    if (shape %in% c("User", "DPM")) {
      design <- multisitedgp_design(
        J = 10L,
        true_dist = shape,
        g_fn = function(J, ...) seq_len(J)
      )
    } else {
      theta <- switch(
        shape,
        StudentT = list(nu = 5),
        SkewN = list(slant = 2),
        ALD = list(rho = 0.3),
        Mixture = list(delta = 5, eps = 0.3, ups = 2),
        PointMassSlab = list(pi0 = 0.3),
        list()
      )
      design <- multisitedgp_design(J = 10L, true_dist = shape, theta_G = theta)
    }
    expect_identical(design$true_dist, shape)
  }
})
# nolint end
