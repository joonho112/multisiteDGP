internal <- function(name) {
  getFromNamespace(name, "multisiteDGP")
}

test_that("abort helper creates umbrella and specific classes", {
  err <- expect_multisitedgp_error(
    internal(".abort_arg")("problem", "context", "Try a fix."),
    "multisitedgp_arg_error"
  )
  expect_match(conditionMessage(err), "problem")
  expect_match(conditionMessage(err), "context")
  expect_match(conditionMessage(err), "Try a fix")
})

test_that("E01 validates J lower bound", {
  expect_multisitedgp_error(
    internal(".validate_j")(5L),
    "multisitedgp_arg_error"
  )
  expect_identical(internal(".validate_j")(10L), invisible(10L))
})

test_that("E02 validates sigma_tau positivity", {
  expect_multisitedgp_error(
    internal(".validate_sigma_tau")(0),
    "multisitedgp_arg_error"
  )
  expect_identical(internal(".validate_sigma_tau")(0.2), invisible(0.2))
})

test_that("E03 validates nj_mean and nj_min relation with deterministic equality exception", {
  expect_multisitedgp_error(
    internal(".validate_nj_mean_nj_min")(10, 0L),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    internal(".validate_nj_mean_nj_min")(10, 15L),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    internal(".validate_nj_mean_nj_min")(10, 10L),
    "multisitedgp_arg_error"
  )
  expect_silent(internal(".validate_nj_mean_nj_min")(10, 10L, allow_equal = TRUE))
  expect_silent(internal(".validate_nj_mean_nj_min")(40, 5L))
})

test_that("E04 validates Engine A1 dependence rejection", {
  expect_multisitedgp_error(
    internal(".validate_engine_dependence")("A1_legacy", "rank", rank_corr = 0.3),
    "multisitedgp_engine_dependence_error"
  )
  expect_silent(internal(".validate_engine_dependence")("A1_legacy", "none"))
})

test_that("E05 validates direct designs reject site-size args internally", {
  expect_multisitedgp_error(
    internal(".validate_direct_args")(nj_mean = 40, cv = 0.5),
    "multisitedgp_coherence_error"
  )
  expect_silent(internal(".validate_direct_args")())
})

test_that("E06 validates g_fn and true_dist coherence", {
  expect_multisitedgp_error(
    internal(".validate_function_or_null")(1, "g_fn"),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    internal(".validate_g_fn_true_dist")(function(j) stats::rnorm(j), "Gaussian"),
    "multisitedgp_coherence_error"
  )
  expect_silent(internal(".validate_g_fn_true_dist")(function(j) stats::rnorm(j), "User"))
  expect_silent(internal(".validate_g_fn_true_dist")(function(j) stats::rnorm(j), "DPM"))
})

test_that("E07 validates StudentT theta_G nu", {
  err <- expect_multisitedgp_error(
    internal(".validate_studentt_theta")(list(other = 1)),
    "multisitedgp_arg_error"
  )
  expect_match(conditionMessage(err), "theta_G\\$nu")
  expect_multisitedgp_error(
    internal(".validate_studentt_theta")(list(nu = 2)),
    "multisitedgp_arg_error"
  )
  expect_identical(internal(".validate_studentt_theta")(list(nu = 5)), invisible(5))
})

test_that("E08 validates rank and Pearson correlation exclusivity", {
  expect_multisitedgp_error(
    internal(".validate_corr_exclusive")(rank_corr = 2, pearson_corr = 0),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    internal(".validate_corr_exclusive")(rank_corr = 0.3, pearson_corr = 0.3),
    "multisitedgp_arg_error"
  )
  expect_silent(internal(".validate_corr_exclusive")(rank_corr = 0.3, pearson_corr = 0))
  expect_silent(internal(".validate_corr_exclusive")(rank_corr = 0, pearson_corr = 0.3))
  expect_silent(internal(".validate_corr_exclusive")(rank_corr = 0.3, pearson_corr = 0, dependence = "hybrid"))
  expect_silent(internal(".validate_corr_exclusive")(rank_corr = 0, pearson_corr = 0.3, dependence = "copula"))
  expect_multisitedgp_error(
    internal(".validate_corr_exclusive")(rank_corr = 0, pearson_corr = 0.3, dependence = "hybrid"),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    internal(".validate_corr_exclusive")(rank_corr = 0.3, pearson_corr = 0, dependence = "copula"),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    internal(".validate_corr_exclusive")(rank_corr = 0.3, pearson_corr = 0, dependence = "none"),
    "multisitedgp_arg_error"
  )
})
