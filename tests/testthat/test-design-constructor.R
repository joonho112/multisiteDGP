test_that("multisitedgp_design creates the default immutable config record", {
  des <- multisitedgp_design()

  expect_s3_class(des, "multisitedgp_design")
  expect_s3_class(des, "list")
  expect_true(is_multisitedgp_design(des))
  expect_false(is_multisitedgp_design(list()))
  expect_identical(
    names(des),
    c(
      "J", "paradigm", "true_dist", "tau", "sigma_tau", "variance", "theta_G",
      "formula", "beta", "data", "g_fn", "g_returns", "nj_mean", "cv",
      "nj_min", "p", "R2", "var_outcome", "engine", "I", "R", "shuffle",
      "se_fn", "se_args", "dependence_spec", "obs_spec", "framing", "seed",
      "record_permutation"
    )
  )
  expect_identical(des$J, 50L)
  expect_identical(des$paradigm, "site_size")
  expect_identical(des$true_dist, "Gaussian")
  expect_identical(des$sigma_tau, 0.20)
  expect_identical(des$engine, "A2_modern")
  expect_identical(des$dependence_spec$method, "none")
  expect_identical(
    names(des$dependence_spec),
    c("method", "rank_corr", "pearson_corr", "dependence_fn", "hybrid_init", "hybrid_polish", "max_iter", "tol")
  )
  expect_identical(des$dependence_spec$hybrid_init, "copula")
  expect_identical(des$dependence_spec$hybrid_polish, "hill_climb")
  expect_identical(names(des$obs_spec), "obs_fn")
  expect_identical(des$framing, "superpopulation")
  expect_false(des$record_permutation)
})

test_that("record_permutation default follows dependence mode", {
  no_dep <- multisitedgp_design(dependence = "none")
  rank_dep <- multisitedgp_design(dependence = "rank", rank_corr = 0.3)
  explicit_false <- multisitedgp_design(dependence = "rank", rank_corr = 0.3, record_permutation = FALSE)

  expect_false(no_dep$record_permutation)
  expect_true(rank_dep$record_permutation)
  expect_false(explicit_false$record_permutation)
})

test_that("constructor uses classed validation for early scalar and matching errors", {
  expect_multisitedgp_error(
    multisitedgp_design(J = 5L),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    multisitedgp_design(engine = "A1_legacy", dependence = "rank", rank_corr = 0.3),
    "multisitedgp_engine_dependence_error"
  )
  expect_multisitedgp_error(
    multisitedgp_design(paradigm = "unknown"),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    multisitedgp_design(hybrid_init = "bad"),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    multisitedgp_design(hybrid_polish = "bad"),
    "multisitedgp_arg_error"
  )
})

test_that("constructor exposes hybrid dependence controls in the design spec", {
  des <- multisitedgp_design(
    dependence = "hybrid",
    rank_corr = 0.3,
    hybrid_init = "rank",
    hybrid_polish = "none"
  )

  expect_identical(des$dependence_spec$method, "hybrid")
  expect_identical(des$dependence_spec$rank_corr, 0.3)
  expect_identical(des$dependence_spec$hybrid_init, "rank")
  expect_identical(des$dependence_spec$hybrid_polish, "none")
})

test_that("constructor rejects dependence targets that the selected method ignores", {
  expect_multisitedgp_error(
    multisitedgp_design(dependence = "hybrid", pearson_corr = 0.3),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    multisitedgp_design(dependence = "rank", pearson_corr = 0.3),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    multisitedgp_design(dependence = "copula", rank_corr = 0.3),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    multisitedgp_design(dependence = "none", rank_corr = 0.3),
    "multisitedgp_arg_error"
  )
})

test_that("format and print provide a readable design summary", {
  des <- multisitedgp_design(J = 100L, sigma_tau = 0.25, seed = 1L)
  formatted <- format(des)
  printed <- capture.output(print(des))
  lines <- strsplit(formatted, "\n", fixed = TRUE)[[1L]]

  expect_type(formatted, "character")
  expect_length(formatted, 1L)
  expect_true(length(lines) > 10L)
  expect_match(formatted, "<multisitedgp_design>", fixed = TRUE)
  expect_match(formatted, "Paradigm: site_size", fixed = TRUE)
  expect_match(formatted, "J: 100", fixed = TRUE)
  expect_match(formatted, "sigma_tau:", fixed = TRUE)
  expect_match(formatted, "0.25", fixed = TRUE)
  expect_match(formatted, "[ Layer 3: Dependence ]", fixed = TRUE)
  expect_match(formatted, "hybrid_init:", fixed = TRUE)
  expect_match(formatted, "hybrid_polish:", fixed = TRUE)
  expect_match(formatted, "Use sim_multisite(design) or sim_meta(design) to simulate.", fixed = TRUE)
  expect_identical(printed, lines)
  expect_identical(capture.output(returned <- print(des)), lines)
  expect_identical(returned, des)
})
