# nolint start: object_usage_linter
hybrid_align_upstream <- function(J = 120L, seed = 5801L) {
  effects <- withr::with_seed(seed, gen_effects_gaussian(J = J, tau = 0, sigma_tau = 0.2))
  withr::with_seed(
    seed + 1L,
    gen_site_sizes(effects, J = J, nj_mean = 50, cv = 0.5, nj_min = 5L, engine = "A2_modern")
  )
}

test_that("spearman_to_pearson implements the Gaussian copula mapping", {
  spearman_to_pearson <- multisitedgp_internal("spearman_to_pearson")

  expect_identical(spearman_to_pearson(0), 0)
  expect_equal(spearman_to_pearson(1), 1, tolerance = tol_deterministic)
  expect_equal(spearman_to_pearson(-1), -1, tolerance = tol_deterministic)
  expect_equal(spearman_to_pearson(0.3), 2 * sin(pi * 0.3 / 6), tolerance = tol_deterministic)
  expect_multisitedgp_error(spearman_to_pearson(1.1), "multisitedgp_arg_error")
})

test_that("align_hybrid_corr is exported and preserves empirical margins", {
  expect_true("align_hybrid_corr" %in% getNamespaceExports("multisiteDGP"))

  upstream <- hybrid_align_upstream()
  out <- withr::with_seed(5803L, align_hybrid_corr(upstream, rank_corr = 0.35))
  perm <- attr(out, "permutation_perm", exact = TRUE)
  diag <- attr(out, "dependence_diagnostics", exact = TRUE)

  expect_s3_class(out, "tbl_df")
  expect_identical(sort(perm), seq_len(nrow(upstream)))
  expect_identical(out$site_index, upstream$site_index)
  expect_identical(out$z_j, upstream$z_j)
  expect_identical(out$tau_j, upstream$tau_j)
  expect_identical(out$se2_j, upstream$se2_j[perm])
  expect_identical(out$se_j, upstream$se_j[perm])
  expect_identical(out$n_j, upstream$n_j[perm])
  expect_lt(abs(multisitedgp_internal(".realized_spearman")(out$z_j, out$se2_j) - 0.35), tol_spearman_continuous)
  expect_identical(diag$method, "hybrid")
  expect_identical(diag$mode, "empirical_rank_pairing")
  expect_identical(diag$preservation, "empirical_multiset")
  expect_identical(diag$init, "copula")
  expect_identical(diag$polish, "hill_climb")
  init_error <- abs(diag$init_diagnostics$achieved_spearman - diag$target)
  final_error <- abs(diag$achieved - diag$target)
  expect_lte(final_error, init_error + .Machine$double.eps)
  expect_true(diag$converged)
})

test_that("align_hybrid_corr with no polish reports honest copula initialization", {
  upstream <- hybrid_align_upstream(seed = 5811L)
  out <- withr::with_seed(
    5813L,
    align_hybrid_corr(upstream, rank_corr = 0.45, polish = "none")
  )
  expected <- withr::with_seed(
    5813L,
    align_copula_corr(upstream, pearson_corr = multisitedgp_internal("spearman_to_pearson")(0.45))
  )
  diag <- attr(out, "dependence_diagnostics", exact = TRUE)

  expect_identical(out$se2_j, expected$se2_j)
  expect_identical(attr(out, "permutation_perm", exact = TRUE), attr(expected, "permutation_perm", exact = TRUE))
  expect_identical(diag$method, "hybrid")
  expect_identical(diag$polish, "none")
  expect_true(is.na(diag$converged))
  expect_gt(multisitedgp_internal(".realized_spearman")(out$z_j, out$se2_j), 0.2)
  expect_equal(sort(out$se2_j), sort(upstream$se2_j), tolerance = tol_deterministic)
})

test_that("align_hybrid_corr init rank is deterministic and RNG neutral", {
  upstream <- hybrid_align_upstream(seed = 5821L)

  set.seed(5823L)
  before <- .Random.seed
  out <- align_hybrid_corr(upstream, rank_corr = -0.35, init = "rank")
  after <- .Random.seed
  diag <- attr(out, "dependence_diagnostics", exact = TRUE)

  expect_identical(after, before)
  expect_identical(diag$init, "rank")
  expect_identical(diag$rng_draws, 0L)
  expect_lt(abs(multisitedgp_internal(".realized_spearman")(out$z_j, out$se2_j) - -0.35), tol_spearman_continuous)
})

test_that("align_hybrid_corr init rank with no polish is explicit identity", {
  upstream <- hybrid_align_upstream(seed = 5826L)
  out <- align_hybrid_corr(upstream, rank_corr = 0.35, init = "rank", polish = "none")
  diag <- attr(out, "dependence_diagnostics", exact = TRUE)

  expect_identical(out$se2_j, upstream$se2_j)
  expect_identical(out$se_j, upstream$se_j)
  expect_identical(out$n_j, upstream$n_j)
  expect_identical(attr(out, "permutation_perm", exact = TRUE), seq_len(nrow(upstream)))
  expect_identical(diag$init, "rank")
  expect_identical(diag$polish, "none")
  expect_false(diag$converged)
})

test_that("align_hybrid_corr RNG policy follows copula initialization", {
  upstream <- hybrid_align_upstream(J = 60L, seed = 5827L)

  set.seed(5828L)
  align_hybrid_corr(upstream, rank_corr = 0.4, polish = "none")
  after_call <- .Random.seed
  set.seed(5828L)
  stats::rnorm(nrow(upstream))
  after_rnorm <- .Random.seed
  expect_identical(after_call, after_rnorm)

  set.seed(5829L)
  before <- .Random.seed
  endpoint <- suppressWarnings(align_hybrid_corr(upstream, rank_corr = 1, polish = "none"))
  after_endpoint <- .Random.seed
  expect_identical(after_endpoint, before)
  expect_identical(attr(endpoint, "dependence_diagnostics", exact = TRUE)$rng_draws, 0L)
})

test_that("align_hybrid_corr target zero is identity and RNG neutral", {
  upstream <- hybrid_align_upstream(seed = 5831L)

  set.seed(5833L)
  before <- .Random.seed
  out <- align_hybrid_corr(upstream, rank_corr = 0)
  after <- .Random.seed
  diag <- attr(out, "dependence_diagnostics", exact = TRUE)

  expect_identical(out$se2_j, upstream$se2_j)
  expect_identical(out$se_j, upstream$se_j)
  expect_identical(out$n_j, upstream$n_j)
  expect_identical(attr(out, "permutation_perm", exact = TRUE), seq_len(nrow(upstream)))
  expect_identical(after, before)
  expect_identical(diag$rng_draws, 0L)
})

test_that("align_hybrid_corr validates arguments", {
  upstream <- hybrid_align_upstream(J = 50L, seed = 5841L)

  expect_multisitedgp_error(align_hybrid_corr(upstream, rank_corr = 1.1), "multisitedgp_arg_error")
  expect_multisitedgp_error(align_hybrid_corr(upstream, init = "bad"), "multisitedgp_arg_error")
  expect_multisitedgp_error(align_hybrid_corr(upstream, polish = "bad"), "multisitedgp_arg_error")
  expect_multisitedgp_error(align_hybrid_corr(upstream, max_iter = 99L), "multisitedgp_arg_error")
  expect_multisitedgp_error(align_hybrid_corr(upstream, tol = 0), "multisitedgp_arg_error")
})
# nolint end
