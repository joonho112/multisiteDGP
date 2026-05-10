# nolint start: object_usage_linter
rank_align_upstream <- function(J = 50L, seed = 5101L) {
  effects <- withr::with_seed(seed, gen_effects_gaussian(J = J, tau = 0, sigma_tau = 0.2))
  withr::with_seed(
    seed + 1L,
    gen_site_sizes(effects, J = J, nj_mean = 50, cv = 0.5, nj_min = 5L, engine = "A2_modern")
  )
}

tied_rank_upstream <- function() {
  n_j <- seq(15L, 94L)
  tibble::tibble(
    site_index = seq_along(n_j),
    z_j = c(rep(-1, 20), rep(0, 20), rep(1, 20), rep(2, 20)),
    tau_j = 0.2 * c(rep(-1, 20), rep(0, 20), rep(1, 20), rep(2, 20)),
    n_j = n_j,
    se2_j = 4 / n_j,
    se_j = sqrt(4 / n_j)
  )
}

constant_precision_upstream <- function() {
  J <- 25L
  tibble::tibble(
    site_index = seq_len(J),
    z_j = seq(-2, 2, length.out = J),
    tau_j = 0.2 * z_j,
    n_j = rep(50L, J),
    se2_j = rep(0.08, J),
    se_j = rep(sqrt(0.08), J)
  )
}

residual_contract_upstream <- function() {
  upstream <- rank_align_upstream(J = 60L, seed = 5401L)
  dplyr::mutate(
    upstream,
    tau_j = rev(z_j) * 0.2,
    covariate_signal = seq_len(dplyr::n())
  )
}

pmslab_tie_upstream <- function() {
  effects <- withr::with_seed(
    5501L,
    gen_effects_pmslab(J = 100L, tau = 0, sigma_tau = 0.2, pi0 = 0.5, slab_shape = "Gaussian")
  )
  withr::with_seed(
    5502L,
    gen_site_sizes(effects, J = 100L, nj_mean = 50, cv = 0.5, nj_min = 5L, engine = "A2_modern")
  )
}

test_that("align_rank_corr is exported and records residual-target diagnostics", {
  expect_true("align_rank_corr" %in% getNamespaceExports("multisiteDGP"))

  upstream <- rank_align_upstream()
  out <- align_rank_corr(upstream, rank_corr = 0.3, max_iter = 20000L, tol = tol_spearman_continuous)
  diag <- attr(out, "dependence_diagnostics", exact = TRUE)

  expect_s3_class(out, "tbl_df")
  expect_identical(out$site_index, upstream$site_index)
  expect_identical(out$z_j, upstream$z_j)
  expect_identical(out$tau_j, upstream$tau_j)
  expect_equal(sort(out$se2_j), sort(upstream$se2_j), tolerance = tol_deterministic)
  expect_equal(out$se_j, sqrt(out$se2_j), tolerance = tol_deterministic)
  expect_equal(out$se2_j * out$n_j, rep(upstream$se2_j[1] * upstream$n_j[1], nrow(out)), tolerance = tol_deterministic)
  expect_identical(sort(attr(out, "permutation_perm", exact = TRUE)), seq_len(nrow(upstream)))
  achieved <- multisitedgp_internal(".realized_spearman")(out$z_j, out$se2_j)
  expect_lt(abs(achieved - 0.3), tol_spearman_continuous)
  expect_identical(diag$method, "rank")
  expect_identical(diag$target_type, "residual_spearman")
  expect_true(diag$converged)
})

test_that("align_rank_corr targets residual z_j rather than marginal tau_j", {
  upstream <- residual_contract_upstream()
  out <- align_rank_corr(upstream, rank_corr = 0.35, max_iter = 20000L, tol = tol_spearman_continuous)
  realized_spearman <- multisitedgp_internal(".realized_spearman")

  residual_rho <- realized_spearman(out$z_j, out$se2_j)
  marginal_rho <- realized_spearman(out$tau_j, out$se2_j)

  expect_identical(out$covariate_signal, upstream$covariate_signal)
  expect_lt(abs(residual_rho - 0.35), tol_spearman_continuous)
  expect_gt(abs(marginal_rho - 0.35), tol_spearman_ties)
})

test_that("align_rank_corr handles negative targets and preserves paired columns", {
  upstream <- rank_align_upstream(seed = 5201L)
  out <- align_rank_corr(upstream, rank_corr = -0.4, max_iter = 20000L, tol = tol_spearman_continuous)
  perm <- attr(out, "permutation_perm", exact = TRUE)

  expect_identical(out$se2_j, upstream$se2_j[perm])
  expect_identical(out$se_j, upstream$se_j[perm])
  expect_identical(out$n_j, upstream$n_j[perm])
  achieved <- multisitedgp_internal(".realized_spearman")(out$z_j, out$se2_j)
  expect_lt(abs(achieved - -0.4), tol_spearman_continuous)
})

test_that("align_rank_corr target zero is deterministic aligned and RNG neutral", {
  upstream <- rank_align_upstream(seed = 5002L)
  realized_spearman <- multisitedgp_internal(".realized_spearman")
  identity_rho <- realized_spearman(upstream$z_j, upstream$se2_j)
  expect_gt(abs(identity_rho), tol_spearman_continuous)

  set.seed(5302L)
  before <- .Random.seed
  out <- align_rank_corr(upstream, rank_corr = 0)
  after <- .Random.seed
  diag <- attr(out, "dependence_diagnostics", exact = TRUE)
  perm <- attr(out, "permutation_perm", exact = TRUE)

  expect_identical(out$se2_j, upstream$se2_j[perm])
  expect_identical(out$se_j, upstream$se_j[perm])
  expect_identical(out$n_j, upstream$n_j[perm])
  expect_equal(sort(out$se2_j), sort(upstream$se2_j), tolerance = tol_deterministic)
  expect_equal(sort(out$se_j), sort(upstream$se_j), tolerance = tol_deterministic)
  expect_identical(sort(out$n_j), sort(upstream$n_j))
  expect_identical(sort(perm), seq_len(nrow(upstream)))
  expect_false(identical(perm, seq_len(nrow(upstream))))
  expect_lt(abs(realized_spearman(out$z_j, out$se2_j)), tol_spearman_continuous)
  expect_true(diag$converged)
  expect_gt(diag$iterations, 0L)
  expect_identical(after, before)
})

test_that("align_rank_corr uses average-rank tie policy with relaxed tolerance", {
  upstream <- tied_rank_upstream()
  out <- align_rank_corr(upstream, rank_corr = 0.3, max_iter = 20000L, tol = tol_spearman_ties)
  achieved <- multisitedgp_internal(".realized_spearman")(out$z_j, out$se2_j)

  expect_gt(sign(achieved), 0)
  expect_lt(abs(achieved - 0.3), tol_spearman_ties)
})

test_that("align_rank_corr handles PMSlab ties on the A2 modern path", {
  upstream <- pmslab_tie_upstream()
  out <- align_rank_corr(upstream, rank_corr = 0.3, max_iter = 20000L, tol = tol_spearman_ties)
  achieved <- multisitedgp_internal(".realized_spearman")(out$z_j, out$se2_j)

  expect_gt(sign(achieved), 0)
  expect_lt(abs(achieved - 0.3), tol_spearman_ties)
  expect_true(any(duplicated(upstream$z_j)))
})

test_that("align_rank_corr solver error handles undefined rank correlations", {
  expect_multisitedgp_error(
    suppressWarnings(align_rank_corr(constant_precision_upstream(), rank_corr = -0.95)),
    "multisitedgp_dependence_solver_error"
  )
})

test_that("align_rank_corr validates inputs", {
  upstream <- rank_align_upstream()

  expect_multisitedgp_error(
    align_rank_corr(upstream[c("site_index", "z_j")], rank_corr = 0.3),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    align_rank_corr(upstream, rank_corr = 1.1),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    align_rank_corr(upstream, rank_corr = 0.3, max_iter = 99L),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    align_rank_corr(upstream, rank_corr = 0.3, tol = 0),
    "multisitedgp_arg_error"
  )
})
# nolint end
