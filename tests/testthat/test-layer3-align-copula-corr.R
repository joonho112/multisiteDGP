# nolint start: object_usage_linter
copula_align_upstream <- function(J = 200L, seed = 5601L) {
  effects <- withr::with_seed(seed, gen_effects_gaussian(J = J, tau = 0, sigma_tau = 0.2))
  withr::with_seed(
    seed + 1L,
    gen_site_sizes(effects, J = J, nj_mean = 50, cv = 0.5, nj_min = 5L, engine = "A2_modern")
  )
}

constant_copula_precision_upstream <- function() {
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

constant_copula_residual_upstream <- function() {
  upstream <- copula_align_upstream(J = 50L, seed = 5701L)
  dplyr::mutate(upstream, z_j = 0, tau_j = 0)
}

residual_copula_upstream <- function() {
  upstream <- copula_align_upstream(J = 200L, seed = 5711L)
  dplyr::mutate(
    upstream,
    tau_j = -z_j * 0.2,
    covariate_signal = seq_len(dplyr::n())
  )
}

tied_copula_upstream <- function() {
  n_j <- rep(c(20L, 40L, 80L, 120L), each = 15L)
  z_j <- rep(c(-1, 0, 1, 2), each = 15L)
  tibble::tibble(
    site_index = seq_along(n_j),
    z_j = z_j,
    tau_j = 0.2 * z_j,
    n_j = n_j,
    se2_j = 4 / n_j,
    se_j = sqrt(4 / n_j)
  )
}

test_that("align_copula_corr is exported and preserves empirical margins", {
  expect_true("align_copula_corr" %in% getNamespaceExports("multisiteDGP"))

  upstream <- copula_align_upstream()
  out <- withr::with_seed(5603L, align_copula_corr(upstream, pearson_corr = 0.5))
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
  expect_equal(sort(out$se2_j), sort(upstream$se2_j), tolerance = tol_deterministic)
  expect_gt(multisitedgp_internal(".realized_spearman")(out$z_j, out$se2_j), 0.25)
  expect_identical(diag$method, "copula")
  expect_identical(diag$mode, "empirical_rank_pairing")
  expect_identical(diag$preservation, "empirical_multiset")
  expect_identical(diag$target_type, "latent_gaussian_pearson")
  expect_identical(diag$ties_method, "average")
  expect_identical(diag$rng_draws, nrow(upstream))
  expect_true(is.na(diag$converged))
})

test_that("align_copula_corr targets residual z_j rather than marginal tau_j", {
  upstream <- residual_copula_upstream()
  out <- withr::with_seed(5713L, align_copula_corr(upstream, pearson_corr = 0.65))
  realized_spearman <- multisitedgp_internal(".realized_spearman")

  residual_rho <- realized_spearman(out$z_j, out$se2_j)
  marginal_rho <- realized_spearman(out$tau_j, out$se2_j)

  expect_identical(out$covariate_signal, upstream$covariate_signal)
  expect_gt(residual_rho, 0.35)
  expect_lt(marginal_rho, -0.35)
})

test_that("align_copula_corr handles negative latent correlations", {
  upstream <- copula_align_upstream(seed = 5611L)
  out <- withr::with_seed(5613L, align_copula_corr(upstream, pearson_corr = -0.6))
  achieved <- multisitedgp_internal(".realized_spearman")(out$z_j, out$se2_j)

  expect_lt(achieved, -0.25)
  expect_equal(sort(out$se2_j), sort(upstream$se2_j), tolerance = tol_deterministic)
})

test_that("align_copula_corr handles tied residual and precision ranks", {
  upstream <- tied_copula_upstream()
  out <- withr::with_seed(5614L, align_copula_corr(upstream, pearson_corr = 0.8))
  achieved <- multisitedgp_internal(".realized_spearman")(out$z_j, out$se2_j)
  diag <- attr(out, "dependence_diagnostics", exact = TRUE)

  expect_true(any(duplicated(upstream$z_j)))
  expect_true(any(duplicated(upstream$se2_j)))
  expect_gt(achieved, 0.5)
  expect_identical(diag$ties_method, "average")
  expect_equal(sort(out$se2_j), sort(upstream$se2_j), tolerance = tol_deterministic)
})

test_that("align_copula_corr is reproducible under fixed seed", {
  upstream <- copula_align_upstream(seed = 5621L)

  out_1 <- withr::with_seed(5623L, align_copula_corr(upstream, pearson_corr = 0.4))
  out_2 <- withr::with_seed(5623L, align_copula_corr(upstream, pearson_corr = 0.4))

  expect_identical(out_1, out_2)
  expect_identical(attr(out_1, "permutation_perm", exact = TRUE), attr(out_2, "permutation_perm", exact = TRUE))
})

test_that("align_copula_corr RNG policy is explicit", {
  upstream <- copula_align_upstream(J = 50L, seed = 5636L)

  set.seed(5637L)
  before <- .Random.seed
  endpoint <- suppressWarnings(align_copula_corr(upstream, pearson_corr = 1))
  after_endpoint <- .Random.seed
  expect_identical(after_endpoint, before)
  expect_identical(attr(endpoint, "dependence_diagnostics", exact = TRUE)$rng_draws, 0L)

  set.seed(5638L)
  align_copula_corr(upstream, pearson_corr = 0.4)
  after_call <- .Random.seed
  set.seed(5638L)
  stats::rnorm(nrow(upstream))
  after_rnorm <- .Random.seed
  expect_identical(after_call, after_rnorm)
})

test_that("align_copula_corr target zero is random independent pairing, not identity", {
  upstream <- copula_align_upstream(seed = 5631L)
  out <- withr::with_seed(5633L, align_copula_corr(upstream, pearson_corr = 0))

  expect_equal(sort(out$se2_j), sort(upstream$se2_j), tolerance = tol_deterministic)
  expect_false(identical(attr(out, "permutation_perm", exact = TRUE), seq_len(nrow(upstream))))
  expect_lt(abs(multisitedgp_internal(".realized_spearman")(out$z_j, out$se2_j)), 0.2)
})

test_that("align_copula_corr validates correlations and constant ranks", {
  upstream <- copula_align_upstream(J = 50L, seed = 5641L)

  expect_multisitedgp_error(
    align_copula_corr(upstream, pearson_corr = 1.1),
    "multisitedgp_arg_error"
  )
  expect_rng_neutral_error(
    align_copula_corr(constant_copula_precision_upstream(), pearson_corr = 0.4),
    "multisitedgp_dependence_solver_error",
    seed = 5642L
  )
  expect_rng_neutral_error(
    align_copula_corr(constant_copula_residual_upstream(), pearson_corr = 0.4),
    "multisitedgp_dependence_solver_error",
    seed = 5643L
  )
})
# nolint end
