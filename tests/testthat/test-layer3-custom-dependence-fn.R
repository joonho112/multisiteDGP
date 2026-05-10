# nolint start: object_usage_linter
custom_dependence_upstream <- function() {
  n_j <- c(101L, 102L, 103L, 104L)
  se2_j <- c(0.04, 0.04, 0.09, 0.16)
  tibble::tibble(
    site_index = seq_along(n_j),
    z_j = c(-2, -1, 1, 2),
    tau_j = c(2, 1, -1, -2),
    n_j = n_j,
    se2_j = se2_j,
    se_j = sqrt(se2_j),
    x_school = letters[seq_along(n_j)]
  )
}

valid_custom_hook <- function(z_j, se2_j, target, reverse = TRUE) {
  if (isTRUE(reverse)) {
    perm <- rev(seq_along(se2_j))
  } else {
    perm <- seq_along(se2_j)
  }
  list(
    se2_j = se2_j[perm],
    perm = perm,
    received_z = z_j,
    received_target = target
  )
}

test_that("custom dependence_fn receives residual z_j and preserves paired columns", {
  upstream <- custom_dependence_upstream()
  seen <- new.env(parent = emptyenv())
  capturing_hook <- function(z_j, se2_j, target, reverse = TRUE) {
    seen$z_j <- z_j
    seen$target <- target
    valid_custom_hook(z_j = z_j, se2_j = se2_j, target = target, reverse = reverse)
  }

  out <- align_rank_corr(upstream, rank_corr = 0.25, dependence_fn = capturing_hook)
  perm <- attr(out, "permutation_perm", exact = TRUE)
  diag <- attr(out, "dependence_diagnostics", exact = TRUE)

  expect_identical(seen$z_j, upstream$z_j)
  expect_identical(seen$target, 0.25)
  expect_identical(perm, rev(seq_len(nrow(upstream))))
  expect_identical(out$se2_j, upstream$se2_j[perm])
  expect_identical(out$se_j, upstream$se_j[perm])
  expect_identical(out$n_j, upstream$n_j[perm])
  expect_identical(out$site_index, upstream$site_index)
  expect_identical(out$z_j, upstream$z_j)
  expect_identical(out$tau_j, upstream$tau_j)
  expect_identical(out$x_school, upstream$x_school)
  expect_identical(diag$method, "custom")
  expect_identical(diag$target_type, "residual_spearman")
  expect_identical(diag$target, 0.25)
  expect_true(diag$custom)
})

test_that("custom dependence_fn is available from copula and hybrid aligners", {
  upstream <- custom_dependence_upstream()

  copula <- align_copula_corr(upstream, pearson_corr = -0.4, dependence_fn = valid_custom_hook)
  hybrid <- align_hybrid_corr(upstream, rank_corr = 0.5, dependence_fn = valid_custom_hook, reverse = FALSE)

  expect_identical(attr(copula, "dependence_diagnostics", exact = TRUE)$target_type, "latent_gaussian_pearson")
  expect_true(is.na(attr(copula, "dependence_diagnostics", exact = TRUE)$achieved))
  expect_identical(attr(copula, "permutation_perm", exact = TRUE), rev(seq_len(nrow(upstream))))
  expect_identical(attr(hybrid, "dependence_diagnostics", exact = TRUE)$target_type, "residual_spearman")
  expect_identical(attr(hybrid, "permutation_perm", exact = TRUE), seq_len(nrow(upstream)))
  expect_identical(hybrid$se2_j, upstream$se2_j)
})

test_that("custom dependence_fn is called exactly once and owns its RNG use", {
  upstream <- custom_dependence_upstream()
  calls <- 0L
  hook <- function(z_j, se2_j, target) {
    calls <<- calls + 1L
    stats::rnorm(1L)
    perm <- seq_along(se2_j)
    list(se2_j = se2_j[perm], perm = perm)
  }

  set.seed(6101L)
  align_rank_corr(upstream, rank_corr = 0.3, dependence_fn = hook)
  after_call <- .Random.seed
  set.seed(6101L)
  stats::rnorm(1L)
  after_hook_draw <- .Random.seed

  expect_identical(calls, 1L)
  expect_identical(after_call, after_hook_draw)
})

test_that("custom dependence_fn rejects numeric-only or incomplete returns", {
  upstream <- custom_dependence_upstream()

  expect_multisitedgp_error(
    align_rank_corr(upstream, rank_corr = 0.3, dependence_fn = function(z_j, se2_j, target) se2_j),
    "multisitedgp_marginal_violation_error"
  )
  expect_multisitedgp_error(
    align_rank_corr(upstream, rank_corr = 0.3, dependence_fn = function(z_j, se2_j, target) list(se2_j = se2_j)),
    "multisitedgp_marginal_violation_error"
  )
})

test_that("custom dependence_fn rejects invalid permutations", {
  upstream <- custom_dependence_upstream()

  duplicate_perm <- function(z_j, se2_j, target) {
    list(se2_j = se2_j[c(1L, 1L, 3L, 4L)], perm = c(1L, 1L, 3L, 4L))
  }
  fractional_perm <- function(z_j, se2_j, target) {
    list(se2_j = se2_j, perm = c(1, 2, 3, 4.5))
  }

  expect_multisitedgp_error(
    align_rank_corr(upstream, rank_corr = 0.3, dependence_fn = duplicate_perm),
    "multisitedgp_marginal_violation_error"
  )
  expect_multisitedgp_error(
    align_rank_corr(upstream, rank_corr = 0.3, dependence_fn = fractional_perm),
    "multisitedgp_marginal_violation_error"
  )
})

test_that("custom dependence_fn rejects marginal drift and perm inconsistency", {
  upstream <- custom_dependence_upstream()

  count_drift <- function(z_j, se2_j, target) {
    list(se2_j = c(0.04, 0.09, 0.09, 0.16), perm = seq_along(se2_j))
  }
  numeric_drift <- function(z_j, se2_j, target) {
    perm <- rev(seq_along(se2_j))
    list(se2_j = se2_j[perm] + 1e-12, perm = perm)
  }
  inconsistent <- function(z_j, se2_j, target) {
    list(se2_j = rev(se2_j), perm = seq_along(se2_j))
  }

  expect_multisitedgp_error(
    align_rank_corr(upstream, rank_corr = 0.3, dependence_fn = count_drift),
    "multisitedgp_marginal_violation_error"
  )
  expect_multisitedgp_error(
    align_rank_corr(upstream, rank_corr = 0.3, dependence_fn = numeric_drift),
    "multisitedgp_marginal_violation_error"
  )
  expect_multisitedgp_error(
    align_rank_corr(upstream, rank_corr = 0.3, dependence_fn = inconsistent),
    "multisitedgp_marginal_violation_error"
  )
})
# nolint end
