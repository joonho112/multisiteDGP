# nolint start: object_usage_linter
layer3_upstream <- function() {
  n_j <- c(10L, 20L, 40L, 80L)
  se2_j <- 4 / n_j
  tibble::tibble(
    site_index = seq_along(n_j),
    z_j = c(-1.5, -0.25, 0.5, 1.75),
    tau_j = c(-0.3, -0.05, 0.1, 0.35),
    n_j = n_j,
    se2_j = se2_j,
    se_j = sqrt(se2_j)
  )
}

test_that("mdgp rank uses average ties as the canonical Spearman policy", {
  mdgp_rank <- multisitedgp_internal(".mdgp_rank")

  x <- c(4, 2, 2, 9, 9, 9)
  expect_equal(mdgp_rank(x), base::rank(x, ties.method = "average"))
  expect_equal(mdgp_rank(x), c(3, 1.5, 1.5, 5, 5, 5), tolerance = tol_deterministic)
})

test_that("realized Spearman returns NA silently for constant margins", {
  realized_spearman <- multisitedgp_internal(".realized_spearman")

  expect_warning(value_x <- realized_spearman(rep(1, 4), seq_len(4)), NA)
  expect_warning(value_y <- realized_spearman(seq_len(4), rep(2, 4)), NA)

  expect_true(is.na(value_x))
  expect_true(is.na(value_y))
})

test_that("permutation validation accepts only complete 1:J permutations", {
  validate_permutation <- multisitedgp_internal(".validate_permutation")

  expect_identical(validate_permutation(c(3, 1, 2), 3L), c(3L, 1L, 2L))
  expect_multisitedgp_error(validate_permutation(c(1, 1, 2), 3L), "multisitedgp_arg_error")
  expect_multisitedgp_error(validate_permutation(c(1, 2), 3L), "multisitedgp_arg_error")
  expect_multisitedgp_error(validate_permutation(c(0, 1, 2), 3L), "multisitedgp_arg_error")
  expect_multisitedgp_error(validate_permutation(c(-1, 2, 3), 3L), "multisitedgp_arg_error")
  expect_multisitedgp_error(validate_permutation(c(1, 2, 4), 3L), "multisitedgp_arg_error")
  expect_multisitedgp_error(validate_permutation(c(1, NA, 3), 3L), "multisitedgp_arg_error")
  expect_multisitedgp_error(validate_permutation(c(1, Inf, 3), 3L), "multisitedgp_arg_error")
  expect_multisitedgp_error(validate_permutation(c(1.5, 2, 3), 3L), "multisitedgp_arg_error")
  expect_multisitedgp_error(validate_permutation(c("1", "2", "3"), 3L), "multisitedgp_arg_error")
})

test_that("reorder_for_spearman returns deterministic endpoint permutations", {
  reorder_for_spearman <- multisitedgp_internal("reorder_for_spearman")
  realized_spearman <- multisitedgp_internal(".realized_spearman")
  x <- c(10, 20, 30, 40, 50)
  y <- c(7, 2, 9, 1, 5)

  pos <- reorder_for_spearman(x, y, target_corr = 1)
  neg <- reorder_for_spearman(x, y, target_corr = -1)
  zero <- reorder_for_spearman(x, y, target_corr = 0)

  expect_setequal(y[pos], y)
  expect_setequal(y[neg], y)
  expect_identical(zero, seq_along(x))
  expect_equal(realized_spearman(x, y[pos]), 1, tolerance = tol_deterministic)
  expect_equal(realized_spearman(x, y[neg]), -1, tolerance = tol_deterministic)
  expect_multisitedgp_error(
    reorder_for_spearman(x, y[-1], target_corr = 1),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    reorder_for_spearman(x, y, target_corr = 2),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    realized_spearman(x, y[-1]),
    "multisitedgp_arg_error"
  )
})

test_that("reorder_for_spearman preserves ties with the average-rank convention", {
  reorder_for_spearman <- multisitedgp_internal("reorder_for_spearman")
  realized_spearman <- multisitedgp_internal(".realized_spearman")
  mdgp_rank <- multisitedgp_internal(".mdgp_rank")
  x <- c(1, 1, 2, 3, 3, 4)
  y <- c(10, 20, 20, 30, 40, 40)

  perm <- reorder_for_spearman(x, y, target_corr = 1)

  expect_setequal(y[perm], y)
  expect_false(identical(mdgp_rank(x), base::rank(x, ties.method = "first")))
  expect_false(identical(mdgp_rank(y), base::rank(y, ties.method = "min")))
  expect_equal(
    realized_spearman(x, y[perm]),
    stats::cor(base::rank(x, ties.method = "average"), base::rank(sort(y), ties.method = "average")),
    tolerance = tol_deterministic
  )
})

test_that("Layer 3 permutation helper reorders paired precision columns only", {
  apply_l3_permutation <- multisitedgp_internal(".apply_l3_permutation")
  upstream <- dplyr::mutate(layer3_upstream(), x_school = c("a", "b", "c", "d"))
  perm <- c(3L, 1L, 4L, 2L)

  out <- apply_l3_permutation(upstream, perm)

  expect_identical(out$site_index, upstream$site_index)
  expect_identical(out$z_j, upstream$z_j)
  expect_identical(out$tau_j, upstream$tau_j)
  expect_identical(out$x_school, upstream$x_school)
  expect_identical(out$n_j, upstream$n_j[perm])
  expect_identical(out$se2_j, upstream$se2_j[perm])
  expect_identical(out$se_j, upstream$se_j[perm])
  expect_identical(attr(out, "permutation_perm", exact = TRUE), perm)
})

test_that("Layer 3 permutation helper preserves duplicate precision pairings exactly", {
  apply_l3_permutation <- multisitedgp_internal(".apply_l3_permutation")
  upstream <- tibble::tibble(
    site_index = 1:4,
    z_j = c(-2, -1, 1, 2),
    tau_j = c(-0.4, -0.2, 0.2, 0.4),
    n_j = c(101L, 102L, 103L, 104L),
    se2_j = c(0.04, 0.04, 0.09, 0.16),
    se_j = sqrt(c(0.04, 0.04, 0.09, 0.16))
  )
  perm <- c(2L, 1L, 4L, 3L)

  out <- apply_l3_permutation(upstream, perm)

  expect_identical(out$se2_j, upstream$se2_j[perm])
  expect_identical(out$se_j, upstream$se_j[perm])
  expect_identical(out$n_j, upstream$n_j[perm])
  expect_identical(attr(out, "permutation_perm", exact = TRUE), perm)
})

test_that("Layer 3 permutation helper accepts direct-meta NA integer n_j", {
  apply_l3_permutation <- multisitedgp_internal(".apply_l3_permutation")
  upstream <- dplyr::mutate(layer3_upstream(), n_j = rep(NA_integer_, dplyr::n()))
  perm <- c(4L, 3L, 2L, 1L)

  out <- apply_l3_permutation(upstream, perm)

  expect_identical(out$n_j, rep(NA_integer_, nrow(upstream)))
  expect_identical(out$se2_j, upstream$se2_j[perm])
  expect_identical(out$se_j, upstream$se_j[perm])
  expect_identical(attr(out, "permutation_perm", exact = TRUE), perm)
})

test_that("Layer 3 upstream validation rejects noncanonical frames", {
  apply_l3_permutation <- multisitedgp_internal(".apply_l3_permutation")
  upstream <- layer3_upstream()

  expect_multisitedgp_error(
    apply_l3_permutation(1:4, seq_len(nrow(upstream))),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    apply_l3_permutation(upstream[1, ], 1L),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    apply_l3_permutation(upstream[c("site_index", "z_j")], seq_len(nrow(upstream))),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    apply_l3_permutation(dplyr::mutate(upstream, n_j = as.numeric(n_j)), seq_len(nrow(upstream))),
    "multisitedgp_arg_error"
  )
})
# nolint end
