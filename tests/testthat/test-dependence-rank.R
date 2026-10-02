four_site_upstream <- function() {
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

rank_align_upstream <- function(J = 50L, seed = 5101L) {
  effects <- withr::with_seed(seed, gen_effects_gaussian(J = J, tau = 0, sigma_tau = 0.2))
  withr::with_seed(
    seed + 1L,
    gen_site_sizes(effects, J = J, nj_mean = 50, cv = 0.5, nj_min = 5L, engine = "truncated_gamma")
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

residual_target_upstream <- function() {
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
    gen_site_sizes(effects, J = 100L, nj_mean = 50, cv = 0.5, nj_min = 5L, engine = "truncated_gamma")
  )
}

test_that("ranks use average ties, the Spearman convention", {
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
  expect_multisitedgp_error(validate_permutation(c(1, 1, 2), 3L))
  expect_multisitedgp_error(validate_permutation(c(1, 2), 3L))
  expect_multisitedgp_error(validate_permutation(c(0, 1, 2), 3L))
  expect_multisitedgp_error(validate_permutation(c(-1, 2, 3), 3L))
  expect_multisitedgp_error(validate_permutation(c(1, 2, 4), 3L))
  expect_multisitedgp_error(validate_permutation(c(1, NA, 3), 3L))
  expect_multisitedgp_error(validate_permutation(c(1, Inf, 3), 3L))
  expect_multisitedgp_error(validate_permutation(c(1.5, 2, 3), 3L))
  expect_multisitedgp_error(validate_permutation(c("1", "2", "3"), 3L))
})

test_that("reorder_for_spearman returns deterministic endpoint permutations", {
  reorder_for_spearman <- multisitedgp_internal(".reorder_for_spearman")
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
    reorder_for_spearman(x, y[-1], target_corr = 1)
  )
  expect_multisitedgp_error(
    reorder_for_spearman(x, y, target_corr = 2)
  )
  expect_multisitedgp_error(
    realized_spearman(x, y[-1])
  )
})

test_that("reorder_for_spearman preserves ties with the average-rank convention", {
  reorder_for_spearman <- multisitedgp_internal(".reorder_for_spearman")
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

test_that("applying a permutation reorders n_j, se2_j and se_j only", {
  apply_dependence_permutation <- multisitedgp_internal(".apply_dependence_permutation")
  upstream <- dplyr::mutate(four_site_upstream(), x_school = c("a", "b", "c", "d"))
  perm <- c(3L, 1L, 4L, 2L)

  out <- apply_dependence_permutation(upstream, perm)

  expect_identical(out$site_index, upstream$site_index)
  expect_identical(out$z_j, upstream$z_j)
  expect_identical(out$tau_j, upstream$tau_j)
  expect_identical(out$x_school, upstream$x_school)
  expect_identical(out$n_j, upstream$n_j[perm])
  expect_identical(out$se2_j, upstream$se2_j[perm])
  expect_identical(out$se_j, upstream$se_j[perm])
  expect_identical(attr(out, "permutation_perm", exact = TRUE), perm)
})

test_that("applying a permutation keeps duplicated se2_j values paired with their n_j", {
  apply_dependence_permutation <- multisitedgp_internal(".apply_dependence_permutation")
  upstream <- tibble::tibble(
    site_index = 1:4,
    z_j = c(-2, -1, 1, 2),
    tau_j = c(-0.4, -0.2, 0.2, 0.4),
    n_j = c(101L, 102L, 103L, 104L),
    se2_j = c(0.04, 0.04, 0.09, 0.16),
    se_j = sqrt(c(0.04, 0.04, 0.09, 0.16))
  )
  perm <- c(2L, 1L, 4L, 3L)

  out <- apply_dependence_permutation(upstream, perm)

  expect_identical(out$se2_j, upstream$se2_j[perm])
  expect_identical(out$se_j, upstream$se_j[perm])
  expect_identical(out$n_j, upstream$n_j[perm])
  expect_identical(attr(out, "permutation_perm", exact = TRUE), perm)
})

test_that("applying a permutation accepts the NA n_j of a direct design", {
  apply_dependence_permutation <- multisitedgp_internal(".apply_dependence_permutation")
  upstream <- dplyr::mutate(four_site_upstream(), n_j = rep(NA_integer_, dplyr::n()))
  perm <- c(4L, 3L, 2L, 1L)

  out <- apply_dependence_permutation(upstream, perm)

  expect_identical(out$n_j, rep(NA_integer_, nrow(upstream)))
  expect_identical(out$se2_j, upstream$se2_j[perm])
  expect_identical(out$se_j, upstream$se_j[perm])
  expect_identical(attr(out, "permutation_perm", exact = TRUE), perm)
})

test_that("applying a permutation rejects malformed upstream data", {
  apply_dependence_permutation <- multisitedgp_internal(".apply_dependence_permutation")
  upstream <- four_site_upstream()

  expect_multisitedgp_error(
    apply_dependence_permutation(1:4, seq_len(nrow(upstream)))
  )
  expect_multisitedgp_error(
    apply_dependence_permutation(upstream[1, ], 1L)
  )
  expect_multisitedgp_error(
    apply_dependence_permutation(upstream[c("site_index", "z_j")], seq_len(nrow(upstream)))
  )
  expect_multisitedgp_error(
    apply_dependence_permutation(dplyr::mutate(upstream, n_j = as.numeric(n_j)), seq_len(nrow(upstream)))
  )
})

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
  upstream <- residual_target_upstream()
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

test_that("align_rank_corr handles PMSlab ties with truncated-gamma site sizes", {
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
    "multisitedgp_solver_error"
  )
})

test_that("align_rank_corr validates inputs", {
  upstream <- rank_align_upstream()

  expect_multisitedgp_error(
    align_rank_corr(upstream[c("site_index", "z_j")], rank_corr = 0.3)
  )
  expect_multisitedgp_error(
    align_rank_corr(upstream, rank_corr = 1.1)
  )
  expect_multisitedgp_error(
    align_rank_corr(upstream, rank_corr = 0.3, max_iter = 99L)
  )
  expect_multisitedgp_error(
    align_rank_corr(upstream, rank_corr = 0.3, tol = 0)
  )
})

test_that("the rank fit check stops when the achieved correlation has the wrong sign", {
  check_rank_fit <- multisitedgp_internal(".check_rank_fit")

  expect_multisitedgp_error(
    check_rank_fit(list(achieved = 0.1, converged = FALSE), target = -0.3, tol = 0.02, J = 25L),
    "multisitedgp_solver_error"
  )
})

test_that("`sim_multisite()` reaches the rank target when PMSlab effects are tied", {
  df <- sim_multisite(
    J = 100L,
    true_dist = "PointMassSlab",
    theta_G = list(pi0 = 0.5, slab_shape = "Gaussian"),
    sigma_tau = 0.20,
    nj_mean = 80,
    cv = 0.50,
    nj_min = 5L,
    p = 0.5,
    R2 = 0,
    engine = "truncated_gamma",
    dependence = "rank",
    rank_corr = 0.3,
    seed = 19L
  )
  achieved <- stats::cor(df$z_j, df$se2_j, method = "spearman", use = "complete.obs")
  dependence_diag <- attr(df, "diagnostics", exact = TRUE)$dependence_diagnostics

  expect_true(any(duplicated(df$z_j)))
  expect_identical(dependence_diag$target_type, "residual_spearman")
  expect_identical(dependence_diag$converged, TRUE)
  expect_equal(dependence_diag$tol, tol_spearman_continuous, tolerance = tol_deterministic)
  expect_identical(sign(achieved), sign(0.3))
  expect_lt(abs(achieved - 0.3), tol_spearman_ties)
})
