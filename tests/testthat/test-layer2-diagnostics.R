# nolint start: object_usage_linter
sample_diagnostic_frame <- function() {
  data.frame(
    site_index = 1:4,
    z_j = c(-1, -0.2, 0.4, 1.1),
    tau_j = c(-0.2, -0.04, 0.08, 0.22),
    tau_j_hat = c(-0.18, -0.01, 0.11, 0.19),
    se_j = c(0.2, 0.25, 0.3, 0.35),
    se2_j = c(0.04, 0.0625, 0.09, 0.1225),
    n_j = c(50L, 45L, 60L, 55L)
  )
}

new_diagnostic_data <- function(design = multisitedgp_design(sigma_tau = 0.2, seed = 42L)) {
  multisitedgp_internal(".new_multisitedgp_data")(
    sample_diagnostic_frame(),
    design = design,
    diagnostics = list(I_hat = 999)
  )
}

test_that("T2 compute_kappa default equals four exactly", {
  expect_equal(compute_kappa(), 4, tolerance = tol_deterministic)
  expect_equal(compute_kappa(p = 0.5, R2 = 0.2, var_outcome = 2), 6.4, tolerance = tol_deterministic)

  expect_multisitedgp_error(
    compute_kappa(p = 1),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    compute_kappa(R2 = 1),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    compute_kappa(var_outcome = 0),
    "multisitedgp_arg_error"
  )
})

test_that("T3 rate form preserves se2_j times n_j equals kappa", {
  n_j <- c(20L, 40L, 80L)
  kappa <- compute_kappa()
  se2_j <- kappa / n_j

  expect_equal(se2_j * n_j, rep(kappa, length(n_j)), tolerance = tol_deterministic)
})

test_that("T8 compute_I uses geometric mean of se2_j, not arithmetic mean", {
  se2_j <- c(0.01, 0.05, 0.1, 0.5, 1.0)
  sigma_tau <- 0.5
  expected <- sigma_tau^2 / (sigma_tau^2 + exp(mean(log(se2_j))))
  arithmetic <- sigma_tau^2 / (sigma_tau^2 + mean(se2_j))

  expect_equal(compute_I(se2_j, sigma_tau), expected, tolerance = tol_deterministic)
  expect_false(isTRUE(all.equal(compute_I(se2_j, sigma_tau), arithmetic, tolerance = 1e-3)))
})

test_that("compute_I validates inputs and supports zero sigma_tau diagnostic edge", {
  expect_identical(compute_I(c(0.04, 0.09), sigma_tau = 0), 0)

  for (bad_se2 in list(numeric(0), 0.04, c(0.04, 0), c(0.04, -0.1), c(0.04, NA), c(0.04, Inf))) {
    expect_multisitedgp_error(
      compute_I(bad_se2, sigma_tau = 0.2),
      "multisitedgp_arg_error"
    )
  }
  expect_multisitedgp_error(
    compute_I(c(0.04, 0.09), sigma_tau = -0.2),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    compute_I(c(0.04, 0.09), sigma_tau = c(0.2, 0.3)),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    compute_I(c(0.04, 0.09), sigma_tau = 0.2, tau_j = 1),
    "multisitedgp_arg_error"
  )
})

test_that("compute_shrinkage returns per-site retention proportions", {
  se2_j <- c(0.04, 0.09, 0.16)
  sigma_tau <- 0.2
  expected <- sigma_tau^2 / (sigma_tau^2 + se2_j)

  expect_equal(compute_shrinkage(se2_j, sigma_tau), expected, tolerance = tol_deterministic)
  expect_equal(compute_shrinkage(rev(se2_j), sigma_tau, monotone = TRUE), sort(expected), tolerance = tol_deterministic)
  expect_identical(compute_shrinkage(se2_j, sigma_tau = 0), rep(0, length(se2_j)))
  expect_multisitedgp_error(
    compute_shrinkage(c(0.04, 0), sigma_tau = 0.2),
    "multisitedgp_arg_error"
  )
})

test_that("informativeness dispatches numeric and multisitedgp_data paths", {
  se2_j <- c(0.04, 0.09, 0.16)
  dat <- new_diagnostic_data()
  attr(dat, "diagnostics")$I_hat <- 999

  expect_equal(informativeness(se2_j, sigma_tau = 0.2), compute_I(se2_j, 0.2), tolerance = tol_deterministic)
  expect_equal(
    informativeness(dat),
    compute_I(dat$se2_j, attr(dat, "design", exact = TRUE)$sigma_tau, tau_j = dat$tau_j),
    tolerance = tol_deterministic
  )
  expect_false(identical(informativeness(dat), attr(dat, "diagnostics")$I_hat))

  dat_without_design <- multisitedgp_internal(".new_multisitedgp_data")(sample_diagnostic_frame())
  expect_multisitedgp_error(
    informativeness(dat_without_design),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    informativeness(se2_j),
    "multisitedgp_arg_error"
  )
})

test_that("mean_shrinkage supports data, numeric, and named closed-form paths", {
  se2_j <- c(0.04, 0.09)
  sigma_tau <- 0.2
  dat <- new_diagnostic_data()

  expect_equal(
    mean_shrinkage(se2_j, sigma_tau = sigma_tau),
    mean(sigma_tau^2 / (sigma_tau^2 + se2_j)),
    tolerance = tol_deterministic
  )
  expect_equal(
    mean_shrinkage(dat),
    mean_shrinkage(dat$se2_j, sigma_tau = attr(dat, "design", exact = TRUE)$sigma_tau),
    tolerance = tol_deterministic
  )

  expected_closed <- 0.15^2 / (0.15^2 + 0.064)
  expect_equal(
    mean_shrinkage(nj_mean = 50, sigma_tau = 0.15, varY = 1, p = 0.5, R2 = 0.2),
    expected_closed,
    tolerance = tol_deterministic
  )

  vectorized <- mean_shrinkage(nj_mean = c(20, 50), sigma_tau = c(0.1, 0.2))
  expected_vectorized <- c(0.1, 0.2)^2 / (c(0.1, 0.2)^2 + compute_kappa() / c(20, 50))
  expect_equal(vectorized, expected_vectorized, tolerance = tol_deterministic)
})

test_that("mean_shrinkage validates named closed-form path", {
  expect_multisitedgp_error(
    mean_shrinkage(50, 0.15, R2 = 0.2),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    mean_shrinkage(nj_mean = 0, sigma_tau = 0.15),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    mean_shrinkage(nj_mean = 50, sigma_tau = -0.15),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    mean_shrinkage(nj_mean = 50, sigma_tau = 0.15, varY = 0),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    mean_shrinkage(nj_mean = 50, sigma_tau = 0.15, p = 1),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    mean_shrinkage(nj_mean = 50, sigma_tau = 0.15, R2 = 1),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    mean_shrinkage(nj_mean = 1:3, sigma_tau = c(0.1, 0.2)),
    "multisitedgp_arg_error"
  )
})

test_that("Step 6.3 diagnostic helpers are exported and validate inputs", {
  expect_true(all(c(
    "heterogeneity_ratio", "realized_rank_corr", "realized_rank_corr_marginal",
    "bhattacharyya_coef", "ks_distance"
  ) %in% getNamespaceExports("multisiteDGP")))

  se2_j <- c(1, 2, 4, 8)
  expect_equal(heterogeneity_ratio(se2_j), 8, tolerance = tol_deterministic)
  expect_warning(
    trimmed <- heterogeneity_ratio(se2_j, trimmed = TRUE),
    "5th/95th percentile ratio"
  )
  qs <- stats::quantile(se2_j, c(0.05, 0.95), names = FALSE, type = 7)
  expect_equal(trimmed, qs[[2L]] / qs[[1L]], tolerance = tol_deterministic)
  expect_warning(heterogeneity_ratio(seq_len(19L), trimmed = TRUE), "5th/95th percentile ratio")
  expect_warning(heterogeneity_ratio(seq_len(20L), trimmed = TRUE), NA)

  expect_multisitedgp_error(heterogeneity_ratio(1), "multisitedgp_arg_error")
  expect_multisitedgp_error(heterogeneity_ratio(c(1, 0)), "multisitedgp_arg_error")
  expect_multisitedgp_error(realized_rank_corr(data.frame()), "multisitedgp_arg_error")
  expect_multisitedgp_error(bhattacharyya_coef(1:3, bins = 1L), "multisitedgp_arg_error")
})

test_that("heterogeneity_ratio and rank-correlation helpers support wrapper objects", {
  data_x <- tibble::tibble(x_site = seq(-1, 1, length.out = 60L))
  out <- sim_multisite(
    J = 60L,
    formula = ~ x_site,
    beta = 1.5,
    data = data_x,
    dependence = "rank",
    rank_corr = -0.3,
    seed = 6301L
  )

  expect_equal(heterogeneity_ratio(out), max(out$se2_j) / min(out$se2_j), tolerance = tol_deterministic)
  expect_equal(
    realized_rank_corr(out, on = "residual"),
    multisitedgp_internal(".realized_spearman")(out$z_j, out$se2_j),
    tolerance = tol_deterministic
  )
  expect_equal(
    realized_rank_corr_marginal(out),
    realized_rank_corr(out, on = "marginal"),
    tolerance = tol_deterministic
  )
  expect_lt(abs(realized_rank_corr(out, on = "residual") - -0.3), tol_spearman_continuous)
  expect_gt(abs(realized_rank_corr(out, on = "marginal") - realized_rank_corr(out, on = "residual")), tol_spearman_ties)
})

test_that("distribution-distance helpers support numeric paths and preserve RNG on data paths", {
  x <- seq(-1, 1, length.out = 100L)
  y <- seq(2, 4, length.out = 100L)

  expect_equal(ks_distance(x, x), 0, tolerance = tol_deterministic)
  expect_equal(bhattacharyya_coef(x, x, bins = 20L), 1, tolerance = tol_deterministic)
  expect_gt(ks_distance(x, y), ks_distance(x, x))
  expect_lt(bhattacharyya_coef(x, y, bins = 20L), bhattacharyya_coef(x, x, bins = 20L))

  out <- sim_multisite(J = 80L, tau = 2, seed = 6302L)
  set.seed(9302L)
  before <- .Random.seed
  ks_1 <- ks_distance(out)
  bc_1 <- bhattacharyya_coef(out)
  qq_1 <- multisitedgp_internal(".qq_residual_max_abs")(out)
  after <- .Random.seed

  expect_identical(after, before)
  expect_identical(ks_1, ks_distance(out))
  expect_identical(bc_1, bhattacharyya_coef(out))
  expect_identical(qq_1, multisitedgp_internal(".qq_residual_max_abs")(out))
  expect_true(is.finite(ks_1))
  expect_true(is.finite(bc_1))
  expect_true(is.finite(qq_1))
})

test_that("core target-vs-realized diagnostics table is attached to wrapper output", {
  out <- sim_meta(I = 0.4, R = 3, seed = 6303L)
  tab <- attr(out, "diagnostics", exact = TRUE)$target_vs_realized

  expect_s3_class(tab, "tbl_df")
  expect_named(tab, c(
    "group", "diagnostic", "basis", "target", "realized", "delta", "rel_delta",
    "target_source", "helper", "api_id", "p_value", "note", "status",
    "threshold_metric", "pass_rule", "warn_rule", "fail_rule",
    "threshold_source", "threshold_note"
  ))
  expect_setequal(tab$group, c("A", "B", "C", "D"))
  expect_identical(nrow(tab), 14L)
  expect_true(all(c("API030", "API034", "API038", "API039") %in% tab$api_id))

  row_i <- tab[tab$diagnostic == "informativeness" & tab$basis == "overall", ]
  row_r <- tab[tab$diagnostic == "heterogeneity_ratio" & tab$basis == "overall", ]
  row_gm <- tab[tab$diagnostic == "GM_se2" & tab$basis == "overall", ]
  row_rank <- tab[tab$diagnostic == "realized_rank_corr" & tab$basis == "residual", ]
  row_ks <- tab[tab$diagnostic == "ks_distance" & tab$basis == "target_G", ]
  row_feas <- tab[tab$diagnostic == "feasibility_index" & tab$basis == "efron", ]

  expect_equal(row_i$target, 0.4, tolerance = tol_deterministic)
  expect_equal(row_i$realized, compute_I(out$se2_j, sigma_tau = 0.2), tolerance = tol_deterministic)
  expect_identical(row_i$target_source, "direct_design")
  expect_equal(row_r$target, 3, tolerance = tol_deterministic)
  expect_equal(row_r$realized, heterogeneity_ratio(out), tolerance = tol_deterministic)
  expect_equal(row_gm$target, 0.2^2 * (1 - 0.4) / 0.4, tolerance = tol_deterministic)
  expect_equal(row_rank$target, 0, tolerance = tol_deterministic)
  expect_equal(row_ks$target, 0, tolerance = tol_deterministic)
  expect_true(is.finite(row_ks$p_value))
  expect_identical(row_feas$helper, "feasibility_index")
  expect_equal(row_feas$realized, sum(compute_shrinkage(out$se2_j, sigma_tau = 0.2)), tolerance = tol_deterministic)
})

test_that("core diagnostics table keeps unsupported automatic G references as NA", {
  out <- sim_multisite(
    J = 50L,
    true_dist = "Mixture",
    theta_G = list(delta = 5, eps = 0.3, ups = 2),
    seed = 6304L
  )
  tab <- attr(out, "diagnostics", exact = TRUE)$target_vs_realized
  dist_rows <- tab[tab$group == "C", ]

  expect_multisitedgp_error(ks_distance(out), "multisitedgp_arg_error")
  expect_true(all(is.na(dist_rows$realized)))
})
# nolint end
