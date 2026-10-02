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

dat_ok <- function(...) {
  sim_multisite(multisitedgp_design(J = 20L, sigma_tau = 0.2, nj_mean = 100,
                                    seed = 1L, ...))
}

test_that("compute_kappa is 4 by default and validates p, R2 and var_outcome", {
  expect_equal(compute_kappa(), 4, tolerance = tol_deterministic)
  expect_equal(compute_kappa(p = 0.5, R2 = 0.2, var_outcome = 2), 6.4, tolerance = tol_deterministic)

  expect_multisitedgp_error(
    compute_kappa(p = 1)
  )
  expect_multisitedgp_error(
    compute_kappa(R2 = 1)
  )
  expect_multisitedgp_error(
    compute_kappa(var_outcome = 0)
  )
})

test_that("se2_j = kappa / n_j gives se2_j times n_j equal to kappa", {
  n_j <- c(20L, 40L, 80L)
  kappa <- compute_kappa()
  se2_j <- kappa / n_j

  expect_equal(se2_j * n_j, rep(kappa, length(n_j)), tolerance = tol_deterministic)
})

test_that("compute_I uses the geometric mean of se2_j, not the arithmetic mean", {
  se2_j <- c(0.01, 0.05, 0.1, 0.5, 1.0)
  sigma_tau <- 0.5
  expected <- sigma_tau^2 / (sigma_tau^2 + exp(mean(log(se2_j))))
  arithmetic <- sigma_tau^2 / (sigma_tau^2 + mean(se2_j))

  expect_equal(compute_I(se2_j, sigma_tau), expected, tolerance = tol_deterministic)
  expect_false(isTRUE(all.equal(compute_I(se2_j, sigma_tau), arithmetic, tolerance = 1e-3)))
})

test_that("compute_I equals the geometric-mean formula and differs from the arithmetic-mean one", {
  se2_j <- c(0.01, 0.05, 0.1, 0.5, 1.0)
  sigma_tau <- 0.5
  expected <- sigma_tau^2 / (sigma_tau^2 + exp(mean(log(se2_j))))
  arithmetic <- sigma_tau^2 / (sigma_tau^2 + mean(se2_j))

  actual <- compute_I(se2_j, sigma_tau)

  expect_equal(actual, expected, tolerance = tol_deterministic)
  expect_false(isTRUE(all.equal(actual, arithmetic, tolerance = 1e-3)))
})

test_that("compute_I validates inputs and supports zero sigma_tau diagnostic edge", {
  expect_identical(compute_I(c(0.04, 0.09), sigma_tau = 0), 0)

  for (bad_se2 in list(numeric(0), 0.04, c(0.04, 0), c(0.04, -0.1), c(0.04, NA), c(0.04, Inf))) {
    expect_multisitedgp_error(
      compute_I(bad_se2, sigma_tau = 0.2)
    )
  }
  expect_multisitedgp_error(
    compute_I(c(0.04, 0.09), sigma_tau = -0.2)
  )
  expect_multisitedgp_error(
    compute_I(c(0.04, 0.09), sigma_tau = c(0.2, 0.3))
  )
  expect_multisitedgp_error(
    compute_I(c(0.04, 0.09), sigma_tau = 0.2, tau_j = 1)
  )
})

test_that("compute_I refuses a tau_j that does not match se2_j", {
  # A tau_j that is too short is caught by the minimum-length check. This tests
  # the separate check for one that is too long.
  expect_multisitedgp_error(
    compute_I(se2_j = c(0.01, 0.02), sigma_tau = 0.2, tau_j = c(1, 2, 3))
  )
  expect_no_error(
    compute_I(se2_j = c(0.01, 0.02), sigma_tau = 0.2, tau_j = c(1, 2))
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
    compute_shrinkage(c(0.04, 0), sigma_tau = 0.2)
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
    informativeness(dat_without_design)
  )
  expect_multisitedgp_error(
    informativeness(se2_j)
  )
})

test_that("informativeness rejects an object it has no method for", {
  for (x in list(list(a = 1), TRUE, as.Date("2026-08-15"))) {
    expect_multisitedgp_error(informativeness(x))
  }
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
    mean_shrinkage(50, 0.15, R2 = 0.2)
  )
  expect_multisitedgp_error(
    mean_shrinkage(nj_mean = 0, sigma_tau = 0.15)
  )
  expect_multisitedgp_error(
    mean_shrinkage(nj_mean = 50, sigma_tau = -0.15)
  )
  expect_multisitedgp_error(
    mean_shrinkage(nj_mean = 50, sigma_tau = 0.15, varY = 0)
  )
  expect_multisitedgp_error(
    mean_shrinkage(nj_mean = 50, sigma_tau = 0.15, p = 1)
  )
  expect_multisitedgp_error(
    mean_shrinkage(nj_mean = 50, sigma_tau = 0.15, R2 = 1)
  )
  expect_multisitedgp_error(
    mean_shrinkage(nj_mean = 1:3, sigma_tau = c(0.1, 0.2))
  )
})

test_that("mean_shrinkage rejects an object it has no method for", {
  expect_multisitedgp_error(mean_shrinkage(list(a = 1)))
  expect_multisitedgp_error(mean_shrinkage(as.Date("2026-08-15")))
})

test_that("mean_shrinkage on data needs the design's sigma_tau", {
  dat <- dat_ok()
  attr(dat, "design") <- NULL
  expect_multisitedgp_error(mean_shrinkage(dat))

  stripped <- dat_ok()
  design <- attr(stripped, "design")
  design$sigma_tau <- NULL
  attr(stripped, "design") <- design
  expect_multisitedgp_error(mean_shrinkage(stripped))
})

test_that("mean_shrinkage's numeric method needs sigma_tau", {
  expect_multisitedgp_error(mean_shrinkage(c(0.01, 0.02)))
  expect_no_error(mean_shrinkage(c(0.01, 0.02), sigma_tau = 0.2))
})

test_that("the closed-form path needs both nj_mean and sigma_tau", {
  expect_multisitedgp_error(mean_shrinkage(nj_mean = 50))
  expect_multisitedgp_error(mean_shrinkage(sigma_tau = 0.2))
  expect_no_error(mean_shrinkage(nj_mean = 50, sigma_tau = 0.2))
})

test_that("the feasibility index is not part of the package", {
  # Removed in 0.3.0. Its value was sum(compute_shrinkage(se2_j, sigma_tau)).
  expect_false("feasibility_index" %in% getNamespaceExports("multisiteDGP"))

  out <- sim_meta(J = 25L, I = 0.4, R = 3, seed = 6401L)
  tab <- attr(out, "diagnostics", exact = TRUE)$target_vs_realized
  expect_false("feasibility_index" %in% tab$diagnostic)
  expect_false("feasibility" %in% tab$group)
})

test_that("default_thresholds returns the public audit gates", {
  expect_true("default_thresholds" %in% getNamespaceExports("multisiteDGP"))
  expect_identical(
    default_thresholds(),
    list(
      mean_shrinkage_min = 0.30,
      R_max = 30.0,
      ks_scaled_max = 1.63
    )
  )
})

test_that("default_thresholds stays exported alongside scenario_audit", {
  exports <- getNamespaceExports("multisiteDGP")

  expect_true("default_thresholds" %in% exports)
})

test_that("diagnostic threshold boundary helpers classify exact edges", {
  expect_identical(multisitedgp_internal(".status_by_abs_delta")(0.019, "informativeness"), "pass")
  expect_identical(multisitedgp_internal(".status_by_abs_delta")(0.02, "informativeness"), "warn")
  expect_identical(multisitedgp_internal(".status_by_abs_delta")(0.05, "informativeness"), "fail")

  expect_identical(multisitedgp_internal(".status_by_rel_delta")(0.099, "heterogeneity_ratio"), "pass")
  expect_identical(multisitedgp_internal(".status_by_rel_delta")(0.10, "heterogeneity_ratio"), "warn")
  expect_identical(multisitedgp_internal(".status_by_rel_delta")(0.25, "heterogeneity_ratio"), "fail")

  expect_identical(multisitedgp_internal(".status_ks")(0.051), "pass")
  expect_identical(multisitedgp_internal(".status_ks")(0.05), "warn")
  expect_identical(multisitedgp_internal(".status_ks")(0.01), "fail")

  expect_identical(multisitedgp_internal(".status_mean_shrinkage")(0.10), "pass")
  expect_identical(multisitedgp_internal(".status_mean_shrinkage")(0.05), "warn")
  expect_identical(multisitedgp_internal(".status_mean_shrinkage")(0.049), "fail")
  expect_identical(multisitedgp_internal(".status_mean_shrinkage")(0.951), "warn")
  expect_identical(multisitedgp_internal(".status_mean_shrinkage")(0.991), "fail")

  expect_identical(multisitedgp_internal(".status_average_moe")(0.40, sigma_tau = 0.20), "pass")
  expect_identical(multisitedgp_internal(".status_average_moe")(0.41, sigma_tau = 0.20), "warn")
  expect_identical(multisitedgp_internal(".status_average_moe")(0.81, sigma_tau = 0.20), "fail")

  drift <- multisitedgp_internal(".sampling_drift_cutoffs")(50L)
  expect_equal(drift, c(pass = 2 / sqrt(49), warn = 3 / sqrt(49)), tolerance = tol_deterministic)
  expect_identical(
    multisitedgp_internal(".status_by_cutoffs")(drift[["pass"]], drift[["pass"]], drift[["warn"]]),
    "pass"
  )
  expect_identical(
    multisitedgp_internal(".status_by_cutoffs")(mean(drift), drift[["pass"]], drift[["warn"]]),
    "warn"
  )
  expect_identical(
    multisitedgp_internal(".status_by_cutoffs")(drift[["warn"]] + 0.001, drift[["pass"]], drift[["warn"]]),
    "fail"
  )
})

test_that("diagnostic helpers are exported and validate inputs", {
  expect_true(all(c(
    "heterogeneity_ratio", "realized_rank_corr", "realized_rank_corr_marginal",
    "bhattacharyya_coef", "ks_distance"
  ) %in% getNamespaceExports("multisiteDGP")))

  se2_j <- c(1, 2, 4, 8)
  expect_equal(heterogeneity_ratio(se2_j), 8, tolerance = tol_deterministic)
  expect_warning(
    trimmed <- heterogeneity_ratio(se2_j, trimmed = TRUE),
    "unstable with fewer than 20 values"
  )
  qs <- stats::quantile(se2_j, c(0.05, 0.95), names = FALSE, type = 7)
  expect_equal(trimmed, qs[[2L]] / qs[[1L]], tolerance = tol_deterministic)
  expect_warning(heterogeneity_ratio(seq_len(19L), trimmed = TRUE), "unstable with fewer than 20 values")
  expect_warning(heterogeneity_ratio(seq_len(20L), trimmed = TRUE), NA)

  expect_multisitedgp_error(heterogeneity_ratio(1))
  expect_multisitedgp_error(heterogeneity_ratio(c(1, 0)))
  expect_multisitedgp_error(realized_rank_corr(data.frame()))
  expect_multisitedgp_error(bhattacharyya_coef(1:3, bins = 1L))
})

test_that("heterogeneity_ratio and the rank-correlation helpers accept `sim_multisite()` output", {
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

test_that("the target-versus-realized table is attached to `sim_meta()` output", {
  out <- sim_meta(I = 0.4, R = 3, seed = 6303L)
  tab <- attr(out, "diagnostics", exact = TRUE)$target_vs_realized

  expect_s3_class(tab, "tbl_df")
  expect_named(tab, c(
    "group", "diagnostic", "basis", "target", "realized", "delta", "rel_delta",
    "target_source", "p_value", "note", "status",
    "threshold_metric", "pass_rule", "warn_rule", "fail_rule",
    "threshold_source", "threshold_note"
  ))
  expect_identical(
    tab$group,
    rep(c("scale", "dependence", "shape", "precision"), times = c(4L, 5L, 3L, 2L))
  )
  expect_identical(nrow(tab), 14L)

  row_i <- tab[tab$diagnostic == "informativeness" & tab$basis == "overall", ]
  row_r <- tab[tab$diagnostic == "heterogeneity_ratio" & tab$basis == "overall", ]
  row_gm <- tab[tab$diagnostic == "GM_se2" & tab$basis == "overall", ]
  row_rank <- tab[tab$diagnostic == "realized_rank_corr" & tab$basis == "residual", ]
  row_ks <- tab[tab$diagnostic == "ks_distance" & tab$basis == "target_G", ]
  row_shrinkage <- tab[tab$diagnostic == "mean_shrinkage" & tab$basis == "overall", ]

  expect_equal(row_i$target, 0.4, tolerance = tol_deterministic)
  expect_equal(row_i$realized, compute_I(out$se2_j, sigma_tau = 0.2), tolerance = tol_deterministic)
  expect_identical(row_i$target_source, "direct_design")
  expect_equal(row_r$target, 3, tolerance = tol_deterministic)
  expect_equal(row_r$realized, heterogeneity_ratio(out), tolerance = tol_deterministic)
  expect_equal(row_gm$target, 0.2^2 * (1 - 0.4) / 0.4, tolerance = tol_deterministic)
  expect_equal(row_rank$target, 0, tolerance = tol_deterministic)
  expect_equal(row_ks$target, 0, tolerance = tol_deterministic)
  expect_true(is.finite(row_ks$p_value))
  expect_identical(row_ks$threshold_source, "analytic-one-sample")
  expect_identical(row_ks$note, "one-sample KS against analytic target CDF")
  expect_identical(row_shrinkage$group, "precision")
  expect_true(all(tab$status %in% c("pass", "warn", "fail", NA)))
  expect_equal(
    row_shrinkage$realized,
    mean(compute_shrinkage(out$se2_j, sigma_tau = 0.2)),
    tolerance = tol_deterministic
  )
})

test_that("the diagnostics table of `sim_meta()` output carries status and threshold columns", {
  out <- sim_meta(I = 0.4, R = 3, seed = 6402L)
  tab <- attr(out, "diagnostics", exact = TRUE)$target_vs_realized
  row_i <- tab[tab$diagnostic == "informativeness" & tab$basis == "overall", ]
  row_r <- tab[tab$diagnostic == "heterogeneity_ratio" & tab$basis == "overall", ]
  row_sigma <- tab[tab$diagnostic == "sigma_tau" & tab$basis == "residual", ]
  row_bc <- tab[tab$diagnostic == "bhattacharyya_coef", ]
  row_qq <- tab[tab$diagnostic == "qq_residuals", ]
  row_rank_marg <- tab[tab$diagnostic == "realized_rank_corr" & tab$basis == "marginal", ]

  expect_true(all(c(
    "status", "threshold_metric", "pass_rule", "warn_rule", "fail_rule",
    "threshold_source", "threshold_note"
  ) %in% names(tab)))
  expect_identical(row_i$status, "pass")
  expect_identical(row_i$threshold_metric, "abs_delta")
  expect_identical(row_r$status, "pass")
  expect_identical(row_sigma$threshold_metric, "sampling_se")
  expect_identical(row_sigma$threshold_source, "sampling-theory")
  expect_match(row_sigma$pass_rule, "2 * SE", fixed = TRUE)
  # sigma_tau = 0.2, Gaussian, J = 50: 0.2 * sqrt(2 / (4 * 49)) = 0.0202
  expect_match(row_sigma$threshold_note, "SE = 0.0202", fixed = TRUE)
  # The Bhattacharyya coefficient and the Q-Q residual are reported without a
  # status and without a rule.
  for (row in list(row_bc, row_qq)) {
    expect_true(is.finite(row$realized))
    expect_identical(row$status, NA_character_)
    expect_identical(row$threshold_metric, NA_character_)
    expect_identical(row$pass_rule, NA_character_)
  }
  expect_identical(row_rank_marg$status, NA_character_)
  expect_identical(row_rank_marg$threshold_metric, "not_applicable")
})

# The status of the sigma_tau row compares sd(z_j) * sigma_tau with sigma_tau
# in units of the standard error of a sample standard deviation,
# sigma_tau * sqrt((kurtosis - 1 + 2 / (J - 1)) / (4 * J)), where the kurtosis
# is that of the design's distribution of z_j. For a kurtosis of 3 this is
# sigma_tau / sqrt(2 * (J - 1)).
kurtosis_cases <- function() {
  list(
    list(true_dist = "Gaussian", theta_G = list(), kurtosis = 3),
    list(true_dist = "StudentT", theta_G = list(nu = 10), kurtosis = 4),
    list(true_dist = "SkewN", theta_G = list(slant = 2), kurtosis = 3.31),
    list(true_dist = "ALD", theta_G = list(rho = 0.3), kurtosis = 7.43),
    list(true_dist = "Mixture", theta_G = list(delta = 5, eps = 0.3, ups = 2), kurtosis = 3.18),
    list(true_dist = "PointMassSlab", theta_G = list(pi0 = 0.3, slab_shape = "Gaussian"), kurtosis = 3 / 0.7),
    list(true_dist = "PointMassSlab", theta_G = list(pi0 = 0.3, slab_shape = "Laplace"), kurtosis = 6 / 0.7),
    # The generator takes the slab shape in any letter case.
    list(true_dist = "PointMassSlab", theta_G = list(pi0 = 0.5, slab_shape = "laplace"), kurtosis = 12)
  )
}

test_that("the kurtosis of each built-in distribution has its closed-form value", {
  z_kurtosis <- multisitedgp_internal(".z_kurtosis")
  # The expected values are computed independently of the package. Those with
  # two decimals are rounded, so the difference may be up to 0.005.
  for (case in kurtosis_cases()) {
    design <- multisitedgp_design(true_dist = case$true_dist, theta_G = case$theta_G)
    expect_lt(abs(z_kurtosis(design) - case$kurtosis), 0.005, label = case$true_dist)
  }

  # Limiting cases with a known value.
  expect_equal(z_kurtosis(multisitedgp_design(true_dist = "StudentT", theta_G = list(nu = 5))), 9)
  expect_equal(z_kurtosis(multisitedgp_design(true_dist = "ALD", theta_G = list(rho = 0.5))), 6)
})

test_that("the kurtosis is NA when the distribution has none or it is unknown", {
  z_kurtosis <- multisitedgp_internal(".z_kurtosis")
  user <- multisitedgp_design(g_fn = function(J) stats::rnorm(J))
  heavy <- suppressWarnings(multisitedgp_design(true_dist = "StudentT", theta_G = list(nu = 4)))

  expect_identical(z_kurtosis(user), NA_real_)
  expect_identical(z_kurtosis(heavy), NA_real_)
  expect_identical(z_kurtosis(NULL), NA_real_)
})

test_that("the standard error of the sigma_tau row matches the spread of sd(z_j)", {
  standard_error <- multisitedgp_internal(".sigma_tau_standard_error")
  # 4,000 samples of 200 sites. Their standard deviations have a standard
  # deviation that is itself estimated with a relative standard error of about
  # 1.1 percent, so 4 standard errors are 4.5 percent. The formula halves the
  # relative standard error of the sample variance, which overstates the
  # spread of sd(z_j) by up to 3.5 percent at J = 200 for the heavy-tailed
  # distributions. The tolerance is their sum.
  J <- 200L
  n_samples <- 4000L
  for (case in kurtosis_cases()) {
    design <- multisitedgp_design(J = J, sigma_tau = 0.2, true_dist = case$true_dist, theta_G = case$theta_G)
    z <- withr::with_seed(
      6421L,
      gen_effects(J = J * n_samples, true_dist = case$true_dist, theta_G = case$theta_G)$z_j
    )
    spread <- stats::sd(vapply(consecutive_samples(z, J), stats::sd, numeric(1))) * 0.2
    expect_equal(
      spread,
      standard_error(0.2, design = design, J = J),
      tolerance = 0.08,
      info = case$true_dist
    )
  }
})

test_that("the sigma_tau status is pass within 2 standard errors and fail beyond 3", {
  spec_for_row <- multisitedgp_internal(".threshold_spec_for_row")
  evaluate <- multisitedgp_internal(".evaluate_threshold_status")
  specs <- multisitedgp_internal(".diagnostic_threshold_rubric")()
  design <- multisitedgp_design(J = 50L, sigma_tau = 0.2)
  se <- 0.2 * sqrt(2 / (4 * 49))

  status_at <- function(n_se) {
    row <- tibble::tibble(
      group = "scale", diagnostic = "sigma_tau", basis = "residual",
      target = 0.2, realized = 0.2 + n_se * se, delta = n_se * se, rel_delta = n_se * se / 0.2,
      p_value = NA_real_
    )
    evaluate(row, spec_for_row(row, specs, design = design, J = 50L), sigma_tau = 0.2)
  }

  expect_identical(status_at(0), "pass")
  expect_identical(status_at(1.99), "pass")
  expect_identical(status_at(-1.99), "pass")
  expect_identical(status_at(2.01), "warn")
  expect_identical(status_at(-2.99), "warn")
  expect_identical(status_at(3.01), "fail")
  expect_identical(status_at(-3.01), "fail")
  # A row without a realized value has no status.
  expect_identical(status_at(NA_real_), NA_character_)
})

test_that("the sigma_tau status of simulated data follows the rule at every number of sites", {
  # The rule is recomputed here from the table's own target and realized value.
  for (J in c(25L, 50L, 400L)) {
    se <- 0.2 * sqrt(2 / (4 * (J - 1)))
    for (seed in 6431:6440) {
      out <- sim_meta(J = J, I = 0.4, R = 3, sigma_tau = 0.2, seed = seed)
      tab <- attr(out, "diagnostics", exact = TRUE)$target_vs_realized
      row <- tab[tab$diagnostic == "sigma_tau", ]
      expected <- as.character(cut(
        abs(row$realized - 0.2) / se, c(-Inf, 2, 3, Inf),
        labels = c("pass", "warn", "fail")
      ))
      expect_identical(row$status, expected, info = sprintf("J = %d, seed = %d", J, seed))
    }
  }
})

test_that("a correctly simulated design passes the sigma_tau row about 95 times in 100", {
  spec_for_row <- multisitedgp_internal(".threshold_spec_for_row")
  evaluate <- multisitedgp_internal(".evaluate_threshold_status")
  specs <- multisitedgp_internal(".diagnostic_threshold_rubric")()

  # For normal z_j, (J - 1) * var(z_j) is chi-square with J - 1 degrees of
  # freedom, which gives the exact probability of each status from the limits
  # of the rule: pass 0.956 at 25 sites and 0.955 at 400. (The rule that 0.3.0
  # replaced, within 5 percent of sigma_tau, passed with probability 0.27 at
  # 25 sites and 0.84 at 400.) The observed rates over 4,000 samples must be
  # within 4 standard errors of the exact ones.
  n_samples <- 4000L
  for (J in c(25L, 400L)) {
    design <- multisitedgp_design(J = J, sigma_tau = 0.2)
    target <- list(diagnostic = "sigma_tau", basis = "residual", target = 0.2)
    spec <- spec_for_row(target, specs, design = design, J = J)
    z <- withr::with_seed(6450L + J, gen_effects(J = J * n_samples)$z_j)
    realized <- 0.2 * vapply(consecutive_samples(z, J), stats::sd, numeric(1))
    status <- vapply(realized, function(value) {
      evaluate(c(target, list(realized = value, delta = value - 0.2)), spec, sigma_tau = 0.2)
    }, character(1))

    within <- function(limit) {
      half_width <- limit / 0.2
      stats::pchisq((J - 1) * (1 + half_width)^2, J - 1) - stats::pchisq((J - 1) * (1 - half_width)^2, J - 1)
    }
    p_pass <- within(spec$pass_cutoff)
    p_fail <- 1 - within(spec$warn_cutoff)
    expect_gt(p_pass, 0.95)
    expect_lt(p_pass, 0.96)
    expect_lt(p_fail, 0.003)
    expect_lt(abs(mean(status == "pass") - p_pass), 4 * se_proportion(n_samples, p_pass))
    expect_lt(abs(mean(status == "fail") - p_fail), 4 * se_proportion(n_samples, p_fail))
  }
})

test_that("the sigma_tau rule holds for a kurtosis below 3 and few sites", {
  spec_for_row <- multisitedgp_internal(".threshold_spec_for_row")
  evaluate <- multisitedgp_internal(".evaluate_threshold_status")
  specs <- multisitedgp_internal(".diagnostic_threshold_rubric")()

  # Two well separated components give a kurtosis of 1.23, so the term
  # 2 / (J - 1) of the standard error matters at small J. Without it the row
  # passed 87 percent of such samples at 10 sites and 91 percent at 20. With
  # it the pass rate is 0.946 and 0.952 (30 other seeds). The distribution of
  # sd(z_j) is far from normal here, so the rate is not compared with 0.954.
  # The lower limit is 4 standard errors of a proportion below 0.946.
  theta <- list(delta = 8, eps = 0.5, ups = 1)
  n_samples <- 4000L
  design <- multisitedgp_design(sigma_tau = 0.2, true_dist = "Mixture", theta_G = theta)
  for (J in c(10L, 20L)) {
    target <- list(diagnostic = "sigma_tau", basis = "residual", target = 0.2)
    spec <- spec_for_row(target, specs, design = design, J = J)
    z <- withr::with_seed(6470L + J, gen_effects(J = J * n_samples, true_dist = "Mixture", theta_G = theta)$z_j)
    realized <- 0.2 * vapply(consecutive_samples(z, J), stats::sd, numeric(1))
    status <- vapply(realized, function(value) {
      evaluate(c(target, list(realized = value, delta = value - 0.2)), spec, sigma_tau = 0.2)
    }, character(1))

    expect_gt(mean(status == "pass"), 0.946 - 4 * se_proportion(n_samples, 0.946))
    expect_lt(mean(status == "fail"), 0.03)
  }
})

test_that("the sigma_tau row has no status when 2 standard errors reach sigma_tau", {
  spec_for_row <- multisitedgp_internal(".threshold_spec_for_row")
  specs <- multisitedgp_internal(".diagnostic_threshold_rubric")()
  target <- list(diagnostic = "sigma_tau", basis = "residual", target = 0.2)

  # A standard deviation is never negative, so such a row could not fail from
  # below. Normal draws: the standard error is sigma_tau / 2 with 3 sites and
  # sigma_tau / sqrt(6) with 4.
  gaussian <- multisitedgp_design(sigma_tau = 0.2)
  expect_identical(spec_for_row(target, specs, design = gaussian, J = 3L)$metric, "not_applicable")
  expect_identical(spec_for_row(target, specs, design = gaussian, J = 4L)$metric, "sampling_se")

  # A point mass of 0.9 with a Laplace slab has kurtosis 60. With 20 sites
  # the standard error is 0.86 times sigma_tau.
  out <- sim_multisite(
    J = 20L, true_dist = "PointMassSlab", theta_G = list(pi0 = 0.9, slab_shape = "Laplace"), seed = 6481L
  )
  tab <- attr(out, "diagnostics", exact = TRUE)$target_vs_realized
  row <- tab[tab$diagnostic == "sigma_tau", ]
  expect_identical(row$status, NA_character_)
  expect_identical(row$threshold_metric, "not_applicable")
  expect_match(row$threshold_note, "half of sigma_tau", fixed = TRUE)
})

test_that("the sigma_tau row has no status without a finite kurtosis", {
  user <- sim_multisite(J = 50L, g_fn = function(J) stats::rnorm(J), seed = 6461L)
  heavy <- suppressWarnings(sim_multisite(J = 50L, true_dist = "StudentT", theta_G = list(nu = 3), seed = 6462L))

  for (out in list(user, heavy)) {
    tab <- attr(out, "diagnostics", exact = TRUE)$target_vs_realized
    row <- tab[tab$diagnostic == "sigma_tau", ]
    expect_true(is.finite(row$realized))
    expect_identical(row$status, NA_character_)
    expect_identical(row$threshold_metric, "not_applicable")
    expect_match(row$threshold_note, "no standard error", fixed = TRUE)
  }
})

# The standard error of a sample standard deviation, from the fourth moment of
# the standardized distribution: the exact variance of the unbiased sample
# variance, then the derivative of the square root. No function of the
# package is used.
sd_standard_error <- function(sigma, fourth, J) {
  variance <- sigma^2
  variance_of_sample_variance <- (fourth - 1) * variance^2 / J + 2 * variance^2 / (J * (J - 1))
  sqrt(variance_of_sample_variance) / (2 * sigma)
}

# The rule of the sigma_tau row, written out without the package.
sigma_tau_status <- function(realized, sigma, se) {
  if (!is.finite(se) || 2 * se >= sigma) {
    return(NA_character_)
  }
  distance <- abs(realized - sigma)
  if (distance <= 2 * se) {
    "pass"
  } else if (distance <= 3 * se) {
    "warn"
  } else {
    "fail"
  }
}

# One line of the printed summary, found by its label. Some data below have
# tied values on purpose, and the KS test then warns about ties. That warning
# is muffled. Any other warning stays.
summary_line <- function(x, label) {
  lines <- utils::capture.output(withCallingHandlers(
    summary(x),
    warning = function(w) {
      if (grepl("ties should not be present", conditionMessage(w), fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  ))
  found <- lines[grepl(paste0("^\\s*", label, "\\s"), lines, perl = TRUE)]
  expect_length(found, 1L)
  found[[1L]]
}

# The status at the end of a summary line, or NA when the line has none.
summary_status <- function(x, label) {
  line <- summary_line(x, label)
  if (!grepl("(pass|warn|fail)$", line)) {
    return(NA_character_)
  }
  sub(".* (pass|warn|fail)$", "\\1", line)
}

test_that("the sigma_tau status of simulated data follows a standard error from the fourth moment", {
  # Student t: E(T^2) = nu / (nu - 2) and E(T^4) = 3 nu^2 / ((nu - 2)(nu - 4)).
  student_fourth <- function(nu) {
    raw_variance <- nu / (nu - 2)
    raw_fourth <- 3 * nu^2 / ((nu - 2) * (nu - 4))
    raw_fourth / raw_variance^2
  }
  # Point mass and slab: only the slab contributes, with probability 1 - pi0.
  # A standard normal slab has fourth moment 3 and a standardized Laplace slab 6.
  slab_fourth <- function(pi0, slab) slab / (1 - pi0)
  laplace <- function(pi0) list(pi0 = pi0, slab_shape = "Laplace")
  cases <- list(
    list(true_dist = "Gaussian", theta_G = list(), fourth = 3),
    list(true_dist = "StudentT", theta_G = list(nu = 5), fourth = student_fourth(5)),
    list(true_dist = "StudentT", theta_G = list(nu = 10), fourth = student_fourth(10)),
    list(true_dist = "PointMassSlab", theta_G = list(pi0 = 0.25), fourth = slab_fourth(0.25, 3)),
    list(true_dist = "PointMassSlab", theta_G = list(pi0 = 0.5), fourth = slab_fourth(0.5, 3)),
    list(true_dist = "PointMassSlab", theta_G = laplace(0.25), fourth = slab_fourth(0.25, 6)),
    list(true_dist = "PointMassSlab", theta_G = laplace(0.5), fourth = slab_fourth(0.5, 6))
  )
  standard_error <- multisitedgp_internal(".sigma_tau_standard_error")
  z_kurtosis <- multisitedgp_internal(".z_kurtosis")
  sigma <- 0.25
  for (case in cases) {
    for (J in c(10L, 33L, 200L)) {
      design <- multisitedgp_design(
        paradigm = "direct", J = J, I = 0.5, R = 2, sigma_tau = sigma,
        true_dist = case$true_dist, theta_G = case$theta_G
      )
      se <- sd_standard_error(sigma, case$fourth, J)
      expect_equal(z_kurtosis(design), case$fourth, tolerance = tol_deterministic)
      expect_equal(standard_error(sigma, design, J), se, tolerance = tol_deterministic)

      out <- sim_meta(design, seed = 17L)
      tab <- attr(out, "diagnostics", exact = TRUE)$target_vs_realized
      row <- tab[tab$diagnostic == "sigma_tau", ]
      realized <- sigma * stats::sd(out$z_j)
      expect_equal(row$realized, realized, tolerance = tol_deterministic)
      expect_identical(row$status, sigma_tau_status(realized, sigma, se))
    }
  }
})

test_that("summary computes the sigma_tau status again at exactly 2 and 3 standard errors", {
  J <- 33L
  sigma <- 1
  # Sixteen pairs of -1 and 1 and one 0 have a standard deviation of exactly 1.
  # For normal effects and 33 sites the standard error is 1 / 8. The scaled
  # values below therefore sit exactly on the two limits or just beyond them.
  z <- c(rep(c(-1, 1), 16L), 0)
  expect_identical(stats::sd(z), 1)
  expect_equal(sd_standard_error(sigma, 3, J), 1 / 8, tolerance = tol_deterministic)
  base <- sim_meta(J = J, I = 0.5, R = 1, sigma_tau = sigma, seed = 17L)
  scales <- c(0.75, 1.25, 0.625, 1.375, 0.75 - 1e-6, 1.25 + 1e-6, 0.625 - 1e-6, 1.375 + 1e-6)
  expected <- c("pass", "pass", "warn", "warn", "warn", "warn", "fail", "fail")
  for (i in seq_along(scales)) {
    out <- base
    out$z_j <- z * scales[[i]]
    out$tau_j <- sigma * out$z_j
    out$tau_j_hat <- out$tau_j
    expect_null(attr(out, "diagnostics"))
    expect_identical(summary_status(out, "sigma_tau"), expected[[i]])
    # The Bhattacharyya coefficient is printed without a status.
    expect_identical(summary_status(out, "Bhattacharyya coefficient"), NA_character_)
  }
})

test_that("summary gives the sigma_tau row no status without a kurtosis or when 2 standard errors reach sigma_tau", {
  unknown <- sim_meta(
    J = 33L, I = 0.5, R = 2, g_fn = function(J) 0.2 * stats::rnorm(J), g_returns = "raw", seed = 17L
  )
  # A Student t distribution with nu = 4 has no finite kurtosis, and the
  # design warns about it.
  infinite <- suppressWarnings(
    sim_meta(J = 33L, I = 0.5, R = 2, true_dist = "StudentT", theta_G = list(nu = 4), seed = 17L)
  )
  for (out in list(unknown, infinite)) {
    attr(out, "diagnostics") <- NULL
    expect_identical(summary_status(out, "sigma_tau"), NA_character_)
  }

  # A point mass of 0.9 with a Laplace slab has a fourth moment of 60. With 59
  # sites 2 standard errors reach sigma_tau, and with 60 sites they do not.
  sigma <- 0.25
  for (J in c(59L, 60L)) {
    se <- sd_standard_error(sigma, 60, J)
    expect_identical(2 * se >= sigma, J == 59L)
    out <- sim_meta(
      J = J, I = 0.5, R = 2, sigma_tau = sigma, true_dist = "PointMassSlab",
      theta_G = list(pi0 = 0.9, slab_shape = "Laplace"), seed = 17L
    )
    attr(out, "diagnostics") <- NULL
    expect_identical(summary_status(out, "sigma_tau"), sigma_tau_status(sigma * stats::sd(out$z_j), sigma, se))
  }
})

test_that("shape diagnostics: KS p-values use the analytic one-sample null", {
  out <- sim_multisite(J = 50L, true_dist = "Gaussian", seed = 6309L)
  tab <- attr(out, "diagnostics", exact = TRUE)$target_vs_realized
  row <- tab[tab$diagnostic == "ks_distance", ]
  direct <- suppressWarnings(stats::ks.test(out$z_j, stats::pnorm))

  expect_equal(row$realized, unname(direct$statistic), tolerance = tol_deterministic)
  expect_equal(row$p_value, direct$p.value, tolerance = tol_deterministic)

  student <- sim_multisite(
    J = 100L,
    true_dist = "StudentT",
    theta_G = list(nu = 6),
    seed = 6311L
  )
  student_row <- attr(student, "diagnostics", exact = TRUE)$target_vs_realized
  student_row <- student_row[student_row$diagnostic == "ks_distance", ]
  student_cdf <- function(q) stats::pt(q / sqrt((6 - 2) / 6), df = 6)
  student_direct <- suppressWarnings(stats::ks.test(student$z_j, student_cdf))

  expect_equal(student_row$realized, unname(student_direct$statistic), tolerance = tol_deterministic)
  expect_equal(student_row$p_value, student_direct$p.value, tolerance = tol_deterministic)
})

test_that("one-sample Gaussian KS calibration is approximately nominal at J 50", {
  p_values <- withr::with_seed(6312L, replicate(
    2000L,
    suppressWarnings(stats::ks.test(stats::rnorm(50L), stats::pnorm)$p.value)
  ))
  rejection_rate <- mean(p_values <= 0.05)

  expect_gte(rejection_rate, 0.03)
  expect_lte(rejection_rate, 0.07)
})

test_that("shape diagnostics are NA for a distribution without a reference", {
  out <- sim_multisite(
    J = 50L,
    true_dist = "Mixture",
    theta_G = list(delta = 5, eps = 0.3, ups = 2),
    seed = 6304L
  )
  tab <- attr(out, "diagnostics", exact = TRUE)$target_vs_realized
  dist_rows <- tab[tab$group == "shape", ]

  expect_multisitedgp_error(ks_distance(out))
  expect_identical(nrow(dist_rows), 3L)
  expect_true(all(is.na(dist_rows$realized)))
})

test_that("copula diagnostics compare target and achievement on the latent scale", {
  out <- sim_multisite(
    J = 1000L,
    dependence = "copula",
    pearson_corr = 0.7,
    seed = 6310L
  )
  diagnostics <- attr(out, "diagnostics", exact = TRUE)
  tab <- diagnostics$target_vs_realized
  latent <- tab[
    tab$diagnostic == "realized_pearson_corr" & tab$basis == "copula_latent",
  ]
  raw <- tab[
    tab$diagnostic == "raw_pearson_corr" & tab$basis == "residual",
  ]
  spearman <- tab[
    tab$diagnostic == "realized_rank_corr" & tab$basis == "residual",
  ]

  expect_identical(latent$target, 0.7)
  expect_equal(
    latent$realized,
    diagnostics$dependence_diagnostics$achieved_latent_pearson,
    tolerance = tol_deterministic
  )
  # The rule uses (1 - rho^2) / sqrt(J - 1), the standard error of the
  # correlation of J normal pairs, here 0.51 / sqrt(999) = 0.0161. It is an
  # upper bound, because the scores of z_j are fixed and only the other
  # scores are drawn.
  se <- (1 - 0.7^2) / sqrt(999)
  expect_identical(latent$threshold_metric, "sampling_se")
  expect_match(latent$threshold_note, sprintf("SE = %.3g", se), fixed = TRUE)
  expect_identical(
    latent$status,
    as.character(cut(abs(latent$delta) / se, c(-Inf, 2, 3, Inf), labels = c("pass", "warn", "fail")))
  )
  expect_identical(latent$note, "latent Gaussian copula target")
  expect_true(is.na(raw$target))
  expect_true(is.na(raw$status))
  expect_identical(raw$target_source, "not_targeted")
  expect_gt(abs(raw$realized - latent$realized), 0.03)
  expect_true(is.na(spearman$target))
  expect_true(is.na(spearman$status))
})

test_that("a copula correlation of 1 or -1 is reached exactly and passes", {
  # The standard error is 0 at a correlation of 1 or -1. The difference from
  # the target is then 0 up to rounding, which the rule accepts.
  for (rho in c(1, -1, 1 - 1e-8)) {
    # The design warns that a correlation beyond 0.95 nearly fixes the order.
    out <- suppressWarnings(sim_multisite(J = 50L, dependence = "copula", pearson_corr = rho, seed = 6491L))
    tab <- attr(out, "diagnostics", exact = TRUE)$target_vs_realized
    latent <- tab[tab$diagnostic == "realized_pearson_corr" & tab$basis == "copula_latent", ]

    expect_lt(abs(latent$delta), 1e-7)
    expect_identical(latent$status, "pass", info = format(rho, digits = 12))
  }
})

test_that("at a copula correlation of 1 or -1 the latent row accepts rounding error and nothing larger", {
  for (rho in c(-1, 1)) {
    # The design warns that a correlation beyond 0.95 nearly fixes the order.
    base <- suppressWarnings(
      sim_meta(J = 33L, I = 0.5, R = 2, dependence = "copula", pearson_corr = rho, seed = 17L)
    )
    # The standard error is 0 at these two values. The rule then allows
    # sqrt(.Machine$double.eps), about 1.5e-8: a stored correlation that is off
    # by 1e-9 passes and one that is off by 1e-7 fails.
    for (drift in c(1e-9, 1e-7)) {
      out <- base
      diagnostics <- attr(out, "diagnostics", exact = TRUE)
      diagnostics$target_vs_realized <- NULL
      diagnostics$dependence_diagnostics$achieved_latent_pearson <- rho - sign(rho) * drift
      attr(out, "diagnostics") <- diagnostics
      expected <- if (drift == 1e-9) "pass" else "fail"
      expect_identical(summary_status(out, "Pearson correlation, copula latent"), expected)
    }
  }
})

test_that("independence rank-correlation diagnostics use finite-J drift bands", {
  out <- sim_multisite(J = 50L, dependence = "none", seed = 1234L)
  tab <- attr(out, "diagnostics", exact = TRUE)$target_vs_realized
  row_rank <- tab[tab$diagnostic == "realized_rank_corr" & tab$basis == "residual", ]

  expect_identical(row_rank$target, 0)
  expect_identical(row_rank$threshold_metric, "sampling_drift")
  expect_identical(row_rank$threshold_source, "sampling-theory")
  expect_identical(row_rank$status, "pass")
  expect_match(row_rank$pass_rule, "2 / sqrt(J - 1)", fixed = TRUE)
  expect_match(row_rank$threshold_note, "independence baseline", fixed = TRUE)

  out_meta <- sim_meta(J = 50L, I = 0.4, R = 3, dependence = "none", seed = 1234L)
  tab_meta <- attr(out_meta, "diagnostics", exact = TRUE)$target_vs_realized
  row_meta <- tab_meta[tab_meta$diagnostic == "realized_rank_corr" & tab_meta$basis == "residual", ]

  expect_identical(row_meta$threshold_metric, "sampling_drift")
  expect_identical(row_meta$status, "pass")
})

test_that("zero residual rank targets use drift bands even through rank and hybrid modes", {
  out_rank <- sim_multisite(J = 50L, dependence = "rank", rank_corr = 0, seed = 1234L)
  tab_rank <- attr(out_rank, "diagnostics", exact = TRUE)$target_vs_realized
  row_rank <- tab_rank[tab_rank$diagnostic == "realized_rank_corr" & tab_rank$basis == "residual", ]

  expect_identical(row_rank$threshold_metric, "sampling_drift")
  expect_identical(row_rank$status, "pass")
  expect_match(row_rank$threshold_note, "rank zero target", fixed = TRUE)

  out_hybrid <- sim_multisite(J = 50L, dependence = "hybrid", rank_corr = 0, seed = 1234L)
  tab_hybrid <- attr(out_hybrid, "diagnostics", exact = TRUE)$target_vs_realized
  row_hybrid <- tab_hybrid[tab_hybrid$diagnostic == "realized_rank_corr" & tab_hybrid$basis == "residual", ]

  expect_identical(row_hybrid$threshold_metric, "sampling_drift")
  expect_identical(row_hybrid$status, "pass")
  expect_match(row_hybrid$threshold_note, "hybrid zero target", fixed = TRUE)
})

test_that("rank and hybrid correlation diagnostics record design tolerance", {
  out_rank <- sim_multisite(J = 60L, dependence = "rank", rank_corr = 0.3, tol = 0.06, seed = 6411L)
  tab_rank <- attr(out_rank, "diagnostics", exact = TRUE)$target_vs_realized
  row_rank <- tab_rank[tab_rank$diagnostic == "realized_rank_corr" & tab_rank$basis == "residual", ]

  expect_identical(row_rank$threshold_metric, "rank_corr_tolerance")
  expect_match(row_rank$threshold_note, "design tolerance = 0.060", fixed = TRUE)
  expect_true(row_rank$status %in% c("pass", "warn", "fail"))

  out_hybrid <- sim_multisite(J = 60L, dependence = "hybrid", rank_corr = 0.3, tol = 0.06, seed = 6412L)
  tab_hybrid <- attr(out_hybrid, "diagnostics", exact = TRUE)$target_vs_realized
  row_hybrid <- tab_hybrid[tab_hybrid$diagnostic == "realized_rank_corr" & tab_hybrid$basis == "residual", ]

  expect_identical(row_hybrid$threshold_metric, "rank_corr_tolerance")
  expect_match(row_hybrid$threshold_note, "design tolerance = 0.060", fixed = TRUE)
})

test_that("without covariates the residual and marginal sigma_tau diagnostics are equal", {
  # The standard deviation of J normal draws has relative standard error
  # 1 / sqrt(2 * (J - 1)), which is 0.0125 at J = 3200. The 5 percent tolerance
  # of the second check is 4 standard errors.
  df <- sim_default(seed = 820601L, J = 3200L)
  diagnostics <- attr(df, "diagnostics", exact = TRUE)

  expect_equal(diagnostics$sigma_tau_resid, diagnostics$sigma_tau_marg, tolerance = tol_deterministic)
  expect_equal(diagnostics$sigma_tau_resid, diagnostics$target_sigma_tau_resid, tolerance = 0.05)
})

test_that("the JEBS design grid spans informativeness from below 0.01 to above 0.70", {
  J_grid <- c(25L, 50L, 75L, 100L, 300L)
  nj_mean_grid <- c(10, 20, 40, 80, 160)
  cv_grid <- c(0, 0.25, 0.50, 0.75)
  sigma_grid <- c(0.05, 0.10, 0.15, 0.20, 0.25)
  # I_hat rises with nj_mean and with sigma_tau, so the smallest and the
  # largest value over the 500 cells are in corner cells. The 16 corners and one
  # interior cell are simulated. The corners include cv = 0 and cv > 0, which
  # take different code paths.
  cells <- rbind(
    expand.grid(
      J = range(J_grid),
      nj_mean = range(nj_mean_grid),
      cv = range(cv_grid),
      sigma_tau = range(sigma_grid)
    ),
    data.frame(J = 100L, nj_mean = 40, cv = 0.50, sigma_tau = 0.15)
  )
  seeds <- replication_seeds(nrow(cells), 820111L)
  I_values <- vapply(seq_len(nrow(cells)), function(i) {
    df <- sim_default(
      seed = seeds[[i]],
      J = cells$J[[i]],
      nj_mean = cells$nj_mean[[i]],
      cv = cells$cv[[i]],
      sigma_tau = cells$sigma_tau[[i]],
      engine = "censored"
    )
    attr(df, "diagnostics", exact = TRUE)$I_hat
  }, numeric(1))

  expect_length(I_values, 17L)
  expect_gte(min(I_values), 0.004)
  expect_lte(min(I_values), 0.01)
  expect_gte(max(I_values), 0.70)
  expect_lte(max(I_values), 0.75)
})
