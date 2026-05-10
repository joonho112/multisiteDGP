# nolint start: object_usage_linter
test_that("feasibility_index is exported and computes Efron and Morris conventions", {
  expect_true("feasibility_index" %in% getNamespaceExports("multisiteDGP"))
  se2_j <- c(0.04, 0.09, 0.16)
  sigma_tau <- 0.2
  S_j <- compute_shrinkage(se2_j, sigma_tau = sigma_tau)

  expect_equal(feasibility_index(se2_j, sigma_tau = sigma_tau, warn = FALSE), sum(S_j), tolerance = tol_deterministic)
  expect_equal(feasibility_index(se2_j, sigma_tau = sigma_tau, kind = "morris"), sum(1 - S_j), tolerance = tol_deterministic)

  out <- sim_meta(J = 25L, I = 0.4, R = 3, seed = 6401L)
  expect_equal(
    feasibility_index(out, warn = FALSE),
    feasibility_index(out$se2_j, sigma_tau = attr(out, "design", exact = TRUE)$sigma_tau, warn = FALSE),
    tolerance = tol_deterministic
  )
})

test_that("feasibility_index validates arguments and E28 is warn-not-abort", {
  expect_multisitedgp_error(feasibility_index(c(0.04, 0.09), warn = FALSE), "multisitedgp_arg_error")
  expect_multisitedgp_error(feasibility_index(c(0.04, 0), sigma_tau = 0.2, warn = FALSE), "multisitedgp_arg_error")
  expect_multisitedgp_error(feasibility_index(c(0.04, 0.09), sigma_tau = c(0.2, 0.3), warn = FALSE), "multisitedgp_arg_error")
  expect_multisitedgp_error(feasibility_index(c(0.04, 0.09), sigma_tau = 0.2, kind = "bad", warn = FALSE), "multisitedgp_arg_error")
  dat_without_design <- multisitedgp_internal(".new_multisitedgp_data")(tibble::tibble(
    site_index = 1:2,
    z_j = c(-1, 1),
    tau_j = c(-0.2, 0.2),
    tau_j_hat = c(-0.1, 0.1),
    se_j = c(0.2, 0.3),
    se2_j = c(0.04, 0.09),
    n_j = c(50L, 50L)
  ))
  expect_multisitedgp_error(feasibility_index(dat_without_design, warn = FALSE), "multisitedgp_arg_error")

  low <- rep(1, 10L)
  expect_warning(
    value <- feasibility_index(low, sigma_tau = 0.1),
    "Feasibility index = FAIL"
  )
  expect_true(is.numeric(value))
  expect_equal(
    suppressWarnings(feasibility_index(low, sigma_tau = 0.1)),
    feasibility_index(low, sigma_tau = 0.1, warn = FALSE),
    tolerance = tol_deterministic
  )
  expect_silent(feasibility_index(low, sigma_tau = 0.1, kind = "morris"))
})

test_that("default_thresholds returns the Step 6.4 public audit gates", {
  expect_true("default_thresholds" %in% getNamespaceExports("multisiteDGP"))
  expect_identical(
    default_thresholds(),
    list(
      mean_shrinkage_min = 0.30,
      feasibility_min = 5.0,
      R_max = 30.0,
      bhattacharyya_min = 0.85,
      ks_max = 0.10
    )
  )
})

test_that("diagnostic threshold boundary helpers classify exact edges", {
  expect_identical(multisitedgp_internal(".status_by_abs_delta")(0.019, "informativeness"), "PASS")
  expect_identical(multisitedgp_internal(".status_by_abs_delta")(0.02, "informativeness"), "WARN")
  expect_identical(multisitedgp_internal(".status_by_abs_delta")(0.05, "informativeness"), "FAIL")

  expect_identical(multisitedgp_internal(".status_by_rel_delta")(0.099, "heterogeneity_ratio"), "PASS")
  expect_identical(multisitedgp_internal(".status_by_rel_delta")(0.10, "heterogeneity_ratio"), "WARN")
  expect_identical(multisitedgp_internal(".status_by_rel_delta")(0.25, "heterogeneity_ratio"), "FAIL")

  expect_identical(multisitedgp_internal(".status_ks")(0.051), "PASS")
  expect_identical(multisitedgp_internal(".status_ks")(0.05), "WARN")
  expect_identical(multisitedgp_internal(".status_ks")(0.01), "FAIL")

  expect_identical(multisitedgp_internal(".status_bhattacharyya")(0.90), "PASS")
  expect_identical(multisitedgp_internal(".status_bhattacharyya")(0.80), "WARN")
  expect_identical(multisitedgp_internal(".status_bhattacharyya")(0.799), "FAIL")

  expect_identical(multisitedgp_internal(".status_mean_shrinkage")(0.10), "PASS")
  expect_identical(multisitedgp_internal(".status_mean_shrinkage")(0.05), "WARN")
  expect_identical(multisitedgp_internal(".status_mean_shrinkage")(0.049), "FAIL")
  expect_identical(multisitedgp_internal(".status_mean_shrinkage")(0.951), "WARN")
  expect_identical(multisitedgp_internal(".status_mean_shrinkage")(0.991), "FAIL")

  expect_identical(multisitedgp_internal(".status_average_moe")(0.40, sigma_tau = 0.20), "PASS")
  expect_identical(multisitedgp_internal(".status_average_moe")(0.41, sigma_tau = 0.20), "WARN")
  expect_identical(multisitedgp_internal(".status_average_moe")(0.81, sigma_tau = 0.20), "FAIL")

  expect_identical(multisitedgp_internal(".feasibility_status")(30), "PASS")
  expect_identical(multisitedgp_internal(".feasibility_status")(5), "WARN")
  expect_identical(multisitedgp_internal(".feasibility_status")(4.999), "FAIL")

  drift <- multisitedgp_internal(".sampling_drift_cutoffs")(50L)
  expect_equal(drift, c(pass = 2 / sqrt(49), warn = 3 / sqrt(49)), tolerance = tol_deterministic)
  expect_identical(multisitedgp_internal(".status_by_cutoffs")(drift[["pass"]], drift[["pass"]], drift[["warn"]]), "PASS")
  expect_identical(multisitedgp_internal(".status_by_cutoffs")(mean(drift), drift[["pass"]], drift[["warn"]]), "WARN")
  expect_identical(multisitedgp_internal(".status_by_cutoffs")(drift[["warn"]] + 0.001, drift[["pass"]], drift[["warn"]]), "FAIL")
})

test_that("wrapper diagnostics table includes threshold status metadata", {
  out <- sim_meta(I = 0.4, R = 3, seed = 6402L)
  tab <- attr(out, "diagnostics", exact = TRUE)$target_vs_realized
  row_i <- tab[tab$diagnostic == "informativeness" & tab$basis == "overall", ]
  row_r <- tab[tab$diagnostic == "heterogeneity_ratio" & tab$basis == "overall", ]
  row_feas <- tab[tab$diagnostic == "feasibility_index" & tab$basis == "efron", ]
  row_qq <- tab[tab$diagnostic == "qq_residuals", ]
  row_rank_marg <- tab[tab$diagnostic == "realized_rank_corr" & tab$basis == "marginal", ]

  expect_true(all(c(
    "status", "threshold_metric", "pass_rule", "warn_rule", "fail_rule",
    "threshold_source", "threshold_note"
  ) %in% names(tab)))
  expect_identical(row_i$status, "PASS")
  expect_identical(row_i$threshold_metric, "abs_delta")
  expect_identical(row_r$status, "PASS")
  expect_identical(row_feas$status, "WARN")
  expect_identical(row_feas$threshold_source, "simulation-calibrated")
  expect_identical(row_qq$status, NA_character_)
  expect_identical(row_qq$threshold_metric, "deferred")
  expect_identical(row_rank_marg$status, NA_character_)
  expect_identical(row_rank_marg$threshold_metric, "not_applicable")
})

test_that("independence rank-correlation diagnostics use finite-J drift bands", {
  out <- sim_multisite(J = 50L, dependence = "none", seed = 1234L)
  tab <- attr(out, "diagnostics", exact = TRUE)$target_vs_realized
  row_rank <- tab[tab$diagnostic == "realized_rank_corr" & tab$basis == "residual", ]

  expect_identical(row_rank$target, 0)
  expect_identical(row_rank$threshold_metric, "sampling_drift")
  expect_identical(row_rank$threshold_source, "sampling-theory")
  expect_identical(row_rank$status, "PASS")
  expect_match(row_rank$pass_rule, "2 / sqrt(J - 1)", fixed = TRUE)
  expect_match(row_rank$threshold_note, "independence baseline", fixed = TRUE)

  out_meta <- sim_meta(J = 50L, I = 0.4, R = 3, dependence = "none", seed = 1234L)
  tab_meta <- attr(out_meta, "diagnostics", exact = TRUE)$target_vs_realized
  row_meta <- tab_meta[tab_meta$diagnostic == "realized_rank_corr" & tab_meta$basis == "residual", ]

  expect_identical(row_meta$threshold_metric, "sampling_drift")
  expect_identical(row_meta$status, "PASS")
})

test_that("zero residual rank targets use drift bands even through rank and hybrid modes", {
  out_rank <- sim_multisite(J = 50L, dependence = "rank", rank_corr = 0, seed = 1234L)
  tab_rank <- attr(out_rank, "diagnostics", exact = TRUE)$target_vs_realized
  row_rank <- tab_rank[tab_rank$diagnostic == "realized_rank_corr" & tab_rank$basis == "residual", ]

  expect_identical(row_rank$threshold_metric, "sampling_drift")
  expect_identical(row_rank$status, "PASS")
  expect_match(row_rank$threshold_note, "rank zero target", fixed = TRUE)

  out_hybrid <- sim_multisite(J = 50L, dependence = "hybrid", rank_corr = 0, seed = 1234L)
  tab_hybrid <- attr(out_hybrid, "diagnostics", exact = TRUE)$target_vs_realized
  row_hybrid <- tab_hybrid[tab_hybrid$diagnostic == "realized_rank_corr" & tab_hybrid$basis == "residual", ]

  expect_identical(row_hybrid$threshold_metric, "sampling_drift")
  expect_identical(row_hybrid$status, "PASS")
  expect_match(row_hybrid$threshold_note, "hybrid zero target", fixed = TRUE)
})

test_that("rank and hybrid correlation diagnostics record design tolerance", {
  out_rank <- sim_multisite(J = 60L, dependence = "rank", rank_corr = 0.3, tol = 0.06, seed = 6411L)
  tab_rank <- attr(out_rank, "diagnostics", exact = TRUE)$target_vs_realized
  row_rank <- tab_rank[tab_rank$diagnostic == "realized_rank_corr" & tab_rank$basis == "residual", ]

  expect_identical(row_rank$threshold_metric, "rank_corr_tolerance")
  expect_match(row_rank$threshold_note, "design tolerance = 0.060", fixed = TRUE)
  expect_true(row_rank$status %in% c("PASS", "WARN", "FAIL"))

  out_hybrid <- sim_multisite(J = 60L, dependence = "hybrid", rank_corr = 0.3, tol = 0.06, seed = 6412L)
  tab_hybrid <- attr(out_hybrid, "diagnostics", exact = TRUE)$target_vs_realized
  row_hybrid <- tab_hybrid[tab_hybrid$diagnostic == "realized_rank_corr" & tab_hybrid$basis == "residual", ]

  expect_identical(row_hybrid$threshold_metric, "rank_corr_tolerance")
  expect_match(row_hybrid$threshold_note, "design tolerance = 0.060", fixed = TRUE)
})

test_that("Step 6.4 threshold helper stays available after scenario audit lands", {
  exports <- getNamespaceExports("multisiteDGP")

  expect_true("default_thresholds" %in% exports)
})
# nolint end
