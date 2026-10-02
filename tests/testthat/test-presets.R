test_that("education and JEBS presets are exported", {
  exports <- getNamespaceExports("multisiteDGP")
  expect_true(all(c(
    "preset_education_small",
    "preset_education_modest",
    "preset_education_substantial",
    "preset_jebs_paper",
    "preset_jebs_strict"
  ) %in% exports))
})

test_that("Walters, twin-towers, meta, and small-area presets are exported", {
  exports <- getNamespaceExports("multisiteDGP")
  expect_true(all(c(
    "preset_walters_2024",
    "preset_twin_towers",
    "preset_meta_modest",
    "preset_small_area_estimation"
  ) %in% exports))
})

test_that("education presets return their documented site-size designs", {
  small <- preset_education_small()
  modest <- preset_education_modest()
  substantial <- preset_education_substantial()

  expect_true(all(vapply(list(small, modest, substantial), is_multisitedgp_design, logical(1))))
  expect_identical(small$J, 50L)
  expect_identical(small$sigma_tau, 0.05)
  expect_identical(small$nj_mean, 40)
  expect_identical(small$nj_min, 5L)
  expect_identical(small$engine, "truncated_gamma")

  expect_identical(modest$J, 50L)
  expect_identical(modest$sigma_tau, 0.20)
  expect_identical(modest$nj_mean, 50)
  expect_identical(modest$nj_min, 10L)

  expect_identical(substantial$J, 100L)
  expect_identical(substantial$sigma_tau, 0.30)
  expect_identical(substantial$nj_mean, 80)
  expect_identical(substantial$nj_min, 10L)
  expect_identical(substantial$true_dist, "Gaussian")
  expect_identical(substantial$dependence_spec$method, "none")
})

test_that("the two JEBS presets differ in size and share the mixture and the censored generator", {
  paper <- preset_jebs_paper()
  strict <- preset_jebs_strict()

  expect_identical(paper$J, 50L)
  expect_identical(paper$sigma_tau, 0.20)
  expect_identical(paper$nj_mean, 40)
  expect_identical(paper$nj_min, 5L)

  expect_identical(strict$J, 100L)
  expect_identical(strict$sigma_tau, 0.15)
  expect_identical(strict$nj_mean, 80)
  expect_identical(strict$nj_min, 5L)

  for (des in list(paper, strict)) {
    expect_identical(des$paradigm, "site_size")
    expect_identical(des$true_dist, "Mixture")
    expect_identical(des$theta_G, list(delta = 5, eps = 0.3, ups = 2))
    expect_identical(des$engine, "censored")
    expect_identical(des$dependence_spec$method, "none")
    expect_false(des$record_permutation)
  }
})

test_that("Walters and twin-towers presets return their documented site-size designs", {
  walters <- preset_walters_2024()
  twin <- preset_twin_towers()

  expect_true(is_multisitedgp_design(walters))
  expect_identical(walters$J, 46L)
  expect_identical(walters$paradigm, "site_size")
  expect_identical(walters$true_dist, "Gaussian")
  expect_identical(walters$sigma_tau, 0.197)
  expect_identical(walters$nj_mean, 240)
  expect_identical(walters$cv, 0.30)
  expect_identical(walters$nj_min, 50L)
  expect_identical(walters$R2, 0.40)
  expect_identical(walters$engine, "truncated_gamma")
  metadata <- attr(walters, "preset_metadata", exact = TRUE)
  expect_identical(attr(walters, "preset_name", exact = TRUE), "preset_walters_2024")
  expect_identical(metadata$direct$average_se2, 0.010)
  expect_identical(metadata$derived$nj_mean, 240)
  expect_identical(metadata$assumed$outcome_R2, 0.40)
  expect_identical(metadata$excluded_source_quantity$conditional_prior_R2, 0.502)

  expect_true(is_multisitedgp_design(twin))
  expect_identical(twin$J, 1000L)
  expect_identical(twin$paradigm, "site_size")
  expect_identical(twin$true_dist, "Mixture")
  expect_identical(twin$theta_G, list(delta = 4, eps = 0.5, ups = 1))
  expect_identical(twin$sigma_tau, 2.0)
  expect_identical(twin$nj_mean, 100)
  expect_identical(twin$cv, 0)
  expect_identical(twin$nj_min, 100L)
  expect_identical(twin$engine, "truncated_gamma")
})

test_that("direct presets return their documented meta-analysis designs", {
  meta <- preset_meta_modest()
  sae <- preset_small_area_estimation()

  expect_true(is_multisitedgp_design(meta))
  expect_identical(meta$J, 50L)
  expect_identical(meta$paradigm, "direct")
  expect_identical(meta$true_dist, "Gaussian")
  expect_identical(meta$sigma_tau, 0.20)
  expect_identical(meta$I, 0.30)
  expect_identical(meta$R, 1.5)
  expect_true(meta$shuffle)

  expect_true(is_multisitedgp_design(sae))
  expect_identical(sae$J, 30L)
  expect_identical(sae$paradigm, "direct")
  expect_identical(sae$true_dist, "Gaussian")
  expect_identical(sae$sigma_tau, 0.20)
  expect_identical(sae$I, 0.20)
  expect_identical(sae$R, 3.0)
  expect_true(sae$shuffle)
})

test_that("preset named overrides replay through design validation", {
  des <- preset_education_modest(J = 75L, sigma_tau = 0.25, seed = 7071L)

  expect_identical(des$J, 75L)
  expect_identical(des$sigma_tau, 0.25)
  expect_identical(des$seed, 7071L)
  expect_identical(des$nj_min, 10L)

  expect_multisitedgp_error(
    preset_education_modest(75L),
    regexp = "Every argument in `...` must be named"
  )
  expect_multisitedgp_error(
    preset_education_modest(not_a_design_arg = TRUE),
    regexp = "Not an argument: not_a_design_arg"
  )
  expect_multisitedgp_error(
    preset_jebs_paper(dependence = "rank", rank_corr = 0.3),
    regexp = "is not compatible with"
  )
})

test_that("deterministic site-size presets may set nj_mean equal to nj_min only when cv is zero", {
  twin <- preset_twin_towers()
  out <- sim_multisite(twin, seed = 7271L)

  expect_s3_class(out, "multisitedgp_data")
  expect_true(all(out$n_j == 100L))
  expect_identical(attr(out, "diagnostics", exact = TRUE)$R_hat, 1)

  expect_multisitedgp_error(
    multisitedgp_design(nj_mean = 100, nj_min = 100L, cv = 0.1)
  )
})

test_that("education and JEBS presets simulate and produce valid diagnostics", {
  presets <- list(
    education_small = preset_education_small(),
    education_modest = preset_education_modest(),
    education_substantial = preset_education_substantial(),
    jebs_paper = preset_jebs_paper(),
    jebs_strict = preset_jebs_strict()
  )

  outputs <- lapply(presets, sim_multisite, seed = 7072L)
  expect_true(all(vapply(outputs, is_multisitedgp_data, logical(1))))
  expect_true(all(vapply(presets, function(des) {
    err <- tryCatch(sim_meta(des), error = function(e) e)
    inherits(err, "multisitedgp_error") &&
      grepl("requires `paradigm = \"direct\"`", conditionMessage(err), fixed = TRUE)
  }, logical(1))))

  diagnostics <- lapply(outputs, attr, which = "diagnostics", exact = TRUE)
  expect_true(all(vapply(diagnostics, function(x) is.finite(x$I_hat), logical(1))))
  expect_true(all(vapply(diagnostics, function(x) is.finite(x$R_hat), logical(1))))
  expect_true(all(vapply(diagnostics, function(x) identical(x$dependence_method, "none"), logical(1))))
  expect_true(all(vapply(diagnostics, function(x) abs(x$rho_S_residual) < 0.35 || is.na(x$rho_S_residual), logical(1))))

  modest_diag <- diagnostics$education_modest
  expect_gt(modest_diag$I_hat, 0.20)
  expect_lt(modest_diag$I_hat, 0.40)
})

test_that("site-size presets run in `sim_multisite()` and direct presets in `sim_meta()`", {
  site_size_presets <- list(
    walters = preset_walters_2024(),
    twin_towers = preset_twin_towers()
  )
  direct_presets <- list(
    meta_modest = preset_meta_modest(),
    small_area_estimation = preset_small_area_estimation()
  )

  site_outputs <- lapply(site_size_presets, sim_multisite, seed = 7272L)
  direct_outputs <- lapply(direct_presets, sim_meta, seed = 7272L)

  expect_true(all(vapply(site_outputs, is_multisitedgp_data, logical(1))))
  expect_true(all(vapply(direct_outputs, is_multisitedgp_data, logical(1))))
  expect_true(all(vapply(direct_outputs, inherits, logical(1), what = "multisitedgp_meta")))

  expect_true(all(vapply(site_size_presets, function(des) {
    err <- tryCatch(sim_meta(des), error = function(e) e)
    inherits(err, "multisitedgp_error") &&
      grepl("requires `paradigm = \"direct\"`", conditionMessage(err), fixed = TRUE)
  }, logical(1))))
  expect_true(all(vapply(direct_presets, function(des) {
    err <- tryCatch(sim_multisite(des), error = function(e) e)
    inherits(err, "multisitedgp_error") &&
      grepl("requires `paradigm = \"site_size\"`", conditionMessage(err), fixed = TRUE)
  }, logical(1))))

  meta_diag <- attr(direct_outputs$meta_modest, "diagnostics", exact = TRUE)
  sae_diag <- attr(direct_outputs$small_area_estimation, "diagnostics", exact = TRUE)
  expect_true(meta_diag$I_exact)
  expect_true(meta_diag$R_exact)
  expect_equal(meta_diag$I_hat, 0.30, tolerance = tol_deterministic)
  expect_equal(meta_diag$R_hat, 1.5, tolerance = tol_deterministic)
  expect_true(sae_diag$I_exact)
  expect_true(sae_diag$R_exact)
  expect_equal(sae_diag$I_hat, 0.20, tolerance = tol_deterministic)
  expect_equal(sae_diag$R_hat, 3.0, tolerance = tol_deterministic)
})
