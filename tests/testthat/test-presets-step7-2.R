test_that("Step 7.2 remaining presets are exported", {
  exports <- getNamespaceExports("multisiteDGP")
  expect_true(all(c(
    "preset_walters_2024",
    "preset_twin_towers",
    "preset_meta_modest",
    "preset_small_area_estimation"
  ) %in% exports))
})

test_that("Walters and twin-towers presets return ch13 site-size designs", {
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
  expect_identical(walters$engine, "A2_modern")

  expect_true(is_multisitedgp_design(twin))
  expect_identical(twin$J, 1000L)
  expect_identical(twin$paradigm, "site_size")
  expect_identical(twin$true_dist, "Mixture")
  expect_identical(twin$theta_G, list(delta = 4, eps = 0.5, ups = 1))
  expect_identical(twin$sigma_tau, 2.0)
  expect_identical(twin$nj_mean, 100)
  expect_identical(twin$cv, 0)
  expect_identical(twin$nj_min, 100L)
  expect_identical(twin$engine, "A2_modern")
})

test_that("direct presets return ch13 direct meta-analysis designs", {
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

test_that("deterministic site-size presets may set nj_mean equal to nj_min only when cv is zero", {
  twin <- preset_twin_towers()
  out <- sim_multisite(twin, seed = 7271L)

  expect_s3_class(out, "multisitedgp_data")
  expect_true(all(out$n_j == 100L))
  expect_identical(attr(out, "diagnostics", exact = TRUE)$R_hat, 1)

  expect_multisitedgp_error(
    multisitedgp_design(nj_mean = 100, nj_min = 100L, cv = 0.1),
    "multisitedgp_arg_error"
  )
})

test_that("Step 7.2 presets use their intended front doors", {
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
    inherits(tryCatch(sim_meta(des), error = function(e) e), "multisitedgp_coherence_error")
  }, logical(1))))
  expect_true(all(vapply(direct_presets, function(des) {
    inherits(tryCatch(sim_multisite(des), error = function(e) e), "multisitedgp_coherence_error")
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

test_that("Step 7.2 preset traceability statuses and decisions are synchronized", {
  trace_dir <- test_path("../../tools/traceability")
  skip_if_not(dir.exists(trace_dir), "Development-only traceability ledgers are not shipped in the package tarball.")

  api <- read.csv(file.path(trace_dir, "api-index.csv"), stringsAsFactors = FALSE)
  presets <- read.csv(file.path(trace_dir, "preset-index.csv"), stringsAsFactors = FALSE)
  decisions <- read.csv(file.path(trace_dir, "decision-index.csv"), stringsAsFactors = FALSE)
  conflicts <- read.csv(file.path(trace_dir, "conflict-checklist.csv"), stringsAsFactors = FALSE)

  api_ids <- sprintf("API%03d", 48:51)
  preset_ids <- sprintf("P%02d", 6:9)

  expect_true(all(api$status[match(api_ids, api$id)] == "implemented"))
  expect_true(all(api$owner_step[match(api_ids, api$id)] == "Step 7.2"))
  expect_true(all(presets$status[match(preset_ids, presets$id)] == "implemented"))
  expect_true(all(presets$owner_step[match(preset_ids, presets$id)] == "Step 7.2"))
  expect_identical(decisions$status[match("Q12", decisions$id)], "deferred-v1.1")
  expect_identical(decisions$status[match("Q13", decisions$id)], "noted")
  expect_identical(decisions$status[match("PA3", decisions$id)], "deferred-v1.1")
  expect_identical(decisions$status[match("PA4", decisions$id)], "noted")
  expect_identical(conflicts$status[match("C06", conflicts$id)], "noted")
  expect_identical(conflicts$status[match("C07", conflicts$id)], "deferred-v1.1")
})
