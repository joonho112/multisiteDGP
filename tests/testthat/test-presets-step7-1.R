test_that("Step 7.1 education and JEBS presets are exported", {
  exports <- getNamespaceExports("multisiteDGP")
  expect_true(all(c(
    "preset_education_small",
    "preset_education_modest",
    "preset_education_substantial",
    "preset_jebs_paper",
    "preset_jebs_strict"
  ) %in% exports))
})

test_that("education presets return the ch13 site-size designs", {
  small <- preset_education_small()
  modest <- preset_education_modest()
  substantial <- preset_education_substantial()

  expect_true(all(vapply(list(small, modest, substantial), is_multisitedgp_design, logical(1))))
  expect_identical(small$J, 50L)
  expect_identical(small$sigma_tau, 0.05)
  expect_identical(small$nj_mean, 40)
  expect_identical(small$nj_min, 5L)
  expect_identical(small$engine, "A2_modern")

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

test_that("JEBS presets keep UX and strict anchors separate", {
  paper <- preset_jebs_paper()
  strict <- preset_jebs_strict()

  expect_identical(paper$J, 50L)
  expect_identical(paper$sigma_tau, 0.20)
  expect_identical(paper$nj_mean, 40)
  expect_identical(paper$nj_min, 5L)

  expect_identical(strict$J, 100L)
  expect_identical(strict$sigma_tau, 0.15)
  expect_identical(strict$nj_mean, 80)
  expect_identical(strict$nj_min, 4L)

  for (des in list(paper, strict)) {
    expect_identical(des$paradigm, "site_size")
    expect_identical(des$true_dist, "Mixture")
    expect_identical(des$theta_G, list(delta = 5, eps = 0.3, ups = 2))
    expect_identical(des$variance, 1)
    expect_identical(des$engine, "A1_legacy")
    expect_identical(des$dependence_spec$method, "none")
    expect_false(des$record_permutation)
  }
})

test_that("preset named overrides replay through design validation", {
  des <- preset_education_modest(J = 75L, sigma_tau = 0.25, seed = 7071L)

  expect_identical(des$J, 75L)
  expect_identical(des$sigma_tau, 0.25)
  expect_identical(des$seed, 7071L)
  expect_identical(des$nj_min, 10L)

  expect_multisitedgp_error(
    preset_education_modest(75L),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    preset_education_modest(not_a_design_arg = TRUE),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    preset_jebs_paper(dependence = "rank", rank_corr = 0.3),
    "multisitedgp_engine_dependence_error"
  )
})

test_that("Step 7.1 presets simulate and produce valid diagnostics", {
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
    inherits(tryCatch(sim_meta(des), error = function(e) e), "multisitedgp_coherence_error")
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

test_that("Step 7.1 preset traceability statuses are synchronized", {
  trace_dir <- test_path("../../tools/traceability")
  skip_if_not(dir.exists(trace_dir), "Development-only traceability ledgers are not shipped in the package tarball.")

  api <- read.csv(file.path(trace_dir, "api-index.csv"), stringsAsFactors = FALSE)
  presets <- read.csv(file.path(trace_dir, "preset-index.csv"), stringsAsFactors = FALSE)

  api_ids <- sprintf("API%03d", 43:47)
  preset_ids <- sprintf("P%02d", 1:5)

  expect_true(all(api$status[match(api_ids, api$id)] == "implemented"))
  expect_true(all(api$owner_step[match(api_ids, api$id)] == "Step 7.1"))
  expect_true(all(presets$status[match(preset_ids, presets$id)] == "implemented"))
  expect_true(all(presets$owner_step[match(preset_ids, presets$id)] == "Step 7.1"))
})
