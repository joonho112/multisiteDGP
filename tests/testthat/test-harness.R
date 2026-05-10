test_that("package test harness is active", {
  expect_true(requireNamespace("multisiteDGP", quietly = TRUE))
  expect_true("multisiteDGP" %in% loadedNamespaces())
})

test_that("DESCRIPTION locks testthat edition 3", {
  desc_path <- system.file("DESCRIPTION", package = "multisiteDGP")
  if (!nzchar(desc_path)) {
    desc_path <- test_path("../../DESCRIPTION")
  }
  desc <- read.dcf(desc_path)
  expect_identical(unname(desc[, "Config/testthat/edition"]), "3")
})

test_that("DESCRIPTION records the approved maintainer email", {
  desc_path <- system.file("DESCRIPTION", package = "multisiteDGP")
  if (!nzchar(desc_path)) {
    desc_path <- test_path("../../DESCRIPTION")
  }
  desc <- read.dcf(desc_path)
  authors <- unname(desc[, "Authors@R"])

  expect_match(authors, "jlee296@ua.edu", fixed = TRUE)
  expect_no_match(authors, "example.com", fixed = TRUE)
})

test_that("Phase 1 unused Imports policy matches DESCRIPTION", {
  desc_path <- system.file("DESCRIPTION", package = "multisiteDGP")
  if (!nzchar(desc_path)) {
    desc_path <- test_path("../../DESCRIPTION")
  }
  import_pkgs <- phase1_desc_imports(desc_path)

  expect_length(import_pkgs, length(unique(import_pkgs)))
  expect_false(any(expected_unused_imports_phase1 %in% expected_used_imports_phase1))
  expect_setequal(import_pkgs, c(expected_used_imports_phase1, expected_unused_imports_phase1))
})

test_that("Phase 1 used Imports policy is backed by R source references", {
  source_dir <- test_path("../../R")
  source_refs <- phase1_r_pkg_refs(source_dir)
  skip_if_not(length(source_refs) > 0L, "Package R source references are unavailable in this test context.")

  desc_path <- system.file("DESCRIPTION", package = "multisiteDGP")
  if (!nzchar(desc_path)) {
    desc_path <- test_path("../../DESCRIPTION")
  }
  import_pkgs <- phase1_desc_imports(desc_path)
  expected_backed_imports <- setdiff(expected_used_imports_phase1, expected_import_excepts_p1)

  expect_setequal(intersect(import_pkgs, source_refs), expected_backed_imports)
  expect_setequal(
    setdiff(import_pkgs, source_refs),
    c(expected_unused_imports_phase1, expected_import_excepts_p1)
  )
})

test_that("tolerance constants match blueprint ch.18 policy", {
  expect_identical(tol_deterministic, 1e-12)
  expect_identical(tol_mc_moment_n1e5, 0.01)
  expect_identical(tol_mc_moment_n1e6, 0.005)
  expect_identical(tol_spearman_continuous, 0.02)
  expect_identical(tol_spearman_ties, 0.05)
  expect_identical(mc_moment_clt_multiplier, 2)
  expect_identical(default_m_replications, 1000L)
})

test_that("snapshot and golden policies track active Step 8.5 artifacts", {
  expect_true(snapshot_policy$accept_requires_log)
  expect_identical(snapshot_policy$error_snapshots_owner, "Step 8.5")
  expect_identical(snapshot_policy$print_snapshots_owner, "Step 8.6")
  expect_identical(snapshot_policy$error_snapshot_count, 30L)
  expect_identical(snapshot_policy$active_error_snapshot_count, 29L)
  expect_identical(snapshot_policy$deferred_error_placeholder_count, 1L)
  expect_identical(snapshot_policy$warning_not_abort_count, 1L)
  expect_identical(snapshot_policy$print_snapshot_count, 24L)
  expect_identical(golden_fixture_policy$count, 9L)
  expect_identical(golden_fixture_policy$jebs_appendix_seed_files, 4L)
  expect_identical(golden_fixture_policy$preset_output_files, 5L)
  golden_files <- list.files(
    test_path("_snaps/golden"),
    pattern = "[.]rds$",
    all.files = FALSE,
    recursive = FALSE
  )
  expect_length(golden_files, golden_fixture_policy$count)

  snapshot_files <- list.files(
    test_path("_snaps"),
    all.files = FALSE,
    recursive = TRUE
  )
  non_golden_snapshots <- snapshot_files[!grepl("^golden/", snapshot_files)]
  expected_non_golden <- c(
    snapshot_policy$error_snapshot_files,
    snapshot_policy$print_snapshot_files,
    snapshot_policy$generated_print_example_files
  )
  expect_setequal(non_golden_snapshots, expected_non_golden)
})

test_that("property-test policy is opt-in and count-locked", {
  expect_identical(property_policy$owner, "Step 8.4")
  expect_identical(property_policy$count, 12L)
  expect_identical(property_policy$gate_envvar, "MULTISITEDGP_RUN_PROPERTY")
  expect_identical(property_policy$default_skipped, TRUE)
  expect_identical(property_policy$generator, "fixed_seed_direct")
})

test_that("Step 6.1 core wrapper API implementation status is synchronized", {
  expect_true(exists("sim_multisite", envir = asNamespace("multisiteDGP"), mode = "function", inherits = FALSE))
  expect_true(exists("sim_meta", envir = asNamespace("multisiteDGP"), mode = "function", inherits = FALSE))
})
