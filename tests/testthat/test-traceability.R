test_that("traceability ledgers preserve required counts when available", {
  trace_dir <- test_path("../../tools/traceability")
  skip_if_not(dir.exists(trace_dir), "Development-only traceability ledgers are not shipped in the package tarball.")

  observed <- vapply(
    names(expected_traceability_counts),
    function(file) nrow(read.csv(file.path(trace_dir, file), stringsAsFactors = FALSE)),
    integer(1)
  )

  expect_identical(observed, expected_traceability_counts)
})

test_that("traceability conflict ledger records known count conflicts", {
  conflict_file <- test_path("../../tools/traceability/conflict-checklist.csv")
  skip_if_not(file.exists(conflict_file), "Development-only conflict ledger is not shipped in the package tarball.")

  conflicts <- read.csv(conflict_file, stringsAsFactors = FALSE)
  expected_conflicts <- c(
    "C01", "C02", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C20", "C21"
  )
  expect_true(all(expected_conflicts %in% conflicts$id))
})

test_that("validation traceability statuses match validation manifest", {
  trace_file <- test_path("../../tools/traceability/validation-index.csv")
  plan_file <- test_path("../../tools/validation/validation-plan-manifest.csv")
  report_file <- test_path("../../tools/validation/reports/validation_report.md")
  skip_if_not(
    file.exists(trace_file),
    "Development-only validation traceability is not shipped in the package tarball."
  )
  skip_if_not(
    file.exists(plan_file),
    "Development-only validation manifest is not shipped in the package tarball."
  )

  trace <- read.csv(trace_file, stringsAsFactors = FALSE)
  plan <- read.csv(plan_file, stringsAsFactors = FALSE)
  rows <- match(trace$id, plan$experiment_id)

  expect_false(anyNA(rows))
  expect_identical(trace$status, plan$status[rows])
  expect_false(any(trace$status == "planned"))
  report_paths <- test_path("../../", plan$report_file)
  expect_true(all(file.exists(report_paths)))
  expect_true(file.exists(report_file))
})

test_that("Step 3.5 traceability statuses are synchronized", {
  trace_dir <- test_path("../../tools/traceability")
  skip_if_not(dir.exists(trace_dir), "Development-only traceability ledgers are not shipped in the package tarball.")

  api <- read.csv(file.path(trace_dir, "api-index.csv"), stringsAsFactors = FALSE)
  errors <- read.csv(file.path(trace_dir, "error-index.csv"), stringsAsFactors = FALSE)
  invariants <- read.csv(file.path(trace_dir, "invariant-index.csv"), stringsAsFactors = FALSE)

  expect_true(all(api$status[match(c("API013", "API014", "API015"), api$id)] == "implemented"))
  expect_true(all(errors$status[match(c("E14", "E21", "E22"), errors$id)] == "implemented"))
  expect_identical(invariants$status[match("T17", invariants$id)], "regression-implemented")
})

test_that("Step 3.6 traceability statuses are synchronized", {
  trace_dir <- test_path("../../tools/traceability")
  skip_if_not(dir.exists(trace_dir), "Development-only traceability ledgers are not shipped in the package tarball.")

  api <- read.csv(file.path(trace_dir, "api-index.csv"), stringsAsFactors = FALSE)
  row <- match("API007", api$id)

  expect_identical(api$status[row], "implemented")
  expect_identical(api$owner_step[row], "Step 3.6")
})

test_that("Step 8.1 JEBS fixture manifest status is synchronized", {
  trace_dir <- test_path("../../tools/traceability")
  manifest_file <- test_path("../../tools/jebs-golden-fixtures/jebs-golden-fixture-manifest.csv")
  golden_manifest_file <- system.file(
    "extdata", "golden", "golden-fixture-manifest.csv",
    package = "multisiteDGP"
  )
  skip_if_not(dir.exists(trace_dir), "Development-only traceability ledgers are not shipped in the package tarball.")
  skip_if_not(file.exists(manifest_file), "Step 4.1 JEBS fixture manifest is not shipped in the package tarball.")
  skip_if_not(
    file.exists(golden_manifest_file),
    "Step 8.1 golden fixture manifest is not shipped in the package tarball."
  )

  fixtures <- read.csv(file.path(trace_dir, "fixture-index.csv"), stringsAsFactors = FALSE)
  manifest <- read.csv(manifest_file, stringsAsFactors = FALSE)
  golden <- read.csv(golden_manifest_file, stringsAsFactors = FALSE)
  ids <- sprintf("F%02d", 1:4)

  expect_true(all(fixtures$status == "rds-generated"))
  expect_identical(sort(manifest$fixture_id), ids)
  expect_identical(golden$fixture_id, fixtures$id)
  expect_true(all(manifest$status == "manifest-hash-recorded"))
  expect_true(all(manifest$seed_policy == "single_stream_package_T1a"))
})

test_that("Step 4.2 diagnostic primitive traceability statuses are synchronized", {
  trace_dir <- test_path("../../tools/traceability")
  skip_if_not(dir.exists(trace_dir), "Development-only traceability ledgers are not shipped in the package tarball.")

  api <- read.csv(file.path(trace_dir, "api-index.csv"), stringsAsFactors = FALSE)
  invariants <- read.csv(file.path(trace_dir, "invariant-index.csv"), stringsAsFactors = FALSE)

  expect_true(all(api$status[match(c("API026", "API027", "API029", "API033", "API060"), api$id)] == "implemented"))
  expect_true(all(api$owner_step[match(c("API026", "API027", "API029", "API033", "API060"), api$id)] == "Step 4.2"))
  expect_identical(invariants$status[match("T8", invariants$id)], "regression-implemented")
})

test_that("Step 4.3 Engine A1 traceability statuses are synchronized", {
  trace_dir <- test_path("../../tools/traceability")
  skip_if_not(dir.exists(trace_dir), "Development-only traceability ledgers are not shipped in the package tarball.")

  invariants <- read.csv(file.path(trace_dir, "invariant-index.csv"), stringsAsFactors = FALSE)

  expect_identical(invariants$status[match("T12", invariants$id)], "regression-implemented")
})

test_that("Step 4.4 Engine A2 traceability statuses are synchronized", {
  trace_dir <- test_path("../../tools/traceability")
  skip_if_not(dir.exists(trace_dir), "Development-only traceability ledgers are not shipped in the package tarball.")

  errors <- read.csv(file.path(trace_dir, "error-index.csv"), stringsAsFactors = FALSE)
  invariants <- read.csv(file.path(trace_dir, "invariant-index.csv"), stringsAsFactors = FALSE)

  expect_identical(errors$status[match("E15", errors$id)], "implemented")
  expect_identical(invariants$status[match("T1b", invariants$id)], "regression-implemented")
  expect_identical(invariants$status[match("T20", invariants$id)], "regression-implemented")
})

test_that("Step 8.3 T13-T20 regression traceability statuses are synchronized", {
  trace_dir <- test_path("../../tools/traceability")
  skip_if_not(dir.exists(trace_dir), "Development-only traceability ledgers are not shipped in the package tarball.")

  invariants <- read.csv(file.path(trace_dir, "invariant-index.csv"), stringsAsFactors = FALSE)
  ids <- c("T13", "T14a", "T14b", paste0("T", 15:20))
  rows <- match(ids, invariants$id)

  expect_false(anyNA(rows))
  expect_true(all(invariants$owner_step[rows] == "Step 8.3"))
  expect_true(all(invariants$status[rows] == "regression-implemented"))
  expect_true(all(file.exists(test_path(invariants$test_file[rows]))))
})

test_that("Step 8.5 error snapshot traceability statuses are synchronized", {
  trace_dir <- test_path("../../tools/traceability")
  skip_if_not(dir.exists(trace_dir), "Development-only traceability ledgers are not shipped in the package tarball.")

  errors <- read.csv(file.path(trace_dir, "error-index.csv"), stringsAsFactors = FALSE)
  rows <- match(c("E10", "E17"), errors$id)

  expect_false(anyNA(rows))
  expect_true(all(errors$status[rows] == "snapshot-implemented"))
  expect_true(file.exists(test_path("_snaps/snapshot-errors.md")))
})

test_that("Step 4.5 gen_site_sizes traceability statuses are synchronized", {
  trace_dir <- test_path("../../tools/traceability")
  skip_if_not(dir.exists(trace_dir), "Development-only traceability ledgers are not shipped in the package tarball.")

  api <- read.csv(file.path(trace_dir, "api-index.csv"), stringsAsFactors = FALSE)
  invariants <- read.csv(file.path(trace_dir, "invariant-index.csv"), stringsAsFactors = FALSE)

  expect_identical(api$status[match("API016", api$id)], "implemented")
  expect_identical(invariants$status[match("T2", invariants$id)], "regression-implemented")
  expect_identical(invariants$status[match("T3", invariants$id)], "regression-implemented")
  expect_identical(invariants$status[match("T10", invariants$id)], "regression-implemented")
})

test_that("Step 8.2 T1a-T12 regression traceability statuses are synchronized", {
  trace_dir <- test_path("../../tools/traceability")
  skip_if_not(dir.exists(trace_dir), "Development-only traceability ledgers are not shipped in the package tarball.")

  invariants <- read.csv(file.path(trace_dir, "invariant-index.csv"), stringsAsFactors = FALSE)
  ids <- c("T1a", "T1b", paste0("T", 2:12))
  rows <- match(ids, invariants$id)

  expect_false(anyNA(rows))
  expect_true(all(invariants$owner_step[rows] == "Step 8.2"))
  expect_true(all(invariants$status[rows] == "regression-implemented"))
  expect_true(all(file.exists(test_path(invariants$test_file[rows]))))
})

test_that("Step 4.6 gen_se_direct traceability statuses are synchronized", {
  trace_dir <- test_path("../../tools/traceability")
  skip_if_not(dir.exists(trace_dir), "Development-only traceability ledgers are not shipped in the package tarball.")

  api <- read.csv(file.path(trace_dir, "api-index.csv"), stringsAsFactors = FALSE)
  errors <- read.csv(file.path(trace_dir, "error-index.csv"), stringsAsFactors = FALSE)

  expect_identical(api$status[match("API017", api$id)], "implemented")
  expect_identical(errors$status[match("E16", errors$id)], "implemented")
})

test_that("Step 6.1 sim_multisite traceability status is synchronized", {
  trace_dir <- test_path("../../tools/traceability")
  skip_if_not(dir.exists(trace_dir), "Development-only traceability ledgers are not shipped in the package tarball.")

  api <- read.csv(file.path(trace_dir, "api-index.csv"), stringsAsFactors = FALSE)
  errors <- read.csv(file.path(trace_dir, "error-index.csv"), stringsAsFactors = FALSE)

  expect_identical(api$status[match("API005", api$id)], "implemented")
  expect_identical(errors$status[match("E10", errors$id)], "snapshot-implemented")
})

test_that("Step 6.2 sim_meta traceability statuses are synchronized", {
  trace_dir <- test_path("../../tools/traceability")
  skip_if_not(dir.exists(trace_dir), "Development-only traceability ledgers are not shipped in the package tarball.")

  api <- read.csv(file.path(trace_dir, "api-index.csv"), stringsAsFactors = FALSE)
  errors <- read.csv(file.path(trace_dir, "error-index.csv"), stringsAsFactors = FALSE)

  expect_identical(api$status[match("API006", api$id)], "implemented")
  expect_true(all(errors$status[match(c("E09", "E11"), errors$id)] == "implemented"))
})

test_that("Step 6.3 core diagnostic helper traceability statuses are synchronized", {
  trace_dir <- test_path("../../tools/traceability")
  skip_if_not(dir.exists(trace_dir), "Development-only traceability ledgers are not shipped in the package tarball.")

  api <- read.csv(file.path(trace_dir, "api-index.csv"), stringsAsFactors = FALSE)
  ids <- c("API030", "API034", "API035", "API038", "API039")

  expect_true(all(api$status[match(ids, api$id)] == "implemented"))
  expect_true(all(api$owner_step[match(ids, api$id)] == "Step 6.3"))
})

test_that("Step 6.4 threshold rubric traceability statuses are synchronized", {
  trace_dir <- test_path("../../tools/traceability")
  skip_if_not(dir.exists(trace_dir), "Development-only traceability ledgers are not shipped in the package tarball.")

  api <- read.csv(file.path(trace_dir, "api-index.csv"), stringsAsFactors = FALSE)
  errors <- read.csv(file.path(trace_dir, "error-index.csv"), stringsAsFactors = FALSE)

  expect_true(all(api$status[match(c("API028", "API037"), api$id)] == "implemented"))
  expect_true(all(api$owner_step[match(c("API028", "API037"), api$id)] == "Step 6.4"))
  expect_identical(errors$status[match("E28", errors$id)], "implemented")
})

test_that("Step 6.5 reproducibility helper traceability statuses are synchronized", {
  trace_dir <- test_path("../../tools/traceability")
  skip_if_not(dir.exists(trace_dir), "Development-only traceability ledgers are not shipped in the package tarball.")

  api <- read.csv(file.path(trace_dir, "api-index.csv"), stringsAsFactors = FALSE)

  expect_true(all(api$status[match(c("API031", "API032"), api$id)] == "implemented"))
  expect_true(all(api$owner_step[match(c("API031", "API032"), api$id)] == "Step 6.5"))
})

test_that("Step 6.6 print and summary traceability statuses are synchronized", {
  trace_dir <- test_path("../../tools/traceability")
  skip_if_not(dir.exists(trace_dir), "Development-only traceability ledgers are not shipped in the package tarball.")

  api <- read.csv(file.path(trace_dir, "api-index.csv"), stringsAsFactors = FALSE)
  ids <- c("API053", "API054", "API055", "API056")

  expect_true(all(api$status[match(ids, api$id)] == "implemented"))
  expect_true(all(api$owner_step[match(ids, api$id)] == "Step 6.6"))
})

test_that("Gate C remediation traceability decisions are synchronized", {
  trace_dir <- test_path("../../tools/traceability")
  skip_if_not(dir.exists(trace_dir), "Development-only traceability ledgers are not shipped in the package tarball.")

  api <- read.csv(file.path(trace_dir, "api-index.csv"), stringsAsFactors = FALSE)
  decisions <- read.csv(file.path(trace_dir, "decision-index.csv"), stringsAsFactors = FALSE)
  conflicts <- read.csv(file.path(trace_dir, "conflict-checklist.csv"), stringsAsFactors = FALSE)
  docs <- read.csv(file.path(trace_dir, "docs-index.csv"), stringsAsFactors = FALSE)

  expect_true(all(c("DE", "DF", "Q15", "Q16") %in% decisions$id))
  expect_true(all(decisions$status[match(c("DE", "DF"), decisions$id)] == "locked"))
  expect_identical(decisions$status[match("Q15", decisions$id)], "resolved-internal")
  expect_identical(decisions$status[match("Q16", decisions$id)], "resolved-amended-inventory")
  expect_true(all(decisions$type[match(c("DE", "DF"), decisions$id)] == "Decision"))
  expect_true(all(decisions$type[match(c("Q15", "Q16"), decisions$id)] == "OpenQuestion"))
  expect_true(all(decisions$owner_phase[match(c("DE", "DF"), decisions$id)] == "Gate C"))
  expect_true(all(decisions$owner_phase[match(c("Q15", "Q16"), decisions$id)] == "Phase 11"))
  expect_true(all(decisions$owner_step[match(c("DE", "DF"), decisions$id)] %in% c("Step 1.1", "Step 1.2")))
  expect_true(all(decisions$owner_step[match(c("Q15", "Q16"), decisions$id)] == "Step 11.3"))
  expect_identical(decisions$status[match("Q8", decisions$id)], "resolved")
  q8_policy <- decisions$implementation_policy[match("Q8", decisions$id)]
  expect_match(q8_policy, "rather than a lifecycle dependency", fixed = TRUE)
  expect_identical(conflicts$status[match("C14", conflicts$id)], "resolved-amended-inventory")
  expect_match(conflicts$resolution_policy[match("C14", conflicts$id)], "compute_kappa public", fixed = TRUE)
  expect_true(all(conflicts$status[match(c("C09", "C10", "C12", "C13", "C15"), conflicts$id)] != "open"))
  expect_match(conflicts$resolution_policy[match("C09", conflicts$id)], "no lifecycle import", fixed = TRUE)
  expect_match(conflicts$resolution_policy[match("C10", conflicts$id)], "9 Rmd vignettes", fixed = TRUE)
  expect_match(conflicts$resolution_policy[match("C12", conflicts$id)], "9-preset library", fixed = TRUE)
  expect_match(conflicts$resolution_policy[match("C13", conflicts$id)], "ch13/R implementation values", fixed = TRUE)
  expect_match(conflicts$resolution_policy[match("C15", conflicts$id)], "strict open interval", fixed = TRUE)

  api001 <- match("API001", api$id)
  api005 <- match("API005", api$id)
  api020 <- match("API020", api$id)
  api030 <- match("API030", api$id)
  api034 <- match("API034", api$id)
  api037 <- match("API037", api$id)
  api053 <- match("API053", api$id)
  api055 <- match("API055", api$id)

  expect_match(api$unresolved_conflict[api001], "hybrid_init", fixed = TRUE)
  expect_match(api$unresolved_conflict[api005], "hybrid_init", fixed = TRUE)
  expect_match(api$unresolved_conflict[api005], "hybrid_polish", fixed = TRUE)
  expect_match(api$unresolved_conflict[api020], "init and polish", fixed = TRUE)
  expect_match(api$unresolved_conflict[api030], "5th/95th", fixed = TRUE)
  expect_match(api$unresolved_conflict[api034], "sampling drift", fixed = TRUE)
  expect_match(api$unresolved_conflict[api037], "sampling_drift", fixed = TRUE)
  expect_match(api$unresolved_conflict[api053], "no-target", fixed = TRUE)
  expect_match(api$unresolved_conflict[api055], "preset-aware", fixed = TRUE)
  expect_match(docs$unresolved_conflict[match("D01", docs$id)], "Gate C roxygen patches", fixed = TRUE)
})
