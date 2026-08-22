evidence_identity_ok <- function(row, package_version, source_digest) {
  required <- c(
    "status", "mode", "package_version", "event", "contract_path",
    "contract_sha256", "source_digest_sha256", "hash_schema_version",
    "rng_policy", "script_path", "script_sha256", "result_path",
    "result_sha256", "summary_path", "summary_sha256"
  )
  if (nrow(row) != 1L || length(setdiff(required, names(row))) > 0L) {
    return(FALSE)
  }

  all(c(
    identical(row$status[[1L]], "pass"),
    identical(row$mode[[1L]], "full"),
    identical(row$package_version[[1L]], package_version),
    identical(row$event[[1L]], "produced"),
    identical(row$source_digest_sha256[[1L]], source_digest),
    identical(row$hash_schema_version[[1L]], "multisiteDGP-canonical-hash-v4"),
    identical(row$rng_policy[[1L]], "package-pinned")
  ))
}

evidence_artifacts_ok <- function(row, package_root) {
  pairs <- list(
    c(row$script_path[[1L]], row$script_sha256[[1L]]),
    c(row$result_path[[1L]], row$result_sha256[[1L]]),
    c(row$summary_path[[1L]], row$summary_sha256[[1L]])
  )
  artifacts_current <- all(vapply(pairs, function(pair) {
    path <- file.path(package_root, pair[[1L]])
    file.exists(path) && identical(unname(tools::sha256sum(path)), pair[[2L]])
  }, logical(1)))
  contract_path <- file.path(package_root, row$contract_path[[1L]])
  artifacts_current && file.exists(contract_path) &&
    identical(unname(tools::sha256sum(contract_path)), row$contract_sha256[[1L]])
}

evidence_sidecar_ok <- function(row, package_root, package_version, source_digest) {
  contract_path <- file.path(package_root, row$contract_path[[1L]])
  sidecar <- read.csv(contract_path, stringsAsFactors = FALSE, check.names = FALSE)
  nrow(sidecar) == 1L &&
    identical(sidecar$source_digest_sha256[[1L]], source_digest) &&
    identical(sidecar$script_sha256[[1L]], row$script_sha256[[1L]]) &&
    identical(sidecar$result_sha256[[1L]], row$result_sha256[[1L]]) &&
    identical(sidecar$summary_sha256[[1L]], row$summary_sha256[[1L]]) &&
    identical(sidecar$package_version[[1L]], package_version)
}

validation_row_current <- function(row, package_root, package_version, source_digest) {
  evidence_identity_ok(row, package_version, source_digest) &&
    evidence_artifacts_ok(row, package_root) &&
    evidence_sidecar_ok(row, package_root, package_version, source_digest)
}

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

test_that("a full-pass validation claim requires current semantic evidence", {
  package_root <- normalizePath(test_path("../.."), mustWork = TRUE)
  trace_dir <- file.path(package_root, "tools", "traceability")
  skip_if_not(
    dir.exists(trace_dir),
    "Development-only traceability ledgers are not shipped in the package tarball."
  )
  trace <- read.csv(
    file.path(trace_dir, "validation-index.csv"),
    stringsAsFactors = FALSE
  )
  plan <- read.csv(
    file.path(package_root, "tools/validation/validation-plan-manifest.csv"),
    stringsAsFactors = FALSE
  )
  runs <- read.csv(
    file.path(package_root, "tools/validation/generated/validation-run-manifest.csv"),
    stringsAsFactors = FALSE
  )
  current_version <- as.character(utils::packageVersion("multisiteDGP"))
  harness <- new.env(parent = globalenv())
  sys.source(
    file.path(package_root, "tools/validation/R/validation-harness.R"),
    envir = harness
  )
  source_digest <- harness$validation_source_digest(package_root)

  expect_identical(trace$status, plan$status[match(trace$id, plan$experiment_id)])
  expect_true(all(trace$status %in% c("pending-current-source", "full-pass")))
  for (id in trace$id[trace$status == "full-pass"]) {
    candidates <- runs[
      runs$experiment_id == id & runs$mode == "full" & runs$status == "pass",
      ,
      drop = FALSE
    ]
    expect_true(nrow(candidates) > 0L, info = id)
    latest <- candidates[which.max(as.POSIXct(candidates$ended_at)), , drop = FALSE]
    identity_ok <- evidence_identity_ok(latest, current_version, source_digest)
    artifacts_ok <- evidence_artifacts_ok(latest, package_root)
    sidecar_ok <- evidence_sidecar_ok(latest, package_root, current_version, source_digest)
    expect_true(
      identity_ok && artifacts_ok && sidecar_ok,
      info = sprintf(
        "%s run=%s identity=%s artifacts=%s sidecar=%s source=%s",
        id, latest$run_id[[1L]], identity_ok, artifacts_ok, sidecar_ok,
        source_digest
      )
    )
  }

  # The pre-repair manifest must not be promotable merely because its files exist.
  stale_v12 <- runs[
    runs$experiment_id == "V12" &
      runs$mode == "full" &
      !is.na(runs$source_digest_sha256) &
      runs$source_digest_sha256 != source_digest,
    ,
    drop = FALSE
  ]
  expect_true(nrow(stale_v12) > 0L)
  old_v12 <- tail(stale_v12, 1L)
  expect_false(validation_row_current(old_v12, package_root, current_version, source_digest))
})

test_that("API ledger is the semantic inventory of the current namespace", {
  trace_dir <- test_path("../../tools/traceability")
  skip_if_not(
    dir.exists(trace_dir),
    "Development-only traceability ledgers are not shipped in the package tarball."
  )
  api <- read.csv(
    file.path(trace_dir, "api-index.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  ledger_functions <- api[["function"]]
  exports <- getNamespaceExports("multisiteDGP")
  nonexports <- setdiff(unique(ledger_functions), exports)

  expect_setequal(exports, setdiff(unique(ledger_functions), nonexports))
  expect_setequal(
    nonexports,
    c(
      "as_tibble.multisitedgp_data", "print.multisitedgp_data",
      "print.multisitedgp_design", "summary.multisitedgp_data",
      "format.multisitedgp_design", "[.multisitedgp_data"
    )
  )
  namespace <- asNamespace("multisiteDGP")
  expect_true(all(vapply(
    nonexports,
    exists,
    logical(1),
    envir = namespace,
    inherits = FALSE
  )))
})

test_that("docs ledger exactly covers the 16 registered source vignettes", {
  skip_if_not_installed("yaml")
  package_root <- normalizePath(test_path("../.."), mustWork = TRUE)
  trace_dir <- file.path(package_root, "tools", "traceability")
  skip_if_not(
    dir.exists(trace_dir) && file.exists(file.path(package_root, "_pkgdown.yml")),
    "Development-only docs registry is not shipped in the package tarball."
  )
  docs <- read.csv(
    file.path(trace_dir, "docs-index.csv"),
    stringsAsFactors = FALSE
  )
  pkgdown <- yaml::read_yaml(file.path(package_root, "_pkgdown.yml"))
  registered <- unlist(lapply(pkgdown$articles, function(group) group$contents), use.names = FALSE)
  source_files <- list.files(
    file.path(package_root, "vignettes"),
    pattern = "^[am][0-9].*[.]Rmd$",
    full.names = FALSE
  )
  source_slugs <- tools::file_path_sans_ext(source_files)
  vignette_docs <- docs[docs$doc_type == "vignette", , drop = FALSE]

  expect_length(registered, 16L)
  expect_setequal(registered, source_slugs)
  expect_setequal(vignette_docs$item, registered)
  expect_setequal(
    tools::file_path_sans_ext(basename(vignette_docs$source_path)),
    registered
  )
  expect_true(all(file.exists(file.path(package_root, docs$source_path))))
})

test_that("invariant ledger points to current executable definitions", {
  package_root <- normalizePath(test_path("../.."), mustWork = TRUE)
  trace_dir <- file.path(package_root, "tools", "traceability")
  skip_if_not(
    dir.exists(trace_dir),
    "Development-only invariant ledger is not shipped in the package tarball."
  )
  invariants <- read.csv(
    file.path(trace_dir, "invariant-index.csv"),
    stringsAsFactors = FALSE
  )

  for (idx in seq_len(nrow(invariants))) {
    path <- file.path(package_root, "tests/testthat", invariants$test_file[[idx]])
    expect_true(file.exists(path), info = invariants$id[[idx]])
    source <- paste(readLines(path, warn = FALSE), collapse = "\n")
    expect_match(source, invariants$id[[idx]], fixed = TRUE, info = path)
  }

  t12 <- invariants[invariants$id == "T12", , drop = FALSE]
  expect_match(
    t12$unresolved_conflict,
    "runif-rnorm-rnorm-rgamma-sample.int-rnorm",
    fixed = TRUE
  )
  t12_source <- paste(
    readLines(file.path(package_root, "tests/testthat", t12$test_file), warn = FALSE),
    collapse = "\n"
  )
  expect_match(
    t12_source,
    'c("runif", "rnorm", "rnorm", "rgamma", "sample.int", "rnorm")',
    fixed = TRUE
  )
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
  ids <- c(sprintf("F%02d", 1:4), "F10")

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
  expect_match(conflicts$resolution_policy[match("C10", conflicts$id)], "all 16 registered Rmd vignettes", fixed = TRUE)
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
  expect_match(docs$unresolved_conflict[match("D01", docs$id)], "current namespace exports", fixed = TRUE)
})
