# nolint start: object_usage_linter
validation_harness_test_context <- function() {
  package_root <- normalizePath(test_path("../.."), mustWork = TRUE)
  harness_path <- file.path(package_root, "tools/validation/R/validation-harness.R")
  skip_if_not(
    file.exists(harness_path),
    "Development-only validation harness is not shipped in the package tarball."
  )
  source(harness_path, local = FALSE)
  output_root <- tempfile("multisiteDGP-validation-harness-")
  dir.create(output_root, recursive = TRUE)
  script_path <- file.path(output_root, "job.R")
  writeLines("invisible(TRUE)", script_path)
  result_path <- file.path(output_root, "contract-test-results.csv")
  summary_path <- file.path(output_root, "contract-test-summary.csv")
  paths <- list(
    package_root = package_root,
    generated_dir = output_root,
    manifest_path = file.path(output_root, "validation-run-manifest.csv"),
    index_path = file.path(package_root, "tools/traceability/validation-index.csv")
  )
  list(
    paths = paths,
    script_path = script_path,
    result_path = result_path,
    summary_path = summary_path,
    parameters = list(reps = 2L, tolerance = 0.01),
    run_id = "contract-test",
    experiment_id = "V0",
    mode = "full",
    seed_root = 910001L
  )
}

validation_harness_produce <- function(context) {
  utils::write.csv(data.frame(value = 1:2), context$result_path, row.names = FALSE)
  utils::write.csv(data.frame(acceptance_pass = TRUE), context$summary_path, row.names = FALSE)
  now <- Sys.time()
  validation_record_manifest(
    paths = context$paths,
    run_id = context$run_id,
    experiment_id = context$experiment_id,
    mode = context$mode,
    status = "pass",
    started_at = now,
    ended_at = now,
    seed_root = context$seed_root,
    reps = 2L,
    script_path = context$script_path,
    result_path = context$result_path,
    summary_path = context$summary_path,
    notes = "Harness contract test producer.",
    parameters = context$parameters
  )
  invisible(context)
}

validation_harness_prepare <- function(context, parameters = context$parameters, resume = TRUE, overwrite = FALSE) {
  validation_prepare_run(
    paths = context$paths,
    run_id = context$run_id,
    experiment_id = context$experiment_id,
    mode = context$mode,
    seed_root = context$seed_root,
    parameters = parameters,
    script_path = context$script_path,
    result_path = context$result_path,
    summary_path = context$summary_path,
    resume = resume,
    overwrite = overwrite
  )
}

test_that("validation resume defaults are false in every job", {
  jobs_dir <- test_path("../../tools/validation/jobs")
  skip_if_not(
    dir.exists(jobs_dir),
    "Development-only validation jobs are not shipped in the package tarball."
  )
  jobs <- list.files(
    jobs_dir,
    pattern = "^run-v.*-validation[.]R$",
    full.names = TRUE
  )
  expect_length(jobs, 14L)
  for (job in jobs) {
    text <- paste(readLines(job, warn = FALSE), collapse = "\n")
    expect_match(text, 'MULTISITEDGP_VALIDATION_RESUME", default = FALSE', fixed = TRUE, info = job)
    expect_match(text, "validation_prepare_run(", fixed = TRUE, info = job)
    expect_match(text, "validation_record_reuse(", fixed = TRUE, info = job)
    expect_no_match(text, "validation_existing_run_complete(result_path", fixed = TRUE, info = job)
  }
})

test_that("fresh validation evidence writes a complete contract sidecar", {
  context <- validation_harness_test_context()
  expect_identical(validation_harness_prepare(context, resume = FALSE)$action, "fresh")
  validation_harness_produce(context)

  sidecar_path <- validation_contract_sidecar_path(context$result_path)
  sidecar <- read.csv(sidecar_path, stringsAsFactors = FALSE)
  expect_true(file.exists(sidecar_path))
  expect_identical(sidecar$contract_schema_version, "phase5-validation-contract-v1")
  expect_identical(sidecar$parameter_sha256, validation_parameter_digest(context$parameters))
  expect_identical(sidecar$result_sha256, validation_file_hash(context$result_path))
  expect_identical(sidecar$summary_sha256, validation_file_hash(context$summary_path))
  expect_identical(sidecar$hash_schema_version, "multisiteDGP-canonical-hash-v4")
  expect_identical(sidecar$rng_policy, "package-pinned")
  expect_false(is.na(sidecar$source_git_sha))
  expect_false(is.na(sidecar$source_digest_sha256))
})

test_that("compatible reuse is a distinct event with producer metadata", {
  context <- validation_harness_test_context()
  validation_harness_produce(context)
  state <- validation_harness_prepare(context)
  expect_identical(state$action, "reuse")
  expect_identical(state$reason, "contract-compatible")

  now <- Sys.time()
  validation_record_reuse(context$paths, state, now, now, "Compatible test reuse.")
  manifest <- read.csv(context$paths$manifest_path, stringsAsFactors = FALSE)
  expect_identical(manifest$event, c("produced", "reuse"))
  expect_identical(manifest$status, c("pass", "reused"))
  expect_identical(manifest$producer_status, c("pass", "pass"))
  expect_identical(manifest$package_version[[2L]], state$sidecar$package_version[[1L]])
  expect_identical(manifest$script_sha256[[2L]], state$sidecar$script_sha256[[1L]])
})

test_that("changed parameters, scripts, schema, and artifacts refuse reuse", {
  context <- validation_harness_test_context()
  validation_harness_produce(context)

  expect_error(
    validation_harness_prepare(context, parameters = list(reps = 3L, tolerance = 0.01)),
    "parameter_sha256",
    fixed = TRUE
  )

  writeLines(c("invisible(TRUE)", "# changed"), context$script_path)
  expect_error(validation_harness_prepare(context), "script_sha256", fixed = TRUE)
  writeLines("invisible(TRUE)", context$script_path)

  sidecar_path <- validation_contract_sidecar_path(context$result_path)
  sidecar <- read.csv(sidecar_path, stringsAsFactors = FALSE)
  sidecar$hash_schema_version <- "multisiteDGP-canonical-hash-v999"
  utils::write.csv(sidecar, sidecar_path, row.names = FALSE, quote = TRUE)
  expect_error(validation_harness_prepare(context), "hash_schema_version", fixed = TRUE)
  validation_harness_produce(context)

  write("tampered", context$result_path, append = TRUE)
  expect_error(validation_harness_prepare(context), "result_sha256", fixed = TRUE)
})

test_that("missing sidecars and collisions fail closed while overwrite is explicit", {
  context <- validation_harness_test_context()
  utils::write.csv(data.frame(value = 1), context$result_path, row.names = FALSE)
  utils::write.csv(data.frame(acceptance_pass = TRUE), context$summary_path, row.names = FALSE)

  expect_error(validation_harness_prepare(context), "must all exist", fixed = TRUE)
  expect_error(
    validation_harness_prepare(context, resume = FALSE),
    "resume is disabled",
    fixed = TRUE
  )
  expect_identical(
    validation_harness_prepare(context, resume = FALSE, overwrite = TRUE)$reason,
    "overwrite-explicit"
  )
})
# nolint end
