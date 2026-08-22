# nolint start: object_usage_linter
golden_manifest <- function() {
  manifest_path <- golden_extdata_path("golden-fixture-manifest.csv")
  read.csv(manifest_path, stringsAsFactors = FALSE)
}

expected_golden_fixtures <- function() {
  data.frame(
    fixture_id = c(sprintf("F%02d", 1:9), "F10"),
    fixture_file = c(
      "jebs_appendix_mixture_seed42.rds",
      "jebs_appendix_mixture_seed1.rds",
      "jebs_appendix_mixture_seed2024.rds",
      "jebs_appendix_mixture_seed12345.rds",
      "preset_jebs_paper.rds",
      "preset_jebs_strict.rds",
      "preset_education_modest.rds",
      "preset_walters_2024.rds",
      "preset_small_area_estimation.rds",
      "jebs_appendix_floor_active_seed42.rds"
    ),
    fixture_type = c(
      rep("JEBS appendix seed", 4L),
      rep("preset output", 5L),
      "JEBS appendix floor-active authority"
    ),
    stringsAsFactors = FALSE
  )
}

test_that("Step 8.1 golden fixture inventory matches the shipped manifest", {
  manifest <- golden_manifest()
  expected <- expected_golden_fixtures()
  golden_dir <- test_path("_snaps/golden")
  rds_files <- sort(list.files(golden_dir, pattern = "\\.rds$", full.names = FALSE))
  inst_golden_dir <- dirname(golden_extdata_path("golden-fixture-manifest.csv"))
  inst_rds_files <- list.files(inst_golden_dir, pattern = "\\.rds$", full.names = FALSE)

  expect_equal(nrow(manifest), golden_fixture_policy$count)
  expect_equal(length(rds_files), golden_fixture_policy$count)
  expect_length(inst_rds_files, 0L)
  expect_identical(manifest$fixture_id, expected$fixture_id)
  expect_identical(manifest$fixture_file, expected$fixture_file)
  expect_identical(manifest$fixture_type, expected$fixture_type)
  expect_identical(sort(manifest$fixture_file), rds_files)
  expect_true(file.exists(golden_extdata_path("README.md")))
  expect_true(file.exists(test_path("_snaps/golden/README.md")))
})

test_that("Step 8.1 golden RDS files match the manifest hashes", {
  manifest <- golden_manifest()
  golden_dir <- test_path("_snaps/golden")

  for (idx in seq_len(nrow(manifest))) {
    row <- manifest[idx, ]
    path <- file.path(golden_dir, row$fixture_file)
    object <- readRDS(path)

    expect_identical(unname(tools::sha256sum(path)), row$rds_sha256)
    expect_identical(canonical_hash(object), row$canonical_hash)
    expect_identical(row$hash_schema_version, "multisiteDGP-canonical-hash-v4")
    expect_equal(nrow(object), row$nrow)
    expect_equal(ncol(object), row$ncol)
  }
})

test_that("Step 8.1 full-object fixtures carry current self-consistent provenance", {
  manifest <- golden_manifest()
  golden_dir <- test_path("_snaps/golden")
  preset_rows <- manifest$fixture_id %in% sprintf("F%02d", 5:9)

  for (idx in which(preset_rows)) {
    object <- readRDS(file.path(golden_dir, manifest$fixture_file[[idx]]))
    provenance <- attr(object, "provenance", exact = TRUE)

    expect_identical(provenance$hash_schema_version, manifest$hash_schema_version[[idx]])
    expect_identical(provenance$canonical_hash, canonical_hash(object))
    expect_identical(provenance$canonical_hash, manifest$canonical_hash[[idx]])
    expect_true(manifest$stored_provenance_hash_matches[[idx]])
  }
})

test_that("Step 8.1 preset fixtures are independent live regenerations", {
  manifest <- golden_manifest()
  golden_dir <- test_path("_snaps/golden")
  live_objects <- golden_live_preset_specs()
  preset_rows <- manifest$fixture_id %in% sprintf("F%02d", 5:9)

  expect_identical(manifest$fixture_file[preset_rows], names(live_objects))
  for (file in names(live_objects)) {
    live <- live_objects[[file]]
    golden <- readRDS(file.path(golden_dir, file))
    row <- manifest[manifest$fixture_file == file, , drop = FALSE]

    # The canonical hash is the portable authority; raw doubles differ by one or
    # two ULP between the platform that generated the fixture and any other, and
    # the derived diagnostics differ more because cor()/sd() accumulate in a
    # platform-dependent order. Those diagnostics are exactly what schema v3
    # removed from the payload. Comparing them with identical() asserted the
    # opposite of the contract (D-060).
    expect_identical(canonical_hash(live), canonical_hash(golden), info = file)
    expect_identical(canonical_hash(live), row$canonical_hash[[1L]], info = file)

    expect_identical(class(live), class(golden), info = file)
    expect_identical(names(live), names(golden), info = file)
    expect_identical(dim(as.data.frame(live)), dim(as.data.frame(golden)), info = file)
    expect_equal(as.data.frame(live), as.data.frame(golden),
                 tolerance = 1e-12, info = file)

    # The design is exact — it is the input, not a computed result.
    expect_identical(
      attr(live, "design", exact = TRUE),
      attr(golden, "design", exact = TRUE),
      info = file
    )
    expect_equal(
      attr(live, "diagnostics", exact = TRUE),
      attr(golden, "diagnostics", exact = TRUE),
      tolerance = 1e-10, info = file
    )

    # Provenance records the producing runtime, so a fixture built under one R
    # version can never match a verifier running another. Compare the fields
    # that identify the run, not the ones that identify the machine.
    live_prov <- attr(live, "provenance", exact = TRUE)
    golden_prov <- attr(golden, "provenance", exact = TRUE)
    for (field in c("seed", "paradigm", "design_hash", "hash_algo",
                    "hash_schema_version", "rng_kind")) {
      if (!is.null(golden_prov[[field]])) {
        expect_identical(live_prov[[field]], golden_prov[[field]],
                         info = paste(file, field))
      }
    }
  }
})

test_that("Step 8.1 manifest is bound to the current fixture generator", {
  manifest <- golden_manifest()
  generator_path <- test_path("../data-raw/generate_golden_fixtures.R")
  skip_if_not(
    file.exists(generator_path),
    "Golden fixture generator is not shipped in the package tarball."
  )
  generator_sha <- unname(tools::sha256sum(generator_path))

  expect_true(all(manifest$generator_sha256 == generator_sha))
  expect_true(all(manifest$package_version == as.character(utils::packageVersion("multisiteDGP"))))
  expect_true(all(manifest$hash_schema_version == "multisiteDGP-canonical-hash-v4"))
})

test_that("Step 8.1 golden fixtures preserve object classes and schema", {
  manifest <- golden_manifest()
  golden_dir <- test_path("_snaps/golden")
  canonical_cols <- c("site_index", "z_j", "tau_j", "tau_j_hat", "se_j", "se2_j", "n_j")

  jebs_rows <- manifest$fixture_id %in% c(sprintf("F%02d", 1:4), "F10")
  preset_rows <- manifest$fixture_id %in% sprintf("F%02d", 5:9)

  for (file in manifest$fixture_file[jebs_rows]) {
    object <- readRDS(file.path(golden_dir, file))
    expect_s3_class(object, "data.frame")
    expect_false(is_multisitedgp_data(object))
    expect_setequal(names(object), canonical_cols)
  }

  for (file in manifest$fixture_file[preset_rows]) {
    object <- readRDS(file.path(golden_dir, file))
    expect_true(is_multisitedgp_data(object))
    expect_identical(names(object), canonical_cols)
  }

  sae <- readRDS(file.path(golden_dir, "preset_small_area_estimation.rds"))
  expect_s3_class(sae, "multisitedgp_meta")
})

test_that("Step 8.1 JEBS fixtures agree with the Step 4.1 provenance manifest", {
  provenance_manifest <- test_path("../../tools/jebs-golden-fixtures/jebs-golden-fixture-manifest.csv")
  skip_if_not(file.exists(provenance_manifest), "Development-only JEBS provenance manifest is absent.")

  manifest <- golden_manifest()
  jebs_manifest <- read.csv(provenance_manifest, stringsAsFactors = FALSE)

  rows <- match(c(sprintf("F%02d", 1:4), "F10"), manifest$fixture_id)
  expect_identical(
    manifest$fixture_file[rows],
    jebs_manifest$fixture_file
  )
  expect_identical(
    manifest$source_canonical_hash[rows],
    jebs_manifest$canonical_hash
  )
})

test_that("Step 8.1 golden fixture traceability is synchronized when ledgers are present", {
  fixture_index_path <- test_path("../../tools/traceability/fixture-index.csv")
  skip_if_not(file.exists(fixture_index_path), "Development-only traceability ledgers are absent.")

  manifest <- golden_manifest()
  fixture_index <- read.csv(fixture_index_path, stringsAsFactors = FALSE)

  expect_identical(manifest$fixture_id, fixture_index$id)
  expect_identical(manifest$fixture_file, fixture_index$fixture_file)
  expect_identical(manifest$fixture_type, fixture_index$fixture_type)
  expect_true(all(fixture_index$status == "rds-generated"))
  expect_true(all(grepl("test-only RDS fixture tradeoff|test tarball RDS", fixture_index$unresolved_conflict)))
})
# nolint end
