# nolint start: object_usage_linter
test_that("independent live regeneration matches exact binary artifact snapshots", {
  manifest_path <- golden_extdata_path("golden-fixture-manifest.csv")
  manifest <- read.csv(manifest_path, stringsAsFactors = FALSE)
  live_objects <- c(golden_live_jebs_specs(), golden_live_preset_specs())

  for (file in names(live_objects)) {
    actual_path <- tempfile(pattern = "multisiteDGP-live-golden-", fileext = ".rds")
    saveRDS(live_objects[[file]], actual_path, version = 2)
    expect_snapshot_file(
      actual_path,
      name = file,
      compare = testthat::compare_file_binary
    )
    row <- manifest[manifest$fixture_file == file, , drop = FALSE]
    expect_identical(unname(tools::sha256sum(actual_path)), row$rds_sha256[[1L]])
  }

  readme_source <- test_path("_snaps/golden/README.md")
  readme_copy <- tempfile(pattern = "multisiteDGP-golden-readme-", fileext = ".md")
  writeLines(readLines(readme_source, warn = FALSE), readme_copy, useBytes = TRUE)
  expect_snapshot_file(readme_copy, name = "README.md")
})
# nolint end
