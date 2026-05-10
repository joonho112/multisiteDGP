# nolint start: object_usage_linter
test_that("golden fixture files are registered as file snapshots", {
  manifest_path <- golden_extdata_path("golden-fixture-manifest.csv")
  manifest <- read.csv(manifest_path, stringsAsFactors = FALSE)
  golden_dir <- test_path("_snaps/golden")

  expect_snapshot_file(
    file.path(golden_dir, "README.md"),
    name = "README.md"
  )

  for (file in manifest$fixture_file) {
    expect_snapshot_file(
      file.path(golden_dir, file),
      name = file,
      compare = testthat::compare_file_binary
    )
  }
})
# nolint end
