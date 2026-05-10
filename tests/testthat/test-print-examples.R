# nolint start: object_usage_linter
test_that("generated print example files are registered as file snapshots", {
  files <- basename(snapshot_policy$generated_print_example_files)
  example_dir <- test_path("_snaps/print-examples")

  for (file in files) {
    expect_snapshot_file(
      file.path(example_dir, file),
      name = file,
      compare = testthat::compare_file_text
    )
  }
})
# nolint end
