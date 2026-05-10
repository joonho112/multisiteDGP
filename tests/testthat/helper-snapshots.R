# nolint start: object_usage_linter
# Project snapshot policy: run snapshot expectations in CRAN/check contexts.
expect_snapshot <- function(..., cran = TRUE) {
  testthat::expect_snapshot(..., cran = cran)
}

expect_snapshot_file <- function(..., cran = TRUE) {
  testthat::expect_snapshot_file(..., cran = cran)
}
# nolint end
