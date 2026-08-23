# nolint start: object_usage_linter
test_that("installed error catalog resolves all stable E01 through E30 IDs", {
  catalog <- error_catalog()

  expect_s3_class(catalog, "tbl_df")
  expect_identical(catalog$id, sprintf("E%02d", 1:30))
  expect_identical(
    names(catalog),
    c("id", "condition", "class", "active_v0_2", "api", "remedy", "status")
  )
  expect_false(anyNA(catalog))
  expect_true(all(nzchar(catalog$api)))
  expect_true(all(nzchar(catalog$remedy)))
})

test_that("error_catalog filters case-insensitively and preserves request order", {
  selected <- error_catalog(c("e28", "E04", "e15"))

  expect_identical(selected$id, c("E28", "E04", "E15"))
  expect_identical(selected$class[[1L]], "warn-not-abort")
  expect_match(selected$remedy[[1L]], "warning, not an abort", fixed = TRUE)
})

test_that("error_catalog rejects malformed and unknown IDs", {
  expect_error(error_catalog(character()), class = "multisitedgp_arg_error")
  expect_error(error_catalog("E31"), class = "multisitedgp_arg_error")
})

test_that("installed error catalog is synchronized with the development ledger", {
  ledger_path <- test_path("../../tools/traceability/error-index.csv")
  skip_if_not(file.exists(ledger_path), "Development-only error ledger is absent.")
  ledger <- read.csv(ledger_path, stringsAsFactors = FALSE)
  catalog <- error_catalog()

  expect_identical(catalog$id, ledger$id)
  expect_identical(catalog$condition, ledger$scenario)
  expect_identical(catalog$class, ledger$class)
  expect_identical(catalog$status, ledger$status)
})
# nolint end
