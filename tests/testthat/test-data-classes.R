sample_multisitedgp_frame <- function() {
  data.frame(
    site_index = 1:4,
    z_j = c(-1, -0.2, 0.4, 1.1),
    tau_j = c(-0.2, -0.04, 0.08, 0.22),
    tau_j_hat = c(-0.18, -0.01, 0.11, 0.19),
    se_j = c(0.2, 0.25, 0.3, 0.35),
    se2_j = c(0.04, 0.0625, 0.09, 0.1225),
    n_j = c(50L, 45L, 60L, 55L)
  )
}

test_that("multisitedgp_data constructor creates canonical tibble subclass", {
  des <- multisitedgp_design(seed = 1L)
  diagnostics <- list(I = 0.3, R = 2)
  dat <- multisitedgp_internal(".new_multisitedgp_data")(
    sample_multisitedgp_frame(),
    design = des,
    diagnostics = diagnostics
  )

  expect_identical(class(dat), c("multisitedgp_data", "tbl_df", "tbl", "data.frame"))
  expect_true(is_multisitedgp_data(dat))
  expect_s3_class(dat, "tbl_df")
  expect_identical(attr(dat, "design"), des)
  expect_identical(attr(dat, "diagnostics"), diagnostics)
  expect_identical(attr(dat, "paradigm"), "site_size")
  expect_named(attr(dat, "provenance"), c(
    "seed", "multisitedgp_version", "R_version", "platform", "canonical_hash",
    "design_hash", "hash_algo", "hash_schema_version", "paradigm", "preset",
    "call", "function_exclusion_policy", "custom_hooks"
  ))
  expect_identical(attr(dat, "provenance")$seed, 1L)
  expect_identical(attr(dat, "provenance")$paradigm, "site_size")
  expect_type(attr(dat, "multisitedgp_version"), "character")
})

test_that("multisitedgp_meta inherits from multisitedgp_data", {
  des <- multisitedgp_design(paradigm = "direct", I = 0.5, R = 1.5)
  frame <- sample_multisitedgp_frame()
  frame$n_j <- rep(NA_integer_, nrow(frame))
  meta <- multisitedgp_internal(".new_multisitedgp_meta")(frame, design = des)

  expect_identical(
    class(meta),
    c("multisitedgp_meta", "multisitedgp_data", "tbl_df", "tbl", "data.frame")
  )
  expect_true(is_multisitedgp_data(meta))
  expect_s3_class(meta, "multisitedgp_meta")
  expect_identical(attr(meta, "paradigm"), "direct")
})

test_that("data constructor rejects schema-breaking inputs", {
  missing_col <- sample_multisitedgp_frame()
  missing_col$se2_j <- NULL
  expect_multisitedgp_error(
    multisitedgp_internal(".new_multisitedgp_data")(missing_col),
    "multisitedgp_arg_error"
  )

  bad_type <- sample_multisitedgp_frame()
  bad_type$site_index <- as.numeric(bad_type$site_index)
  expect_multisitedgp_error(
    multisitedgp_internal(".new_multisitedgp_data")(bad_type),
    "multisitedgp_arg_error"
  )

  bad_numeric <- sample_multisitedgp_frame()
  bad_numeric$tau_j_hat <- as.character(bad_numeric$tau_j_hat)
  expect_multisitedgp_error(
    multisitedgp_internal(".new_multisitedgp_data")(bad_numeric),
    "multisitedgp_arg_error"
  )

  bad_n <- sample_multisitedgp_frame()
  bad_n$n_j <- as.numeric(bad_n$n_j)
  expect_multisitedgp_error(
    multisitedgp_internal(".new_multisitedgp_data")(bad_n),
    "multisitedgp_arg_error"
  )
})

test_that("data constructors infer paradigms without an attached design", {
  dat <- multisitedgp_internal(".new_multisitedgp_data")(sample_multisitedgp_frame())
  meta_frame <- sample_multisitedgp_frame()
  meta_frame$n_j <- rep(NA_integer_, nrow(meta_frame))
  meta <- multisitedgp_internal(".new_multisitedgp_meta")(meta_frame)

  expect_identical(attr(dat, "paradigm"), "site_size")
  expect_identical(attr(meta, "paradigm"), "direct")
})

test_that("data constructors reject mismatched design paradigms", {
  direct <- multisitedgp_design(paradigm = "direct", I = 0.5, R = 1.5)
  site_size <- multisitedgp_design(paradigm = "site_size")
  frame <- sample_multisitedgp_frame()

  expect_multisitedgp_error(
    multisitedgp_internal(".new_multisitedgp_data")(frame, design = direct),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    multisitedgp_internal(".new_multisitedgp_meta")(frame, design = site_size),
    "multisitedgp_arg_error"
  )
})

test_that("row subset preserves class and design but drops stale diagnostics", {
  des <- multisitedgp_design(seed = 1L)
  dat <- multisitedgp_internal(".new_multisitedgp_data")(
    sample_multisitedgp_frame(),
    design = des,
    diagnostics = list(I = 0.3)
  )

  expect_warning(
    row_subset <- dat[1:2, ],
    "Diagnostics dropped"
  )
  expect_true(is_multisitedgp_data(row_subset))
  expect_identical(attr(row_subset, "design"), des)
  expect_identical(attr(row_subset, "paradigm"), "site_size")
  expect_null(attr(row_subset, "diagnostics"))
  expect_true(attr(row_subset, "provenance")$row_subset)
  expect_equal(nrow(row_subset), 2L)

  expect_warning(
    same_rows <- dat[TRUE, ],
    "Diagnostics dropped"
  )
  expect_null(attr(same_rows, "diagnostics"))
})

test_that("row subset preserves multisitedgp_meta subclass", {
  des <- multisitedgp_design(paradigm = "direct", I = 0.5, R = 1.5)
  frame <- sample_multisitedgp_frame()
  frame$n_j <- rep(NA_integer_, nrow(frame))
  meta <- multisitedgp_internal(".new_multisitedgp_meta")(
    frame,
    design = des,
    diagnostics = list(I = 0.5)
  )

  expect_warning(
    row_subset <- meta[1:2, ],
    "Diagnostics dropped"
  )
  expect_s3_class(row_subset, "multisitedgp_meta")
  expect_true(is_multisitedgp_data(row_subset))
  expect_identical(attr(row_subset, "paradigm"), "direct")
  expect_null(attr(row_subset, "diagnostics"))
})

test_that("canonical column subset preserves data class and diagnostics", {
  dat <- multisitedgp_internal(".new_multisitedgp_data")(
    sample_multisitedgp_frame(),
    design = multisitedgp_design(),
    diagnostics = list(I = 0.3)
  )
  col_subset <- dat[, names(dat)]

  expect_true(is_multisitedgp_data(col_subset))
  expect_identical(attr(col_subset, "diagnostics"), list(I = 0.3))

  vector_subset <- dat[, "tau_j_hat", drop = TRUE]
  expect_type(vector_subset, "double")
  expect_false(is_multisitedgp_data(vector_subset))
})

test_that("summary formatter reports unavailable diagnostics for bare data objects", {
  dat <- multisitedgp_internal(".new_multisitedgp_data")(sample_multisitedgp_frame())
  summary_lines <- multisitedgp_internal(".format_multisitedgp_summary")(dat)

  expect_true(any(grepl("Diagnostics unavailable", summary_lines, fixed = TRUE)))
  expect_true(any(grepl("Provenance:", summary_lines, fixed = TRUE)))
})

test_that("schema-breaking column subset demotes to plain tibble", {
  dat <- multisitedgp_internal(".new_multisitedgp_data")(
    sample_multisitedgp_frame(),
    design = multisitedgp_design(),
    diagnostics = list(I = 0.3)
  )
  thin <- dat[, c("site_index", "tau_j_hat")]

  expect_s3_class(thin, "tbl_df")
  expect_false(is_multisitedgp_data(thin))
  expect_null(attr(thin, "design"))
  expect_null(attr(thin, "diagnostics"))
  expect_null(attr(thin, "paradigm"))
  expect_null(attr(thin, "provenance"))
})

test_that("as_tibble strips multisiteDGP class and attributes by default", {
  dat <- multisitedgp_internal(".new_multisitedgp_data")(
    sample_multisitedgp_frame(),
    design = multisitedgp_design(),
    diagnostics = list(I = 0.3)
  )
  plain <- tibble::as_tibble(dat)
  preserved <- tibble::as_tibble(dat, .preserve_class = TRUE)

  expect_s3_class(plain, "tbl_df")
  expect_false(is_multisitedgp_data(plain))
  expect_null(attr(plain, "design"))
  expect_null(attr(plain, "diagnostics"))
  expect_null(attr(plain, "paradigm"))
  expect_null(attr(plain, "provenance"))
  expect_true(is_multisitedgp_data(preserved))
  expect_identical(preserved, dat)
})

test_that("dplyr row slicing drops stale diagnostics", {
  dat <- multisitedgp_internal(".new_multisitedgp_data")(
    sample_multisitedgp_frame(),
    design = multisitedgp_design(),
    diagnostics = list(I = 0.3)
  )

  expect_warning(
    filtered <- dplyr::filter(dat, site_index <= 2L),
    "Diagnostics dropped"
  )
  expect_true(is_multisitedgp_data(filtered))
  expect_identical(attr(filtered, "paradigm"), "site_size")
  expect_null(attr(filtered, "diagnostics"))
  expect_equal(nrow(filtered), 2L)
})
