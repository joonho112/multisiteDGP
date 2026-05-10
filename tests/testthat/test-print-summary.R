# nolint start: object_usage_linter
test_that("print.multisitedgp_data reports headline diagnostics and returns invisibly", {
  out <- sim_multisite(J = 25L, seed = 6601L)

  printed <- capture.output(returned <- print(out, n = 3L))
  no_target_headline <- grep("^#   (I|R):", printed, value = TRUE)

  expect_type(getS3method("print", "multisitedgp_data"), "closure")
  expect_identical(returned, out)
  expect_true(any(grepl("# A multisitedgp_data:", printed, fixed = TRUE)))
  expect_true(any(grepl("# Realized vs intended:", printed, fixed = TRUE)))
  expect_true(any(grepl("I:", printed, fixed = TRUE)))
  expect_true(any(grepl("R:", printed, fixed = TRUE)))
  expect_true(any(grepl("sigma_tau:", printed, fixed = TRUE)))
  expect_true(any(grepl("rho_S:", printed, fixed = TRUE)))
  expect_true(any(grepl("rho_S_marg:", printed, fixed = TRUE)))
  expect_true(any(grepl("Feasibility:", printed, fixed = TRUE)))
  expect_true(any(grepl("site_index", printed, fixed = TRUE)))
  expect_true(any(grepl("# Use summary(df) for the full diagnostic report.", printed, fixed = TRUE)))
  expect_length(no_target_headline, 2L)
  expect_true(all(grepl("realized=", no_target_headline, fixed = TRUE)))
  expect_true(all(grepl("(no target)", no_target_headline, fixed = TRUE)))
  expect_false(any(grepl("target=N/A", no_target_headline, fixed = TRUE)))
  expect_false(any(grepl(", N/A", no_target_headline, fixed = TRUE)))
})

test_that("print.multisitedgp_data works for direct meta-analysis objects", {
  out <- sim_meta(I = 0.4, R = 3, seed = 6602L)

  printed <- capture.output(returned <- print(out, n = 3L))

  expect_identical(returned, out)
  expect_true(any(grepl("# A multisitedgp_meta:", printed, fixed = TRUE)))
  expect_true(any(grepl("paradigm = \"direct\"", printed, fixed = TRUE)))
  expect_true(any(grepl("tau_j_hat", printed, fixed = TRUE)))
})

test_that("summary.multisitedgp_data prints the diagnostic report and returns invisibly", {
  out <- sim_meta(I = 0.4, R = 3, seed = 6603L)

  printed <- capture.output(returned <- summary(out))

  expect_type(getS3method("summary", "multisitedgp_data"), "closure")
  expect_identical(returned, out)
  expect_true(any(grepl("multisiteDGP simulation diagnostics", printed, fixed = TRUE)))
  expect_true(any(grepl("A. Realized vs Intended", printed, fixed = TRUE)))
  expect_true(any(grepl("B. Dependence", printed, fixed = TRUE)))
  expect_true(any(grepl("C. G shape fit", printed, fixed = TRUE)))
  expect_true(any(grepl("D. Operational feasibility", printed, fixed = TRUE)))
  expect_true(any(grepl("Overall:", printed, fixed = TRUE)))
  expect_true(any(grepl("Provenance:", printed, fixed = TRUE)))
  expect_true(any(grepl("canonical_hash=", printed, fixed = TRUE)))
  expect_false(any(grepl("(custom)", printed, fixed = TRUE)))
})

test_that("summary.multisitedgp_data title includes preset only when present", {
  out <- sim_multisite(J = 25L, seed = 6607L)

  expect_identical(capture.output(summary(out))[[1L]], "multisiteDGP simulation diagnostics")

  provenance <- attr(out, "provenance", exact = TRUE)
  provenance$preset <- "preset_education_modest"
  attr(out, "provenance") <- provenance

  expect_identical(
    capture.output(summary(out))[[1L]],
    "multisiteDGP simulation diagnostics (preset_education_modest)"
  )
})

test_that("print.multisitedgp_data avoids awkward no-target headline wording", {
  out <- sim_multisite(J = 25L, seed = 6606L)
  diagnostics <- attr(out, "diagnostics", exact = TRUE)
  tab <- diagnostics$target_vs_realized
  row <- tab$diagnostic == "realized_rank_corr" & tab$basis == "residual"
  tab$target[row] <- NA_real_
  tab$status[row] <- NA_character_
  diagnostics$target_vs_realized <- tab
  attr(out, "diagnostics") <- diagnostics

  printed <- capture.output(print(out, n = 1L))
  rho_line <- grep("rho_S:", printed, value = TRUE, fixed = TRUE)

  expect_length(rho_line, 1L)
  expect_match(rho_line, "rho_S: realized=", fixed = TRUE)
  expect_match(rho_line, "(no target)", fixed = TRUE)
  expect_false(grepl("target=N/A", rho_line, fixed = TRUE))
  expect_false(grepl(", N/A", rho_line, fixed = TRUE))
})

test_that("summary recomputes diagnostics for row-subset objects without stale hashes", {
  out <- sim_multisite(J = 25L, seed = 6604L)
  sub <- suppressWarnings(out[seq_len(12L), ])

  printed <- capture.output(returned <- summary(sub))

  expect_identical(returned, sub)
  expect_null(attr(sub, "diagnostics", exact = TRUE))
  expect_true(any(grepl("multisiteDGP simulation diagnostics", printed, fixed = TRUE)))
  expect_true(any(grepl(canonical_hash(sub), printed, fixed = TRUE)))
  expect_false(any(grepl(canonical_hash(out), printed, fixed = TRUE)))
})

test_that("print.multisitedgp_data validates the row display count", {
  out <- sim_multisite(J = 25L, seed = 6605L)

  expect_multisitedgp_error(print(out, n = 0L), "multisitedgp_arg_error")
  expect_multisitedgp_error(print(out, n = NA_integer_), "multisitedgp_arg_error")
})
# nolint end
