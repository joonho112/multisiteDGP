# TEMPORARY — Step 4.1a diagnostic probe. Delete once the drift is located.
#
# Rounding hashed doubles to 9 significant digits did not reconcile macOS and
# Linux, so the difference is larger than 1e-9 in something the payload sees.
# This prints each payload component separately so the two platforms can be
# compared directly from CI logs.

test_that("PROBE: hash payload components", {
  design <- multisitedgp_design(
    paradigm = "site_size", J = 50L, sigma_tau = 0.2,
    nj_mean = 100, cv = 0.4, seed = 12345L
  )
  dat <- sim_multisite(design)
  payload <- .canonical_hash_payload(dat, algo = "xxhash64")

  cat("\n===PROBE-BEGIN===\n")
  cat("platform:", R.version$platform, "\n")
  cat("full_hash:", digest::digest(payload, algo = "xxhash64"), "\n")
  cat("data_only:", digest::digest(payload$data_canonical, algo = "xxhash64"), "\n")
  cat("diag_only:", digest::digest(payload$diagnostics_numeric, algo = "xxhash64"), "\n")
  cat("manifest :", digest::digest(payload$manifest, algo = "xxhash64"), "\n")

  for (col in names(payload$data_canonical)) {
    v <- payload$data_canonical[[col]]
    cat(sprintf("col %-10s digest=%s  head=%s\n", col,
                digest::digest(v, algo = "xxhash64"),
                paste(format(head(v, 3), digits = 17), collapse = " ")))
  }
  for (k in names(payload$diagnostics_numeric)) {
    cat(sprintf("diag %-18s %s\n", k,
                format(payload$diagnostics_numeric[[k]], digits = 17)))
  }
  cat("n_j all integer:", all(dat$n_j == round(dat$n_j)), "\n")
  cat("n_j sum:", sum(dat$n_j), " sorted head:",
      paste(head(sort(dat$n_j), 5), collapse = ","), "\n")
  cat("===PROBE-END===\n")

  expect_true(TRUE)
})
