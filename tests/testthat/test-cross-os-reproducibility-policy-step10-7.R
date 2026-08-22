test_that("the policy document states the portable numerical hash contract", {
  policy_file <- test_path("../../tools/cross-os-reproducibility-policy.md")
  skip_if_not(
    file.exists(policy_file),
    "Development-only reproducibility policy is not shipped in the package tarball."
  )

  policy <- paste(readLines(policy_file, warn = FALSE), collapse = "\n")
  expect_match(policy, "canonical numerical content", fixed = TRUE)
  expect_match(policy, "byte identity", fixed = TRUE)
  expect_match(policy, "Derived diagnostics", fixed = TRUE)
  expect_match(policy, "package-pinned", fixed = TRUE)
})

test_that("the policy document no longer claims a platform hierarchy", {
  # v0.1 named Linux the authoritative baseline and exempted macOS and Windows.
  # The hash is portable now, so any survivor of that wording would be a false
  # statement about what the package promises (defect ledger D-002, D-007).
  policy_file <- test_path("../../tools/cross-os-reproducibility-policy.md")
  skip_if_not(file.exists(policy_file), "Policy is not shipped in the package tarball.")

  contract <- sub("## What changed in v0.2.0.*$", "",
                  paste(readLines(policy_file, warn = FALSE), collapse = "\n"))

  expect_false(grepl("strict cross-run hash baseline", contract, fixed = TRUE))
  expect_false(grepl("are demoted", contract, fixed = TRUE))
})

test_that("Step 10.7 CI workflows advertise the reproducibility policy", {
  workflows_dir <- test_path("../../.github/workflows")
  skip_if_not(dir.exists(workflows_dir), "GitHub workflow files are not shipped in the package tarball.")

  workflow_files <- list.files(workflows_dir, pattern = "[.](yaml|yml)$", full.names = TRUE)
  for (workflow_file in workflow_files) {
    workflow_text <- paste(readLines(workflow_file, warn = FALSE), collapse = "\n")
    expect_match(workflow_text, "MULTISITEDGP_REPRODUCIBILITY_POLICY", fixed = TRUE)
    expect_match(workflow_text, "portable-numerical-hash-v4", fixed = TRUE)
  }
})

test_that("Step 10.7 R-CMD-check matrix locks the five OS/R cells", {
  workflow_file <- test_path("../../.github/workflows/R-CMD-check.yaml")
  skip_if_not(file.exists(workflow_file), "R-CMD-check workflow is not shipped in the package tarball.")
  skip_if_not_installed("yaml")

  workflow <- yaml::read_yaml(workflow_file)
  config <- workflow$jobs$`R-CMD-check`$strategy$matrix$config
  observed <- vapply(config, function(x) {
    paste(x$os, x$r, x$id, sep = "|")
  }, character(1))
  expected <- c(
    "ubuntu-latest|release|linux-release",
    "ubuntu-latest|devel|linux-devel",
    "ubuntu-latest|oldrel-1|linux-oldrel",
    "macos-latest|release|macos-release",
    "windows-latest|release|windows-release"
  )

  expect_identical(sort(observed), sort(expected))
  expect_true(all(vapply(config, function(x) {
    if (grepl("^linux-", x$id)) {
      identical(x$os, "ubuntu-latest")
    } else {
      x$id %in% c("macos-release", "windows-release")
    }
  }, logical(1))))
})

test_that("the hash is identical across the paradigms and engines", {
  # The contract is one promise for every platform, so the thing worth pinning
  # is that a design hashes to one value however it is reached — not that some
  # platform is privileged.
  flat <- sim_multisite(J = 12L, sigma_tau = 0.2, nj_mean = 100, seed = 7304L)
  viad <- sim_multisite(
    multisitedgp_design(
      paradigm = "site_size", J = 12L, sigma_tau = 0.2,
      nj_mean = 100, seed = 7304L
    )
  )

  expect_identical(canonical_hash(flat), canonical_hash(viad))
})

test_that("the golden fixture generator keeps its accidental-regeneration gate", {
  generator_file <- test_path("../../tests/data-raw/generate_golden_fixtures.R")
  skip_if_not(file.exists(generator_file), "Golden fixture generator is not shipped in the package tarball.")

  generator_text <- paste(readLines(generator_file, warn = FALSE), collapse = "\n")
  # The gate survives as a speed bump against accidental regeneration. It is no
  # longer a platform claim; exact artifact SHA and canonical numerical hash
  # have separate roles.
  expect_match(generator_text, "MULTISITEDGP_ALLOW_NON_LINUX_GOLDEN_REGEN", fixed = TRUE)
})

test_that("Step 10.7 same-machine seed reproducibility is stable", {
  design <- preset_education_modest(J = 12L)
  hashes <- vapply(seq_len(4), function(i) {
    canonical_hash(sim_multisite(design, seed = 7301L))
  }, character(1))

  expect_identical(length(unique(hashes)), 1L)
})

test_that("Step 10.7 active RNG reproducibility is stable from the same state", {
  design <- preset_education_modest(J = 12L)
  hashes <- vapply(seq_len(4), function(i) {
    withr::with_seed(
      7302L,
      canonical_hash(sim_multisite(design, seed = NULL))
    )
  }, character(1))

  expect_identical(length(unique(hashes)), 1L)
})

test_that("Step 10.7 provenance hash matches canonical hash on generated data", {
  dat <- sim_multisite(preset_education_modest(J = 12L), seed = 7303L)
  provenance <- attr(dat, "provenance", exact = TRUE)

  expect_identical(provenance$canonical_hash, canonical_hash(dat))
  expect_match(provenance_string(dat), provenance$canonical_hash, fixed = TRUE)
})
