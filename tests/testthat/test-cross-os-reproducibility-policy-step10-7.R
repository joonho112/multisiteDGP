test_that("Step 10.7 policy document records OS-specific hash contract", {
  policy_file <- test_path("../../tools/cross-os-reproducibility-policy.md")
  skip_if_not(
    file.exists(policy_file),
    "Development-only reproducibility policy is not shipped in the package tarball."
  )

  policy <- paste(readLines(policy_file, warn = FALSE), collapse = "\n")
  expect_match(policy, "Linux is the strict cross-run hash baseline", fixed = TRUE)
  expect_match(policy, "macOS and Windows are demoted", fixed = TRUE)
  expect_match(policy, "Same-machine reproducibility is required on every OS", fixed = TRUE)
  expect_match(policy, "MULTISITEDGP_ALLOW_NON_LINUX_PRINT_REGEN=false", fixed = TRUE)
  expect_match(policy, "T1a strict golden hash fails on Linux", fixed = TRUE)
})

test_that("Step 10.7 CI workflows advertise the reproducibility policy", {
  workflows_dir <- test_path("../../.github/workflows")
  skip_if_not(dir.exists(workflows_dir), "GitHub workflow files are not shipped in the package tarball.")

  workflow_files <- list.files(workflows_dir, pattern = "[.](yaml|yml)$", full.names = TRUE)
  for (workflow_file in workflow_files) {
    workflow_text <- paste(readLines(workflow_file, warn = FALSE), collapse = "\n")
    expect_match(workflow_text, "MULTISITEDGP_REPRODUCIBILITY_POLICY", fixed = TRUE)
    expect_match(workflow_text, "linux-strict-hash-cross-os-demoted", fixed = TRUE)
    expect_match(workflow_text, "MULTISITEDGP_ALLOW_NON_LINUX_PRINT_REGEN", fixed = TRUE)
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

test_that("Step 10.7 strict hash helper requires Linux x86_64 or amd64", {
  helper_file <- test_path("helper-skip-policy.R")
  helper_text <- paste(readLines(helper_file, warn = FALSE), collapse = "\n")

  expect_match(helper_text, "R.version$platform", fixed = TRUE)
  expect_match(helper_text, "x86_64|amd64", fixed = TRUE)
  expect_match(helper_text, "Linux x86_64/amd64 baseline only", fixed = TRUE)
})

test_that("Step 10.7 golden fixture generator blocks non-Linux authoritative regeneration", {
  generator_file <- test_path("../../tests/data-raw/generate_golden_fixtures.R")
  skip_if_not(file.exists(generator_file), "Golden fixture generator is not shipped in the package tarball.")

  generator_text <- paste(readLines(generator_file, warn = FALSE), collapse = "\n")
  expect_match(generator_text, "is_linux_x86_64", fixed = TRUE)
  expect_match(generator_text, "MULTISITEDGP_ALLOW_NON_LINUX_GOLDEN_REGEN", fixed = TRUE)
  expect_match(generator_text, "Do not commit non-Linux hash-only diffs", fixed = TRUE)
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
