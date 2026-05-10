# nolint start: object_usage_linter
local_generated_example_output <- function() {
  testthat::local_reproducible_output(width = 80, unicode = FALSE)
  withr::local_options(list(
    cli.width = 80,
    width = 80,
    pillar.print_min = 6L,
    pillar.print_max = 20L,
    pillar.sigfig = 3L
  ))
}

normalize_generated_print_lines <- function(lines) {
  lines <- gsub("R=[0-9.]+", "R=<R>", lines)
  gsub("multisiteDGP [0-9.]+", "multisiteDGP <VERSION>", lines)
}

read_snapshot_lines <- function(file) {
  readLines(test_path(file.path("_snaps", file)), warn = FALSE)
}

print_example_path <- function(file) {
  test_path(file.path("_snaps", "print-examples", file))
}

read_print_example <- function(file) {
  readLines(print_example_path(file), warn = FALSE)
}

capture_generated_print <- function(expr) {
  normalize_generated_print_lines(capture.output(expr))
}

extract_example_number <- function(line) {
  as.numeric(sub(".*# \\[1\\] ([0-9.eE+-]+).*", "\\1", line))
}

test_that("Step 8.6 generated print example inventory is present", {
  expected <- basename(snapshot_policy$generated_print_example_files)
  observed <- sort(list.files(test_path("_snaps/print-examples"), pattern = "[.]txt$"))

  expect_setequal(observed, expected)
})

test_that("Step 8.6 generated print hash policy is explicit", {
  expect_setequal(snapshot_policy$hash_print_files, c(
    "print-examples/ch14_ch17_irb_template.txt",
    "print-examples/ch14_ch17_sae_summary.txt"
  ))
  expect_match(snapshot_policy$hash_print_policy, "Linux x86-64 strict")
  expect_match(snapshot_policy$hash_print_policy, "canonical_hash/design_hash preserved")
  expect_identical(snapshot_policy$hash_print_override_env, "MULTISITEDGP_ALLOW_NON_LINUX_PRINT_REGEN")

  has_hash <- vapply(snapshot_policy$generated_print_example_files, function(file) {
    any(grepl("canonical_hash=|design_hash=", read_snapshot_lines(file)))
  }, logical(1))
  expect_setequal(
    snapshot_policy$generated_print_example_files[has_hash],
    snapshot_policy$hash_print_files
  )
  expect_identical(
    normalize_generated_print_lines("R=4.5.1 canonical_hash=abc design_hash=def"),
    "R=<R> canonical_hash=abc design_hash=def"
  )
})

test_that("Step 8.6 generated print examples match current console output", {
  local_generated_example_output()

  hello <- sim_multisite(seed = 2562L)
  expect_identical(
    read_print_example("ch12_hello_world_print.txt"),
    capture_generated_print(print(hello))
  )

  irb <- sim_multisite(preset_education_modest(), seed = 12345L)
  expect_identical(
    read_print_example("ch14_ch17_irb_template.txt"),
    capture_generated_print(summary(irb))
  )

  sae <- sim_meta(preset_small_area_estimation(), seed = 42L)
  expect_identical(
    read_print_example("ch14_ch17_sae_summary.txt"),
    capture_generated_print(summary(sae))
  )

  expect_identical(
    read_print_example("ch17_print_design.txt"),
    capture_generated_print(print(preset_education_modest()))
  )
})

test_that("Step 8.6 generated diagnostic example is numerically consistent", {
  dat <- sim_multisite(preset_education_modest(), seed = 42L)
  lines <- read_print_example("ch14_group_a_diagnostics.txt")

  observed <- setNames(vapply(lines, extract_example_number, numeric(1)), c(
    "informativeness",
    "sigma_tau",
    "gm_se2",
    "heterogeneity_ratio"
  ))
  expected <- c(
    informativeness = informativeness(dat),
    sigma_tau = stats::sd(dat$tau_j),
    gm_se2 = exp(mean(log(dat$se2_j))),
    heterogeneity_ratio = heterogeneity_ratio(dat$se2_j, trimmed = TRUE)
  )

  expect_lt(abs(observed[["informativeness"]] - expected[["informativeness"]]), 5e-5)
  expect_lt(abs(observed[["sigma_tau"]] - expected[["sigma_tau"]]), 5e-4)
  expect_lt(abs(observed[["gm_se2"]] - expected[["gm_se2"]]), 1e-4)
  expect_lt(abs(observed[["heterogeneity_ratio"]] - expected[["heterogeneity_ratio"]]), 5e-3)
})
# nolint end
