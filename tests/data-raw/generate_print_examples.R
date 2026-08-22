#!/usr/bin/env Rscript
# nolint start: object_name_linter, object_usage_linter

# Regenerate on any platform; see tools/cross-os-reproducibility-policy.md. The
# printed R version, producer/verifier platform, and package version are
# normalized, but canonical_hash and design_hash are preserved deliberately —
# they are the numerical provenance the examples exist to pin.
is_linux_x86_64 <- function() {
  platform <- tolower(R.version$platform)
  identical(tolower(Sys.info()[["sysname"]]), "linux") &&
    grepl("x86_64|amd64", platform)
}

if (!is_linux_x86_64() &&
    !identical(Sys.getenv("MULTISITEDGP_ALLOW_NON_LINUX_PRINT_REGEN"), "true")) {
  stop(
    paste(
      "Generated print examples embed canonical_hash/design_hash values.",
      "This gate is a speed bump against accidental regeneration, not a platform",
      "requirement -- the canonical numerical hash is portable. Set",
      "MULTISITEDGP_ALLOW_NON_LINUX_PRINT_REGEN=true when you mean to regenerate,",
      "and say in the commit why the values should move."
    ),
    call. = FALSE
  )
}

if (!is_linux_x86_64()) {
  warning(
    paste(
      "Regenerating print-examples moves canonical_hash/design_hash.",
      "Commit the diff only if the values are meant to move."
    ),
    call. = FALSE
  )
}

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg) == 1L) {
  normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE)
} else {
  normalizePath("tests/data-raw/generate_print_examples.R", mustWork = TRUE)
}

package_root <- normalizePath(file.path(dirname(script_path), "..", ".."), mustWork = TRUE)
out_dir <- file.path(package_root, "tests", "testthat", "_snaps", "print-examples")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("pkgload is required to generate print examples.", call. = FALSE)
}
pkgload::load_all(package_root, quiet = TRUE)

testthat::local_reproducible_output(width = 80, unicode = FALSE)
withr::local_options(list(
  width = 80,
  cli.width = 80,
  pillar.print_min = 6L,
  pillar.print_max = 20L,
  pillar.sigfig = 3L
))

# Must stay in step with `normalize_generated_print_lines()` in
# tests/testthat/test-generated-print-examples-step8-6.R. The generator used to
# mask only the R version, so every regeneration wrote a literal package
# version where the test expected <VERSION> and the comparison failed
# (defect ledger D-026).
normalize_print_lines <- function(lines) {
  # Mask the whole R-version value: under R-devel the recorded version is
  # "R Under development (unstable) (...)", which a short numeric form does
  # not match, and the raw string then leaks into the comparison (D-060).
  lines <- gsub("(producer|verifier)_R=.*?(?=( \\| )|( [a-z_]+=)|$)",
             "\\1_R=<R>", lines, perl = TRUE)
  lines <- gsub("R=[0-9.]+", "R=<R>", lines)
  lines <- gsub("(producer|verifier)_platform=[^ |]+", "\\1_platform=<PLATFORM>", lines)
  gsub("multisiteDGP [0-9.]+", "multisiteDGP <VERSION>", lines)
}

write_example <- function(filename, lines) {
  writeLines(normalize_print_lines(lines), file.path(out_dir, filename), useBytes = TRUE)
}

dat_hello <- sim_multisite(seed = 2562L)
write_example("ch12_hello_world_print.txt", capture.output(print(dat_hello)))

dat_group_a <- sim_multisite(preset_education_modest(), seed = 42L)
gm_se2 <- exp(mean(log(dat_group_a$se2_j)))
group_a_lines <- c(
  sprintf("informativeness(dat)        # [1] %.4f", informativeness(dat_group_a)),
  sprintf("stats::sd(dat$tau_j)        # [1] %.3f", stats::sd(dat_group_a$tau_j)),
  sprintf("GM_se2(dat)                 # [1] %.4f", gm_se2),
  sprintf(
    "heterogeneity_ratio(...)    # [1] %.2f",
    heterogeneity_ratio(dat_group_a$se2_j, trimmed = TRUE)
  )
)
write_example("ch14_group_a_diagnostics.txt", group_a_lines)

dat_irb <- sim_multisite(preset_education_modest(), seed = 12345L)
write_example("ch14_ch17_irb_template.txt", capture.output(summary(dat_irb)))

dat_sae <- sim_meta(preset_small_area_estimation(), seed = 42L)
write_example("ch14_ch17_sae_summary.txt", capture.output(summary(dat_sae)))

des <- preset_education_modest()
write_example("ch17_print_design.txt", capture.output(print(des)))

# nolint end
