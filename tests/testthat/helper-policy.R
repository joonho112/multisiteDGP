expected_traceability_counts <- c(
  "api-index.csv" = 60L,
  "conflict-checklist.csv" = 21L,
  "decision-index.csv" = 27L,
  "docs-index.csv" = 22L,
  "error-index.csv" = 30L,
  "fixture-index.csv" = 9L,
  "invariant-index.csv" = 22L,
  "preset-index.csv" = 9L,
  "validation-index.csv" = 13L
)

expected_unused_imports_phase1 <- character()

expected_import_excepts_p1 <- character()

expected_used_imports_phase1 <- c(
  "RColorBrewer",
  "cli",
  "digest",
  "dplyr",
  "ggplot2",
  "nleqslv",
  "rlang",
  "stats",
  "tibble",
  "tidyr",
  "viridisLite",
  "withr"
)

phase1_desc_imports <- function(desc_path) {
  desc <- read.dcf(desc_path)
  import_chunks <- unlist(strsplit(desc[, "Imports"], ",", fixed = TRUE), use.names = FALSE)
  import_pkgs <- sub("[[:space:]]*\\(.*\\)[[:space:]]*$", "", import_chunks)
  import_pkgs <- trimws(import_pkgs)
  import_pkgs[nzchar(import_pkgs)]
}

phase1_r_pkg_refs <- function(r_dir) {
  r_files <- list.files(r_dir, pattern = "[.][rR]$", recursive = TRUE, full.names = TRUE)
  if (length(r_files) == 0L) {
    return(character())
  }

  parsed <- lapply(r_files, function(path) {
    getParseData(parse(path, keep.source = TRUE))
  })
  refs <- unlist(lapply(parsed, function(data) data$text[data$token == "SYMBOL_PACKAGE"]), use.names = FALSE)
  sort(unique(refs[nzchar(refs)]))
}

snapshot_policy <- list(
  error_snapshots_owner = "Step 8.5",
  print_snapshots_owner = "Step 8.6",
  accept_requires_log = TRUE,
  random_numeric_snapshots_require_fixed_seed = TRUE,
  error_snapshot_files = "snapshot-errors.md",
  error_snapshot_count = 30L,
  active_error_snapshot_count = 29L,
  deferred_error_placeholder_count = 1L,
  warning_not_abort_count = 1L,
  print_snapshot_files = "snapshot-print.md",
  print_snapshot_count = 24L,
  generated_print_example_files = c(
    "print-examples/ch12_hello_world_print.txt",
    "print-examples/ch14_group_a_diagnostics.txt",
    "print-examples/ch14_ch17_irb_template.txt",
    "print-examples/ch14_ch17_sae_summary.txt",
    "print-examples/ch17_print_design.txt"
  ),
  hash_print_files = c(
    "print-examples/ch14_ch17_irb_template.txt",
    "print-examples/ch14_ch17_sae_summary.txt"
  ),
  hash_print_policy = paste(
    "Linux x86-64 strict regeneration;",
    "R version normalized;",
    "canonical_hash/design_hash preserved"
  ),
  hash_print_override_env = "MULTISITEDGP_ALLOW_NON_LINUX_PRINT_REGEN"
)

golden_fixture_policy <- list(
  owner = "Step 8.1",
  count = 9L,
  jebs_appendix_seed_files = 4L,
  preset_output_files = 5L
)

property_policy <- list(
  owner = "Step 8.4",
  count = 12L,
  gate_envvar = "MULTISITEDGP_RUN_PROPERTY",
  default_skipped = TRUE,
  generator = "fixed_seed_direct"
)
