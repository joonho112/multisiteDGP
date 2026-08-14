#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg) == 1L) {
  normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE)
} else {
  normalizePath("tools/validation/jobs/run-v02-validation.R", mustWork = TRUE)
}
source(file.path(dirname(script_path), "..", "R", "validation-harness.R"))

paths <- validation_paths(script_path)
validation_load_package(paths$package_root)

experiment_id <- "V02"
mode <- Sys.getenv("MULTISITEDGP_VALIDATION_MODE", unset = "full")
seed_root <- validation_env_int("MULTISITEDGP_VALIDATION_SEED_ROOT", 910201L)
resume <- validation_env_flag("MULTISITEDGP_VALIDATION_RESUME", default = TRUE)
overwrite <- validation_env_flag("MULTISITEDGP_VALIDATION_OVERWRITE", default = FALSE)
run_id <- validation_run_id(experiment_id, mode)

J_grid <- c(25L, 50L, 100L, 200L, 300L)
nj_mean_grid <- c(10, 20, 40, 80, 160)
cv_grid <- c(0, 0.25, 0.50, 0.75)
sigma_grid <- c(0.05, 0.10, 0.15, 0.20, 0.25)
seed_grid <- c(42L, 1L, 2024L, 12345L)
if (!identical(mode, "full")) {
  J_grid <- c(25L, 50L, 100L)
  nj_mean_grid <- c(10, 80, 160)
  cv_grid <- c(0.25, 0.50, 0.75)
  sigma_grid <- c(0.05, 0.15, 0.25)
  seed_grid <- c(42L, 1L)
}
grid <- expand.grid(
  J = J_grid,
  nj_mean = nj_mean_grid,
  cv = cv_grid,
  sigma_tau = sigma_grid,
  seed = seed_grid,
  KEEP.OUT.ATTRS = FALSE
)
reps <- nrow(grid)

result_path <- file.path(paths$generated_dir, paste0(run_id, "-results.csv"))
summary_path <- file.path(paths$generated_dir, paste0(run_id, "-summary.csv"))
cell_stability_path <- file.path(paths$generated_dir, paste0(run_id, "-cell-stability.csv"))
seed_summary_path <- file.path(paths$generated_dir, paste0(run_id, "-seed-summary.csv"))
started_at <- Sys.time()

if (isTRUE(resume) && !isTRUE(overwrite) && validation_existing_run_complete(result_path, summary_path)) {
  summary <- utils::read.csv(summary_path, stringsAsFactors = FALSE)
  status <- if (nrow(summary) == 1L && isTRUE(summary$acceptance_pass[[1L]])) "pass" else "fail"
  ended_at <- Sys.time()
  validation_record_manifest(paths, run_id, experiment_id, mode, status, started_at, ended_at, seed_root, reps, script_path, result_path, summary_path, "Existing V02 output reused.")
  message("Resumed existing V02 output: ", result_path)
  validation_maybe_stop_for_blocker(status, "V02 validation failed in resumed output.")
  quit(status = 0)
}

plain_jebs_frame <- function(x) {
  data.frame(
    site_index = as.integer(x$site_index),
    z_j = as.numeric(x$z_j),
    tau_j = as.numeric(x$tau_j),
    tau_j_hat = as.numeric(x$tau_j_hat),
    se_j = as.numeric(x$se_j),
    se2_j = as.numeric(x$se2_j),
    n_j = as.integer(x$n_j)
  )
}

golden_hash <- function(seed) {
  path <- file.path(paths$package_root, "tests", "testthat", "_snaps", "golden", sprintf("jebs_appendix_mixture_seed%d.rds", seed))
  multisiteDGP::canonical_hash(readRDS(path))
}

run_one <- function(row_id) {
  r <- grid[row_id, ]
  tryCatch({
    design <- multisiteDGP::preset_jebs_strict(
      J = r$J,
      nj_mean = r$nj_mean,
      cv = r$cv,
      sigma_tau = r$sigma_tau
    )
    dat <- multisiteDGP::sim_multisite(design, seed = r$seed)
    diagnostics <- attr(dat, "diagnostics", exact = TRUE)
    provenance <- attr(dat, "provenance", exact = TRUE)
    is_anchor <- r$J == 100L && r$nj_mean == 80 && r$cv == 0.50 && r$sigma_tau == 0.15
    actual_plain_hash <- if (is_anchor) multisiteDGP::canonical_hash(plain_jebs_frame(dat)) else NA_character_
    expected_hash <- if (is_anchor) golden_hash(r$seed) else NA_character_
    data.frame(
      result_schema_version = "phase9-validation-v1",
      run_id = run_id,
      experiment_id = experiment_id,
      mode = mode,
      cell_id = sprintf("J%s_n%s_cv%s_sigma%s", r$J, r$nj_mean, r$cv, r$sigma_tau),
      row_id = row_id,
      seed = r$seed,
      status = "completed",
      skip_reason = NA_character_,
      J = r$J,
      nj_mean = r$nj_mean,
      cv = r$cv,
      sigma_tau = r$sigma_tau,
      estimate_I = diagnostics$I_hat,
      estimate_R = diagnostics$R_hat,
      anchor_hash_check = if (is_anchor) identical(actual_plain_hash, expected_hash) else NA,
      actual_plain_hash = actual_plain_hash,
      expected_golden_hash = expected_hash,
      design_hash = provenance$design_hash,
      canonical_hash = provenance$canonical_hash,
      provenance_string = multisiteDGP::provenance_string(dat),
      error_class = NA_character_,
      error_message = NA_character_,
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    data.frame(
      result_schema_version = "phase9-validation-v1",
      run_id = run_id,
      experiment_id = experiment_id,
      mode = mode,
      cell_id = sprintf("J%s_n%s_cv%s_sigma%s", r$J, r$nj_mean, r$cv, r$sigma_tau),
      row_id = row_id,
      seed = r$seed,
      status = "failed",
      skip_reason = NA_character_,
      J = r$J,
      nj_mean = r$nj_mean,
      cv = r$cv,
      sigma_tau = r$sigma_tau,
      estimate_I = NA_real_,
      estimate_R = NA_real_,
      anchor_hash_check = NA,
      actual_plain_hash = NA_character_,
      expected_golden_hash = NA_character_,
      design_hash = NA_character_,
      canonical_hash = NA_character_,
      provenance_string = NA_character_,
      error_class = paste(class(e), collapse = "/"),
      error_message = conditionMessage(e),
      stringsAsFactors = FALSE
    )
  })
}

rows <- vector("list", nrow(grid))
for (row_id in seq_len(nrow(grid))) {
  if (row_id %% 100L == 0L || row_id == 1L) {
    message("V02 row ", row_id, " / ", nrow(grid))
  }
  rows[[row_id]] <- run_one(row_id)
}
results <- do.call(rbind, rows)
result_path <- validation_write_csv(results, result_path)

completed <- results[results$status == "completed", , drop = FALSE]
cell_means <- stats::aggregate(estimate_I ~ cell_id + J + nj_mean + cv + sigma_tau, data = completed, FUN = mean)
cell_ranges <- stats::aggregate(estimate_I ~ cell_id, data = completed, FUN = function(x) max(x) - min(x))
names(cell_ranges)[names(cell_ranges) == "estimate_I"] <- "I_seed_range"
cell_stability <- merge(cell_means, cell_ranges, by = "cell_id")
names(cell_stability)[names(cell_stability) == "estimate_I"] <- "mean_I"
cell_stability$legacy_cell_range_gate <- cell_stability$I_seed_range <= 0.02
cell_stability_path <- validation_write_csv(cell_stability, cell_stability_path)

seed_summary <- do.call(rbind, lapply(split(completed, completed$seed), function(x) {
  data.frame(
    seed = x$seed[[1L]],
    n = nrow(x),
    min_I = min(x$estimate_I),
    max_I = max(x$estimate_I),
    mean_I = mean(x$estimate_I),
    median_I = stats::median(x$estimate_I),
    sd_I = stats::sd(x$estimate_I),
    stringsAsFactors = FALSE
  )
}))
seed_summary <- seed_summary[match(seed_grid, seed_summary$seed), , drop = FALSE]
seed_summary_path <- validation_write_csv(seed_summary, seed_summary_path)

seed_grid_mean_range <- max(seed_summary$mean_I) - min(seed_summary$mean_I)
seed_grid_mean_stability_gate <- seed_grid_mean_range <= 0.02
seed_grid_min_I_lower_gate <- all(seed_summary$min_I >= 0.004 & seed_summary$min_I <= 0.01)
seed_grid_max_I_upper_gate <- all(seed_summary$max_I >= 0.70 & seed_summary$max_I <= 0.75)
cell_seed_range_q95 <- unname(stats::quantile(cell_stability$I_seed_range, probs = 0.95))
legacy_cell_range_gate <- all(cell_stability$I_seed_range <= 0.02)

anchor_rows <- completed[!is.na(completed$anchor_hash_check), , drop = FALSE]
summary <- data.frame(
  run_id = run_id,
  experiment_id = experiment_id,
  mode = mode,
  rows = nrow(results),
  completed = nrow(completed),
  failed = sum(results$status == "failed"),
  min_I = min(completed$estimate_I),
  max_I = max(completed$estimate_I),
  min_I_lower_gate = min(completed$estimate_I) >= 0.004 && min(completed$estimate_I) <= 0.01,
  max_I_upper_gate = max(completed$estimate_I) >= 0.70 && max(completed$estimate_I) <= 0.75,
  max_seed_range = max(cell_stability$I_seed_range),
  cell_seed_range_q95 = cell_seed_range_q95,
  cell_seed_range_over_0_02 = mean(cell_stability$I_seed_range > 0.02),
  legacy_cell_range_gate = legacy_cell_range_gate,
  seed_grid_mean_range = seed_grid_mean_range,
  seed_grid_mean_stability_gate = seed_grid_mean_stability_gate,
  seed_grid_min_I_lower_gate = seed_grid_min_I_lower_gate,
  seed_grid_max_I_upper_gate = seed_grid_max_I_upper_gate,
  seed_stability_gate = seed_grid_mean_stability_gate,
  anchor_hash_pass = nrow(anchor_rows) == length(seed_grid) && all(anchor_rows$anchor_hash_check),
  acceptance_pass = nrow(completed) == nrow(results) &&
    min(completed$estimate_I) >= 0.004 &&
    min(completed$estimate_I) <= 0.01 &&
    max(completed$estimate_I) >= 0.70 &&
    max(completed$estimate_I) <= 0.75 &&
    seed_grid_mean_stability_gate &&
    seed_grid_min_I_lower_gate &&
    seed_grid_max_I_upper_gate &&
    nrow(anchor_rows) == length(seed_grid) &&
    all(anchor_rows$anchor_hash_check),
  acceptance_note = "Uses Gate D C16 lower-tail envelope [0.004, 0.01], ch19 upper-tail [0.70, 0.75], T1a anchor hashes, and grid-level 4-seed mean-I stability <= 0.02; per-cell seed ranges are reporting-only finite-sample diagnostics.",
  stringsAsFactors = FALSE
)
summary_path <- validation_write_csv(summary, summary_path)

status <- if (isTRUE(summary$acceptance_pass)) "pass" else "fail"
ended_at <- Sys.time()
validation_record_manifest(paths, run_id, experiment_id, mode, status, started_at, ended_at, seed_root, reps, script_path, result_path, summary_path, "V02 strict JEBS grid and T1a anchor evidence.")
print(summary)
message("V02 status: ", status)
validation_maybe_stop_for_blocker(status, "V02 validation failed acceptance criteria.")
