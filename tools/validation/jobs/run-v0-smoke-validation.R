#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg) == 1L) {
  normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE)
} else {
  normalizePath("tools/validation/jobs/run-v0-smoke-validation.R", mustWork = TRUE)
}
harness_path <- file.path(dirname(script_path), "..", "R", "validation-harness.R")
source(harness_path)

paths <- validation_paths(script_path)
validation_load_package(paths$package_root)

experiment_id <- "V0"
mode <- Sys.getenv("MULTISITEDGP_VALIDATION_MODE", unset = "smoke")
reps <- validation_env_int("MULTISITEDGP_VALIDATION_REPS", 3L)
seed_root <- validation_env_int("MULTISITEDGP_VALIDATION_SEED_ROOT", 910001L)
resume <- validation_env_flag("MULTISITEDGP_VALIDATION_RESUME", default = TRUE)
overwrite <- validation_env_flag("MULTISITEDGP_VALIDATION_OVERWRITE", default = FALSE)
run_id <- validation_run_id(experiment_id, mode)

result_path <- file.path(paths$generated_dir, paste0(run_id, "-results.csv"))
summary_path <- file.path(paths$generated_dir, paste0(run_id, "-summary.csv"))
started_at <- Sys.time()

if (isTRUE(resume) && !isTRUE(overwrite) && validation_existing_run_complete(result_path, summary_path)) {
  existing_summary <- utils::read.csv(summary_path, stringsAsFactors = FALSE)
  resumed_status <- if (nrow(existing_summary) == 1L && isTRUE(existing_summary$smoke_pass[[1L]])) {
    "pass"
  } else {
    "fail"
  }
  ended_at <- Sys.time()
  validation_record_manifest(
    paths = paths,
    run_id = run_id,
    experiment_id = experiment_id,
    mode = mode,
    status = resumed_status,
    started_at = started_at,
    ended_at = ended_at,
    seed_root = seed_root,
    reps = reps,
    script_path = script_path,
    result_path = result_path,
    summary_path = summary_path,
    notes = "Existing result and summary files reused because resume=true and overwrite=false."
  )
  message("Resumed existing validation output: ", result_path)
  quit(status = 0)
}

seeds <- validation_seed_stream(reps, seed_root)
rows <- lapply(seq_along(seeds), function(rep_id) {
  seed <- seeds[[rep_id]]
  tryCatch({
    design <- multisiteDGP::preset_education_modest()
    dat <- multisiteDGP::sim_multisite(design, seed = seed)
    diagnostics <- attr(dat, "diagnostics", exact = TRUE)
    provenance <- attr(dat, "provenance", exact = TRUE)
    data.frame(
      result_schema_version = "phase9-validation-v1",
      run_id = run_id,
      experiment_id = experiment_id,
      mode = mode,
      cell_id = "preset_education_modest",
      rep_id = rep_id,
      seed = seed,
      status = "completed",
      skip_reason = NA_character_,
      target_I = 0.30,
      estimate_I = diagnostics$I_hat,
      tolerance_I = NA_real_,
      target_R = 5.0,
      estimate_R = diagnostics$R_hat,
      tolerance_R = NA_real_,
      target_sigma_tau = 0.20,
      estimate_sigma_tau_resid = diagnostics$sigma_tau_resid,
      estimate_sigma_tau_marg = diagnostics$sigma_tau_marg,
      target_GM_se2 = 0.091,
      estimate_GM_se2 = diagnostics$GM_se2,
      true_dist = diagnostics$true_dist,
      J = diagnostics$J,
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
      cell_id = "preset_education_modest",
      rep_id = rep_id,
      seed = seed,
      status = "failed",
      skip_reason = NA_character_,
      target_I = 0.30,
      estimate_I = NA_real_,
      tolerance_I = NA_real_,
      target_R = 5.0,
      estimate_R = NA_real_,
      tolerance_R = NA_real_,
      target_sigma_tau = 0.20,
      estimate_sigma_tau_resid = NA_real_,
      estimate_sigma_tau_marg = NA_real_,
      target_GM_se2 = 0.091,
      estimate_GM_se2 = NA_real_,
      true_dist = NA_character_,
      J = NA_integer_,
      design_hash = NA_character_,
      canonical_hash = NA_character_,
      provenance_string = NA_character_,
      error_class = paste(class(e), collapse = "/"),
      error_message = conditionMessage(e),
      stringsAsFactors = FALSE
    )
  })
})

results <- do.call(rbind, rows)
required <- c(
  "result_schema_version", "run_id", "experiment_id", "cell_id", "rep_id",
  "seed", "status", "estimate_I", "estimate_R", "estimate_sigma_tau_resid",
  "estimate_GM_se2", "design_hash", "canonical_hash", "provenance_string"
)
validation_stop_if_missing_columns(results, required, "V0 smoke results")

summary <- data.frame(
  run_id = run_id,
  experiment_id = experiment_id,
  mode = mode,
  reps = nrow(results),
  completed = sum(results$status == "completed"),
  failed = sum(results$status == "failed"),
  all_finite_diagnostics = all(is.finite(results$estimate_I[results$status == "completed"])) &&
    all(is.finite(results$estimate_R[results$status == "completed"])) &&
    all(is.finite(results$estimate_sigma_tau_resid[results$status == "completed"])) &&
    all(is.finite(results$estimate_GM_se2[results$status == "completed"])),
  all_hashes_present = all(nzchar(results$canonical_hash)),
  all_design_hashes_present = all(nzchar(results$design_hash)),
  mean_I_hat = mean(results$estimate_I, na.rm = TRUE),
  mean_R_hat = mean(results$estimate_R, na.rm = TRUE),
  mean_sigma_tau_resid = mean(results$estimate_sigma_tau_resid, na.rm = TRUE),
  mean_GM_se2 = mean(results$estimate_GM_se2, na.rm = TRUE),
  stringsAsFactors = FALSE
)
summary$smoke_pass <- summary$completed == reps &&
  isTRUE(summary$all_finite_diagnostics) &&
  isTRUE(summary$all_hashes_present) &&
  isTRUE(summary$all_design_hashes_present)

result_path <- validation_write_csv(results, result_path)
summary_path <- validation_write_csv(summary, summary_path)
ended_at <- Sys.time()

validation_record_manifest(
  paths = paths,
  run_id = run_id,
  experiment_id = experiment_id,
  mode = mode,
  status = if (isTRUE(summary$smoke_pass)) "pass" else "fail",
  started_at = started_at,
  ended_at = ended_at,
  seed_root = seed_root,
  reps = reps,
  script_path = script_path,
  result_path = result_path,
  summary_path = summary_path,
  notes = "Step 9.1 tiny harness smoke job. This checks plumbing only, not full V0 acceptance."
)

print(results)
print(summary)
message("Wrote results to: ", result_path)
message("Wrote summary to: ", summary_path)
message("Wrote manifest to: ", paths$manifest_path)

if (!isTRUE(summary$smoke_pass)) {
  stop("V0 smoke validation failed harness-level checks.", call. = FALSE)
}
