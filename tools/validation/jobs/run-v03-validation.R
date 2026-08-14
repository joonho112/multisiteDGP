#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg) == 1L) {
  normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE)
} else {
  normalizePath("tools/validation/jobs/run-v03-validation.R", mustWork = TRUE)
}
source(file.path(dirname(script_path), "..", "R", "validation-harness.R"))

paths <- validation_paths(script_path)
validation_load_package(paths$package_root)

experiment_id <- "V03"
mode <- Sys.getenv("MULTISITEDGP_VALIDATION_MODE", unset = "full")
reps <- validation_env_int("MULTISITEDGP_VALIDATION_REPS", if (identical(mode, "full")) 200L else 10L)
seed_root <- validation_env_int("MULTISITEDGP_VALIDATION_SEED_ROOT", 910301L)
resume <- validation_env_flag("MULTISITEDGP_VALIDATION_RESUME", default = TRUE)
overwrite <- validation_env_flag("MULTISITEDGP_VALIDATION_OVERWRITE", default = FALSE)
run_id <- validation_run_id(experiment_id, mode)

result_path <- file.path(paths$generated_dir, paste0(run_id, "-results.csv"))
summary_path <- file.path(paths$generated_dir, paste0(run_id, "-summary.csv"))
started_at <- Sys.time()

if (isTRUE(resume) && !isTRUE(overwrite) && validation_existing_run_complete(result_path, summary_path)) {
  summary <- utils::read.csv(summary_path, stringsAsFactors = FALSE)
  status <- if (nrow(summary) == 1L && isTRUE(summary$acceptance_pass[[1L]])) "pass" else "fail"
  ended_at <- Sys.time()
  validation_record_manifest(paths, run_id, experiment_id, mode, status, started_at, ended_at, seed_root, reps, script_path, result_path, summary_path, "Existing V03 output reused.")
  message("Resumed existing V03 output: ", result_path)
  validation_maybe_stop_for_blocker(status, "V03 validation failed in resumed output.")
  quit(status = 0)
}

seeds <- validation_seed_stream(reps, seed_root)

run_one <- function(rep_id, seed) {
  tryCatch({
    design <- multisiteDGP::preset_walters_2024()
    dat <- multisiteDGP::sim_multisite(design, seed = seed)
    diagnostics <- attr(dat, "diagnostics", exact = TRUE)
    provenance <- attr(dat, "provenance", exact = TRUE)
    realized_i <- multisiteDGP::informativeness(dat)
    mean_s <- multisiteDGP::mean_shrinkage(dat)
    shrinkage <- multisiteDGP::compute_shrinkage(dat$se2_j, sigma_tau = design$sigma_tau)
    expected_mse_ratio <- mean(shrinkage * dat$se2_j) / mean(dat$se2_j)
    expected_rmse_ratio <- sqrt(expected_mse_ratio)
    ml <- dat$tau_j_hat
    pm <- design$tau + shrinkage * (dat$tau_j_hat - design$tau)
    mse_ml <- mean((ml - dat$tau_j)^2)
    mse_pm <- mean((pm - dat$tau_j)^2)
    rmse_ml <- sqrt(mse_ml)
    rmse_pm <- sqrt(mse_pm)
    rmse_ratio <- rmse_pm / rmse_ml
    mse_ratio <- mse_pm / mse_ml
    data.frame(
      result_schema_version = "phase9-validation-v1",
      run_id = run_id,
      experiment_id = experiment_id,
      mode = mode,
      cell_id = "preset_walters_2024",
      rep_id = rep_id,
      seed = seed,
      status = "completed",
      target_I_lower = 0.75,
      target_I_upper = 0.85,
      estimate_I = diagnostics$I_hat,
      informativeness = realized_i,
      I_pass = diagnostics$I_hat >= 0.75 && diagnostics$I_hat <= 0.85,
      mean_shrinkage = mean_s,
      expected_mse_ratio = expected_mse_ratio,
      expected_mse_reduction = 1 - expected_mse_ratio,
      target_mse_reduction = 0.36,
      mse_reduction = 1 - mse_ratio,
      target_mse_ratio = 0.64,
      mse_ratio = mse_ratio,
      mse_ratio_deviation = mse_ratio - expected_mse_ratio,
      mse_ratio_pass = abs(mse_ratio - 0.64) <= 0.05,
      target_rmse_ratio_literal = 0.64,
      expected_rmse_ratio = expected_rmse_ratio,
      rmse_ratio = rmse_ratio,
      rmse_ratio_deviation = rmse_ratio - expected_rmse_ratio,
      rmse_ratio_literal_pass = abs(rmse_ratio - 0.64) <= 0.05,
      target_rmse_ratio_corrected = sqrt(0.64),
      rmse_ratio_corrected_pass = abs(rmse_ratio - sqrt(0.64)) <= 0.05,
      pm_improves_mse = mse_pm < mse_ml,
      pm_improves_rmse = rmse_pm < rmse_ml,
      rmse_ml = rmse_ml,
      rmse_pm = rmse_pm,
      mse_ml = mse_ml,
      mse_pm = mse_pm,
      J = design$J,
      sigma_tau = design$sigma_tau,
      nj_mean = design$nj_mean,
      cv = design$cv,
      R2 = design$R2,
      engine = design$engine,
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
      cell_id = "preset_walters_2024",
      rep_id = rep_id,
      seed = seed,
      status = "failed",
      target_I_lower = 0.75,
      target_I_upper = 0.85,
      estimate_I = NA_real_,
      informativeness = NA_real_,
      I_pass = FALSE,
      mean_shrinkage = NA_real_,
      expected_mse_ratio = NA_real_,
      expected_mse_reduction = NA_real_,
      target_mse_reduction = 0.36,
      mse_reduction = NA_real_,
      target_mse_ratio = 0.64,
      mse_ratio = NA_real_,
      mse_ratio_deviation = NA_real_,
      mse_ratio_pass = FALSE,
      target_rmse_ratio_literal = 0.64,
      expected_rmse_ratio = NA_real_,
      rmse_ratio = NA_real_,
      rmse_ratio_deviation = NA_real_,
      rmse_ratio_literal_pass = FALSE,
      target_rmse_ratio_corrected = sqrt(0.64),
      rmse_ratio_corrected_pass = FALSE,
      pm_improves_mse = FALSE,
      pm_improves_rmse = FALSE,
      rmse_ml = NA_real_,
      rmse_pm = NA_real_,
      mse_ml = NA_real_,
      mse_pm = NA_real_,
      J = NA_integer_,
      sigma_tau = NA_real_,
      nj_mean = NA_real_,
      cv = NA_real_,
      R2 = NA_real_,
      engine = NA_character_,
      design_hash = NA_character_,
      canonical_hash = NA_character_,
      provenance_string = NA_character_,
      error_class = paste(class(e), collapse = "/"),
      error_message = conditionMessage(e),
      stringsAsFactors = FALSE
    )
  })
}

rows <- vector("list", reps)
for (rep_id in seq_len(reps)) {
  if (rep_id %% 25L == 0L || rep_id == 1L || rep_id == reps) {
    message("V03 replication ", rep_id, " / ", reps)
  }
  rows[[rep_id]] <- run_one(rep_id, seeds[[rep_id]])
}
results <- do.call(rbind, rows)
result_path <- validation_write_csv(results, result_path)

completed <- results[results$status == "completed", , drop = FALSE]
mse_consistency_tolerance <- max(0.03, 2 * stats::sd(completed$mse_ratio) / sqrt(nrow(completed)))
rmse_consistency_tolerance <- max(0.03, 2 * stats::sd(completed$rmse_ratio) / sqrt(nrow(completed)))
summary <- data.frame(
  run_id = run_id,
  experiment_id = experiment_id,
  mode = mode,
  reps = nrow(results),
  completed = nrow(completed),
  failed = sum(results$status == "failed"),
  I_pass_rate = mean(completed$I_pass),
  mean_I = mean(completed$estimate_I),
  sd_I = stats::sd(completed$estimate_I),
  mean_informativeness = mean(completed$informativeness),
  mean_shrinkage = mean(completed$mean_shrinkage),
  mean_expected_mse_ratio = mean(completed$expected_mse_ratio),
  mean_expected_mse_reduction = mean(completed$expected_mse_reduction),
  mean_rmse_ratio = mean(completed$rmse_ratio),
  sd_rmse_ratio = stats::sd(completed$rmse_ratio),
  mean_expected_rmse_ratio = mean(completed$expected_rmse_ratio),
  mean_rmse_ratio_deviation = mean(completed$rmse_ratio_deviation),
  rmse_ratio_literal_pass_rate = mean(completed$rmse_ratio_literal_pass),
  rmse_ratio_corrected_pass_rate = mean(completed$rmse_ratio_corrected_pass),
  mean_mse_ratio = mean(completed$mse_ratio),
  mean_mse_reduction = mean(completed$mse_reduction),
  mean_mse_ratio_deviation = mean(completed$mse_ratio_deviation),
  pm_mse_improvement_rate = mean(completed$pm_improves_mse),
  pm_rmse_improvement_rate = mean(completed$pm_improves_rmse),
  mse_ratio_pass_rate = mean(completed$mse_ratio_pass),
  mse_ratio_consistency_tolerance = mse_consistency_tolerance,
  rmse_ratio_consistency_tolerance = rmse_consistency_tolerance,
  mse_ratio_consistency_pass = abs(mean(completed$mse_ratio_deviation)) <= mse_consistency_tolerance,
  rmse_ratio_consistency_pass = abs(mean(completed$rmse_ratio_deviation)) <= rmse_consistency_tolerance,
  acceptance_pass = nrow(completed) == reps &&
    mean(completed$I_pass) >= 0.95 &&
    abs(mean(completed$mse_ratio_deviation)) <= mse_consistency_tolerance &&
    abs(mean(completed$rmse_ratio_deviation)) <= rmse_consistency_tolerance &&
    mean(completed$pm_improves_mse) >= 0.90,
  acceptance_note = "V03 gates Walters preset I calibration and oracle EB shrinkage consistency against the normal-normal shrinkage-implied MSE/RMSE ratios using max(0.03, 2*MCSE) aggregate tolerances; literal 0.64 RMSE gate is retained only as historical evidence.",
  stringsAsFactors = FALSE
)
summary_path <- validation_write_csv(summary, summary_path)

status <- if (isTRUE(summary$acceptance_pass)) "pass" else "fail"
ended_at <- Sys.time()
validation_record_manifest(paths, run_id, experiment_id, mode, status, started_at, ended_at, seed_root, reps, script_path, result_path, summary_path, "V03 Walters synthetic calibration with shrinkage-implied oracle EB MSE/RMSE consistency gate; literal 0.64 RMSE gate retained as historical evidence only.")
print(summary)
message("V03 status: ", status)
validation_maybe_stop_for_blocker(status, "V03 validation failed acceptance criteria.")
