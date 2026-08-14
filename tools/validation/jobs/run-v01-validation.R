#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg) == 1L) {
  normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE)
} else {
  normalizePath("tools/validation/jobs/run-v01-validation.R", mustWork = TRUE)
}
source(file.path(dirname(script_path), "..", "R", "validation-harness.R"))

paths <- validation_paths(script_path)
validation_load_package(paths$package_root)

experiment_id <- "V01"
mode <- Sys.getenv("MULTISITEDGP_VALIDATION_MODE", unset = "full")
reps <- validation_env_int("MULTISITEDGP_VALIDATION_REPS", if (identical(mode, "full")) 1000L else 10L)
seed_root <- validation_env_int("MULTISITEDGP_VALIDATION_SEED_ROOT", 910101L)
resume <- validation_env_flag("MULTISITEDGP_VALIDATION_RESUME", default = TRUE)
overwrite <- validation_env_flag("MULTISITEDGP_VALIDATION_OVERWRITE", default = FALSE)
run_id <- validation_run_id(experiment_id, mode)

result_path <- file.path(paths$generated_dir, paste0(run_id, "-results.csv"))
summary_path <- file.path(paths$generated_dir, paste0(run_id, "-summary.csv"))
started_at <- Sys.time()

if (isTRUE(resume) && !isTRUE(overwrite) && validation_existing_run_complete(result_path, summary_path)) {
  summary <- utils::read.csv(summary_path, stringsAsFactors = FALSE)
  active_summary <- summary[summary$shape != "DPM", , drop = FALSE]
  status <- if (nrow(active_summary) > 0L && isTRUE(all(active_summary$acceptance_pass))) "pass" else "fail"
  ended_at <- Sys.time()
  validation_record_manifest(paths, run_id, experiment_id, mode, status, started_at, ended_at, seed_root, reps, script_path, result_path, summary_path, "Existing V01 output reused.")
  message("Resumed existing V01 output: ", result_path)
  validation_maybe_stop_for_blocker(status, "V01 validation failed in resumed output.")
  quit(status = 0)
}

shape_specs <- validation_shape_specs(dpm = "skip")
active_shapes <- names(shape_specs)[names(shape_specs) != "DPM"]
seeds <- validation_seed_stream(length(active_shapes) * reps, seed_root)
rank_corr_target <- -0.30
n_sites <- 200L
data_x <- tibble::tibble(x = seq(-1, 1, length.out = n_sites))

run_one <- function(shape, spec, rep_id, seed) {
  tryCatch({
    design <- multisiteDGP::multisitedgp_design(
      J = n_sites,
      true_dist = spec$true_dist,
      theta_G = spec$theta_G,
      sigma_tau = 0.20,
      formula = ~ x,
      beta = 1.5,
      data = data_x,
      g_fn = spec$g_fn,
      g_returns = spec$g_returns,
      nj_mean = 80,
      cv = 0.50,
      nj_min = 5L,
      engine = "A2_modern",
      dependence = "rank",
      rank_corr = rank_corr_target,
      tol = if (identical(shape, "PointMassSlab")) 0.05 else 0.02
    )
    dat <- multisiteDGP::sim_multisite(design, seed = seed)
    diagnostics <- attr(dat, "diagnostics", exact = TRUE)
    provenance <- attr(dat, "provenance", exact = TRUE)
    dep_diag <- diagnostics$dependence_diagnostics
    tolerance <- if (identical(shape, "PointMassSlab")) 0.05 else 0.02
    data.frame(
      result_schema_version = "phase9-validation-v1",
      run_id = run_id,
      experiment_id = experiment_id,
      mode = mode,
      cell_id = shape,
      rep_id = rep_id,
      seed = seed,
      status = "completed",
      skip_reason = NA_character_,
      target_residual_spearman = rank_corr_target,
      estimate_residual_spearman = diagnostics$rho_S_residual,
      residual_tolerance = tolerance,
      residual_pass = abs(diagnostics$rho_S_residual - rank_corr_target) <= tolerance,
      marginal_target_status = "reporting_only_derived",
      estimate_marginal_spearman = diagnostics$rho_S_marginal,
      estimate_residual_pearson = diagnostics$rho_P_residual,
      estimate_marginal_pearson = diagnostics$rho_P_marginal,
      marginal_finite = is.finite(diagnostics$rho_S_marginal),
      two_number_recorded = is.finite(diagnostics$rho_S_residual) && is.finite(diagnostics$rho_S_marginal),
      two_number_distinct = is.finite(diagnostics$rho_S_residual) &&
        is.finite(diagnostics$rho_S_marginal) &&
        abs(diagnostics$rho_S_residual - diagnostics$rho_S_marginal) >= 0.05,
      diagnostics_target_type = dep_diag$target_type,
      diagnostics_converged = isTRUE(dep_diag$converged),
      diagnostics_iterations = dep_diag$iterations,
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
      cell_id = shape,
      rep_id = rep_id,
      seed = seed,
      status = "failed",
      skip_reason = NA_character_,
      target_residual_spearman = rank_corr_target,
      estimate_residual_spearman = NA_real_,
      residual_tolerance = if (identical(shape, "PointMassSlab")) 0.05 else 0.02,
      residual_pass = FALSE,
      marginal_target_status = "reporting_only_derived",
      estimate_marginal_spearman = NA_real_,
      estimate_residual_pearson = NA_real_,
      estimate_marginal_pearson = NA_real_,
      marginal_finite = FALSE,
      two_number_recorded = FALSE,
      two_number_distinct = FALSE,
      diagnostics_target_type = NA_character_,
      diagnostics_converged = FALSE,
      diagnostics_iterations = NA_integer_,
      true_dist = spec$true_dist,
      J = NA_integer_,
      design_hash = NA_character_,
      canonical_hash = NA_character_,
      provenance_string = NA_character_,
      error_class = paste(class(e), collapse = "/"),
      error_message = conditionMessage(e),
      stringsAsFactors = FALSE
    )
  })
}

rows <- list()
idx <- 0L
for (shape in active_shapes) {
  message("V01 shape: ", shape)
  for (rep_id in seq_len(reps)) {
    idx <- idx + 1L
    rows[[idx]] <- run_one(shape, shape_specs[[shape]], rep_id, seeds[[idx]])
  }
}
rows[[length(rows) + 1L]] <- data.frame(
  result_schema_version = "phase9-validation-v1",
  run_id = run_id,
  experiment_id = experiment_id,
  mode = mode,
  cell_id = "DPM",
  rep_id = NA_integer_,
  seed = NA_integer_,
  status = "skipped",
  skip_reason = shape_specs$DPM$skip_reason,
  target_residual_spearman = rank_corr_target,
  estimate_residual_spearman = NA_real_,
  residual_tolerance = NA_real_,
  residual_pass = NA,
  marginal_target_status = "skipped_v1_stub",
  estimate_marginal_spearman = NA_real_,
  estimate_residual_pearson = NA_real_,
  estimate_marginal_pearson = NA_real_,
  marginal_finite = NA,
  two_number_recorded = NA,
  two_number_distinct = NA,
  diagnostics_target_type = "skipped",
  diagnostics_converged = NA,
  diagnostics_iterations = NA_integer_,
  true_dist = "DPM",
  J = n_sites,
  design_hash = NA_character_,
  canonical_hash = NA_character_,
  provenance_string = NA_character_,
  error_class = NA_character_,
  error_message = NA_character_,
  stringsAsFactors = FALSE
)

results <- do.call(rbind, rows)
result_path <- validation_write_csv(results, result_path)

completed_by_shape <- split(results[results$status == "completed", , drop = FALSE], results$cell_id[results$status == "completed"])
summary <- do.call(rbind, lapply(completed_by_shape, function(x) {
  data.frame(
    run_id = run_id,
    experiment_id = experiment_id,
    mode = mode,
    shape = x$cell_id[[1L]],
    reps = nrow(x),
    completed = sum(x$status == "completed"),
    failed = sum(x$status == "failed"),
    residual_pass_rate = mean(x$residual_pass),
    two_number_recorded_rate = mean(x$two_number_recorded),
    two_number_distinct_rate = mean(x$two_number_distinct),
    marginal_reporting_rate = mean(x$marginal_finite),
    target_type_rate = mean(x$diagnostics_target_type == "residual_spearman"),
    convergence_rate = mean(x$diagnostics_converged),
    mean_residual_spearman = mean(x$estimate_residual_spearman),
    mean_marginal_spearman = mean(x$estimate_marginal_spearman),
    marginal_target_status = "reporting_only_derived",
    acceptance_pass = nrow(x) == reps &&
      mean(x$residual_pass) >= 0.95 &&
      mean(x$two_number_recorded) >= 0.95 &&
      mean(x$marginal_finite) >= 0.95 &&
      mean(x$diagnostics_target_type == "residual_spearman") >= 0.95 &&
      mean(x$diagnostics_converged) >= 0.95,
    acceptance_note = "Marginal Spearman is a finite derived reporting diagnostic under the ch08 residual-only v1 contract; no numeric marginal target gate is applied.",
    stringsAsFactors = FALSE
  )
}))
dpm_summary <- data.frame(
  run_id = run_id,
  experiment_id = experiment_id,
  mode = mode,
  shape = "DPM",
  reps = 0L,
  completed = 0L,
  failed = 0L,
  residual_pass_rate = NA_real_,
  two_number_recorded_rate = NA_real_,
  two_number_distinct_rate = NA_real_,
  marginal_reporting_rate = NA_real_,
  target_type_rate = NA_real_,
  convergence_rate = NA_real_,
  mean_residual_spearman = NA_real_,
  mean_marginal_spearman = NA_real_,
  marginal_target_status = "skipped_v1_stub",
  acceptance_pass = NA,
  acceptance_note = shape_specs$DPM$skip_reason,
  stringsAsFactors = FALSE
)
summary <- rbind(summary, dpm_summary)
summary_path <- validation_write_csv(summary, summary_path)

active_summary <- summary[summary$shape != "DPM", , drop = FALSE]
status <- if (nrow(active_summary) > 0L && isTRUE(all(active_summary$acceptance_pass))) "pass" else "fail"
ended_at <- Sys.time()
validation_record_manifest(paths, run_id, experiment_id, mode, status, started_at, ended_at, seed_root, reps, script_path, result_path, summary_path, "V01 residual evidence recorded; marginal Spearman is reporting-only derived evidence under the v1 residual contract.")
print(summary)
message("V01 status: ", status)
validation_maybe_stop_for_blocker(status, "V01 validation failed acceptance criteria.")
