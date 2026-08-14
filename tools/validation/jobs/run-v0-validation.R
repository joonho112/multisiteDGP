#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg) == 1L) {
  normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE)
} else {
  normalizePath("tools/validation/jobs/run-v0-validation.R", mustWork = TRUE)
}
source(file.path(dirname(script_path), "..", "R", "validation-harness.R"))

paths <- validation_paths(script_path)
validation_load_package(paths$package_root)

experiment_id <- "V0"
mode <- Sys.getenv("MULTISITEDGP_VALIDATION_MODE", unset = "full")
reps <- validation_env_int("MULTISITEDGP_VALIDATION_REPS", if (identical(mode, "full")) 1000L else 10L)
seed_root <- validation_env_int("MULTISITEDGP_VALIDATION_SEED_ROOT", 910001L)
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
  validation_record_manifest(paths, run_id, experiment_id, mode, status, started_at, ended_at, seed_root, reps, script_path, result_path, summary_path, "Existing V0 output reused.")
  message("Resumed existing V0 output: ", result_path)
  validation_maybe_stop_for_blocker(status, "V0 validation failed in resumed output.")
  quit(status = 0)
}

shape_specs <- validation_shape_specs(dpm = "skip")
active_shapes <- names(shape_specs)[names(shape_specs) != "DPM"]
seeds <- validation_seed_stream(length(active_shapes) * reps, seed_root)

v0_expected_targets <- function(design) {
  kappa <- multisiteDGP::compute_kappa(
    p = design$p,
    R2 = design$R2,
    var_outcome = if (!is.null(design$variance)) design$variance else 1
  )
  target_gm_se2 <- kappa * exp(design$cv^2 / 2) / design$nj_mean
  target_i <- design$sigma_tau^2 / (design$sigma_tau^2 + target_gm_se2)
  log_cv <- sqrt(log1p(design$cv^2))
  target_r_trimmed <- exp(2 * stats::qnorm(0.95) * log_cv)
  list(
    I = target_i,
    I_display = 0.30,
    sigma_tau = design$sigma_tau,
    GM_se2 = target_gm_se2,
    R_trimmed = target_r_trimmed,
    R_display = 5.0
  )
}

base_design <- multisiteDGP::preset_education_modest()
v0_targets <- v0_expected_targets(base_design)

run_one <- function(shape, spec, rep_id, seed) {
  tryCatch({
    design <- multisiteDGP::update_multisitedgp_design(
      base_design,
      true_dist = spec$true_dist,
      theta_G = spec$theta_G,
      g_fn = spec$g_fn,
      g_returns = spec$g_returns
    )
    dat <- multisiteDGP::sim_multisite(design, seed = seed)
    diagnostics <- attr(dat, "diagnostics", exact = TRUE)
    provenance <- attr(dat, "provenance", exact = TRUE)
    sigma_hat <- diagnostics$sigma_tau_resid
    r_trimmed <- multisiteDGP::heterogeneity_ratio(dat, trimmed = TRUE)
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
      target_I = v0_targets$I,
      display_target_I = v0_targets$I_display,
      estimate_I = diagnostics$I_hat,
      I_distribution_lower = 0.25,
      I_distribution_upper = 0.35,
      I_distribution_pass = diagnostics$I_hat >= 0.25 && diagnostics$I_hat <= 0.35,
      legacy_I_pass = abs(diagnostics$I_hat - 0.30) < 0.02,
      target_R_trimmed = v0_targets$R_trimmed,
      display_target_R = v0_targets$R_display,
      estimate_R_trimmed = r_trimmed,
      R_trimmed_pass = abs(r_trimmed - v0_targets$R_trimmed) / v0_targets$R_trimmed < 0.20,
      estimate_R_untrimmed = diagnostics$R_hat,
      legacy_R_untrimmed_pass = abs(diagnostics$R_hat - 5.0) / 5.0 < 0.20,
      target_sigma_tau = v0_targets$sigma_tau,
      estimate_sigma_tau_resid = sigma_hat,
      legacy_sigma_tau_pass = abs(sigma_hat - v0_targets$sigma_tau) / v0_targets$sigma_tau < 0.05,
      target_GM_se2 = v0_targets$GM_se2,
      estimate_GM_se2 = diagnostics$GM_se2,
      legacy_GM_se2_pass = abs(diagnostics$GM_se2 - v0_targets$GM_se2) / v0_targets$GM_se2 < 0.05,
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
      target_I = v0_targets$I,
      display_target_I = v0_targets$I_display,
      estimate_I = NA_real_,
      I_distribution_lower = 0.25,
      I_distribution_upper = 0.35,
      I_distribution_pass = FALSE,
      legacy_I_pass = FALSE,
      target_R_trimmed = v0_targets$R_trimmed,
      display_target_R = v0_targets$R_display,
      estimate_R_trimmed = NA_real_,
      R_trimmed_pass = FALSE,
      estimate_R_untrimmed = NA_real_,
      legacy_R_untrimmed_pass = FALSE,
      target_sigma_tau = v0_targets$sigma_tau,
      estimate_sigma_tau_resid = NA_real_,
      legacy_sigma_tau_pass = FALSE,
      target_GM_se2 = v0_targets$GM_se2,
      estimate_GM_se2 = NA_real_,
      legacy_GM_se2_pass = FALSE,
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
  message("V0 shape: ", shape)
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
  target_I = NA_real_,
  display_target_I = 0.30,
  estimate_I = NA_real_,
  I_distribution_lower = 0.25,
  I_distribution_upper = 0.35,
  I_distribution_pass = NA,
  legacy_I_pass = NA,
  target_R_trimmed = NA_real_,
  display_target_R = 5.0,
  estimate_R_trimmed = NA_real_,
  R_trimmed_pass = NA,
  estimate_R_untrimmed = NA_real_,
  legacy_R_untrimmed_pass = NA,
  target_sigma_tau = 0.20,
  estimate_sigma_tau_resid = NA_real_,
  legacy_sigma_tau_pass = NA,
  target_GM_se2 = NA_real_,
  estimate_GM_se2 = NA_real_,
  legacy_GM_se2_pass = NA,
  true_dist = "DPM",
  J = 50L,
  design_hash = NA_character_,
  canonical_hash = NA_character_,
  provenance_string = NA_character_,
  error_class = NA_character_,
  error_message = NA_character_,
  stringsAsFactors = FALSE
)
results <- do.call(rbind, rows)
result_path <- validation_write_csv(results, result_path)

complete <- results[results$status == "completed", , drop = FALSE]
summary <- do.call(rbind, lapply(split(results[results$cell_id != "DPM", , drop = FALSE], results$cell_id[results$cell_id != "DPM"]), function(x) {
  completed <- x[x$status == "completed", , drop = FALSE]
  mean_I <- mean(completed$estimate_I)
  mean_sigma_tau <- mean(completed$estimate_sigma_tau_resid)
  mean_R_trimmed <- mean(completed$estimate_R_trimmed)
  mean_GM_se2 <- mean(completed$estimate_GM_se2)
  I_mean_tolerance <- max(0.01, 3 * stats::sd(completed$estimate_I) / sqrt(nrow(completed)))
  sigma_tau_mean_tolerance <- max(0.01, 3 * stats::sd(completed$estimate_sigma_tau_resid) / sqrt(nrow(completed)))
  R_trimmed_mean_rel_tolerance <- 0.20
  GM_se2_mean_rel_tolerance <- max(
    0.02,
    3 * stats::sd(completed$estimate_GM_se2) /
      sqrt(nrow(completed)) / completed$target_GM_se2[[1L]]
  )
  I_mean_pass <- abs(mean_I - completed$target_I[[1L]]) <= I_mean_tolerance
  sigma_tau_mean_pass <- abs(mean_sigma_tau - completed$target_sigma_tau[[1L]]) <= sigma_tau_mean_tolerance
  R_trimmed_mean_pass <- abs(mean_R_trimmed - completed$target_R_trimmed[[1L]]) / completed$target_R_trimmed[[1L]] <= R_trimmed_mean_rel_tolerance
  GM_se2_mean_pass <- abs(mean_GM_se2 - completed$target_GM_se2[[1L]]) / completed$target_GM_se2[[1L]] <= GM_se2_mean_rel_tolerance
  I_distribution_pass_rate <- mean(completed$I_distribution_pass)
  data.frame(
    run_id = run_id,
    experiment_id = experiment_id,
    mode = mode,
    shape = x$cell_id[[1L]],
    reps = nrow(x),
    completed = nrow(completed),
    failed = sum(x$status == "failed"),
    target_I = completed$target_I[[1L]],
    target_sigma_tau = completed$target_sigma_tau[[1L]],
    target_R_trimmed = completed$target_R_trimmed[[1L]],
    target_GM_se2 = completed$target_GM_se2[[1L]],
    I_distribution_pass_rate = I_distribution_pass_rate,
    legacy_I_pass_rate = mean(completed$legacy_I_pass),
    legacy_sigma_tau_pass_rate = mean(completed$legacy_sigma_tau_pass),
    legacy_R_untrimmed_pass_rate = mean(completed$legacy_R_untrimmed_pass),
    legacy_GM_se2_pass_rate = mean(completed$legacy_GM_se2_pass),
    mean_I = mean_I,
    sd_I = stats::sd(completed$estimate_I),
    I_mean_tolerance = I_mean_tolerance,
    I_mean_pass = I_mean_pass,
    mean_sigma_tau = mean_sigma_tau,
    sd_sigma_tau = stats::sd(completed$estimate_sigma_tau_resid),
    sigma_tau_mean_tolerance = sigma_tau_mean_tolerance,
    sigma_tau_mean_pass = sigma_tau_mean_pass,
    mean_R_trimmed = mean_R_trimmed,
    sd_R_trimmed = stats::sd(completed$estimate_R_trimmed),
    mean_R_untrimmed = mean(completed$estimate_R_untrimmed),
    R_trimmed_mean_rel_error = abs(mean_R_trimmed - completed$target_R_trimmed[[1L]]) / completed$target_R_trimmed[[1L]],
    R_trimmed_mean_pass = R_trimmed_mean_pass,
    mean_GM_se2 = mean_GM_se2,
    sd_GM_se2 = stats::sd(completed$estimate_GM_se2),
    GM_se2_mean_rel_tolerance = GM_se2_mean_rel_tolerance,
    GM_se2_mean_rel_error = abs(mean_GM_se2 - completed$target_GM_se2[[1L]]) / completed$target_GM_se2[[1L]],
    GM_se2_mean_pass = GM_se2_mean_pass,
    acceptance_pass = nrow(completed) == reps &&
      I_distribution_pass_rate >= 0.95 &&
      isTRUE(I_mean_pass) &&
      isTRUE(sigma_tau_mean_pass) &&
      isTRUE(R_trimmed_mean_pass) &&
      isTRUE(GM_se2_mean_pass),
    acceptance_note = "Aggregate V0 gate: full completion, I distribution in [0.25,0.35] at >=95%, shape-level mean calibration for I/sigma_tau/GM, and trimmed R mean within 20%; legacy per-rep gates are reporting-only.",
    stringsAsFactors = FALSE
  )
}))
summary <- rbind(summary, data.frame(
  run_id = run_id,
  experiment_id = experiment_id,
  mode = mode,
  shape = "DPM",
  reps = 0L,
  completed = 0L,
  failed = 0L,
  target_I = NA_real_,
  target_sigma_tau = 0.20,
  target_R_trimmed = NA_real_,
  target_GM_se2 = NA_real_,
  I_distribution_pass_rate = NA_real_,
  legacy_I_pass_rate = NA_real_,
  legacy_sigma_tau_pass_rate = NA_real_,
  legacy_R_untrimmed_pass_rate = NA_real_,
  legacy_GM_se2_pass_rate = NA_real_,
  mean_I = NA_real_,
  sd_I = NA_real_,
  I_mean_tolerance = NA_real_,
  I_mean_pass = NA,
  mean_sigma_tau = NA_real_,
  sd_sigma_tau = NA_real_,
  sigma_tau_mean_tolerance = NA_real_,
  sigma_tau_mean_pass = NA,
  mean_R_trimmed = NA_real_,
  sd_R_trimmed = NA_real_,
  mean_R_untrimmed = NA_real_,
  R_trimmed_mean_rel_error = NA_real_,
  R_trimmed_mean_pass = NA,
  mean_GM_se2 = NA_real_,
  sd_GM_se2 = NA_real_,
  GM_se2_mean_rel_tolerance = NA_real_,
  GM_se2_mean_rel_error = NA_real_,
  GM_se2_mean_pass = NA,
  acceptance_pass = NA,
  acceptance_note = "DPM is explicit v1 skip evidence.",
  stringsAsFactors = FALSE
))
summary_path <- validation_write_csv(summary, summary_path)

status <- if (nrow(complete) == length(active_shapes) * reps && all(summary$acceptance_pass[summary$shape != "DPM"])) "pass" else "fail"
ended_at <- Sys.time()
validation_record_manifest(paths, run_id, experiment_id, mode, status, started_at, ended_at, seed_root, reps, script_path, result_path, summary_path, "Full V0 acceptance uses aggregate shape-level calibration, trimmed R, and reporting-only legacy per-rep gates.")
print(summary)
message("V0 status: ", status)
validation_maybe_stop_for_blocker(status, "V0 validation failed acceptance criteria.")
