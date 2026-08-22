#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg) == 1L) {
  normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE)
} else {
  normalizePath("tools/validation/jobs/run-v11-validation.R", mustWork = TRUE)
}
source(file.path(dirname(script_path), "..", "R", "validation-harness.R"))

paths <- validation_paths(script_path)
validation_load_package(paths$package_root)

experiment_id <- "V11"
mode <- Sys.getenv("MULTISITEDGP_VALIDATION_MODE", unset = "full")
M <- validation_env_int("MULTISITEDGP_VALIDATION_REPS", if (identical(mode, "full")) 100L else 3L)
seed_root <- validation_env_int("MULTISITEDGP_VALIDATION_SEED_ROOT", 911101L)
resume <- validation_env_flag("MULTISITEDGP_VALIDATION_RESUME", default = FALSE)
overwrite <- validation_env_flag("MULTISITEDGP_VALIDATION_OVERWRITE", default = FALSE)
parallel <- validation_env_flag("MULTISITEDGP_VALIDATION_PARALLEL", default = FALSE)
run_id <- validation_run_id(experiment_id, mode)
reuse_source_run_id <- Sys.getenv("MULTISITEDGP_VALIDATION_REUSE_SOURCE_RUN_ID", unset = "")

result_path <- file.path(paths$generated_dir, paste0(run_id, "-results.csv"))
summary_path <- file.path(paths$generated_dir, paste0(run_id, "-summary.csv"))
started_at <- Sys.time()
acceptance_rule_version <- "phase9-v11-artifact-calibration-v2"
source_result_path <- NA_character_
parameters <- list(
  M = M,
  parallel = parallel,
  reuse_source_run_id = reuse_source_run_id,
  acceptance_rule_version = acceptance_rule_version
)

if (nzchar(reuse_source_run_id)) {
  source_result_candidate <- file.path(paths$generated_dir, paste0(reuse_source_run_id, "-results.csv"))
  source_summary_candidate <- file.path(paths$generated_dir, paste0(reuse_source_run_id, "-summary.csv"))
  source_parameters <- list(
    M = M,
    parallel = parallel,
    reuse_source_run_id = "",
    acceptance_rule_version = acceptance_rule_version
  )
  source_state <- validation_prepare_run(
    paths, reuse_source_run_id, experiment_id, mode, seed_root, source_parameters,
    script_path, source_result_candidate, source_summary_candidate,
    resume = TRUE, overwrite = FALSE
  )
  if (!identical(source_state$action, "reuse")) {
    stop("Requested V11 source run did not resolve to compatible evidence.", call. = FALSE)
  }
  parameters$source_result_sha256 <- validation_file_hash(source_result_candidate)
  parameters$source_contract_sha256 <- validation_file_hash(source_state$sidecar_path)
}

run_state <- validation_prepare_run(
  paths, run_id, experiment_id, mode, seed_root, parameters, script_path,
  result_path, summary_path, resume = resume, overwrite = overwrite
)

if (identical(run_state$action, "reuse")) {
  summary <- utils::read.csv(summary_path, stringsAsFactors = FALSE)
  status <- if (nrow(summary) == 1L && isTRUE(summary$acceptance_pass[[1L]])) "pass" else "fail"
  ended_at <- Sys.time()
  validation_record_reuse(paths, run_state, started_at, ended_at, "Compatible V11 output reused.")
  message("Resumed existing V11 output: ", result_path)
  validation_maybe_stop_for_blocker(status, "V11 validation failed in resumed output.")
  quit(status = 0)
}

if (isTRUE(parallel) && !requireNamespace("furrr", quietly = TRUE)) {
  stop("`MULTISITEDGP_VALIDATION_PARALLEL=true` requires the furrr package.", call. = FALSE)
}

if (identical(mode, "full")) {
  grid <- multisiteDGP::design_grid(
    J = c(10L, 14L, 25L, 50L, 100L),
    nj_mean = c(20, 40, 80, 160, 320),
    cv = c(0, 0.25, 0.50),
    sigma_tau = c(0.05, 0.10, 0.15, 0.20, 0.30),
    R2 = c(0, 0.10, 0.20, 0.30),
    true_dist = "Gaussian",
    seed_stream = TRUE,
    seed_root = seed_root
  )
} else {
  grid <- multisiteDGP::design_grid(
    J = c(10L, 50L, 100L),
    nj_mean = c(20, 80, 320),
    cv = c(0, 0.50),
    sigma_tau = c(0.05, 0.20, 0.30),
    R2 = c(0, 0.20),
    true_dist = "Gaussian",
    seed_stream = TRUE,
    seed_root = seed_root
  )
}

grid_manifest <- grid[, setdiff(names(grid), "design"), drop = FALSE]
grid_manifest_path <- validation_write_csv(
  grid_manifest,
  file.path(paths$generated_dir, paste0(run_id, "-grid.csv"))
)

one_cell_grid <- function(cell_id) {
  one <- grid[cell_id, , drop = FALSE]
  class(one) <- c("multisitedgp_design_grid", "tbl_df", "tbl", "data.frame")
  one
}

error_cell_row <- function(cell_id, e) {
  design <- grid$design[[cell_id]]
  data.frame(
    cell_id = cell_id,
    J = grid$J[[cell_id]],
    nj_mean = grid$nj_mean[[cell_id]],
    cv = grid$cv[[cell_id]],
    sigma_tau = grid$sigma_tau[[cell_id]],
    R2 = grid$R2[[cell_id]],
    true_dist = grid$true_dist[[cell_id]],
    M = M,
    seed_root = grid$seed[[cell_id]],
    design_hash = multisiteDGP::canonical_hash(design),
    status = "ERROR",
    pass = FALSE,
    n_violations = NA_integer_,
    fail_reasons = "scenario_audit_error",
    warn_reasons = "",
    med_I_hat = NA_real_,
    q05_I_hat = NA_real_,
    q95_I_hat = NA_real_,
    med_R_hat = NA_real_,
    q95_R_hat = NA_real_,
    med_mean_shrinkage = NA_real_,
    q05_mean_shrinkage = NA_real_,
    med_feasibility_efron = NA_real_,
    q05_feasibility_efron = NA_real_,
    med_feasibility_morris = NA_real_,
    med_bhattacharyya = NA_real_,
    q05_bhattacharyya = NA_real_,
    med_ks = NA_real_,
    q95_ks = NA_real_,
    stringsAsFactors = FALSE
  )
}

scenario_parameter_key <- function(x) {
  key_cols <- c("J", "nj_mean", "cv", "sigma_tau", "R2", "true_dist")
  do.call(
    paste,
    c(lapply(x[key_cols], as.character), sep = "|")
  )
}

threshold_frame <- function(thresholds, run_id, experiment_id) {
  data.frame(
    run_id = run_id,
    experiment_id = experiment_id,
    threshold = names(thresholds),
    value = as.numeric(thresholds),
    role = "scenario_audit_public_quality_gate",
    acceptance_role = "reported_not_tuned_for_v11_pass_rate",
    stringsAsFactors = FALSE
  )
}

reason_count_frame <- function(audit, run_id, experiment_id) {
  count_one <- function(reason_values, reason_type) {
    reason_values <- reason_values[nzchar(reason_values)]
    reasons <- unlist(strsplit(reason_values, "; ", fixed = TRUE), use.names = FALSE)
    reasons <- reasons[nzchar(reasons)]
    if (length(reasons) == 0L) {
      return(data.frame(
        reason_type = character(),
        reason = character(),
        n = integer(),
        stringsAsFactors = FALSE
      ))
    }
    counts <- as.data.frame(table(reasons), stringsAsFactors = FALSE)
    names(counts) <- c("reason", "n")
    counts$reason_type <- reason_type
    counts[c("reason_type", "reason", "n")]
  }

  out <- rbind(
    count_one(audit$fail_reasons, "fail"),
    count_one(audit$warn_reasons, "warn")
  )
  if (nrow(out) == 0L) {
    out <- data.frame(
      reason_type = character(),
      reason = character(),
      n = integer(),
      stringsAsFactors = FALSE
    )
  }
  out$run_id <- run_id
  out$experiment_id <- experiment_id
  out[c("run_id", "experiment_id", "reason_type", "reason", "n")]
}

dimension_summary_frame <- function(audit, run_id, experiment_id) {
  dimension_cols <- c("J", "nj_mean", "cv", "sigma_tau", "R2")
  rows <- lapply(dimension_cols, function(dimension) {
    values <- sort(unique(audit[[dimension]]))
    do.call(rbind, lapply(values, function(value) {
      one <- audit[audit[[dimension]] == value, , drop = FALSE]
      data.frame(
        run_id = run_id,
        experiment_id = experiment_id,
        dimension = dimension,
        value = as.character(value),
        cells = nrow(one),
        pass_cells = sum(one$status == "PASS"),
        warn_cells = sum(one$status == "WARN"),
        fail_cells = sum(one$status == "FAIL"),
        error_cells = sum(one$status == "ERROR"),
        public_pass_rate = mean(one$public_pass),
        strict_pass_rate = mean(one$strict_pass),
        feasibility_fail_rate = mean(one$feasibility_fail),
        finite_metric_rate = mean(one$finite_metrics),
        stringsAsFactors = FALSE
      )
    }))
  })
  do.call(rbind, rows)
}

calibration_region_summary <- function(audit, run_id, experiment_id) {
  regions <- list(
    full_broad_stress_grid = rep(TRUE, nrow(audit)),
    J_eq_100 = audit$J == 100L,
    J_eq_100_sigma_ge_0_15 = audit$J == 100L & audit$sigma_tau >= 0.15,
    high_information_region = audit$J >= 50L & audit$nj_mean >= 80 & audit$sigma_tau >= 0.15,
    very_high_information_region = audit$J >= 50L & audit$nj_mean >= 160 & audit$sigma_tau >= 0.20
  )
  do.call(rbind, lapply(names(regions), function(region) {
    one <- audit[regions[[region]], , drop = FALSE]
    data.frame(
      run_id = run_id,
      experiment_id = experiment_id,
      region = region,
      cells = nrow(one),
      pass_cells = sum(one$status == "PASS"),
      warn_cells = sum(one$status == "WARN"),
      fail_cells = sum(one$status == "FAIL"),
      public_pass_rate = mean(one$public_pass),
      strict_pass_rate = mean(one$strict_pass),
      reporting_role = "post_hoc_calibration_evidence_not_release_gate",
      stringsAsFactors = FALSE
    )
  }))
}

run_cell <- function(cell_id) {
  out <- tryCatch(
    multisiteDGP::scenario_audit(
      grid = one_cell_grid(cell_id),
      M = M,
      thresholds = multisiteDGP::default_thresholds(),
      parallel = FALSE
    ),
    error = function(e) error_cell_row(cell_id, e)
  )
  out$cell_id <- cell_id
  out
}

if (nzchar(reuse_source_run_id)) {
  source_result_path <- file.path(paths$generated_dir, paste0(reuse_source_run_id, "-results.csv"))
  if (!file.exists(source_result_path)) {
    stop("Requested V11 source results do not exist: ", source_result_path, call. = FALSE)
  }
  audit <- utils::read.csv(source_result_path, stringsAsFactors = FALSE)
  if (nrow(audit) != nrow(grid)) {
    stop(
      sprintf(
        "Requested V11 source results have %s rows but current grid has %s cells.",
        nrow(audit),
        nrow(grid)
      ),
      call. = FALSE
    )
  }
  message("V11 reusing completed source results: ", source_result_path)
  audit$cell_id <- seq_len(nrow(audit))
} else {
  rows <- vector("list", nrow(grid))
  for (cell_id in seq_len(nrow(grid))) {
    if (cell_id %% 25L == 0L || cell_id == 1L || cell_id == nrow(grid)) {
      message("V11 cell ", cell_id, " / ", nrow(grid))
    }
    rows[[cell_id]] <- run_cell(cell_id)
  }
  audit <- do.call(rbind, rows)
}
audit$result_schema_version <- "phase9-validation-v1"
audit$run_id <- run_id
audit$experiment_id <- experiment_id
audit$mode <- mode
audit$error_class <- NA_character_
audit$error_message <- NA_character_
audit$error_class[audit$status == "ERROR"] <- "scenario_audit_error"
audit$error_message[audit$status == "ERROR"] <- audit$fail_reasons[audit$status == "ERROR"]
audit$strict_pass <- audit$status == "PASS"
audit$public_pass <- audit$pass
audit$parameter_key <- scenario_parameter_key(audit)
audit$feasibility_fail <- grepl("feasibility", audit$fail_reasons, fixed = TRUE)
audit$plausible_feasibility_fail <- !audit$feasibility_fail |
  audit$J <= 25L |
  audit$nj_mean <= 40 |
  audit$sigma_tau <= 0.10 |
  audit$med_feasibility_efron < 5
audit$acceptance_row_pass <- audit$status != "ERROR"
audit$finite_metrics <- stats::complete.cases(audit[, c(
  "med_I_hat", "med_R_hat", "med_mean_shrinkage",
  "med_feasibility_efron", "med_bhattacharyya", "med_ks"
)])
audit <- audit[c(
  "result_schema_version", "run_id", "experiment_id", "mode",
  setdiff(names(audit), c("result_schema_version", "run_id", "experiment_id", "mode"))
)]
result_path <- validation_write_csv(audit, result_path)

threshold_path <- validation_write_csv(
  threshold_frame(multisiteDGP::default_thresholds(), run_id, experiment_id),
  file.path(paths$generated_dir, paste0(run_id, "-thresholds.csv"))
)

status_counts <- as.data.frame(table(audit$status), stringsAsFactors = FALSE)
names(status_counts) <- c("status", "n")
status_counts$run_id <- run_id
status_counts$experiment_id <- experiment_id
validation_write_csv(
  status_counts[c("run_id", "experiment_id", "status", "n")],
  file.path(paths$generated_dir, paste0(run_id, "-status-counts.csv"))
)

reason_counts_path <- validation_write_csv(
  reason_count_frame(audit, run_id, experiment_id),
  file.path(paths$generated_dir, paste0(run_id, "-reason-counts.csv"))
)

fail_by_motif <- stats::aggregate(
  cbind(
    public_pass = audit$public_pass,
    strict_pass = audit$strict_pass,
    feasibility_fail = audit$feasibility_fail,
    finite_metrics = audit$finite_metrics
  ) ~ J + nj_mean + sigma_tau,
  data = audit,
  FUN = mean
)
motif_summary_path <- validation_write_csv(
  fail_by_motif,
  file.path(paths$generated_dir, paste0(run_id, "-motif-summary.csv"))
)

dimension_summary_path <- validation_write_csv(
  dimension_summary_frame(audit, run_id, experiment_id),
  file.path(paths$generated_dir, paste0(run_id, "-dimension-summary.csv"))
)

calibration_summary_path <- validation_write_csv(
  calibration_region_summary(audit, run_id, experiment_id),
  file.path(paths$generated_dir, paste0(run_id, "-calibration-summary.csv"))
)

public_pass_rate <- mean(audit$public_pass)
strict_pass_rate <- mean(audit$strict_pass)
feasibility_fail_rows <- audit[audit$feasibility_fail, , drop = FALSE]
feasibility_fail_plausible_rate <- if (nrow(feasibility_fail_rows) == 0L) {
  1
} else {
  mean(feasibility_fail_rows$plausible_feasibility_fail)
}
expected_grid_cells <- if (identical(mode, "full")) 1500L else nrow(grid)
distinct_cell_ids <- length(unique(audit$cell_id))
distinct_parameter_keys <- length(unique(audit$parameter_key))
finite_metric_rate <- mean(audit$finite_metrics)

summary <- data.frame(
  run_id = run_id,
  experiment_id = experiment_id,
  mode = mode,
  acceptance_rule_version = acceptance_rule_version,
  grid_cells = nrow(grid),
  rows = nrow(audit),
  M = M,
  simulated_replications = nrow(grid) * M,
  expected_full_grid_cells = expected_grid_cells,
  source_run_id = if (nzchar(reuse_source_run_id)) reuse_source_run_id else NA_character_,
  source_result_path = .relative_to_root(source_result_path, paths$package_root),
  distinct_cell_ids = distinct_cell_ids,
  distinct_parameter_keys = distinct_parameter_keys,
  completed_cells = sum(audit$status != "ERROR"),
  error_cells = sum(audit$status == "ERROR"),
  pass_cells = sum(audit$status == "PASS"),
  warn_cells = sum(audit$status == "WARN"),
  fail_cells = sum(audit$status == "FAIL"),
  public_pass_rate = public_pass_rate,
  public_pass_rate_gate = "reporting_only_empirical",
  strict_pass_rate = strict_pass_rate,
  warn_rate = mean(audit$status == "WARN"),
  fail_rate = mean(audit$status == "FAIL"),
  feasibility_fail_cells = nrow(feasibility_fail_rows),
  feasibility_fail_plausible_rate = feasibility_fail_plausible_rate,
  finite_metric_rate = finite_metric_rate,
  grid_manifest_path = .relative_to_root(grid_manifest_path, paths$package_root),
  threshold_path = .relative_to_root(threshold_path, paths$package_root),
  reason_counts_path = .relative_to_root(reason_counts_path, paths$package_root),
  motif_summary_path = .relative_to_root(motif_summary_path, paths$package_root),
  dimension_summary_path = .relative_to_root(dimension_summary_path, paths$package_root),
  calibration_summary_path = .relative_to_root(calibration_summary_path, paths$package_root),
  acceptance_pass = nrow(grid) == expected_grid_cells &&
    nrow(audit) == nrow(grid) &&
    distinct_cell_ids == nrow(grid) &&
    distinct_parameter_keys == nrow(grid) &&
    sum(audit$status == "ERROR") == 0L &&
    finite_metric_rate == 1 &&
    feasibility_fail_plausible_rate == 1,
  acceptance_note = "V11 is a broad 1,500-cell scenario_audit stress-grid artifact. Public pass rate is recorded for empirical calibration only; no release-blocking interval is applied until a PRIMO operational subgrid and expected mix are preregistered. WARN remains public pass by scenario_audit() contract.",
  stringsAsFactors = FALSE
)
summary_path <- validation_write_csv(summary, summary_path)

status <- if (isTRUE(summary$acceptance_pass)) "pass" else "fail"
ended_at <- Sys.time()
manifest_note <- if (nzchar(reuse_source_run_id)) {
  paste("V11 scenario_audit baseline calibration evidence regenerated from source run", reuse_source_run_id)
} else {
  "V11 scenario_audit baseline calibration evidence."
}
validation_record_manifest(paths, run_id, experiment_id, mode, status, started_at, ended_at, seed_root, nrow(grid) * M, script_path, result_path, summary_path, manifest_note, parameters = parameters)
print(summary)
message("V11 status: ", status)
validation_maybe_stop_for_blocker(status, "V11 validation failed acceptance criteria.")
