#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg) == 1L) {
  normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE)
} else {
  normalizePath("tools/validation/jobs/run-v09-validation.R", mustWork = TRUE)
}
source(file.path(dirname(script_path), "..", "R", "validation-harness.R"))

paths <- validation_paths(script_path)
validation_load_package(paths$package_root)

experiment_id <- "V09"
mode <- Sys.getenv("MULTISITEDGP_VALIDATION_MODE", unset = "full")
reps <- validation_env_int("MULTISITEDGP_VALIDATION_REPS", if (identical(mode, "full")) 100L else 3L)
max_iter <- validation_env_int("MULTISITEDGP_VALIDATION_MAX_ITER", 20000L)
seed_root <- validation_env_int("MULTISITEDGP_VALIDATION_SEED_ROOT", 910901L)
resume <- validation_env_flag("MULTISITEDGP_VALIDATION_RESUME", default = FALSE)
overwrite <- validation_env_flag("MULTISITEDGP_VALIDATION_OVERWRITE", default = FALSE)
run_id <- validation_run_id(experiment_id, mode)

result_path <- file.path(paths$generated_dir, paste0(run_id, "-results.csv"))
summary_path <- file.path(paths$generated_dir, paste0(run_id, "-summary.csv"))
started_at <- Sys.time()
parameters <- list(reps = reps, max_iter = max_iter)
run_state <- validation_prepare_run(
  paths, run_id, experiment_id, mode, seed_root, parameters, script_path,
  result_path, summary_path, resume = resume, overwrite = overwrite
)

if (identical(run_state$action, "reuse")) {
  summary <- utils::read.csv(summary_path, stringsAsFactors = FALSE)
  status <- if (nrow(summary) == 1L && isTRUE(summary$acceptance_pass[[1L]])) "pass" else "fail"
  ended_at <- Sys.time()
  validation_record_reuse(paths, run_state, started_at, ended_at, "Compatible V09 output reused.")
  message("Resumed existing V09 output: ", result_path)
  validation_maybe_stop_for_blocker(status, "V09 validation failed in resumed output.")
  quit(status = 0)
}

rho_grid <- c(-0.95, -0.7, -0.5, -0.3, 0, 0.3, 0.5, 0.7, 0.95)
J_grid <- c(50L, 100L, 200L, 500L)
if (!identical(mode, "full")) {
  rho_grid <- c(-0.95, -0.7, 0, 0.7, 0.95)
  J_grid <- c(50L, 100L)
}
grid <- expand.grid(
  J = J_grid,
  target_rho = rho_grid,
  rep_id = seq_len(reps),
  KEEP.OUT.ATTRS = FALSE
)
grid$row_id <- seq_len(nrow(grid))
grid$seed <- validation_seed_stream(nrow(grid), seed_root)

cell_type <- function(J, target_rho) {
  if (abs(target_rho) <= 0.7) {
    return("core")
  }
  if (abs(target_rho) == 0.95 && J >= 100L) {
    return("boundary_largeJ")
  }
  "boundary_J50_expected"
}

capture_alignment <- function(upstream, target_rho) {
  warning_messages <- character()
  out <- withCallingHandlers(
    multisiteDGP::align_rank_corr(
      upstream,
      rank_corr = target_rho,
      max_iter = max_iter,
      tol = 0.02
    ),
    warning = function(w) {
      warning_messages <<- c(warning_messages, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(out = out, warning_messages = warning_messages)
}

run_one <- function(row_id) {
  r <- grid[row_id, ]
  type <- cell_type(r$J, r$target_rho)
  expected_boundary <- identical(type, "boundary_J50_expected")
  tryCatch({
    effects <- withr::with_seed(
      r$seed,
      multisiteDGP::gen_effects_gaussian(J = r$J, tau = 0, sigma_tau = 0.20)
    )
    upstream <- withr::with_seed(
      r$seed + 1L,
      multisiteDGP::gen_site_sizes(
        effects,
        J = r$J,
        nj_mean = 80,
        cv = 0.5,
        nj_min = 5L,
        engine = "A2_modern"
      )
    )
    fit <- capture_alignment(upstream, r$target_rho)
    out <- fit$out
    diag <- attr(out, "dependence_diagnostics", exact = TRUE)
    achieved <- diag$achieved
    abs_error <- abs(achieved - r$target_rho)
    multiset_preserved <- isTRUE(all.equal(sort(out$se2_j), sort(upstream$se2_j), tolerance = 1e-12))
    within_core <- abs_error < 0.02
    within_boundary <- abs_error < 0.05
    data.frame(
      result_schema_version = "phase9-validation-v1",
      run_id = run_id,
      experiment_id = experiment_id,
      mode = mode,
      cell_id = sprintf("J%s_rho%s", r$J, r$target_rho),
      row_id = row_id,
      J = r$J,
      target_rho = r$target_rho,
      rep_id = r$rep_id,
      seed = r$seed,
      status = "completed",
      cell_type = type,
      expected_boundary = expected_boundary,
      achieved_rho = achieved,
      abs_error = abs_error,
      converged = isTRUE(diag$converged),
      iterations = diag$iterations,
      tol = diag$tol,
      sign_match = if (r$target_rho == 0) TRUE else identical(sign(achieved), sign(r$target_rho)),
      within_core_tol = within_core,
      within_boundary_tol = within_boundary,
      warning_count = length(fit$warning_messages),
      warning_message = paste(unique(fit$warning_messages), collapse = " | "),
      multiset_preserved = multiset_preserved,
      acceptance_row_pass = if (identical(type, "core")) within_core else if (identical(type, "boundary_largeJ")) within_boundary else NA,
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
      cell_id = sprintf("J%s_rho%s", r$J, r$target_rho),
      row_id = row_id,
      J = r$J,
      target_rho = r$target_rho,
      rep_id = r$rep_id,
      seed = r$seed,
      status = if (expected_boundary && inherits(e, "multisitedgp_dependence_solver_error")) "expected_boundary_failure" else "failed",
      cell_type = type,
      expected_boundary = expected_boundary,
      achieved_rho = NA_real_,
      abs_error = NA_real_,
      converged = FALSE,
      iterations = NA_integer_,
      tol = 0.02,
      sign_match = FALSE,
      within_core_tol = FALSE,
      within_boundary_tol = FALSE,
      warning_count = NA_integer_,
      warning_message = NA_character_,
      multiset_preserved = FALSE,
      acceptance_row_pass = if (expected_boundary) NA else FALSE,
      error_class = paste(class(e), collapse = "/"),
      error_message = conditionMessage(e),
      stringsAsFactors = FALSE
    )
  })
}

rows <- vector("list", nrow(grid))
for (row_id in seq_len(nrow(grid))) {
  if (row_id %% 100L == 0L || row_id == 1L || row_id == nrow(grid)) {
    message("V09 row ", row_id, " / ", nrow(grid))
  }
  rows[[row_id]] <- run_one(row_id)
}
results <- do.call(rbind, rows)
result_path <- validation_write_csv(results, result_path)

gated <- results[!results$expected_boundary, , drop = FALSE]
cell_summary <- do.call(rbind, lapply(split(gated, gated$cell_id), function(x) {
  type <- x$cell_type[[1L]]
  pass_rate <- mean(x$acceptance_row_pass, na.rm = TRUE)
  threshold <- if (identical(type, "core")) 0.95 else 0.90
  data.frame(
    run_id = run_id,
    experiment_id = experiment_id,
    mode = mode,
    cell_id = x$cell_id[[1L]],
    J = x$J[[1L]],
    target_rho = x$target_rho[[1L]],
    cell_type = type,
    rows = nrow(x),
    completed = sum(x$status == "completed"),
    failed = sum(x$status == "failed"),
    pass_rate = pass_rate,
    required_pass_rate = threshold,
    mean_abs_error = mean(x$abs_error, na.rm = TRUE),
    max_abs_error = max(x$abs_error, na.rm = TRUE),
    converged_rate = mean(x$converged, na.rm = TRUE),
    warning_rate = mean(x$warning_count > 0, na.rm = TRUE),
    gate_pass = sum(x$status == "failed") == 0L && pass_rate >= threshold,
    stringsAsFactors = FALSE
  )
}))
boundary_j50 <- results[results$expected_boundary, , drop = FALSE]
summary <- data.frame(
  run_id = run_id,
  experiment_id = experiment_id,
  mode = mode,
  reps = reps,
  max_iter = max_iter,
  rows = nrow(results),
  gated_rows = nrow(gated),
  boundary_j50_rows = nrow(boundary_j50),
  completed = sum(results$status == "completed"),
  failed_unexpected = sum(results$status == "failed"),
  expected_boundary_failures = sum(results$status == "expected_boundary_failure"),
  gated_cell_count = nrow(cell_summary),
  gated_cell_pass_count = sum(cell_summary$gate_pass),
  min_cell_pass_rate = min(cell_summary$pass_rate),
  core_min_pass_rate = min(cell_summary$pass_rate[cell_summary$cell_type == "core"]),
  boundary_largeJ_min_pass_rate = min(cell_summary$pass_rate[cell_summary$cell_type == "boundary_largeJ"]),
  boundary_j50_completed = sum(boundary_j50$status == "completed"),
  boundary_j50_expected_failures = sum(boundary_j50$status == "expected_boundary_failure"),
  multiset_preserved_rate = mean(results$multiset_preserved[results$status == "completed"]),
  acceptance_pass = sum(results$status == "failed") == 0L &&
    all(cell_summary$gate_pass) &&
    all(results$multiset_preserved[results$status == "completed"]),
  acceptance_note = "J=50 and |rho|=0.95 rows are classified as expected boundary evidence and excluded from normal pass-rate gates.",
  stringsAsFactors = FALSE
)
summary_path <- validation_write_csv(summary, summary_path)
validation_write_csv(cell_summary, file.path(paths$generated_dir, paste0(run_id, "-cell-summary.csv")))

status <- if (isTRUE(summary$acceptance_pass)) "pass" else "fail"
ended_at <- Sys.time()
validation_record_manifest(paths, run_id, experiment_id, mode, status, started_at, ended_at, seed_root, nrow(results), script_path, result_path, summary_path, "V09 hill-climb boundary convergence evidence.", parameters = parameters)
print(summary)
message("V09 status: ", status)
validation_maybe_stop_for_blocker(status, "V09 validation failed acceptance criteria.")
