#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg) == 1L) {
  normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE)
} else {
  normalizePath("tools/validation/jobs/run-v07-validation.R", mustWork = TRUE)
}
source(file.path(dirname(script_path), "..", "R", "validation-harness.R"))

paths <- validation_paths(script_path)
validation_load_package(paths$package_root)

experiment_id <- "V07"
mode <- Sys.getenv("MULTISITEDGP_VALIDATION_MODE", unset = "full")
fixtures <- validation_env_int("MULTISITEDGP_VALIDATION_REPS", if (identical(mode, "full")) 100L else 10L)
seeds_per_fixture <- validation_env_int("MULTISITEDGP_VALIDATION_SEEDS_PER_FIXTURE", if (identical(mode, "full")) 5L else 2L)
seed_root <- validation_env_int("MULTISITEDGP_VALIDATION_SEED_ROOT", 910701L)
resume <- validation_env_flag("MULTISITEDGP_VALIDATION_RESUME", default = FALSE)
overwrite <- validation_env_flag("MULTISITEDGP_VALIDATION_OVERWRITE", default = FALSE)
run_id <- validation_run_id(experiment_id, mode)

result_path <- file.path(paths$generated_dir, paste0(run_id, "-results.csv"))
summary_path <- file.path(paths$generated_dir, paste0(run_id, "-summary.csv"))
started_at <- Sys.time()
parameters <- list(fixtures = fixtures, seeds_per_fixture = seeds_per_fixture)
run_state <- validation_prepare_run(
  paths, run_id, experiment_id, mode, seed_root, parameters, script_path,
  result_path, summary_path, resume = resume, overwrite = overwrite
)

if (identical(run_state$action, "reuse")) {
  summary <- utils::read.csv(summary_path, stringsAsFactors = FALSE)
  status <- if (nrow(summary) == 1L && isTRUE(summary$acceptance_pass[[1L]])) "pass" else "fail"
  ended_at <- Sys.time()
  validation_record_reuse(paths, run_state, started_at, ended_at, "Compatible V07 output reused.")
  message("Resumed existing V07 output: ", result_path)
  validation_maybe_stop_for_blocker(status, "V07 validation failed in resumed output.")
  quit(status = 0)
}

fixture_seeds <- validation_seed_stream(fixtures, seed_root)
make_fixture <- function(fixture_id, seed) {
  withr::with_seed(seed, {
    data.frame(
      fixture_id = fixture_id,
      J = sample(10L:500L, 1L),
      I = stats::runif(1L, min = 0.05, max = 0.95),
      R = stats::runif(1L, min = 1, max = 10),
      sigma_tau = stats::runif(1L, min = 0.05, max = 0.40),
      stringsAsFactors = FALSE
    )
  })
}
fixtures_tbl <- do.call(rbind, Map(make_fixture, seq_len(fixtures), fixture_seeds))
sim_seeds <- validation_seed_stream(fixtures * seeds_per_fixture, seed_root + 1000L)
grid <- merge(fixtures_tbl, data.frame(seed_rep = seq_len(seeds_per_fixture), stringsAsFactors = FALSE), all = TRUE)
grid <- grid[order(grid$fixture_id, grid$seed_rep), , drop = FALSE]
grid$row_id <- seq_len(nrow(grid))
grid$seed <- sim_seeds

run_one <- function(row_id) {
  r <- grid[row_id, ]
  tryCatch({
    dat <- multisiteDGP::sim_meta(
      J = r$J,
      I = r$I,
      R = r$R,
      sigma_tau = r$sigma_tau,
      shuffle = TRUE,
      seed = r$seed
    )
    diagnostics <- attr(dat, "diagnostics", exact = TRUE)
    provenance <- attr(dat, "provenance", exact = TRUE)
    estimate_i <- multisiteDGP::compute_I(dat$se2_j, sigma_tau = r$sigma_tau, tau_j = dat$tau_j)
    estimate_r <- multisiteDGP::heterogeneity_ratio(dat$se2_j)
    data.frame(
      result_schema_version = "phase9-validation-v1",
      run_id = run_id,
      experiment_id = experiment_id,
      mode = mode,
      cell_id = sprintf("fixture_%03d", r$fixture_id),
      row_id = row_id,
      fixture_id = r$fixture_id,
      seed_rep = r$seed_rep,
      seed = r$seed,
      status = "completed",
      J = r$J,
      target_I = r$I,
      target_R = r$R,
      sigma_tau = r$sigma_tau,
      estimate_I = estimate_i,
      estimate_R = estimate_r,
      diagnostics_I = diagnostics$I_hat,
      diagnostics_R = diagnostics$R_hat,
      I_abs_error = abs(estimate_i - r$I),
      R_abs_error = abs(estimate_r - r$R),
      I_pass = abs(estimate_i - r$I) <= 1e-12,
      R_pass = abs(estimate_r - r$R) <= 1e-12,
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
      cell_id = sprintf("fixture_%03d", r$fixture_id),
      row_id = row_id,
      fixture_id = r$fixture_id,
      seed_rep = r$seed_rep,
      seed = r$seed,
      status = "failed",
      J = r$J,
      target_I = r$I,
      target_R = r$R,
      sigma_tau = r$sigma_tau,
      estimate_I = NA_real_,
      estimate_R = NA_real_,
      diagnostics_I = NA_real_,
      diagnostics_R = NA_real_,
      I_abs_error = NA_real_,
      R_abs_error = NA_real_,
      I_pass = FALSE,
      R_pass = FALSE,
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
  if (row_id %% 100L == 0L || row_id == 1L || row_id == nrow(grid)) {
    message("V07 row ", row_id, " / ", nrow(grid))
  }
  rows[[row_id]] <- run_one(row_id)
}
results <- do.call(rbind, rows)
result_path <- validation_write_csv(results, result_path)

completed <- results[results$status == "completed", , drop = FALSE]
fixture_summary <- stats::aggregate(
  cbind(I_pass, R_pass, I_abs_error, R_abs_error) ~ fixture_id,
  data = completed,
  FUN = function(x) if (is.logical(x)) all(x) else max(x)
)
names(fixture_summary)[names(fixture_summary) == "I_pass"] <- "fixture_I_pass"
names(fixture_summary)[names(fixture_summary) == "R_pass"] <- "fixture_R_pass"
summary <- data.frame(
  run_id = run_id,
  experiment_id = experiment_id,
  mode = mode,
  fixtures = fixtures,
  seeds_per_fixture = seeds_per_fixture,
  rows = nrow(results),
  completed = nrow(completed),
  failed = sum(results$status == "failed"),
  I_pass_rate = mean(completed$I_pass),
  R_pass_rate = mean(completed$R_pass),
  fixture_I_pass_rate = mean(fixture_summary$fixture_I_pass),
  fixture_R_pass_rate = mean(fixture_summary$fixture_R_pass),
  max_I_abs_error = max(completed$I_abs_error),
  max_R_abs_error = max(completed$R_abs_error),
  acceptance_pass = nrow(completed) == nrow(results) &&
    all(completed$I_pass) &&
    all(completed$R_pass),
  acceptance_note = "V07 uses sim_meta wrapper path with shuffle=TRUE; exactness is measured with compute_I() and heterogeneity_ratio() on se2_j.",
  stringsAsFactors = FALSE
)
summary_path <- validation_write_csv(summary, summary_path)
validation_write_csv(fixture_summary, file.path(paths$generated_dir, paste0(run_id, "-fixture-summary.csv")))

status <- if (isTRUE(summary$acceptance_pass)) "pass" else "fail"
ended_at <- Sys.time()
validation_record_manifest(paths, run_id, experiment_id, mode, status, started_at, ended_at, seed_root, nrow(results), script_path, result_path, summary_path, "V07 Paradigm B exact I/R recovery evidence.", parameters = parameters)
print(summary)
message("V07 status: ", status)
validation_maybe_stop_for_blocker(status, "V07 validation failed acceptance criteria.")
