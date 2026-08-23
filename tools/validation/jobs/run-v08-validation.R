#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg) == 1L) {
  normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE)
} else {
  normalizePath("tools/validation/jobs/run-v08-validation.R", mustWork = TRUE)
}
source(file.path(dirname(script_path), "..", "R", "validation-harness.R"))

paths <- validation_paths(script_path)
validation_load_package(paths$package_root)

experiment_id <- "V08"
mode <- Sys.getenv("MULTISITEDGP_VALIDATION_MODE", unset = "full")
fixtures <- validation_env_int("MULTISITEDGP_VALIDATION_REPS", if (identical(mode, "full")) 50L else 10L)
reruns <- validation_env_int("MULTISITEDGP_VALIDATION_RERUNS", if (identical(mode, "full")) 5L else 3L)
seed_root <- validation_env_int("MULTISITEDGP_VALIDATION_SEED_ROOT", 910801L)
resume <- validation_env_flag("MULTISITEDGP_VALIDATION_RESUME", default = FALSE)
overwrite <- validation_env_flag("MULTISITEDGP_VALIDATION_OVERWRITE", default = FALSE)
run_id <- validation_run_id(experiment_id, mode)

result_path <- file.path(paths$generated_dir, paste0(run_id, "-results.csv"))
summary_path <- file.path(paths$generated_dir, paste0(run_id, "-summary.csv"))
started_at <- Sys.time()
parameters <- list(fixtures = fixtures, reruns = reruns)
run_state <- validation_prepare_run(
  paths, run_id, experiment_id, mode, seed_root, parameters, script_path,
  result_path, summary_path, resume = resume, overwrite = overwrite
)

if (identical(run_state$action, "reuse")) {
  summary <- utils::read.csv(summary_path, stringsAsFactors = FALSE)
  status <- if (nrow(summary) == 1L && isTRUE(summary$acceptance_pass[[1L]])) "pass" else "fail"
  ended_at <- Sys.time()
  validation_record_reuse(paths, run_state, started_at, ended_at, "Compatible V08 output reused.")
  message("Resumed existing V08 output: ", result_path)
  validation_maybe_stop_for_blocker(status, "V08 validation failed in resumed output.")
  quit(status = 0)
}

fixture_seeds <- validation_seed_stream(fixtures, seed_root)
make_fixture <- function(fixture_id, seed) {
  withr::with_seed(seed, {
    nj_mean <- sample(40L:180L, 1L)
    data.frame(
      fixture_id = fixture_id,
      fixture_seed = seed,
      sim_seed = sample.int(.Machine$integer.max, 1L),
      J = sample(c(25L, 50L, 75L, 100L, 150L), 1L),
      sigma_tau = stats::runif(1L, min = 0.05, max = 0.35),
      nj_mean = nj_mean,
      cv = sample(c(0, 0.10, 0.25, 0.50), 1L),
      nj_min = sample(4L:10L, 1L),
      R2 = stats::runif(1L, min = 0, max = 0.5),
      stringsAsFactors = FALSE
    )
  })
}
fixtures_tbl <- do.call(rbind, Map(make_fixture, seq_len(fixtures), fixture_seeds))

run_fixture <- function(row_id) {
  r <- fixtures_tbl[row_id, ]
  tryCatch({
    design <- multisiteDGP::multisitedgp_design(
      J = r$J,
      true_dist = "Gaussian",
      tau = 0,
      sigma_tau = r$sigma_tau,
      nj_mean = r$nj_mean,
      cv = r$cv,
      nj_min = r$nj_min,
      R2 = r$R2,
      engine = "A2_modern",
      dependence = "none"
    )
    set.seed(r$fixture_seed)
    before_all <- .Random.seed
    rerun_rows <- vector("list", reruns)
    for (rerun_id in seq_len(reruns)) {
      before_call <- .Random.seed
      out <- multisiteDGP::sim_multisite(design, seed = r$sim_seed)
      after_call <- .Random.seed
      provenance <- attr(out, "provenance", exact = TRUE)
      actual_hash <- multisiteDGP::canonical_hash(out)
      actual_design_hash <- multisiteDGP::canonical_hash(attr(out, "design", exact = TRUE))
      rerun_rows[[rerun_id]] <- data.frame(
        result_schema_version = "phase9-validation-v1",
        run_id = run_id,
        experiment_id = experiment_id,
        mode = mode,
        cell_id = sprintf("fixture_%03d", r$fixture_id),
        row_id = (row_id - 1L) * reruns + rerun_id,
        fixture_id = r$fixture_id,
        rerun_id = rerun_id,
        fixture_seed = r$fixture_seed,
        sim_seed = r$sim_seed,
        status = "completed",
        J = r$J,
        sigma_tau = r$sigma_tau,
        nj_mean = r$nj_mean,
        cv = r$cv,
        nj_min = r$nj_min,
        R2 = r$R2,
        canonical_hash = actual_hash,
        provenance_canonical_hash = provenance$canonical_hash,
        canonical_hash_matches_provenance = identical(actual_hash, provenance$canonical_hash),
        design_hash = actual_design_hash,
        provenance_design_hash = provenance$design_hash,
        design_hash_matches_provenance = identical(actual_design_hash, provenance$design_hash),
        rng_preserved_call = identical(before_call, after_call),
        error_class = NA_character_,
        error_message = NA_character_,
        stringsAsFactors = FALSE
      )
    }
    fixture_rows <- do.call(rbind, rerun_rows)
    fixture_rows$rng_preserved_fixture <- identical(before_all, .Random.seed)
    fixture_rows
  }, error = function(e) {
    data.frame(
      result_schema_version = "phase9-validation-v1",
      run_id = run_id,
      experiment_id = experiment_id,
      mode = mode,
      cell_id = sprintf("fixture_%03d", r$fixture_id),
      row_id = (row_id - 1L) * reruns + seq_len(reruns),
      fixture_id = r$fixture_id,
      rerun_id = seq_len(reruns),
      fixture_seed = r$fixture_seed,
      sim_seed = r$sim_seed,
      status = "failed",
      J = r$J,
      sigma_tau = r$sigma_tau,
      nj_mean = r$nj_mean,
      cv = r$cv,
      nj_min = r$nj_min,
      R2 = r$R2,
      canonical_hash = NA_character_,
      provenance_canonical_hash = NA_character_,
      canonical_hash_matches_provenance = FALSE,
      design_hash = NA_character_,
      provenance_design_hash = NA_character_,
      design_hash_matches_provenance = FALSE,
      rng_preserved_call = FALSE,
      rng_preserved_fixture = FALSE,
      error_class = paste(class(e), collapse = "/"),
      error_message = conditionMessage(e),
      stringsAsFactors = FALSE
    )
  })
}

rows <- vector("list", nrow(fixtures_tbl))
for (row_id in seq_len(nrow(fixtures_tbl))) {
  if (row_id %% 10L == 0L || row_id == 1L || row_id == nrow(fixtures_tbl)) {
    message("V08 fixture ", row_id, " / ", nrow(fixtures_tbl))
  }
  rows[[row_id]] <- run_fixture(row_id)
}
results <- do.call(rbind, rows)
result_path <- validation_write_csv(results, result_path)

completed <- results[results$status == "completed", , drop = FALSE]
fixture_summary <- do.call(rbind, lapply(split(results, results$fixture_id), function(x) {
  completed_x <- x[x$status == "completed", , drop = FALSE]
  data.frame(
    run_id = run_id,
    experiment_id = experiment_id,
    mode = mode,
    fixture_id = x$fixture_id[[1L]],
    reruns = nrow(x),
    completed = nrow(completed_x),
    failed = sum(x$status == "failed"),
    unique_hashes = length(unique(stats::na.omit(completed_x$canonical_hash))),
    local_hash_identical = nrow(completed_x) == reruns && length(unique(completed_x$canonical_hash)) == 1L,
    rng_preserved = nrow(completed_x) == reruns && all(completed_x$rng_preserved_call) && all(completed_x$rng_preserved_fixture),
    provenance_hash_match = nrow(completed_x) == reruns && all(completed_x$canonical_hash_matches_provenance) && all(completed_x$design_hash_matches_provenance),
    stringsAsFactors = FALSE
  )
}))
local_os <- Sys.info()[["sysname"]]
required_local_rate <- if (identical(local_os, "Linux")) 1 else 0.95
summary <- data.frame(
  run_id = run_id,
  experiment_id = experiment_id,
  mode = mode,
  local_os = local_os,
  fixtures = fixtures,
  reruns = reruns,
  rows = nrow(results),
  completed = nrow(completed),
  failed = sum(results$status == "failed"),
  local_hash_identical_rate = mean(fixture_summary$local_hash_identical),
  rng_preserved_rate = mean(fixture_summary$rng_preserved),
  provenance_hash_match_rate = mean(fixture_summary$provenance_hash_match),
  required_local_hash_rate = required_local_rate,
  cross_os_matrix_status = "not_run_in_local_phase9_job",
  acceptance_pass = nrow(completed) == nrow(results) &&
    mean(fixture_summary$local_hash_identical) >= required_local_rate &&
    all(fixture_summary$rng_preserved) &&
    all(fixture_summary$provenance_hash_match),
  acceptance_note = "V08 records local canonical_hash rerun evidence only; Linux/Windows/macOS cross-machine matrix remains external to this local job.",
  stringsAsFactors = FALSE
)
summary_path <- validation_write_csv(summary, summary_path)
validation_write_csv(fixture_summary, file.path(paths$generated_dir, paste0(run_id, "-fixture-summary.csv")))

status <- if (isTRUE(summary$acceptance_pass)) "pass" else "fail"
ended_at <- Sys.time()
validation_record_manifest(paths, run_id, experiment_id, mode, status, started_at, ended_at, seed_root, nrow(results), script_path, result_path, summary_path, "V08 local Engine A2 reproducibility evidence; cross-OS matrix not run locally.", parameters = parameters)
print(summary)
message("V08 status: ", status)
validation_maybe_stop_for_blocker(status, "V08 validation failed acceptance criteria.")
