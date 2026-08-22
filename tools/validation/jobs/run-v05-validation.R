#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg) == 1L) {
  normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE)
} else {
  normalizePath("tools/validation/jobs/run-v05-validation.R", mustWork = TRUE)
}
source(file.path(dirname(script_path), "..", "R", "validation-harness.R"))

paths <- validation_paths(script_path)
validation_load_package(paths$package_root)

experiment_id <- "V05"
mode <- Sys.getenv("MULTISITEDGP_VALIDATION_MODE", unset = "full")
fixtures <- validation_env_int("MULTISITEDGP_VALIDATION_REPS", if (identical(mode, "full")) 100L else 9L)
seed_root <- validation_env_int("MULTISITEDGP_VALIDATION_SEED_ROOT", 910501L)
resume <- validation_env_flag("MULTISITEDGP_VALIDATION_RESUME", default = FALSE)
overwrite <- validation_env_flag("MULTISITEDGP_VALIDATION_OVERWRITE", default = FALSE)
run_id <- validation_run_id(experiment_id, mode)

result_path <- file.path(paths$generated_dir, paste0(run_id, "-results.csv"))
summary_path <- file.path(paths$generated_dir, paste0(run_id, "-summary.csv"))
started_at <- Sys.time()
parameters <- list(fixtures = fixtures)
run_state <- validation_prepare_run(
  paths, run_id, experiment_id, mode, seed_root, parameters, script_path,
  result_path, summary_path, resume = resume, overwrite = overwrite
)

if (identical(run_state$action, "reuse")) {
  summary <- utils::read.csv(summary_path, stringsAsFactors = FALSE)
  status <- if (nrow(summary) == 1L && isTRUE(summary$acceptance_pass[[1L]])) "pass" else "fail"
  ended_at <- Sys.time()
  validation_record_reuse(paths, run_state, started_at, ended_at, "Compatible V05 output reused.")
  message("Resumed existing V05 output: ", result_path)
  validation_maybe_stop_for_blocker(status, "V05 validation failed in resumed output.")
  quit(status = 0)
}

fixture_seeds <- validation_seed_stream(fixtures, seed_root)
dependencies <- c("rank", "copula", "hybrid")

make_fixture <- function(fixture_id, seed) {
  withr::with_seed(seed, {
    dependence <- dependencies[((fixture_id - 1L) %% length(dependencies)) + 1L]
    corr <- stats::runif(1L, min = -0.70, max = 0.70)
    if (abs(corr) < 0.10) {
      corr <- sign(corr + 1e-8) * 0.30
    }
    nj_mean <- stats::runif(1L, min = 20, max = 120)
    data.frame(
      fixture_id = fixture_id,
      seed = seed,
      J = sample(20L:120L, 1L),
      sigma_tau = stats::runif(1L, min = 0.05, max = 0.40),
      nj_mean = nj_mean,
      cv = stats::runif(1L, min = 0, max = 0.75),
      nj_min = sample(seq_len(max(1L, floor(nj_mean / 2))), 1L),
      dependence = dependence,
      rank_corr = if (dependence %in% c("rank", "hybrid")) corr else 0,
      pearson_corr = if (identical(dependence, "copula")) corr else 0,
      stringsAsFactors = FALSE
    )
  })
}
fixtures_tbl <- do.call(rbind, Map(make_fixture, seq_len(fixtures), fixture_seeds))
grid <- merge(
  fixtures_tbl,
  data.frame(construction_path = c("constructor", "wrapper"), stringsAsFactors = FALSE),
  all = TRUE
)
grid$row_id <- seq_len(nrow(grid))
grid <- grid[order(grid$row_id), , drop = FALSE]

call_candidate <- function(args, construction_path, seed) {
  if (identical(construction_path, "constructor")) {
    do.call(multisiteDGP::multisitedgp_design, args)
  } else {
    do.call(multisiteDGP::sim_multisite, c(args, list(seed = seed)))
  }
}

run_one <- function(row_id) {
  r <- grid[row_id, ]
  design_args <- list(
    J = r$J,
    true_dist = "Gaussian",
    tau = 0,
    sigma_tau = r$sigma_tau,
    nj_mean = r$nj_mean,
    cv = r$cv,
    nj_min = r$nj_min,
    engine = "A1_legacy",
    dependence = r$dependence,
    rank_corr = r$rank_corr,
    pearson_corr = r$pearson_corr
  )
  set.seed(r$seed)
  rng_before <- .Random.seed
  err <- tryCatch({
    call_candidate(design_args, r$construction_path, r$seed)
    NULL
  }, error = function(e) e)
  rng_after <- .Random.seed
  accepted <- is.null(err)
  error_class <- if (accepted) NA_character_ else paste(class(err), collapse = "/")
  error_message <- if (accepted) NA_character_ else conditionMessage(err)
  class_pass <- !accepted && inherits(err, "multisitedgp_engine_dependence_error")
  data.frame(
    result_schema_version = "phase9-validation-v1",
    run_id = run_id,
    experiment_id = experiment_id,
    mode = mode,
    cell_id = sprintf("%s_%s", r$construction_path, r$dependence),
    row_id = row_id,
    fixture_id = r$fixture_id,
    seed = r$seed,
    status = if (class_pass) "completed" else "failed",
    construction_path = r$construction_path,
    J = r$J,
    sigma_tau = r$sigma_tau,
    nj_mean = r$nj_mean,
    cv = r$cv,
    nj_min = r$nj_min,
    engine = "A1_legacy",
    dependence = r$dependence,
    rank_corr = r$rank_corr,
    pearson_corr = r$pearson_corr,
    accepted = accepted,
    expected_error_class = "multisitedgp_engine_dependence_error",
    class_pass = class_pass,
    rng_unchanged = identical(rng_before, rng_after),
    error_class = error_class,
    error_message = error_message,
    stringsAsFactors = FALSE
  )
}

rows <- vector("list", nrow(grid))
for (row_id in seq_len(nrow(grid))) {
  if (row_id %% 40L == 0L || row_id == 1L || row_id == nrow(grid)) {
    message("V05 attempt ", row_id, " / ", nrow(grid))
  }
  rows[[row_id]] <- run_one(row_id)
}
results <- do.call(rbind, rows)
result_path <- validation_write_csv(results, result_path)

completed <- results[results$status == "completed", , drop = FALSE]
summary <- data.frame(
  run_id = run_id,
  experiment_id = experiment_id,
  mode = mode,
  fixtures = fixtures,
  attempts = nrow(results),
  completed = nrow(completed),
  failed = sum(results$status == "failed"),
  accepted_count = sum(results$accepted),
  class_pass_rate = mean(results$class_pass),
  rng_unchanged_rate = mean(results$rng_unchanged),
  constructor_pass_rate = mean(results$class_pass[results$construction_path == "constructor"]),
  wrapper_pass_rate = mean(results$class_pass[results$construction_path == "wrapper"]),
  rank_pass_rate = mean(results$class_pass[results$dependence == "rank"]),
  copula_pass_rate = mean(results$class_pass[results$dependence == "copula"]),
  hybrid_pass_rate = mean(results$class_pass[results$dependence == "hybrid"]),
  acceptance_pass = nrow(completed) == nrow(results) &&
    all(results$class_pass) &&
    all(results$rng_unchanged),
  acceptance_note = "V05 covers constructor and sim_multisite flat-argument paths for A1_legacy plus non-none dependence.",
  stringsAsFactors = FALSE
)
summary_path <- validation_write_csv(summary, summary_path)

status <- if (isTRUE(summary$acceptance_pass)) "pass" else "fail"
ended_at <- Sys.time()
validation_record_manifest(paths, run_id, experiment_id, mode, status, started_at, ended_at, seed_root, nrow(results), script_path, result_path, summary_path, "V05 Decision C classed abort evidence.", parameters = parameters)
print(summary)
message("V05 status: ", status)
validation_maybe_stop_for_blocker(status, "V05 validation failed acceptance criteria.")
