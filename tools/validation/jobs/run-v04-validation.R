#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg) == 1L) {
  normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE)
} else {
  normalizePath("tools/validation/jobs/run-v04-validation.R", mustWork = TRUE)
}
source(file.path(dirname(script_path), "..", "R", "validation-harness.R"))

paths <- validation_paths(script_path)
validation_load_package(paths$package_root)

experiment_id <- "V04"
mode <- Sys.getenv("MULTISITEDGP_VALIDATION_MODE", unset = "full")
fixtures <- validation_env_int("MULTISITEDGP_VALIDATION_REPS", if (identical(mode, "full")) 100L else 10L)
seed_root <- validation_env_int("MULTISITEDGP_VALIDATION_SEED_ROOT", 910401L)
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
  validation_record_manifest(paths, run_id, experiment_id, mode, status, started_at, ended_at, seed_root, fixtures, script_path, result_path, summary_path, "Existing V04 output reused.")
  message("Resumed existing V04 output: ", result_path)
  validation_maybe_stop_for_blocker(status, "V04 validation failed in resumed output.")
  quit(status = 0)
}

solve_trunc_gamma <- getFromNamespace("solve_trunc_gamma", "multisiteDGP")
trunc_gamma_moments <- getFromNamespace("trunc_gamma_moments", "multisiteDGP")

beta_for_trunc_mean <- function(alpha, n_bar, n_min) {
  objective <- function(log_beta) trunc_gamma_moments(alpha, exp(log_beta), n_min)$mean - n_bar
  grid <- seq(-12, 6, length.out = 200L)
  values <- vapply(grid, function(x) suppressWarnings(objective(x)), numeric(1))
  ok <- is.finite(values)
  crossing <- which(ok[-1L] & ok[-length(ok)] & values[-1L] * values[-length(values)] <= 0)
  if (length(crossing) < 1L) {
    return(NA_real_)
  }
  exp(stats::uniroot(objective, c(grid[[crossing[[1L]]]], grid[[crossing[[1L]] + 1L]]), tol = 1e-10)$root)
}

trunc_gamma_cv_capacity <- function(n_bar, n_min) {
  alphas <- exp(seq(log(1e-8), log(1e4), length.out = 500L))
  cvs <- rep(NA_real_, length(alphas))
  for (i in seq_along(alphas)) {
    beta <- beta_for_trunc_mean(alphas[[i]], n_bar = n_bar, n_min = n_min)
    if (is.finite(beta)) {
      cvs[[i]] <- trunc_gamma_moments(alphas[[i]], beta, n_min)$cv
    }
  }
  max(cvs, na.rm = TRUE)
}

fixture_seeds <- validation_seed_stream(fixtures, seed_root)
make_fixture <- function(row_id, seed) {
  withr::with_seed(seed, {
    nj_mean <- stats::runif(1L, min = 10, max = 200)
    nj_min <- sample(seq_len(max(1L, floor(nj_mean / 2))), 1L)
    cv_capacity <- trunc_gamma_cv_capacity(nj_mean, nj_min)
    if (!is.finite(cv_capacity) || cv_capacity <= 0.001) {
      cv_capacity <- 0.10
    }
    cv_lower <- 0.02
    cv_upper <- min(1, 0.90 * cv_capacity)
    cv <- if (row_id %% 10L == 0L) {
      0
    } else if (row_id %% 5L == 0L) {
      stats::runif(1L, min = max(cv_lower, 0.75 * cv_upper), max = cv_upper)
    } else {
      stats::runif(1L, min = cv_lower, max = cv_upper)
    }
    data.frame(
      row_id = row_id,
      seed = seed,
      J = sample(10L:500L, 1L),
      nj_mean = nj_mean,
      cv = cv,
      nj_min = nj_min,
      cv_capacity = cv_capacity,
      cv_capacity_margin = cv_capacity - cv,
      stringsAsFactors = FALSE
    )
  })
}
grid <- do.call(rbind, Map(make_fixture, seq_len(fixtures), fixture_seeds))

run_one <- function(row_id) {
  r <- grid[row_id, ]
  tryCatch({
    solution <- solve_trunc_gamma(
      n_bar = r$nj_mean,
      cv = r$cv,
      n_min = r$nj_min,
      tol = 1e-6,
      max_starts = 5L,
      max_iter = 100L
    )
    mean_rel_error <- abs(solution$mean - r$nj_mean) / r$nj_mean
    cv_abs_error <- abs(solution$cv - r$cv)
    residual_max <- max(abs(solution$residual))
    data.frame(
      result_schema_version = "phase9-validation-v1",
      run_id = run_id,
      experiment_id = experiment_id,
      mode = mode,
      cell_id = sprintf("fixture_%03d", row_id),
      row_id = row_id,
      seed = r$seed,
      status = "completed",
      J = r$J,
      nj_mean = r$nj_mean,
      cv = r$cv,
      nj_min = r$nj_min,
      cv_capacity = r$cv_capacity,
      cv_capacity_margin = r$cv_capacity_margin,
      deterministic_cv0 = identical(as.numeric(r$cv), 0) || isTRUE(all.equal(r$cv, 0)),
      alpha = solution$alpha,
      beta = solution$beta,
      solved_mean = solution$mean,
      solved_sd = solution$sd,
      solved_cv = solution$cv,
      residual_mean = unname(solution$residual[["mean"]]),
      residual_sd = unname(solution$residual[["sd"]]),
      residual_max = residual_max,
      mean_rel_error = mean_rel_error,
      cv_abs_error = cv_abs_error,
      solver_start = solution$start,
      solver_termcd = solution$termcd,
      solver_message = solution$message,
      post_solve_pass = residual_max <= 0.01 && mean_rel_error <= 0.01 && cv_abs_error <= 0.01,
      strict_solver_residual_pass = residual_max <= 1e-6,
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
      cell_id = sprintf("fixture_%03d", row_id),
      row_id = row_id,
      seed = r$seed,
      status = "failed",
      J = r$J,
      nj_mean = r$nj_mean,
      cv = r$cv,
      nj_min = r$nj_min,
      cv_capacity = r$cv_capacity,
      cv_capacity_margin = r$cv_capacity_margin,
      deterministic_cv0 = identical(as.numeric(r$cv), 0) || isTRUE(all.equal(r$cv, 0)),
      alpha = NA_real_,
      beta = NA_real_,
      solved_mean = NA_real_,
      solved_sd = NA_real_,
      solved_cv = NA_real_,
      residual_mean = NA_real_,
      residual_sd = NA_real_,
      residual_max = NA_real_,
      mean_rel_error = NA_real_,
      cv_abs_error = NA_real_,
      solver_start = NA_integer_,
      solver_termcd = NA_integer_,
      solver_message = NA_character_,
      post_solve_pass = FALSE,
      strict_solver_residual_pass = FALSE,
      error_class = paste(class(e), collapse = "/"),
      error_message = conditionMessage(e),
      stringsAsFactors = FALSE
    )
  })
}

rows <- vector("list", nrow(grid))
for (row_id in seq_len(nrow(grid))) {
  if (row_id %% 20L == 0L || row_id == 1L || row_id == nrow(grid)) {
    message("V04 fixture ", row_id, " / ", nrow(grid))
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
  fixtures = nrow(results),
  completed = nrow(completed),
  failed = sum(results$status == "failed"),
  solver_error_count = sum(grepl("multisitedgp_solver_error", results$error_class, fixed = TRUE), na.rm = TRUE),
  deterministic_cv0_count = sum(results$deterministic_cv0),
  min_cv_capacity_margin = min(results$cv_capacity_margin),
  post_solve_pass_rate = mean(completed$post_solve_pass),
  strict_solver_residual_pass_rate = mean(completed$strict_solver_residual_pass),
  max_residual = max(completed$residual_max),
  max_mean_rel_error = max(completed$mean_rel_error),
  max_cv_abs_error = max(completed$cv_abs_error),
  acceptance_pass = nrow(completed) == nrow(results) &&
    all(completed$post_solve_pass) &&
    !any(grepl("multisitedgp_solver_error", results$error_class, fixed = TRUE), na.rm = TRUE),
  acceptance_note = "V04 checks internal A2 solved conditional moments on feasible truncated-Gamma targets; fixtures cap cv below a numerical capacity estimate for each n_bar/n_min pair.",
  stringsAsFactors = FALSE
)
summary_path <- validation_write_csv(summary, summary_path)

status <- if (isTRUE(summary$acceptance_pass)) "pass" else "fail"
ended_at <- Sys.time()
validation_record_manifest(paths, run_id, experiment_id, mode, status, started_at, ended_at, seed_root, fixtures, script_path, result_path, summary_path, "V04 Engine A2 solver convergence and post-solve residual evidence.")
print(summary)
message("V04 status: ", status)
validation_maybe_stop_for_blocker(status, "V04 validation failed acceptance criteria.")
