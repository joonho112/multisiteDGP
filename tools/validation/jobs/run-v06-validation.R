#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg) == 1L) {
  normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE)
} else {
  normalizePath("tools/validation/jobs/run-v06-validation.R", mustWork = TRUE)
}
source(file.path(dirname(script_path), "..", "R", "validation-harness.R"))

paths <- validation_paths(script_path)
validation_load_package(paths$package_root)

experiment_id <- "V06"
mode <- Sys.getenv("MULTISITEDGP_VALIDATION_MODE", unset = "full")
sample_n <- validation_env_int("MULTISITEDGP_VALIDATION_SAMPLE_N", if (identical(mode, "full")) 1000000L else 100000L)
seed_root <- validation_env_int("MULTISITEDGP_VALIDATION_SEED_ROOT", 910601L)
resume <- validation_env_flag("MULTISITEDGP_VALIDATION_RESUME", default = FALSE)
overwrite <- validation_env_flag("MULTISITEDGP_VALIDATION_OVERWRITE", default = FALSE)
run_id <- validation_run_id(experiment_id, mode)

result_path <- file.path(paths$generated_dir, paste0(run_id, "-results.csv"))
summary_path <- file.path(paths$generated_dir, paste0(run_id, "-summary.csv"))
started_at <- Sys.time()
parameters <- list(sample_n = sample_n)
run_state <- validation_prepare_run(
  paths, run_id, experiment_id, mode, seed_root, parameters, script_path,
  result_path, summary_path, resume = resume, overwrite = overwrite
)

if (identical(run_state$action, "reuse")) {
  summary <- utils::read.csv(summary_path, stringsAsFactors = FALSE)
  status <- if (nrow(summary) == 1L && isTRUE(summary$acceptance_pass[[1L]])) "pass" else "fail"
  ended_at <- Sys.time()
  validation_record_reuse(paths, run_state, started_at, ended_at, "Compatible V06 output reused.")
  message("Resumed existing V06 output: ", result_path)
  validation_maybe_stop_for_blocker(status, "V06 validation failed in resumed output.")
  quit(status = 0)
}

user_grid <- function(J, reverse = FALSE, phase = 0, ...) {
  x <- stats::qnorm((seq_len(J) - 0.5) / J)
  shift <- as.integer(round(phase * J)) %% J
  if (shift > 0L) {
    x <- c(x[(shift + 1L):J], x[seq_len(shift)])
  }
  if (isTRUE(reverse)) {
    x <- rev(x)
  }
  x
}

shape_grid <- data.frame(
  shape = rep(c("Gaussian", "StudentT", "SkewN", "ALD", "Mixture", "PointMassSlab", "User", "DPM"), each = 5L),
  param_id = rep(seq_len(5L), times = 8L),
  stringsAsFactors = FALSE
)
make_spec <- function(shape, param_id) {
  if (identical(shape, "Gaussian")) {
    return(list(true_dist = "Gaussian", theta_G = list(), g_fn = NULL, label = "variance_1"))
  }
  if (identical(shape, "StudentT")) {
    nu <- c(3, 5, 10, 20, 50)[[param_id]]
    return(list(true_dist = "StudentT", theta_G = list(nu = nu), g_fn = NULL, label = paste0("nu_", nu)))
  }
  if (identical(shape, "SkewN")) {
    slant <- c(-4, -1, 0, 1, 4)[[param_id]]
    return(list(true_dist = "SkewN", theta_G = list(slant = slant), g_fn = NULL, label = paste0("slant_", slant)))
  }
  if (identical(shape, "ALD")) {
    rho <- c(0.25, 0.5, 0.7, 0.3, 0.85)[[param_id]]
    return(list(true_dist = "ALD", theta_G = list(rho = rho), g_fn = NULL, label = paste0("rho_", rho)))
  }
  if (identical(shape, "Mixture")) {
    params <- list(
      list(delta = 5, eps = 0.3, ups = 2),
      list(delta = 3, eps = 0.2, ups = 1),
      list(delta = 4, eps = 0.5, ups = 0.5),
      list(delta = 6, eps = 0.7, ups = 3),
      list(delta = 2, eps = 0.4, ups = 1.5)
    )[[param_id]]
    return(list(true_dist = "Mixture", theta_G = params, g_fn = NULL, label = paste(names(params), unlist(params), sep = "=", collapse = ";")))
  }
  if (identical(shape, "PointMassSlab")) {
    params <- list(
      list(pi0 = 0.2, slab_shape = "Gaussian"),
      list(pi0 = 0.5, slab_shape = "Gaussian"),
      list(pi0 = 0.8, slab_shape = "Gaussian"),
      list(pi0 = 0.35, slab_shape = "Laplace"),
      list(pi0 = 0.65, slab_shape = "Laplace")
    )[[param_id]]
    return(list(true_dist = "PointMassSlab", theta_G = params, g_fn = NULL, label = paste(names(params), unlist(params), sep = "=", collapse = ";")))
  }
  if (identical(shape, "User")) {
    params <- list(
      list(reverse = FALSE, phase = 0),
      list(reverse = TRUE, phase = 0),
      list(reverse = FALSE, phase = 0.10),
      list(reverse = TRUE, phase = 0.25),
      list(reverse = FALSE, phase = 0.50)
    )[[param_id]]
    return(list(true_dist = "User", theta_G = params, g_fn = user_grid, label = paste(names(params), unlist(params), sep = "=", collapse = ";")))
  }
  list(true_dist = "DPM", theta_G = list(), g_fn = NULL, label = "v1_stub")
}

moment_tolerances <- function(shape, spec) {
  if (identical(shape, "StudentT") && isTRUE(spec$theta_G$nu < 4)) {
    return(list(
      mean_tolerance = 0.005,
      var_tolerance = 0.05,
      var_tolerance_source = "studentt_infinite_kurtosis"
    ))
  }
  list(
    mean_tolerance = 0.005,
    var_tolerance = 0.01,
    var_tolerance_source = "standard_large_sample"
  )
}

seeds <- validation_seed_stream(nrow(shape_grid), seed_root)

run_one <- function(row_id) {
  row <- shape_grid[row_id, ]
  spec <- make_spec(row$shape, row$param_id)
  tolerances <- moment_tolerances(row$shape, spec)
  if (identical(row$shape, "DPM")) {
    return(data.frame(
      result_schema_version = "phase9-validation-v1",
      run_id = run_id,
      experiment_id = experiment_id,
      mode = mode,
      cell_id = sprintf("%s_%02d", row$shape, row$param_id),
      row_id = row_id,
      seed = NA_integer_,
      status = "skipped",
      skip_reason = "DPM is a v1 built-in stub; explicit skip per ch19 and validation plan.",
      shape = row$shape,
      param_id = row$param_id,
      param_label = spec$label,
      sample_n = sample_n,
      mean_z = NA_real_,
      var_z = NA_real_,
      abs_mean = NA_real_,
      abs_var_delta = NA_real_,
      mean_tolerance = tolerances$mean_tolerance,
      var_tolerance = tolerances$var_tolerance,
      var_tolerance_source = tolerances$var_tolerance_source,
      mean_pass = NA,
      var_pass = NA,
      acceptance_pass = NA,
      error_class = NA_character_,
      error_message = NA_character_,
      stringsAsFactors = FALSE
    ))
  }
  tryCatch({
    out <- withr::with_seed(seeds[[row_id]], suppressWarnings(multisiteDGP::gen_effects(
      J = sample_n,
      true_dist = spec$true_dist,
      theta_G = spec$theta_G,
      g_fn = spec$g_fn,
      g_returns = "standardized",
      audit_g = FALSE
    )))
    mean_z <- mean(out$z_j)
    var_z <- stats::var(out$z_j)
    data.frame(
      result_schema_version = "phase9-validation-v1",
      run_id = run_id,
      experiment_id = experiment_id,
      mode = mode,
      cell_id = sprintf("%s_%02d", row$shape, row$param_id),
      row_id = row_id,
      seed = seeds[[row_id]],
      status = "completed",
      skip_reason = NA_character_,
      shape = row$shape,
      param_id = row$param_id,
      param_label = spec$label,
      sample_n = sample_n,
      mean_z = mean_z,
      var_z = var_z,
      abs_mean = abs(mean_z),
      abs_var_delta = abs(var_z - 1),
      mean_tolerance = tolerances$mean_tolerance,
      var_tolerance = tolerances$var_tolerance,
      var_tolerance_source = tolerances$var_tolerance_source,
      mean_pass = abs(mean_z) < tolerances$mean_tolerance,
      var_pass = abs(var_z - 1) < tolerances$var_tolerance,
      acceptance_pass = abs(mean_z) < tolerances$mean_tolerance && abs(var_z - 1) < tolerances$var_tolerance,
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
      cell_id = sprintf("%s_%02d", row$shape, row$param_id),
      row_id = row_id,
      seed = seeds[[row_id]],
      status = "failed",
      skip_reason = NA_character_,
      shape = row$shape,
      param_id = row$param_id,
      param_label = spec$label,
      sample_n = sample_n,
      mean_z = NA_real_,
      var_z = NA_real_,
      abs_mean = NA_real_,
      abs_var_delta = NA_real_,
      mean_tolerance = tolerances$mean_tolerance,
      var_tolerance = tolerances$var_tolerance,
      var_tolerance_source = tolerances$var_tolerance_source,
      mean_pass = FALSE,
      var_pass = FALSE,
      acceptance_pass = FALSE,
      error_class = paste(class(e), collapse = "/"),
      error_message = conditionMessage(e),
      stringsAsFactors = FALSE
    )
  })
}

rows <- vector("list", nrow(shape_grid))
for (row_id in seq_len(nrow(shape_grid))) {
  if (row_id %% 5L == 0L || row_id == 1L) {
    message("V06 row ", row_id, " / ", nrow(shape_grid))
  }
  rows[[row_id]] <- run_one(row_id)
}
results <- do.call(rbind, rows)
result_path <- validation_write_csv(results, result_path)

active <- results[results$status != "skipped", , drop = FALSE]
completed <- active[active$status == "completed", , drop = FALSE]
shape_summary <- do.call(rbind, lapply(split(completed, completed$shape), function(x) {
  data.frame(
    run_id = run_id,
    experiment_id = experiment_id,
    mode = mode,
    shape = x$shape[[1L]],
    rows = nrow(x),
    completed = sum(x$status == "completed"),
    acceptance_pass_rate = mean(x$acceptance_pass),
    mean_pass_rate = mean(x$mean_pass),
    var_pass_rate = mean(x$var_pass),
    max_abs_mean = max(x$abs_mean),
    max_abs_var_delta = max(x$abs_var_delta),
    max_var_tolerance = max(x$var_tolerance),
    heavy_tail_tolerance_rows = sum(x$var_tolerance_source == "studentt_infinite_kurtosis"),
    stringsAsFactors = FALSE
  )
}))
summary <- data.frame(
  run_id = run_id,
  experiment_id = experiment_id,
  mode = mode,
  sample_n = sample_n,
  rows = nrow(results),
  active_rows = nrow(active),
  completed = nrow(completed),
  failed = sum(active$status == "failed"),
  skipped = sum(results$status == "skipped"),
  active_shape_count = length(unique(active$shape)),
  mean_pass_rate = mean(completed$mean_pass),
  var_pass_rate = mean(completed$var_pass),
  acceptance_pass_rate = mean(completed$acceptance_pass),
  max_abs_mean = max(completed$abs_mean),
  max_abs_var_delta = max(completed$abs_var_delta),
  max_var_tolerance = max(completed$var_tolerance),
  heavy_tail_tolerance_rows = sum(completed$var_tolerance_source == "studentt_infinite_kurtosis"),
  ald_required_pass = all(completed$acceptance_pass[completed$shape == "ALD" & completed$param_label %in% c("rho_0.25", "rho_0.5", "rho_0.7")]),
  mixture_ups_coverage = all(c("ups=2", "ups=1", "ups=0.5", "ups=3", "ups=1.5") %in% sub(".*ups=", "ups=", completed$param_label[completed$shape == "Mixture"])),
  acceptance_pass = nrow(completed) == 35L &&
    sum(results$status == "skipped" & results$shape == "DPM") == 5L &&
    all(completed$acceptance_pass),
  acceptance_note = "Active v1 shapes are gated; DPM is recorded as five explicit skipped v1-stub rows and is not counted as accepted. StudentT nu < 4 uses a 0.05 variance tolerance because the fourth moment is non-finite and sample variance convergence is heavy-tailed.",
  stringsAsFactors = FALSE
)
summary_path <- validation_write_csv(summary, summary_path)
validation_write_csv(shape_summary, file.path(paths$generated_dir, paste0(run_id, "-shape-summary.csv")))

status <- if (isTRUE(summary$acceptance_pass)) "pass" else "fail"
ended_at <- Sys.time()
validation_record_manifest(paths, run_id, experiment_id, mode, status, started_at, ended_at, seed_root, nrow(results), script_path, result_path, summary_path, "V06 active G shape standardization with explicit reserved-DPM skip.", parameters = parameters)
print(summary)
print(shape_summary)
message("V06 status: ", status)
validation_maybe_stop_for_blocker(status, "V06 validation failed acceptance criteria.")
