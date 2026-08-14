# Shared helpers for Phase 9 validation experiments.
# These are development-time tools, not package exports.

validation_script_path <- function(default_relative) {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) == 1L) {
    return(normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE))
  }
  normalizePath(default_relative, mustWork = TRUE)
}

validation_paths <- function(script_path, package_root = NULL) {
  if (is.null(package_root)) {
    package_root <- .validation_find_package_root(dirname(script_path))
  } else {
    package_root <- normalizePath(package_root, mustWork = TRUE)
  }
  validation_dir <- file.path(package_root, "tools", "validation")
  generated_dir <- file.path(validation_dir, "generated")
  dir.create(generated_dir, recursive = TRUE, showWarnings = FALSE)

  list(
    package_root = package_root,
    validation_dir = validation_dir,
    jobs_dir = file.path(validation_dir, "jobs"),
    reports_dir = file.path(validation_dir, "reports"),
    generated_dir = generated_dir,
    manifest_path = file.path(generated_dir, "validation-run-manifest.csv"),
    plan_manifest_path = file.path(validation_dir, "validation-plan-manifest.csv"),
    index_path = file.path(package_root, "tools", "traceability", "validation-index.csv")
  )
}

.validation_find_package_root <- function(start_dir) {
  current <- normalizePath(start_dir, mustWork = TRUE)
  repeat {
    if (file.exists(file.path(current, "DESCRIPTION")) && dir.exists(file.path(current, "R"))) {
      return(current)
    }
    parent <- dirname(current)
    if (identical(parent, current)) {
      stop("Could not find package root above validation script.", call. = FALSE)
    }
    current <- parent
  }
}

validation_load_package <- function(package_root) {
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    stop("pkgload is required to load multisiteDGP from source.", call. = FALSE)
  }
  pkgload::load_all(package_root, quiet = TRUE)
  invisible(TRUE)
}

validation_experiment_index <- function(paths) {
  utils::read.csv(paths$index_path, stringsAsFactors = FALSE)
}

validation_lookup_experiment <- function(paths, experiment_id) {
  index <- validation_experiment_index(paths)
  row <- index[index$id == experiment_id, , drop = FALSE]
  if (nrow(row) != 1L) {
    stop(sprintf("Expected one validation-index row for `%s`.", experiment_id), call. = FALSE)
  }
  row
}

validation_env_flag <- function(name, default = FALSE) {
  value <- Sys.getenv(name, unset = if (isTRUE(default)) "true" else "false")
  tolower(value) %in% c("1", "true", "yes", "y")
}

validation_env_int <- function(name, default) {
  value <- Sys.getenv(name, unset = as.character(default))
  out <- suppressWarnings(as.integer(value))
  if (length(out) != 1L || is.na(out)) {
    stop(sprintf("Environment variable `%s` must be a single integer.", name), call. = FALSE)
  }
  out
}

validation_run_id <- function(experiment_id, mode) {
  override <- Sys.getenv("MULTISITEDGP_VALIDATION_RUN_ID", unset = "")
  if (nzchar(override)) {
    return(override)
  }
  timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  paste(tolower(experiment_id), tolower(mode), timestamp, sep = "-")
}

validation_seed_stream <- function(n, seed_root) {
  n <- as.integer(n)
  seed_root <- as.integer(seed_root)
  if (length(n) != 1L || is.na(n) || n < 1L) {
    stop("`n` must be a positive integer.", call. = FALSE)
  }
  if (length(seed_root) != 1L || is.na(seed_root)) {
    stop("`seed_root` must be a single integer.", call. = FALSE)
  }

  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) else NULL
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  set.seed(seed_root)
  sample.int(.Machine$integer.max, n)
}

validation_write_csv <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(x, path, row.names = FALSE, quote = TRUE)
  normalizePath(path, mustWork = TRUE)
}

validation_file_hash <- function(path) {
  if (length(path) != 1L || is.na(path) || !file.exists(path)) {
    return(NA_character_)
  }
  unname(tools::sha256sum(path))
}

validation_existing_run_complete <- function(result_path, summary_path = NULL) {
  file.exists(result_path) && (is.null(summary_path) || file.exists(summary_path))
}

validation_git_sha <- function(package_root) {
  git_dir <- file.path(package_root, ".git")
  if (!dir.exists(git_dir)) {
    return(NA_character_)
  }
  out <- tryCatch(
    system2("git", c("-C", package_root, "rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE),
    error = function(e) character()
  )
  if (length(out) == 1L && nzchar(out)) out else NA_character_
}

validation_record_manifest <- function(
  paths,
  run_id,
  experiment_id,
  mode,
  status,
  started_at,
  ended_at,
  seed_root,
  reps,
  script_path,
  result_path,
  summary_path = NA_character_,
  notes = ""
) {
  experiment <- validation_lookup_experiment(paths, experiment_id)
  elapsed_sec <- as.numeric(difftime(ended_at, started_at, units = "secs"))
  row <- data.frame(
    run_id = run_id,
    experiment_id = experiment_id,
    experiment = experiment$experiment,
    priority = experiment$priority,
    owner_step = experiment$owner_step,
    mode = mode,
    status = status,
    started_at = format(started_at, "%Y-%m-%d %H:%M:%S %Z"),
    ended_at = format(ended_at, "%Y-%m-%d %H:%M:%S %Z"),
    elapsed_sec = elapsed_sec,
    seed_root = seed_root,
    reps = reps,
    result_path = .relative_to_root(result_path, paths$package_root),
    summary_path = .relative_to_root(summary_path, paths$package_root),
    script_path = .relative_to_root(script_path, paths$package_root),
    script_sha256 = validation_file_hash(script_path),
    result_sha256 = validation_file_hash(result_path),
    summary_sha256 = validation_file_hash(summary_path),
    result_schema_version = "phase9-validation-v1",
    package_version = as.character(utils::packageVersion("multisiteDGP")),
    git_sha = validation_git_sha(paths$package_root),
    r_version = R.version.string,
    platform = R.version$platform,
    notes = notes,
    stringsAsFactors = FALSE
  )

  manifest <- if (file.exists(paths$manifest_path)) {
    utils::read.csv(paths$manifest_path, stringsAsFactors = FALSE)
  } else {
    row[0, ]
  }
  if (nrow(manifest) > 0L) {
    same_run <- manifest$run_id == run_id &
      manifest$experiment_id == experiment_id &
      manifest$mode == mode &
      manifest$script_path == row$script_path
    manifest <- manifest[!same_run, , drop = FALSE]
  }
  manifest <- rbind(manifest, row)
  validation_write_csv(manifest, paths$manifest_path)
}

.relative_to_root <- function(path, package_root) {
  if (length(path) != 1L || is.na(path) || !nzchar(path)) {
    return(NA_character_)
  }
  normalized <- normalizePath(path, mustWork = FALSE)
  root <- normalizePath(package_root, mustWork = TRUE)
  prefix <- paste0(root, .Platform$file.sep)
  if (startsWith(normalized, prefix)) {
    return(substring(normalized, nchar(prefix) + 1L))
  }
  normalized
}

validation_stop_if_missing_columns <- function(x, required, label) {
  missing <- setdiff(required, names(x))
  if (length(missing) > 0L) {
    stop(
      sprintf("%s is missing required column(s): %s.", label, paste(missing, collapse = ", ")),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

validation_standardized_g <- function(J, reverse = FALSE, shift = 0) {
  z <- stats::qnorm((seq_len(J) - 0.5) / J)
  if (isTRUE(reverse)) {
    z <- rev(z)
  }
  z + shift
}

validation_shape_specs <- function(dpm = c("bridge", "skip", "omit")) {
  dpm <- match.arg(dpm)
  specs <- list(
    Gaussian = list(true_dist = "Gaussian", theta_G = list(), g_fn = NULL, g_returns = "standardized"),
    StudentT = list(true_dist = "StudentT", theta_G = list(nu = 5), g_fn = NULL, g_returns = "standardized"),
    SkewN = list(true_dist = "SkewN", theta_G = list(slant = 0.7), g_fn = NULL, g_returns = "standardized"),
    ALD = list(true_dist = "ALD", theta_G = list(rho = 0.5), g_fn = NULL, g_returns = "standardized"),
    Mixture = list(
      true_dist = "Mixture",
      theta_G = list(delta = 5, eps = 0.3, ups = 2),
      g_fn = NULL,
      g_returns = "standardized"
    ),
    PointMassSlab = list(
      true_dist = "PointMassSlab",
      theta_G = list(pi0 = 0.3, slab_shape = "Gaussian"),
      g_fn = NULL,
      g_returns = "standardized"
    ),
    User = list(
      true_dist = "User",
      theta_G = list(),
      g_fn = validation_standardized_g,
      g_returns = "standardized"
    )
  )
  if (identical(dpm, "bridge")) {
    specs$DPM <- list(
      true_dist = "DPM",
      theta_G = list(),
      g_fn = validation_standardized_g,
      g_returns = "standardized"
    )
  } else if (identical(dpm, "skip")) {
    specs$DPM <- list(
      true_dist = "DPM",
      theta_G = list(),
      g_fn = NULL,
      g_returns = "standardized",
      skip_reason = "DPM is a v1 built-in stub; explicit skip per ch19 and validation plan."
    )
  }
  specs
}

validation_maybe_stop_for_blocker <- function(status, message) {
  if (identical(status, "fail") && validation_env_flag("MULTISITEDGP_VALIDATION_FAIL_ON_BLOCKER", default = FALSE)) {
    stop(message, call. = FALSE)
  }
  invisible(status)
}
