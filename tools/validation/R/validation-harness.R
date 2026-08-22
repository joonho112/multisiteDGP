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
  out <- tryCatch(
    suppressWarnings(system2(
      "git", c("-C", shQuote(package_root), "rev-parse", "HEAD"),
      stdout = TRUE, stderr = TRUE
    )),
    error = function(e) character()
  )
  if (is.null(attr(out, "status")) && length(out) == 1L && nzchar(out)) out else NA_character_
}

validation_worktree_clean <- function(package_root) {
  out <- tryCatch(
    suppressWarnings(system2(
      "git", c("-C", shQuote(package_root), "status", "--porcelain"),
      stdout = TRUE, stderr = TRUE
    )),
    error = function(e) NA_character_
  )
  if (!is.null(attr(out, "status")) || (length(out) == 1L && is.na(out))) return(NA)
  length(out) == 0L
}

validation_source_digest <- function(package_root) {
  roots <- c(
    file.path(package_root, "R"),
    file.path(package_root, "inst", "extdata"),
    file.path(package_root, "tests", "testthat", "_snaps", "golden"),
    file.path(package_root, "tools", "jebs-golden-fixtures"),
    file.path(package_root, "tools", "validation", "R")
  )
  files <- c(file.path(package_root, "DESCRIPTION"), file.path(package_root, "NAMESPACE"))
  for (root in roots[dir.exists(roots)]) {
    files <- c(files, list.files(root, recursive = TRUE, full.names = TRUE, all.files = TRUE))
  }
  # `sort()` without an explicit method follows the process collation locale.
  # A validation identity must be independent of whether it is computed by a
  # job script, testthat, or CI, so order paths by their byte representation.
  files <- sort(
    unique(files[file.exists(files) & !dir.exists(files)]),
    method = "radix"
  )
  relative <- vapply(files, .relative_to_root, character(1), package_root = package_root)
  hashes <- unname(tools::sha256sum(files))
  digest::digest(paste(relative, hashes, sep = "=", collapse = "\n"), algo = "sha256", serialize = FALSE)
}

.validation_normalize_parameters <- function(x) {
  if (is.list(x)) {
    if (!is.null(names(x))) x <- x[order(names(x))]
    return(lapply(x, .validation_normalize_parameters))
  }
  if (is.factor(x)) return(as.character(x))
  x
}

validation_parameter_text <- function(parameters) {
  parameters <- .validation_normalize_parameters(parameters)
  paste(capture.output(dput(parameters)), collapse = " ")
}

validation_parameter_digest <- function(parameters) {
  digest::digest(.validation_normalize_parameters(parameters), algo = "sha256", serialize = TRUE)
}

validation_contract_sidecar_path <- function(result_path) {
  sub("-results[.]csv$", "-contract.csv", result_path)
}

validation_contract_identity <- function(
  paths,
  run_id,
  experiment_id,
  mode,
  seed_root,
  parameters,
  script_path
) {
  hash_schema <- getFromNamespace(".hash_schema_version", "multisiteDGP")()
  data.frame(
    contract_schema_version = "phase5-validation-contract-v1",
    run_id = run_id,
    experiment_id = experiment_id,
    mode = mode,
    seed_root = as.integer(seed_root),
    parameter_text = validation_parameter_text(parameters),
    parameter_sha256 = validation_parameter_digest(parameters),
    script_path = .relative_to_root(script_path, paths$package_root),
    script_sha256 = validation_file_hash(script_path),
    package_version = as.character(utils::packageVersion("multisiteDGP")),
    source_git_sha = validation_git_sha(paths$package_root),
    source_worktree_clean = validation_worktree_clean(paths$package_root),
    source_digest_sha256 = validation_source_digest(paths$package_root),
    hash_schema_version = hash_schema,
    rng_kind = "Mersenne-Twister/Inversion/Rejection",
    rng_policy = "package-pinned",
    stringsAsFactors = FALSE
  )
}

validation_write_contract_sidecar <- function(
  paths,
  run_id,
  experiment_id,
  mode,
  status,
  seed_root,
  parameters,
  script_path,
  result_path,
  summary_path,
  ended_at
) {
  contract <- validation_contract_identity(
    paths, run_id, experiment_id, mode, seed_root, parameters, script_path
  )
  contract$producer_status <- status
  contract$produced_at <- format(ended_at, "%Y-%m-%d %H:%M:%S %Z")
  contract$result_path <- .relative_to_root(result_path, paths$package_root)
  contract$result_sha256 <- validation_file_hash(result_path)
  contract$summary_path <- .relative_to_root(summary_path, paths$package_root)
  contract$summary_sha256 <- validation_file_hash(summary_path)
  contract$r_version <- R.version.string
  contract$platform <- R.version$platform
  sidecar_path <- validation_contract_sidecar_path(result_path)
  validation_write_csv(contract, sidecar_path)
}

validation_prepare_run <- function(
  paths,
  run_id,
  experiment_id,
  mode,
  seed_root,
  parameters,
  script_path,
  result_path,
  summary_path,
  resume = FALSE,
  overwrite = FALSE
) {
  sidecar_path <- validation_contract_sidecar_path(result_path)
  present <- c(
    result = file.exists(result_path),
    summary = file.exists(summary_path),
    sidecar = file.exists(sidecar_path)
  )

  if (isTRUE(overwrite)) {
    return(list(action = "fresh", reason = "overwrite-explicit", sidecar = NULL))
  }
  if (!isTRUE(resume)) {
    if (any(present)) {
      stop(
        paste0(
          "Validation run ID already has artifacts and resume is disabled: ", run_id,
          ". Use a new run ID or set MULTISITEDGP_VALIDATION_OVERWRITE=true explicitly."
        ),
        call. = FALSE
      )
    }
    return(list(action = "fresh", reason = "resume-disabled", sidecar = NULL))
  }
  if (!any(present)) {
    return(list(action = "fresh", reason = "no-existing-artifacts", sidecar = NULL))
  }
  if (!all(present)) {
    stop(
      sprintf(
        "Refused validation reuse for `%s`: result, summary, and contract sidecar must all exist.",
        run_id
      ),
      call. = FALSE
    )
  }

  sidecar <- utils::read.csv(sidecar_path, stringsAsFactors = FALSE, check.names = FALSE)
  if (nrow(sidecar) != 1L) {
    stop(sprintf("Refused validation reuse for `%s`: malformed contract sidecar.", run_id), call. = FALSE)
  }
  current <- validation_contract_identity(
    paths, run_id, experiment_id, mode, seed_root, parameters, script_path
  )
  identity_fields <- names(current)
  missing_fields <- setdiff(
    c(identity_fields, "producer_status", "result_path", "result_sha256", "summary_path", "summary_sha256"),
    names(sidecar)
  )
  mismatches <- missing_fields
  for (field in setdiff(identity_fields, missing_fields)) {
    if (!identical(as.character(sidecar[[field]][[1L]]), as.character(current[[field]][[1L]]))) {
      mismatches <- c(mismatches, field)
    }
  }
  expected_paths <- c(
    result_path = .relative_to_root(result_path, paths$package_root),
    summary_path = .relative_to_root(summary_path, paths$package_root)
  )
  for (field in names(expected_paths)) {
    if (!field %in% names(sidecar) || !identical(sidecar[[field]][[1L]], expected_paths[[field]])) {
      mismatches <- c(mismatches, field)
    }
  }
  artifact_hashes <- c(
    result_sha256 = validation_file_hash(result_path),
    summary_sha256 = validation_file_hash(summary_path)
  )
  for (field in names(artifact_hashes)) {
    if (!field %in% names(sidecar) || !identical(sidecar[[field]][[1L]], artifact_hashes[[field]])) {
      mismatches <- c(mismatches, field)
    }
  }
  mismatches <- unique(mismatches)
  if (length(mismatches) > 0L) {
    stop(
      sprintf(
        "Refused validation reuse for `%s`: incompatible %s.",
        run_id,
        paste(mismatches, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  list(action = "reuse", reason = "contract-compatible", sidecar = sidecar, sidecar_path = sidecar_path)
}

.validation_append_manifest <- function(paths, row) {
  manifest <- if (file.exists(paths$manifest_path)) {
    utils::read.csv(paths$manifest_path, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    row[0, ]
  }
  all_names <- union(names(manifest), names(row))
  for (name in setdiff(all_names, names(manifest))) manifest[[name]] <- NA
  for (name in setdiff(all_names, names(row))) row[[name]] <- NA
  manifest <- manifest[all_names]
  row <- row[all_names]
  validation_write_csv(rbind(manifest, row), paths$manifest_path)
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
  notes = "",
  parameters = list(reps = reps)
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
    event = "produced",
    producer_status = status,
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
    result_schema_version = "phase5-validation-v2",
    package_version = as.character(utils::packageVersion("multisiteDGP")),
    git_sha = validation_git_sha(paths$package_root),
    r_version = R.version.string,
    platform = R.version$platform,
    notes = notes,
    stringsAsFactors = FALSE
  )
  sidecar_path <- validation_write_contract_sidecar(
    paths, run_id, experiment_id, mode, status, seed_root, parameters,
    script_path, result_path, summary_path, ended_at
  )
  row$contract_path <- .relative_to_root(sidecar_path, paths$package_root)
  row$contract_sha256 <- validation_file_hash(sidecar_path)
  row$parameter_sha256 <- validation_parameter_digest(parameters)
  row$source_digest_sha256 <- validation_source_digest(paths$package_root)
  row$source_worktree_clean <- validation_worktree_clean(paths$package_root)
  row$hash_schema_version <- getFromNamespace(".hash_schema_version", "multisiteDGP")()
  row$rng_policy <- "package-pinned"
  row$rng_kind <- "Mersenne-Twister/Inversion/Rejection"
  .validation_append_manifest(paths, row)
}

validation_record_reuse <- function(paths, reuse, started_at, ended_at, notes = "") {
  sidecar <- reuse$sidecar
  experiment <- validation_lookup_experiment(paths, sidecar$experiment_id[[1L]])
  row <- data.frame(
    run_id = sidecar$run_id[[1L]],
    experiment_id = sidecar$experiment_id[[1L]],
    experiment = experiment$experiment,
    priority = experiment$priority,
    owner_step = experiment$owner_step,
    mode = sidecar$mode[[1L]],
    status = "reused",
    event = "reuse",
    producer_status = sidecar$producer_status[[1L]],
    started_at = format(started_at, "%Y-%m-%d %H:%M:%S %Z"),
    ended_at = format(ended_at, "%Y-%m-%d %H:%M:%S %Z"),
    elapsed_sec = as.numeric(difftime(ended_at, started_at, units = "secs")),
    seed_root = sidecar$seed_root[[1L]],
    reps = NA_integer_,
    result_path = sidecar$result_path[[1L]],
    summary_path = sidecar$summary_path[[1L]],
    script_path = sidecar$script_path[[1L]],
    script_sha256 = sidecar$script_sha256[[1L]],
    result_sha256 = sidecar$result_sha256[[1L]],
    summary_sha256 = sidecar$summary_sha256[[1L]],
    result_schema_version = "phase5-validation-v2",
    package_version = sidecar$package_version[[1L]],
    git_sha = sidecar$source_git_sha[[1L]],
    r_version = sidecar$r_version[[1L]],
    platform = sidecar$platform[[1L]],
    contract_path = .relative_to_root(reuse$sidecar_path, paths$package_root),
    contract_sha256 = validation_file_hash(reuse$sidecar_path),
    parameter_sha256 = sidecar$parameter_sha256[[1L]],
    source_digest_sha256 = sidecar$source_digest_sha256[[1L]],
    source_worktree_clean = sidecar$source_worktree_clean[[1L]],
    hash_schema_version = sidecar$hash_schema_version[[1L]],
    rng_policy = sidecar$rng_policy[[1L]],
    rng_kind = sidecar$rng_kind[[1L]],
    notes = notes,
    stringsAsFactors = FALSE
  )
  .validation_append_manifest(paths, row)
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
