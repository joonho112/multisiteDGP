# nolint start: object_name_linter, object_usage_linter, function_body_linter
#' Audit a grid of multisite design cells against quality-gate thresholds
#'
#' @encoding UTF-8
#'
#' @description
#' Run `M` simulation replicates of each row of a \code{\link{design_grid}}
#' and summarize whether the cell clears the public quality-gate
#' thresholds. **The top-of-funnel diagnostic** of Dr. Chen's four-question
#' rubric: run this *before* committing a scenario grid to a long
#' simulation, so designs that cannot meet your minimum requirements
#' (feasibility, dependence target, distributional fit) are caught early.
#'
#' @details
#' For each cell of `grid`, `scenario_audit()` runs `M` replicates of
#' \code{\link{sim_multisite}} (or \code{\link{sim_meta}}, depending on
#' the cell's `paradigm`), computes the standard Group A/B/C/D
#' diagnostics on each replicate, and aggregates pass/fail status against
#' `thresholds` (defaulting to \code{\link{default_thresholds}}).
#'
#' \strong{Parallel evaluation.} Set `parallel = TRUE` to dispatch cells
#' through \code{furrr::future_map_dfr()} with the caller's active future
#' plan. The \pkg{furrr} package is a Suggests dependency.
#'
#' \strong{Recommended workflow.} (1) Build the grid with
#' \code{\link{design_grid}}; (2) run `scenario_audit(grid, M = 50L)` for
#' a fast pre-check; (3) drop or revise cells that fail the gates; (4)
#' run the long-form simulation only on the surviving cells.
#'
#' For a worked example see the
#' \href{../articles/a6-case-study-multisite.html}{Case study — multisite
#' trial} vignette.
#'
#' @param grid A `multisitedgp_design_grid` object from
#'   \code{\link{design_grid}}.
#' @param M Integer (\eqn{\ge 1}). Number of simulation replicates per
#'   cell. Default `200L`. Use `M = 50L` for a fast pre-check; raise to
#'   `200L`+ for stable across-replicate medians.
#' @param thresholds Named list of quality-gate thresholds. Missing
#'   entries are filled from \code{\link{default_thresholds}}.
#' @param parallel Logical. If `TRUE`, use \code{furrr::future_map_dfr()}
#'   with the caller's active future plan. Default `FALSE`.
#'
#' @return A tibble with one row per design-grid cell, columns include
#'   the cell's design parameters plus aggregated diagnostics and
#'   pass/fail flags.
#'
#' @family family-diagnostics
#' @seealso
#'   \code{\link{design_grid}} for building the input scenario grid;
#'   \code{\link{feasibility_index}} for the per-replicate Group A scalar;
#'   \code{\link{default_thresholds}} for the default quality gates;
#'   \code{\link{sim_multisite}} and \code{\link{sim_meta}} for the
#'   per-replicate simulators called inside;
#'   the \href{../articles/a3-diagnostics-in-practice.html}{A3 Diagnostics
#'   in practice} vignette.
#'
#' @references
#' Lee, J., Che, J., Rabe-Hesketh, S., Feller, A., & Miratrix, L. (2025).
#' Improving the estimation of site-specific effects and their distribution
#' in multisite trials. \emph{Journal of Educational and Behavioral Statistics},
#' \bold{50}(5), 731--764. \doi{10.3102/10769986241254286}.
#'
#' @examples
#' # Fast pre-check on a small grid.
#' grid <- design_grid(J = 10L, sigma_tau = c(0.10, 0.20), seed_root = 1L)
#' scenario_audit(grid, M = 1L)
#'
#' \dontrun{
#'   # Production audit — many replicates, parallel.
#'   future::plan(future::multisession, workers = 4)
#'   scenario_audit(grid, M = 200L, parallel = TRUE)
#' }
#' @export
scenario_audit <- function(
  grid,
  M = 200L,
  thresholds = default_thresholds(),
  parallel = FALSE
) {
  .validate_scenario_grid(grid)
  M <- .validate_scenario_m(M)
  thresholds <- .validate_scenario_thresholds(thresholds)
  parallel <- .validate_scalar_logical(parallel, "parallel")

  cell_ids <- seq_len(nrow(grid))
  audit_one <- function(cell_id) {
    .audit_scenario_cell(grid, cell_id = cell_id, M = M, thresholds = thresholds)
  }

  if (isTRUE(parallel)) {
    .require_soft_dependency("furrr", "scenario_audit")
    return(furrr::future_map_dfr(cell_ids, audit_one))
  }
  dplyr::bind_rows(lapply(cell_ids, audit_one))
}

.validate_scenario_grid <- function(grid) {
  if (!inherits(grid, "multisitedgp_design_grid")) {
    .abort_arg(
      "`grid` must be a multisitedgp_design_grid object.",
      "`scenario_audit()` consumes the classed output of `design_grid()`.",
      "Use `design_grid(..., seed_root = 12345L)` before calling `scenario_audit()`."
    )
  }
  if (!"design" %in% names(grid)) {
    .abort_arg(
      "`grid` must contain a `design` list-column.",
      "Each audit cell replays one `multisitedgp_design` object.",
      "Use `design_grid()` to create the audit input."
    )
  }
  if (nrow(grid) < 1L) {
    .abort_arg(
      "`grid` must contain at least one design cell.",
      "Scenario audit summaries are defined per grid row.",
      "Pass a non-empty `design_grid()` result."
    )
  }
  valid_design <- vapply(grid$design, is_multisitedgp_design, logical(1))
  if (!all(valid_design)) {
    .abort_arg(
      "`grid$design` must contain only multisitedgp_design objects.",
      sprintf("Invalid design rows: %s.", paste(which(!valid_design), collapse = ", ")),
      "Use `design_grid()` to rebuild the grid."
    )
  }
  invisible(grid)
}

.validate_scenario_m <- function(M) {
  M <- .validate_scalar_integer(M, "M")
  if (M < 1L) {
    .abort_arg(
      "`M` must be at least 1.",
      "`scenario_audit()` needs one or more replicates per design cell.",
      "Use `M = 1L` for a smoke audit or a larger integer for stability."
    )
  }
  M
}

.validate_scenario_thresholds <- function(thresholds) {
  defaults <- default_thresholds()
  if (!is.list(thresholds)) {
    .abort_arg(
      "`thresholds` must be a named list.",
      "`scenario_audit()` merges user gates with `default_thresholds()`.",
      "Pass `thresholds = default_thresholds()` or a named override list."
    )
  }
  if (length(thresholds) > 0L && (is.null(names(thresholds)) || any(!nzchar(names(thresholds))))) {
    .abort_arg(
      "`thresholds` entries must be named.",
      "Threshold names identify the quality gate to override.",
      sprintf("Use names from `default_thresholds()`: %s.", paste(names(defaults), collapse = ", "))
    )
  }
  bad <- setdiff(names(thresholds), names(defaults))
  if (length(bad) > 0L) {
    .abort_arg(
      "Unknown `thresholds` entry.",
      sprintf("Unsupported threshold names: %s.", paste(bad, collapse = ", ")),
      sprintf("Use names from `default_thresholds()`: %s.", paste(names(defaults), collapse = ", "))
    )
  }

  out <- defaults
  out[names(thresholds)] <- thresholds
  out$mean_shrinkage_min <- .validate_unit_threshold(out$mean_shrinkage_min, "mean_shrinkage_min")
  out$feasibility_min <- .validate_nonnegative_threshold(out$feasibility_min, "feasibility_min")
  out$R_max <- .validate_positive_threshold(out$R_max, "R_max")
  out$bhattacharyya_min <- .validate_unit_threshold(out$bhattacharyya_min, "bhattacharyya_min")
  out$ks_max <- .validate_unit_threshold(out$ks_max, "ks_max")
  out
}

.validate_positive_threshold <- function(x, arg) {
  x <- .validate_scalar_number(x, arg)
  if (x <= 0) {
    .abort_arg(
      sprintf("`%s` must be positive.", arg),
      "Scenario audit thresholds are scalar numeric quality gates.",
      sprintf("Pass `%s` as a positive number.", arg)
    )
  }
  x
}

.validate_nonnegative_threshold <- function(x, arg) {
  x <- .validate_scalar_number(x, arg)
  if (x < 0) {
    .abort_arg(
      sprintf("`%s` must be non-negative.", arg),
      "Scenario audit thresholds are scalar numeric quality gates.",
      sprintf("Pass `%s` as zero or a positive number.", arg)
    )
  }
  x
}

.validate_unit_threshold <- function(x, arg) {
  x <- .validate_scalar_number(x, arg)
  if (x < 0 || x > 1) {
    .abort_arg(
      sprintf("`%s` must be between 0 and 1.", arg),
      "This threshold is on a unit interval diagnostic scale.",
      sprintf("Pass `%s` as a probability-like value.", arg)
    )
  }
  x
}

.audit_scenario_cell <- function(grid, cell_id, M, thresholds) {
  design <- grid$design[[cell_id]]
  seed_root <- .scenario_cell_seed_root(grid, cell_id, design)
  replicate_seeds <- .scenario_replicate_seeds(M, seed_root)
  reps <- lapply(seq_len(M), function(rep_id) {
    .audit_scenario_replicate(
      design = design,
      cell_id = cell_id,
      rep_id = rep_id,
      seed = replicate_seeds[[rep_id]]
    )
  })
  rep_metrics <- dplyr::bind_rows(reps)
  summary <- .summarize_scenario_metrics(rep_metrics, thresholds = thresholds)
  manifest <- .scenario_cell_manifest(grid, cell_id, design, M, seed_root)
  dplyr::bind_cols(manifest, summary)
}

.scenario_cell_seed_root <- function(grid, cell_id, design) {
  if ("seed" %in% names(grid) && !is.na(grid$seed[[cell_id]])) {
    return(as.integer(grid$seed[[cell_id]]))
  }
  if (!is.null(design$seed)) {
    return(as.integer(design$seed))
  }
  .abort_arg(
    "`scenario_audit()` requires deterministic cell seeds.",
    "Audit replicates must be reproducible and must not consume the caller's global RNG state.",
    "Use `design_grid(..., seed_root = 12345L)` or supply a base design with a fixed `seed`."
  )
}

.scenario_replicate_seeds <- function(M, seed_root) {
  if (is.na(seed_root)) {
    return(rep(NA_integer_, M))
  }
  .local_seed_stream(M, seed_root)
}

.audit_scenario_replicate <- function(design, cell_id, rep_id, seed) {
  dat <- .simulate_scenario_replicate(design, seed)
  diag <- attr(dat, "diagnostics", exact = TRUE)
  tibble::tibble(
    cell_id = cell_id,
    rep = rep_id,
    seed = if (is.na(seed)) NA_integer_ else as.integer(seed),
    I_hat = diag$I_hat,
    R_hat = diag$R_hat,
    mean_shrinkage = mean_shrinkage(dat),
    feasibility_efron = feasibility_index(dat, kind = "efron", warn = FALSE),
    feasibility_morris = feasibility_index(dat, kind = "morris", warn = FALSE),
    bhattacharyya = bhattacharyya_coef(dat),
    ks = ks_distance(dat)
  )
}

.simulate_scenario_replicate <- function(design, seed) {
  if (identical(design$paradigm, "direct")) {
    if (is.na(seed)) {
      return(sim_meta(design))
    }
    return(sim_meta(design, seed = seed))
  }
  if (is.na(seed)) {
    return(sim_multisite(design))
  }
  sim_multisite(design, seed = seed)
}

.scenario_cell_manifest <- function(grid, cell_id, design, M, seed_root) {
  cells <- tibble::as_tibble(grid[cell_id, setdiff(names(grid), c("design", "seed")), drop = FALSE])
  cells$cell_id <- cell_id
  cells$M <- M
  cells$seed_root <- seed_root
  cells$design_hash <- canonical_hash(design)
  cells[c("cell_id", setdiff(names(cells), "cell_id"))]
}

.summarize_scenario_metrics <- function(rep_metrics, thresholds) {
  q <- function(x, prob) unname(stats::quantile(x, prob, names = FALSE, type = 7, na.rm = TRUE))
  med <- function(x) stats::median(x, na.rm = TRUE)

  values <- list(
    mean_shrinkage = list(med = med(rep_metrics$mean_shrinkage), q05 = q(rep_metrics$mean_shrinkage, 0.05)),
    feasibility = list(med = med(rep_metrics$feasibility_efron), q05 = q(rep_metrics$feasibility_efron, 0.05)),
    R = list(med = med(rep_metrics$R_hat), q95 = q(rep_metrics$R_hat, 0.95)),
    bhattacharyya = list(med = med(rep_metrics$bhattacharyya), q05 = q(rep_metrics$bhattacharyya, 0.05)),
    ks = list(med = med(rep_metrics$ks), q95 = q(rep_metrics$ks, 0.95))
  )

  fail_reasons <- .scenario_fail_reasons(values, thresholds)
  warn_reasons <- if (length(fail_reasons) == 0L) {
    .scenario_warn_reasons(values, thresholds)
  } else {
    character()
  }
  status <- if (length(fail_reasons) > 0L) {
    "FAIL"
  } else if (length(warn_reasons) > 0L) {
    "WARN"
  } else {
    "PASS"
  }

  tibble::tibble(
    status = status,
    pass = !identical(status, "FAIL"),
    n_violations = length(fail_reasons),
    fail_reasons = paste(fail_reasons, collapse = "; "),
    warn_reasons = paste(warn_reasons, collapse = "; "),
    med_I_hat = med(rep_metrics$I_hat),
    q05_I_hat = q(rep_metrics$I_hat, 0.05),
    q95_I_hat = q(rep_metrics$I_hat, 0.95),
    med_R_hat = values$R$med,
    q95_R_hat = values$R$q95,
    med_mean_shrinkage = values$mean_shrinkage$med,
    q05_mean_shrinkage = values$mean_shrinkage$q05,
    med_feasibility_efron = values$feasibility$med,
    q05_feasibility_efron = values$feasibility$q05,
    med_feasibility_morris = med(rep_metrics$feasibility_morris),
    med_bhattacharyya = values$bhattacharyya$med,
    q05_bhattacharyya = values$bhattacharyya$q05,
    med_ks = values$ks$med,
    q95_ks = values$ks$q95
  )
}

.scenario_fail_reasons <- function(values, thresholds) {
  reasons <- character()
  if (.gate_low_fail(values$mean_shrinkage$med, thresholds$mean_shrinkage_min)) {
    reasons <- c(reasons, sprintf("mean_shrinkage:median<%.3f", thresholds$mean_shrinkage_min))
  }
  if (.gate_low_fail(values$feasibility$med, thresholds$feasibility_min)) {
    reasons <- c(reasons, sprintf("feasibility:median<%.3f", thresholds$feasibility_min))
  }
  if (.gate_high_fail(values$R$med, thresholds$R_max)) {
    reasons <- c(reasons, sprintf("R:median>%.3f", thresholds$R_max))
  }
  if (.gate_low_fail(values$bhattacharyya$med, thresholds$bhattacharyya_min)) {
    reasons <- c(reasons, sprintf("bhattacharyya:median<%.3f", thresholds$bhattacharyya_min))
  }
  if (.gate_high_fail(values$ks$med, thresholds$ks_max)) {
    reasons <- c(reasons, sprintf("ks:median>%.3f", thresholds$ks_max))
  }
  reasons
}

.scenario_warn_reasons <- function(values, thresholds) {
  reasons <- character()
  if (.gate_low_warn(values$mean_shrinkage$q05, thresholds$mean_shrinkage_min)) {
    reasons <- c(reasons, sprintf("mean_shrinkage:q05<%.3f", thresholds$mean_shrinkage_min))
  }
  if (.gate_low_warn(values$feasibility$q05, thresholds$feasibility_min)) {
    reasons <- c(reasons, sprintf("feasibility:q05<%.3f", thresholds$feasibility_min))
  }
  if (.gate_high_warn(values$R$q95, thresholds$R_max)) {
    reasons <- c(reasons, sprintf("R:q95>%.3f", thresholds$R_max))
  }
  if (.gate_low_warn(values$bhattacharyya$q05, thresholds$bhattacharyya_min)) {
    reasons <- c(reasons, sprintf("bhattacharyya:q05<%.3f", thresholds$bhattacharyya_min))
  }
  if (.gate_high_warn(values$ks$q95, thresholds$ks_max)) {
    reasons <- c(reasons, sprintf("ks:q95>%.3f", thresholds$ks_max))
  }
  reasons
}

.gate_low_fail <- function(value, threshold) {
  !is.finite(value) || value < threshold
}

.gate_high_fail <- function(value, threshold) {
  !is.finite(value) || value > threshold
}

.gate_low_warn <- function(value, threshold) {
  is.finite(value) && value < threshold
}

.gate_high_warn <- function(value, threshold) {
  is.finite(value) && value > threshold
}
# nolint end
