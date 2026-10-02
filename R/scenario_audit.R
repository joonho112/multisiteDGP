#' Simulate a grid of designs and summarize the diagnostics
#'
#' Simulates each row of a [design_grid()] `M` times and summarizes the
#' diagnostics by their median and their 5th and 95th percentiles. Each row
#' gets a status from comparing the summaries with `thresholds`.
#'
#' @details
#' `status` is `"fail"` when a median crosses its threshold: mean shrinkage
#' below `mean_shrinkage_min`, heterogeneity ratio above `R_max`, or KS
#' distance above `ks_scaled_max / sqrt(J)`. If none does, it is `"warn"` when
#' the 5th percentile of mean shrinkage is below its minimum or a 95th
#' percentile of the other two is above its maximum, and `"pass"` otherwise.
#'
#' The KS distance of a correctly simulated data set shrinks with the number
#' of sites like `1 / sqrt(J)`, so its threshold is given for the distance
#' times `sqrt(J)` and is the same for every `J`. The KS distance and the
#' Bhattacharyya coefficient are computed only for `true_dist = "Gaussian"`
#' and `"StudentT"`. The Bhattacharyya coefficient is summarized but has no
#' threshold: it rises with `J`, from a median near 0.5 at 25 sites to 0.99 at
#' 1,000.
#'
#' @param grid A grid from [design_grid()]. Every row needs a seed, which
#'   `design_grid()` assigns when `seed_root` is given.
#' @param M Integer number of simulations per row (at least 1). Default `200`.
#' @param thresholds Named list of thresholds. Entries not given are taken from
#'   [default_thresholds()], the default.
#'
#' @return A tibble with one row per row of `grid`. The identifying columns are
#'   `cell_id` (the row number in `grid`), the arguments given to
#'   [design_grid()], `M`, and `seed_root` (despite its name, the seed of that
#'   row). The status columns are `status`, `pass` (`TRUE` for `"pass"`),
#'   `groups_evaluated` (`"scale, precision, shape"`, or `"scale, precision"`
#'   without the shape diagnostics), `fail_reasons`, and `warn_reasons` (the
#'   comparisons behind the status, with the KS limit given as a distance).
#'   The summary columns are named by a prefix and a diagnostic. `med_`,
#'   `q05_`, and `q95_` are the median and the 5th and 95th percentiles over
#'   the `M` simulations. The diagnostics are `I_hat` (informativeness; all
#'   three), `R_hat` (heterogeneity ratio; `med_`, `q95_`), `mean_shrinkage`
#'   (`med_`, `q05_`), `bhattacharyya` (`med_`, `q05_`), and `ks` (`med_`,
#'   `q95_`).
#'
#' @seealso [design_grid()], [default_thresholds()],
#'   `vignette("diagnostics-and-presets")`.
#' @family diagnostics
#' @examples
#' # A small M keeps the example fast
#' grid <- design_grid(J = 200, sigma_tau = c(0.05, 0.2), seed_root = 1)
#' audit <- scenario_audit(grid, M = 3)
#' audit[c("sigma_tau", "status", "fail_reasons", "med_mean_shrinkage")]
#' @export
scenario_audit <- function(
  grid,
  M = 200L,
  thresholds = default_thresholds()
) {
  .validate_scenario_grid(grid)
  M <- .validate_scenario_m(M)
  thresholds <- .validate_scenario_thresholds(thresholds)

  cell_ids <- seq_len(nrow(grid))
  audit_one <- function(cell_id) {
    .audit_scenario_cell(grid, cell_id = cell_id, M = M, thresholds = thresholds)
  }
  dplyr::bind_rows(lapply(cell_ids, audit_one))
}

.validate_scenario_grid <- function(grid) {
  if (!inherits(grid, "multisitedgp_design_grid")) {
    .abort(
      "`grid` must be a grid from `design_grid()`.",
      .you_passed(grid)
    )
  }
  if (!"design" %in% names(grid)) {
    .abort("`grid` must have the list column `design` that `design_grid()` creates.")
  }
  if (nrow(grid) < 1L) {
    .abort("`grid` must have at least one row.")
  }
  valid_design <- vapply(grid$design, is_multisitedgp_design, logical(1))
  if (!all(valid_design)) {
    .abort(
      "Every element of `grid$design` must be a design.",
      sprintf("Not a design in rows: %s.", paste(which(!valid_design), collapse = ", "))
    )
  }
  invisible(grid)
}

.validate_scenario_m <- function(M) {
  M <- .validate_scalar_integer(M, "M")
  if (M < 1L) {
    .abort("`M` must be at least 1.", .you_passed(M))
  }
  M
}

.validate_scenario_thresholds <- function(thresholds) {
  defaults <- default_thresholds()
  if (!is.list(thresholds)) {
    .abort(
      "`thresholds` must be a named list.",
      .you_passed(thresholds)
    )
  }
  allowed <- sprintf("The thresholds are: %s.", paste(names(defaults), collapse = ", "))
  if (length(thresholds) > 0L && (is.null(names(thresholds)) || any(!nzchar(names(thresholds))))) {
    .abort("Every entry of `thresholds` must be named.", allowed)
  }
  bad <- setdiff(names(thresholds), names(defaults))
  if (length(bad) > 0L) {
    .abort(
      sprintf("`thresholds` has names that are not thresholds: %s.", paste(bad, collapse = ", ")),
      allowed
    )
  }

  out <- defaults
  out[names(thresholds)] <- thresholds
  out$mean_shrinkage_min <- .validate_unit_threshold(out$mean_shrinkage_min, "thresholds$mean_shrinkage_min")
  out$R_max <- .validate_positive_threshold(out$R_max, "thresholds$R_max")
  out$ks_scaled_max <- .validate_positive_threshold(out$ks_scaled_max, "thresholds$ks_scaled_max")
  out
}

.validate_positive_threshold <- function(x, arg) {
  x <- .validate_scalar_number(x, arg)
  if (x <= 0) {
    .abort(sprintf("`%s` must be positive.", arg), .you_passed(x))
  }
  x
}

.validate_unit_threshold <- function(x, arg) {
  x <- .validate_scalar_number(x, arg)
  if (x < 0 || x > 1) {
    .abort(sprintf("`%s` must be between 0 and 1.", arg), .you_passed(x))
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
  summary <- .summarize_scenario_metrics(
    rep_metrics,
    thresholds = thresholds,
    J = design$J,
    has_shape_reference = .has_auto_reference(design)
  )
  cell <- .scenario_cell_table(grid, cell_id, design, M, seed_root)
  dplyr::bind_cols(cell, summary)
}

.scenario_cell_seed_root <- function(grid, cell_id, design) {
  if ("seed" %in% names(grid) && !is.na(grid$seed[[cell_id]])) {
    return(.checked_cell_seed(as.integer(grid$seed[[cell_id]])))
  }
  if (!is.null(design$seed)) {
    return(.checked_cell_seed(as.integer(design$seed)))
  }
  .abort(
    sprintf("Row %s of `grid` has no seed.", cell_id),
    fix = "Build the grid with `design_grid(..., seed_root = 1)`, or give its designs a `seed`."
  )
}

# as.integer() gives NA past the 32-bit range, and with an NA seed the
# simulations would draw from the caller's random number stream.
.checked_cell_seed <- function(seed) {
  if (is.na(seed)) {
    .abort(
      "A seed in `grid` is not an integer that R can store.",
      sprintf("Seeds must be whole numbers from 0 to %s.", format(.Machine$integer.max, scientific = FALSE))
    )
  }
  seed
}

.scenario_replicate_seeds <- function(M, seed_root) {
  .local_seed_stream(M, seed_root)
}

.audit_scenario_replicate <- function(design, cell_id, rep_id, seed) {
  dat <- .simulate_scenario_replicate(design, seed)
  diag <- attr(dat, "diagnostics", exact = TRUE)
  # Only Gaussian and StudentT have a distribution to compare with. For the
  # other distributions the shape diagnostics are NA and are not checked.
  has_shape_reference <- .has_auto_reference(design)
  tibble::tibble(
    cell_id = cell_id,
    rep = rep_id,
    seed = if (is.na(seed)) NA_integer_ else as.integer(seed),
    I_hat = diag$I_hat,
    R_hat = diag$R_hat,
    mean_shrinkage = mean_shrinkage(dat),
    bhattacharyya = if (has_shape_reference) bhattacharyya_coef(dat) else NA_real_,
    ks = if (has_shape_reference) ks_distance(dat) else NA_real_
  )
}

.simulate_scenario_replicate <- function(design, seed) {
  # .checked_cell_seed() has made sure that `seed` is an integer, so both
  # calls are seeded.
  if (identical(design$paradigm, "direct")) {
    return(sim_meta(design, seed = seed))
  }
  sim_multisite(design, seed = seed)
}

.scenario_cell_table <- function(grid, cell_id, design, M, seed_root) {
  cells <- tibble::as_tibble(grid[cell_id, setdiff(names(grid), c("design", "seed")), drop = FALSE])
  cells$cell_id <- cell_id
  cells$M <- M
  cells$seed_root <- seed_root
  cells[c("cell_id", setdiff(names(cells), "cell_id"))]
}

.summarize_scenario_metrics <- function(rep_metrics, thresholds, J, has_shape_reference = TRUE) {
  q <- function(x, prob) unname(stats::quantile(x, prob, names = FALSE, type = 7, na.rm = TRUE))
  med <- function(x) stats::median(x, na.rm = TRUE)

  values <- list(
    mean_shrinkage = list(med = med(rep_metrics$mean_shrinkage), q05 = q(rep_metrics$mean_shrinkage, 0.05)),
    R = list(med = med(rep_metrics$R_hat), q95 = q(rep_metrics$R_hat, 0.95)),
    bhattacharyya = list(med = med(rep_metrics$bhattacharyya), q05 = q(rep_metrics$bhattacharyya, 0.05)),
    ks = list(med = med(rep_metrics$ks), q95 = q(rep_metrics$ks, 0.95))
  )
  # The KS distance shrinks with the number of sites like 1 / sqrt(J), so its
  # limit is given for the distance times sqrt(J) and turned into a distance
  # for this row of the grid.
  limits <- thresholds
  limits$ks_max <- thresholds$ks_scaled_max / sqrt(J)

  fail_reasons <- .scenario_fail_reasons(values, limits, has_shape_reference = has_shape_reference)
  warn_reasons <- if (length(fail_reasons) == 0L) {
    .scenario_warn_reasons(values, limits, has_shape_reference = has_shape_reference)
  } else {
    character()
  }
  status <- if (length(fail_reasons) > 0L) {
    "fail"
  } else if (length(warn_reasons) > 0L) {
    "warn"
  } else {
    "pass"
  }

  tibble::tibble(
    status = status,
    pass = identical(status, "pass"),
    groups_evaluated = if (has_shape_reference) "scale, precision, shape" else "scale, precision",
    fail_reasons = paste(fail_reasons, collapse = "; "),
    warn_reasons = paste(warn_reasons, collapse = "; "),
    med_I_hat = med(rep_metrics$I_hat),
    q05_I_hat = q(rep_metrics$I_hat, 0.05),
    q95_I_hat = q(rep_metrics$I_hat, 0.95),
    med_R_hat = values$R$med,
    q95_R_hat = values$R$q95,
    med_mean_shrinkage = values$mean_shrinkage$med,
    q05_mean_shrinkage = values$mean_shrinkage$q05,
    med_bhattacharyya = values$bhattacharyya$med,
    q05_bhattacharyya = values$bhattacharyya$q05,
    med_ks = values$ks$med,
    q95_ks = values$ks$q95
  )
}

.scenario_fail_reasons <- function(values, thresholds, has_shape_reference = TRUE) {
  reasons <- character()
  if (.gate_low_fail(values$mean_shrinkage$med, thresholds$mean_shrinkage_min)) {
    reasons <- c(reasons, sprintf("mean_shrinkage:median<%.3g", thresholds$mean_shrinkage_min))
  }
  if (.gate_high_fail(values$R$med, thresholds$R_max)) {
    reasons <- c(reasons, sprintf("R:median>%.3g", thresholds$R_max))
  }
  # A value that is not finite counts as a failure. The KS distance is skipped
  # when the design has no distribution to compare with, because its NA then
  # means "not computed".
  if (has_shape_reference && .gate_high_fail(values$ks$med, thresholds$ks_max)) {
    reasons <- c(reasons, sprintf("ks:median>%.3g", thresholds$ks_max))
  }
  reasons
}

.scenario_warn_reasons <- function(values, thresholds, has_shape_reference = TRUE) {
  reasons <- character()
  if (.gate_low_warn(values$mean_shrinkage$q05, thresholds$mean_shrinkage_min)) {
    reasons <- c(reasons, sprintf("mean_shrinkage:q05<%.3g", thresholds$mean_shrinkage_min))
  }
  if (.gate_high_warn(values$R$q95, thresholds$R_max)) {
    reasons <- c(reasons, sprintf("R:q95>%.3g", thresholds$R_max))
  }
  if (has_shape_reference && .gate_high_warn(values$ks$q95, thresholds$ks_max)) {
    reasons <- c(reasons, sprintf("ks:q95>%.3g", thresholds$ks_max))
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
