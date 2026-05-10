# nolint start: object_name_linter, object_usage_linter
#' Create a Cartesian grid of multisite design cells for scenario sweeps
#'
#' @encoding UTF-8
#'
#' @description
#' Expand vectors of \code{\link{multisitedgp_design}} arguments into a
#' tibble of distinct design cells — one row per Cartesian-product
#' combination. Each row gets a locked `multisitedgp_design` object
#' and, optionally, a deterministic seed for reproducible parallel
#' simulation. Pass the result to \code{\link{scenario_audit}} for a
#' top-of-funnel feasibility sweep, or to a custom worker pipeline for
#' the full simulation.
#'
#' @details
#' \strong{Replication vs sweep.} Replication is intentionally NOT
#' represented in `design_grid()` — use \code{scenario_audit(M = ...)}
#' for per-cell replication. The grid represents the *unique* design
#' cells; replication is a separate axis.
#'
#' \strong{`base_design` reuse pattern.} Pass a `multisitedgp_design`
#' object as `base_design` to use it as the starting point for every
#' row; the `...` overrides are applied via
#' \code{\link{update_multisitedgp_design}} on top of the base. Useful
#' when sweeping a single dimension (e.g., `sigma_tau`) while holding
#' all other parameters constant.
#'
#' \strong{Seed allocation.} When `seed_stream = TRUE` (default), the
#' package allocates one deterministic 9-digit integer seed per row
#' from `seed_root` via the local-seed-stream policy. Re-running with
#' the same `seed_root` produces bit-identical results across runs and
#' across machines, and parallel workers get non-overlapping RNG
#' streams.
#'
#' For a worked scenario-grid example see the
#' \href{../articles/a6-case-study-multisite.html}{Case study —
#' multisite trial} vignette.
#'
#' @param ... Named vectors or lists of flat
#'   \code{\link{multisitedgp_design}} arguments to expand as a
#'   Cartesian product. Each name must be a valid constructor
#'   argument; unknown names trigger an informative error.
#' @param base_design Optional `multisitedgp_design` object used as the
#'   base for every row before applying row-specific overrides. Default
#'   `NULL`.
#' @param seed_root Optional integer root seed. Required when
#'   `seed_stream = TRUE`. Pick a 9-digit integer; the package derives
#'   one seed per row from this root.
#' @param seed_stream Logical. Allocate one deterministic seed per
#'   grid row. Default `TRUE`. Set to `FALSE` only when you intend to
#'   manage seeds externally.
#'
#' @return A tibble with class `multisitedgp_design_grid`, one row per
#'   distinct design cell. A `design` list-column carries the
#'   `multisitedgp_design` object for that row; a `seed` integer
#'   column appears when `seed_stream = TRUE`. Override columns appear
#'   as named columns for easy joining / filtering.
#'
#' @family family-design
#' @seealso
#'   \code{\link{multisitedgp_design}} for constructing per-row
#'   designs;
#'   \code{\link{update_multisitedgp_design}} for the override
#'   mechanism used when `base_design` is supplied;
#'   \code{\link{scenario_audit}} for the top-of-funnel grid
#'   feasibility sweep;
#'   the \href{../articles/a6-case-study-multisite.html}{A6 Case study —
#'   multisite trial} vignette.
#' @references
#' Lee, J., Che, J., Rabe-Hesketh, S., Feller, A., & Miratrix, L. (2025).
#' Improving the estimation of site-specific effects and their distribution
#' in multisite trials. \emph{Journal of Educational and Behavioral Statistics},
#' \bold{50}(5), 731--764. \doi{10.3102/10769986241254286}.
#' @examples
#' # Two J values × two sigma_tau values = 4 design cells.
#' grid <- design_grid(J = c(10L, 12L), sigma_tau = c(0.10, 0.20),
#'                     seed_root = 1L)
#' nrow(grid)
#'
#' # Sweep heterogeneity holding everything else at the modest preset.
#' base <- preset_education_modest()
#' het_sweep <- design_grid(sigma_tau = c(0.05, 0.20, 0.30),
#'                          base_design = base, seed_root = 12345L)
#' @export
design_grid <- function(
  ...,
  base_design = NULL,
  seed_root = NULL,
  seed_stream = TRUE
) {
  args <- rlang::list2(...)
  args <- .strip_design_grid_reps(args)
  seed_stream <- .validate_scalar_logical(seed_stream, "seed_stream")
  .validate_design_grid_base(base_design)
  .validate_design_grid_args(args, seed_stream = seed_stream)

  grid <- do.call(tidyr::expand_grid, args)
  if (isTRUE(seed_stream)) {
    grid$seed <- .local_seed_stream(nrow(grid), seed_root)
  }

  arg_names <- names(args)
  grid$design <- lapply(seq_len(nrow(grid)), function(i) {
    overrides <- .design_grid_row_overrides(grid, arg_names, i)
    if (is.null(base_design)) {
      do.call(multisitedgp_design, overrides)
    } else {
      do.call(update_multisitedgp_design, c(list(base_design), overrides))
    }
  })

  if (isTRUE(seed_stream)) {
    grid <- grid[c(arg_names, "design", "seed")]
  } else {
    grid <- grid[c(arg_names, "design")]
  }
  class(grid) <- c("multisitedgp_design_grid", "tbl_df", "tbl", "data.frame")
  grid
}

.strip_design_grid_reps <- function(args) {
  if ("reps" %in% names(args)) {
    cli::cli_warn(
      c(
        "!" = "`reps` is deprecated for `design_grid()`.",
        "i" = "design_grid rows are distinct cells; replication belongs to `scenario_audit(M = ...)`.",
        ">" = "Remove `reps` from `design_grid()` and pass the replicate count to `scenario_audit()`."
      ),
      class = "multisitedgp_lifecycle_warning"
    )
    args$reps <- NULL
  }
  args
}

.validate_design_grid_base <- function(base_design) {
  if (!is.null(base_design)) {
    validate_multisitedgp_design(base_design)
  }
  invisible(base_design)
}

.validate_design_grid_args <- function(args, seed_stream) {
  arg_names <- names(args)
  if (length(args) == 0L) {
    return(invisible(args))
  }
  if (is.null(arg_names) || any(!nzchar(arg_names))) {
    .abort_arg(
      "`design_grid()` arguments in `...` must be named.",
      "Each expanded column is replayed as a `multisitedgp_design()` argument.",
      "Use names such as `J = c(20L, 40L)` or `sigma_tau = c(0.1, 0.2)`."
    )
  }
  duplicated_names <- unique(arg_names[duplicated(arg_names)])
  if (length(duplicated_names) > 0L) {
    .abort_arg(
      "`design_grid()` arguments must have unique names.",
      sprintf("Duplicated names: %s.", paste(duplicated_names, collapse = ", ")),
      "Remove duplicate grid columns or combine their values into one vector."
    )
  }
  valid_names <- names(formals(multisitedgp_design))
  bad <- setdiff(arg_names, valid_names)
  if (length(bad) > 0L) {
    .abort_arg(
      "Unknown `design_grid()` argument.",
      sprintf("Grid columns must match `multisitedgp_design()` arguments; got: %s.", paste(bad, collapse = ", ")),
      "Use flat argument names such as `J`, `sigma_tau`, `I`, `R`, or `rank_corr`."
    )
  }
  empty <- arg_names[vapply(args, length, integer(1)) == 0L]
  if (length(empty) > 0L) {
    .abort_arg(
      "`design_grid()` arguments must contain at least one value.",
      sprintf("Empty grid columns: %s.", paste(empty, collapse = ", ")),
      "Remove empty vectors or supply at least one value per grid argument."
    )
  }
  if (isTRUE(seed_stream) && "seed" %in% arg_names) {
    .abort_arg(
      "`seed` cannot be supplied in `...` when `seed_stream = TRUE`.",
      "`seed_stream = TRUE` owns the per-cell `seed` column.",
      "Remove `seed` from `...` or set `seed_stream = FALSE`."
    )
  }
  invisible(args)
}

.design_grid_row_overrides <- function(grid, arg_names, i) {
  overrides <- lapply(arg_names, function(arg) grid[[arg]][[i]])
  names(overrides) <- arg_names
  overrides
}
# nolint end
