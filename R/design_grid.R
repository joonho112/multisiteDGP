#' Build a grid of designs from combinations of argument values
#'
#' Builds one design for each combination of the values given in `...` and
#' returns the designs in a tibble, one per row. Each row also gets its own
#' seed for simulating from that design. Pass the grid to [scenario_audit()]
#' to simulate each row repeatedly and summarize the diagnostics.
#'
#' @param ... Arguments of [multisitedgp_design()], each given as a vector or
#'   list of the values to combine, such as `J = c(25, 50)`. A value that is
#'   itself a list, function, formula, or data frame goes inside `list()`.
#' @param base_design A design, such as a preset, whose values are used for
#'   the arguments not in `...`. The default `NULL` uses the defaults of
#'   [multisitedgp_design()].
#' @param seed_root Integer seed from which the row seeds are derived, so the
#'   same `seed_root` gives the same row seeds. It is required unless
#'   `seed_stream = FALSE`.
#' @param seed_stream Logical. If `TRUE` (the default), the grid has a column
#'   `seed` with the row seeds. If `FALSE`, no row seeds are made and
#'   `seed_root` is not used.
#'
#' @return A tibble of class `multisitedgp_design_grid` with one row per
#'   combination, the last argument varying fastest. Its columns are the
#'   arguments in `...`, the list column `design`, and the integer column
#'   `seed` (when `seed_stream = TRUE`).
#'
#' @seealso [scenario_audit()], [multisitedgp_design()],
#'   `vignette("diagnostics-and-presets")`.
#' @family simulation
#' @examples
#' grid <- design_grid(J = c(25, 50), sigma_tau = c(0.1, 0.2), seed_root = 1)
#' grid
#'
#' # Simulate one row with its seed
#' sim_multisite(grid$design[[1]], seed = grid$seed[1])
#'
#' # Vary one argument of a preset
#' base <- preset_education_modest()
#' design_grid(sigma_tau = c(0.1, 0.3), base_design = base, seed_root = 1)
#' @export
design_grid <- function(
  ...,
  base_design = NULL,
  seed_root = NULL,
  seed_stream = TRUE
) {
  args <- rlang::list2(...)
  seed_stream <- .validate_scalar_logical(seed_stream, "seed_stream")
  .validate_design_grid_base(base_design)
  .validate_design_grid_args(args, seed_stream = seed_stream)

  grid <- .expand_design_grid(args)
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

# One row per combination of the grid arguments, with the last argument
# varying fastest. Seeds are handed out by row, so this order decides which
# seed each design gets.
.expand_design_grid <- function(args) {
  sizes <- lengths(args)
  n_rows <- prod(sizes)
  # A value is held for as many rows as the arguments after it have combinations.
  run_lengths <- n_rows / cumprod(sizes)
  columns <- Map(
    function(values, run_length) {
      values[rep(rep(seq_along(values), each = run_length), length.out = n_rows)]
    },
    args,
    run_lengths
  )
  tibble::new_tibble(columns, nrow = n_rows)
}

.validate_design_grid_base <- function(base_design) {
  if (!is.null(base_design)) {
    if (!is_multisitedgp_design(base_design)) {
      .abort(
        "`base_design` must be a design from `multisitedgp_design()` or a preset.",
        .you_passed(base_design)
      )
    }
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
    .abort(
      "Every argument in `...` must be named.",
      "Each one is an argument of `multisitedgp_design()` with the values to combine, such as `J = c(25, 50)`."
    )
  }
  duplicated_names <- unique(arg_names[duplicated(arg_names)])
  if (length(duplicated_names) > 0L) {
    .abort(
      "Each argument in `...` may be given only once.",
      sprintf("Given more than once: %s.", paste(duplicated_names, collapse = ", ")),
      "Put all the values of an argument in one vector."
    )
  }
  valid_names <- names(formals(multisitedgp_design))
  bad <- setdiff(arg_names, valid_names)
  if (length(bad) > 0L) {
    .abort_unknown_design_args(bad)
  }
  empty <- arg_names[vapply(args, length, integer(1)) == 0L]
  if (length(empty) > 0L) {
    .abort(
      "Each argument in `...` must have at least one value.",
      sprintf("Empty: %s.", paste(empty, collapse = ", "))
    )
  }
  is_values <- vapply(args, function(x) (is.atomic(x) || is.list(x)) && !is.data.frame(x), logical(1))
  if (!all(is_values)) {
    .abort(
      "Each argument in `...` must be a vector or a list.",
      sprintf("Neither a vector nor a list: %s.", paste(arg_names[!is_values], collapse = ", ")),
      "Wrap a single function, formula, or data frame in `list()`."
    )
  }
  if (isTRUE(seed_stream) && "seed" %in% arg_names) {
    .abort(
      "`seed` cannot be supplied in `...` when `seed_stream = TRUE`.",
      "The grid then has its own column `seed`, derived from `seed_root`."
    )
  }
  invisible(args)
}

.design_grid_row_overrides <- function(grid, arg_names, i) {
  overrides <- lapply(arg_names, function(arg) grid[[arg]][[i]])
  names(overrides) <- arg_names
  overrides
}
