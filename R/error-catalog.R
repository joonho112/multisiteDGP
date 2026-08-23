#' Look up stable multisiteDGP error IDs
#'
#' @encoding UTF-8
#'
#' @description
#' Return the installed E01--E30 catalog linking each stable error ID to its
#' condition class, affected public API, active/deferred status, and concrete
#' remedy. Use this after an error or warning mentions an ID, or when writing
#' calling code that handles typed package conditions.
#'
#' @details
#' The table is generated from `tools/traceability/error-index.csv` during
#' development and installed as package data. E24 is deliberately present but
#' marked deferred because its v0.2.0 trigger is unreachable. E28 is a warning
#' path rather than an abort class. All other active rows map to the typed
#' condition hierarchy documented in \code{\link{multisiteDGP}}.
#'
#' @param id Optional character vector of IDs such as `"E15"`. Matching is
#'   case-insensitive. Leave `NULL` to return the full catalog.
#'
#' @return A tibble with columns `id`, `condition`, `class`, `active_v0_2`,
#'   `api`, `remedy`, and `status`, filtered to `id` when supplied.
#'
#' @family family-diagnostics
#' @seealso \code{\link{multisiteDGP}} for the condition-class hierarchy.
#'
#' @examples
#' error_catalog("E15")
#' error_catalog(c("E04", "E28"))
#' @export
error_catalog <- function(id = NULL) {
  path <- system.file("extdata", "error-catalog.csv", package = "multisiteDGP")
  if (!nzchar(path)) {
    .abort_arg(
      "The installed error catalog could not be found.",
      "`inst/extdata/error-catalog.csv` is required by `error_catalog()`.",
      "Use a complete source or binary package to reinstall multisiteDGP."
    )
  }
  out <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  if (is.null(id)) {
    return(tibble::as_tibble(out))
  }
  if (!is.character(id) || length(id) == 0L || anyNA(id) || any(!nzchar(id))) {
    .abort_arg(
      "`id` must be a non-empty character vector of error IDs.",
      "Stable IDs use the form E01 through E30.",
      "Use `error_catalog('E15')` or leave `id = NULL` for the full table."
    )
  }
  id <- toupper(id)
  unknown <- setdiff(id, out$id)
  if (length(unknown) > 0L) {
    .abort_arg(
      "Unknown multisiteDGP error ID.",
      sprintf("Unknown ID(s): %s.", paste(unknown, collapse = ", ")),
      "Use `error_catalog()` to list E01 through E30."
    )
  }
  tibble::as_tibble(out[match(id, out$id), , drop = FALSE])
}
