.RESERVED_METAFOR <- c("yi", "vi", "sei")
.RESERVED_BAGGR <- c("tau", "se")

#' Convert simulated data for metafor and baggr
#'
#' Each function returns the site estimates and their standard errors under the
#' column names that another package uses. The values are not changed.
#'
#' @details
#' `as_metafor()` returns `yi` (`tau_j_hat`), `vi` (`se2_j`), and `sei`
#' (`se_j`) for `metafor::rma()`. `as_baggr()` returns `tau` (`tau_j_hat`) and
#' `se` (`se_j`) for `baggr::baggr()`.
#'
#' The functions only rename columns, so nothing else needs to be installed.
#'
#' @param x Simulated data from [sim_multisite()] or [sim_meta()].
#' @param ... Not used.
#' @param include_truth Logical. If `TRUE`, `as_baggr()` adds the true site
#'   effects `tau_j` as column `tau_true`. Default `FALSE`.
#'
#' @return A tibble with one row per site: the columns named in Details, then
#'   the columns of the design's `data`, if any. The class and the attributes
#'   of `x` are dropped.
#'
#' @seealso [sim_multisite()], [sim_meta()], `vignette("downstream-analysis")`.
#' @examples
#' dat <- sim_multisite(preset_education_modest(), seed = 1)
#' md <- as_metafor(dat)
#' md
#' as_baggr(dat, include_truth = TRUE)
#' @examplesIf requireNamespace("metafor", quietly = TRUE)
#' metafor::rma(yi, vi, data = md)
#' @name adapters
NULL

#' @rdname adapters
#' @export
as_metafor <- function(x, ...) {
  UseMethod("as_metafor")
}

#' @export
as_metafor.default <- function(x, ...) {
  .abort_not_simulated_data(x)
}

#' @export
as_metafor.multisitedgp_data <- function(x, ...) {
  .check_adapter_reserved_names(x, reserved = .RESERVED_METAFOR, adapter = "metafor")
  .as_metafor_rename(x)
}

#' @rdname adapters
#' @export
as_baggr <- function(x, ..., include_truth = FALSE) {
  UseMethod("as_baggr")
}

#' @export
as_baggr.default <- function(x, ..., include_truth = FALSE) {
  .abort_not_simulated_data(x)
}

#' @export
as_baggr.multisitedgp_data <- function(x, ..., include_truth = FALSE) {
  include_truth <- .validate_scalar_logical(include_truth, "include_truth")
  reserved <- if (isTRUE(include_truth)) c(.RESERVED_BAGGR, "tau_true") else .RESERVED_BAGGR
  .check_adapter_reserved_names(x, reserved = reserved, adapter = "baggr")
  .as_baggr_rename(x, include_truth = include_truth)
}

.check_adapter_reserved_names <- function(x, reserved, adapter) {
  cov_cols <- .adapter_covariate_columns(x)
  collision <- intersect(cov_cols, reserved)
  if (length(collision) > 0L) {
    .abort(
      sprintf(
        "The covariate column %s has a name that `as_%s()` gives to another column.",
        paste(sprintf("`%s`", collision), collapse = ", "),
        adapter
      ),
      sprintf("`as_%s()` creates the columns %s.", adapter, paste(reserved, collapse = ", ")),
      "Rename the covariate in the `data` of the design."
    )
  }
  invisible(TRUE)
}

.adapter_covariate_columns <- function(x) {
  setdiff(names(x), .data_columns())
}

.adapter_covariates <- function(x) {
  cov_cols <- .adapter_covariate_columns(x)
  if (length(cov_cols) == 0L) {
    return(NULL)
  }
  .strip_multisitedgp_data(x[, cov_cols, drop = FALSE])
}

.bind_adapter_covariates <- function(out, x) {
  covariates <- .adapter_covariates(x)
  if (is.null(covariates)) {
    return(out)
  }
  dplyr::bind_cols(out, covariates)
}

.as_metafor_rename <- function(x) {
  out <- tibble::tibble(
    yi = x$tau_j_hat,
    vi = x$se2_j,
    sei = x$se_j
  )
  .bind_adapter_covariates(out, x)
}

.as_baggr_rename <- function(x, include_truth = FALSE) {
  include_truth <- .validate_scalar_logical(include_truth, "include_truth")
  out <- tibble::tibble(
    tau = x$tau_j_hat,
    se = x$se_j
  )
  if (isTRUE(include_truth)) {
    out$tau_true <- x$tau_j
  }
  .bind_adapter_covariates(out, x)
}
