# nolint start: object_usage_linter
.RESERVED_METAFOR <- c("yi", "vi", "sei")
.RESERVED_BAGGR <- c("tau", "se")
.RESERVED_MULTISITEPOWER <- c("site", "estimate", "se", "n")

#' Adapt multisiteDGP output for downstream meta-analysis and power packages
#'
#' @encoding UTF-8
#'
#' @description
#' Strip the multisiteDGP attributes and class from a simulated dataset and
#' rename the canonical columns to the conventions of a downstream
#' analysis package. The numeric values themselves are NEVER transformed —
#' only the column names change. Three adapters ship with the package, each
#' targeting a soft-dependency analysis package:
#' \describe{
#'   \item{\code{\link{as_metafor}}}{Renames to \pkg{metafor}'s `(yi, vi, sei)` convention. Use as input to `metafor::rma()` or `metafor::escalc()`.}
#'   \item{\code{\link{as_baggr}}}{Renames to \pkg{baggr}'s `(tau, se)` convention; optional `tau_true` column for diagnostic plotting.}
#'   \item{\code{\link{as_multisitepower}}}{Renames to \pkg{multisitepower}'s `(site, estimate, se, n)` convention. Use for empirical-Bayes power analysis.}
#' }
#'
#' @details
#' \strong{Soft-dependency guard.} Each adapter checks that the target
#' package is installed and aborts with a friendly install hint if not.
#' \pkg{metafor}, \pkg{baggr}, and \pkg{multisitepower} are Suggests, not
#' Imports — they are not required to use multisiteDGP itself.
#'
#' \strong{Reserved column-name protection.} If your simulation includes
#' covariate columns that collide with the target package's reserved
#' names (e.g., a covariate named `yi` would collide with metafor), the
#' adapter aborts and asks you to rename the offending column first. This
#' prevents silent overwrite.
#'
#' \strong{Covariate pass-through.} Any non-canonical columns in the
#' upstream `multisitedgp_data` (covariates from `formula` /
#' `beta` / `data`) are carried through to the adapted tibble after the
#' renamed canonical columns.
#'
#' For a full workflow walkthrough see the
#' \href{../articles/m6-adapters-downstream.html}{Adapters and downstream
#' packages} vignette.
#'
#' @param x A `multisitedgp_data` object from \code{\link{sim_multisite}}
#'   or \code{\link{sim_meta}}.
#' @param ... Reserved for future extensions.
#'
#' @return A plain `tbl_df` with renamed columns. No multisiteDGP class or
#'   attributes are carried through — the result is suitable to pass
#'   directly to the downstream package's API.
#'
#' @family family-adapters
#' @seealso
#'   \code{\link{sim_multisite}} and \code{\link{sim_meta}} for the
#'   upstream simulators;
#'   the \href{../articles/m6-adapters-downstream.html}{M6 Adapters and
#'   downstream packages} vignette.
#'
#' @references
#' Lee, J., Che, J., Rabe-Hesketh, S., Feller, A., & Miratrix, L. (2025).
#' Improving the estimation of site-specific effects and their distribution
#' in multisite trials. \emph{Journal of Educational and Behavioral Statistics},
#' \bold{50}(5), 731--764. \doi{10.3102/10769986241254286}.
#'
#' @name adapters
#' @examples
#' dat <- sim_multisite(J = 10L, seed = 1L)
#'
#' # metafor: yi = tau_hat, vi = se2, sei = se.
#' if (requireNamespace("metafor", quietly = TRUE)) {
#'   md <- as_metafor(dat)
#'   metafor::rma(yi = yi, vi = vi, data = md)
#' }
#'
#' # baggr: tau = tau_hat, se = se. Add latent truth for diagnostic plots.
#' if (requireNamespace("baggr", quietly = TRUE)) {
#'   bg <- as_baggr(dat, include_truth = TRUE)
#' }
NULL

#' @describeIn adapters Convert to \pkg{metafor}'s `(yi, vi, sei)` column convention. `yi <- tau_j_hat`, `vi <- se2_j`, `sei <- se_j`. Pass the result directly to `metafor::rma()` or `metafor::escalc()`. Soft-dep guard on \pkg{metafor}.
#' @export
as_metafor <- function(x, ...) {
  UseMethod("as_metafor")
}

#' @export
as_metafor.default <- function(x, ...) {
  .abort_adapter_data("as_metafor", x)
}

#' @export
as_metafor.multisitedgp_data <- function(x, ...) {
  .require_soft_dependency("metafor", "as_metafor")
  .check_adapter_reserved_names(x, reserved = .RESERVED_METAFOR, adapter = "metafor")
  .as_metafor_rename(x)
}

#' @describeIn adapters Convert to \pkg{baggr}'s `(tau, se)` column convention. `tau <- tau_j_hat`, `se <- se_j`. Use as input to `baggr::baggr()`. Soft-dep guard on \pkg{baggr}.
#' @param include_truth Logical. When `TRUE`, includes the latent true effects as a `tau_true` column — useful for diagnostic plots that compare baggr's posterior to the simulation truth. Default `FALSE`.
#' @export
as_baggr <- function(x, ..., include_truth = FALSE) {
  UseMethod("as_baggr")
}

#' @export
as_baggr.default <- function(x, ..., include_truth = FALSE) {
  .abort_adapter_data("as_baggr", x)
}

#' @export
as_baggr.multisitedgp_data <- function(x, ..., include_truth = FALSE) {
  .require_soft_dependency("baggr", "as_baggr")
  include_truth <- .validate_scalar_logical(include_truth, "include_truth")
  reserved <- if (isTRUE(include_truth)) c(.RESERVED_BAGGR, "tau_true") else .RESERVED_BAGGR
  .check_adapter_reserved_names(x, reserved = reserved, adapter = "baggr")
  .as_baggr_rename(x, include_truth = include_truth)
}

#' @describeIn adapters Convert to \pkg{multisitepower}'s `(site, estimate, se, n)` column convention. `site <- site_index`, `estimate <- tau_j_hat`, `se <- se_j`, `n <- n_j` (when present). Use as input to `multisitepower::power_calc()`. Soft-dep guard on \pkg{multisitepower}.
#' @export
as_multisitepower <- function(x, ...) {
  UseMethod("as_multisitepower")
}

#' @export
as_multisitepower.default <- function(x, ...) {
  .abort_adapter_data("as_multisitepower", x)
}

#' @export
as_multisitepower.multisitedgp_data <- function(x, ...) {
  .require_soft_dependency("multisitepower", "as_multisitepower")
  .check_adapter_reserved_names(x, reserved = .RESERVED_MULTISITEPOWER, adapter = "multisitepower")
  .as_multisitepower_rename(x)
}

.abort_adapter_data <- function(adapter, x) {
  .abort_arg(
    sprintf("`x` must be a multisitedgp_data object for `%s()`.", adapter),
    sprintf("Got object with class: %s.", paste(class(x), collapse = "/")),
    "Use `sim_multisite()` or `sim_meta()` before calling the adapter."
  )
}

.check_adapter_reserved_names <- function(x, reserved, adapter) {
  cov_cols <- .adapter_covariate_columns(x)
  collision <- intersect(cov_cols, reserved)
  if (length(collision) > 0L) {
    .abort_arg(
      sprintf(
        "Covariate column %s collides with %s reserved name.",
        paste(sprintf('"%s"', collision), collapse = ", "),
        adapter
      ),
      sprintf(
        "%s uses reserved adapter column name(s): %s.",
        adapter,
        paste(reserved, collapse = ", ")
      ),
      "Use a renamed covariate column or `tibble::as_tibble()` for a plain tibble."
    )
  }
  invisible(TRUE)
}

.adapter_covariate_columns <- function(x) {
  setdiff(names(x), .canonical_data_columns())
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

.as_multisitepower_rename <- function(x) {
  out <- tibble::tibble(
    site = x$site_index,
    estimate = x$tau_j_hat,
    se = x$se_j
  )
  if (any(!is.na(x$n_j))) {
    out$n <- x$n_j
  }
  .bind_adapter_covariates(out, x)
}
# nolint end
