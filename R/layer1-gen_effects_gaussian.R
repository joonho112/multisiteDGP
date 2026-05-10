# nolint start: object_name_linter, object_usage_linter
#' Generate Gaussian latent site effects
#'
#' @encoding UTF-8
#'
#' @description
#' Draw J standardized site effects from the standard normal distribution and
#' apply the shared Layer 1 location-scale wrapper to produce response-scale
#' effects \eqn{\tau_j = \tau + X_j\boldsymbol{\beta} + \sigma_\tau\,z_j}. The
#' Gaussian shape is the canonical baseline of the eight-distribution catalog
#' — pick it when you have no specific reason to prefer a heavier-tailed or
#' asymmetric shape, or when matching the JEBS paper's reference design.
#'
#' @details
#' The standardized residuals \eqn{z_j} are drawn from \eqn{\mathcal{N}(0, 1)}
#' directly via \code{\link[stats]{rnorm}}; no rescaling is needed because the
#' standard normal already satisfies the unit-variance convention shared by
#' all Layer 1 generators (see \code{\link{gen_effects}}).
#'
#' For the broader catalog of \eqn{G} distributions and a decision rubric on
#' when to choose a heavier-tailed or asymmetric shape, see the
#' \href{../articles/m2-g-distribution-catalog.html}{G-distribution catalog
#' and standardization} vignette.
#'
#' @param J Integer. Number of sites.
#' @param tau Numeric. Grand mean on the response scale. Default `0`.
#' @param sigma_tau Numeric (\eqn{\ge 0}). Between-site standard deviation
#'   on the response scale. Default `0.20`.
#' @param variance Numeric. Legacy Gaussian variance argument. Default `1`.
#'   The unit-variance convention requires `variance = 1`; passing any other
#'   value aborts. Control heterogeneity through `sigma_tau` instead.
#' @param formula One-sided formula for site-level covariates (e.g.,
#'   `~ x1 + x2`), or `NULL`.
#' @param beta Numeric coefficient vector matching `formula`, or `NULL`.
#' @param data A `data.frame` with the predictors named in `formula`, or
#'   `NULL`.
#'
#' @return A tibble with one row per site and columns `site_index` (integer
#'   `1:J`), `z_j` (standard-normal residual), `tau_j` (response-scale
#'   effect), plus any covariate columns from `data`.
#'
#' @family family-effects
#' @seealso
#'   \code{\link{gen_effects}} for the dispatcher and the full eight-shape
#'   catalog;
#'   \code{\link{gen_effects_studentt}} and \code{\link{gen_effects_skewn}}
#'   for heavier-tailed and asymmetric variants;
#'   the \href{../articles/m2-g-distribution-catalog.html}{M2 G-distribution
#'   catalog} vignette.
#'
#' @references
#' Lee, J., Che, J., Rabe-Hesketh, S., Feller, A., & Miratrix, L. (2025).
#' Improving the estimation of site-specific effects and their distribution
#' in multisite trials. \emph{Journal of Educational and Behavioral Statistics},
#' \bold{50}(5), 731--764. \doi{10.3102/10769986241254286}.
#'
#' @examples
#' # Minimal: ten standardized Gaussian effects.
#' gen_effects_gaussian(J = 10L)
#'
#' # Larger draw with a non-zero grand mean and modest heterogeneity.
#' gen_effects_gaussian(J = 50L, tau = 0.2, sigma_tau = 0.15)
#' @export
gen_effects_gaussian <- function(
  J,
  tau = 0,
  sigma_tau = 0.20,
  variance = 1,
  formula = NULL,
  beta = NULL,
  data = NULL
) {
  .validate_gaussian_variance(variance)
  preflight <- .preflight_effects_frame_inputs(
    J = J,
    tau = tau,
    sigma_tau = sigma_tau,
    formula = formula,
    beta = beta,
    data = data
  )
  z_j <- stats::rnorm(preflight$J)
  .new_effects_frame(
    z_j = z_j,
    J = J,
    tau = tau,
    sigma_tau = sigma_tau,
    formula = formula,
    beta = beta,
    data = data,
    preflight = preflight
  )
}

.validate_gaussian_variance <- function(variance) {
  variance <- .validate_scalar_number(variance, "variance")
  if (variance <= 0) {
    .abort_arg(
      "`variance` must be > 0.",
      "Gaussian legacy variance is validated before enforcing the standardized residual contract.",
      "Use `variance = 1` and control heterogeneity through `sigma_tau`."
    )
  }
  if (!identical(as.numeric(variance), 1)) {
    .abort_arg(
      "`variance` must equal 1 in multisiteDGP v1.",
      "Layer 1 generators must return standardized residuals with unit variance.",
      "Use `sigma_tau` to control between-site heterogeneity."
    )
  }
  invisible(variance)
}
# nolint end
