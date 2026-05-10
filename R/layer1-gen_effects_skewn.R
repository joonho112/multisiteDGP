# nolint start: object_name_linter, object_usage_linter
#' Generate skew-normal latent site effects
#'
#' @encoding UTF-8
#'
#' @description
#' Draw J standardized site effects from the Azzalini skew-normal
#' distribution, then apply the shared Layer 1 location-scale wrapper to
#' produce \eqn{\tau_j = \tau + X_j\boldsymbol{\beta} + \sigma_\tau\,z_j}.
#' Reach for the skew-normal when you want a unimodal-but-asymmetric shape
#' — for example, when site-level treatment effects show a heavier
#' positive (or negative) tail than Gaussian.
#'
#' @details
#' The package draws \eqn{Y_j \sim \mathrm{SN}(\xi=0, \omega=1, \alpha = \mathrm{slant})}
#' and standardizes to \eqn{z_j = (Y_j - \mu_Y) / \sigma_Y}, where
#' \eqn{\mu_Y = \delta\sqrt{2/\pi}} and
#' \eqn{\sigma_Y = \sqrt{1 - 2\delta^2/\pi}} with
#' \eqn{\delta = \mathrm{slant} / \sqrt{1 + \mathrm{slant}^2}}. The
#' standardization satisfies the unit-variance Layer 1 convention.
#'
#' \code{slant = 0} reduces to Gaussian (and emits a hint to switch to
#' \code{\link{gen_effects_gaussian}}). Large `|slant|` (> 30) approaches
#' the half-normal limit and emits a numerical-stability warning.
#'
#' Requires the \pkg{sn} package (a Suggests dependency). The function calls
#' \code{sn::rsn()} and aborts with a friendly install hint if \pkg{sn} is
#' unavailable.
#'
#' For the broader catalog and decision rubric, see the
#' \href{../articles/m2-g-distribution-catalog.html}{G-distribution catalog
#' and standardization} vignette.
#'
#' @param J Integer. Number of sites.
#' @param tau Numeric. Grand mean on the response scale. Default `0`.
#' @param sigma_tau Numeric (\eqn{\ge 0}). Between-site standard deviation
#'   on the response scale. Default `0.20`.
#' @param slant Numeric. Azzalini skewness parameter \eqn{\alpha}. Required
#'   — no default. Positive values produce a right-skewed shape; negative
#'   values, left-skewed. Typical applied values: `2` (modest skew), `5`
#'   (pronounced skew). `slant = 0` is exactly Gaussian (use
#'   \code{\link{gen_effects_gaussian}} instead). `|slant| > 30` triggers a
#'   stability warning.
#' @param formula One-sided formula for site-level covariates, or `NULL`.
#' @param beta Numeric coefficient vector matching `formula`, or `NULL`.
#' @param data A `data.frame` with the predictors named in `formula`, or
#'   `NULL`.
#'
#' @return A tibble with one row per site and columns `site_index` (integer
#'   `1:J`), `z_j` (unit-variance skew-normal residual), `tau_j`
#'   (response-scale effect), plus any covariate columns from `data`.
#'
#' @family family-effects
#' @seealso
#'   \code{\link{gen_effects}} for the dispatcher and the full eight-shape
#'   catalog;
#'   \code{\link{gen_effects_gaussian}} for the symmetric baseline;
#'   \code{\link{gen_effects_studentt}} for symmetric heavy tails;
#'   \code{\link{gen_effects_ald}} for an alternative asymmetric shape with
#'   exponential tails;
#'   \code{\link[sn]{rsn}} for the underlying skew-normal generator;
#'   the \href{../articles/m2-g-distribution-catalog.html}{M2 G-distribution
#'   catalog} vignette.
#'
#' @references
#' Azzalini, A. (1985). A class of distributions which includes the normal
#' ones. \emph{Scandinavian Journal of Statistics}, \bold{12}(2), 171--178.
#'
#' Lee, J., Che, J., Rabe-Hesketh, S., Feller, A., & Miratrix, L. (2025).
#' Improving the estimation of site-specific effects and their distribution
#' in multisite trials. \emph{Journal of Educational and Behavioral Statistics},
#' \bold{50}(5), 731--764. \doi{10.3102/10769986241254286}.
#'
#' @examples
#' if (requireNamespace("sn", quietly = TRUE)) {
#'   # Modest right-skew.
#'   gen_effects_skewn(J = 10L, slant = 2)
#'
#'   # Pronounced left-skew.
#'   gen_effects_skewn(J = 50L, slant = -5, sigma_tau = 0.15)
#' }
#' @export
gen_effects_skewn <- function(
  J,
  tau = 0,
  sigma_tau = 0.20,
  slant,
  formula = NULL,
  beta = NULL,
  data = NULL
) {
  .require_soft_dependency("sn", "gen_effects_skewn")
  slant <- .validate_skewn_slant(slant)
  preflight <- .preflight_effects_frame_inputs(
    J = J,
    tau = tau,
    sigma_tau = sigma_tau,
    formula = formula,
    beta = beta,
    data = data
  )
  J <- preflight$J

  delta <- slant / sqrt(1 + slant^2)
  mu_y <- delta * sqrt(2 / pi)
  sd_y <- sqrt(1 - 2 * delta^2 / pi)
  y <- sn::rsn(n = J, xi = 0, omega = 1, alpha = slant)
  z_j <- (y - mu_y) / sd_y

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

.validate_skewn_theta <- function(theta_g) {
  .validate_named_list(theta_g, "theta_G")
  if (!"slant" %in% names(theta_g)) {
    .abort_arg(
      "`true_dist = \"SkewN\"` requires `theta_G$slant`.",
      "Skew-Normal generation uses the Azzalini slant parameter.",
      "Use `theta_G = list(slant = 2)`."
    )
  }
  .validate_skewn_slant(theta_g$slant)
}

.validate_skewn_slant <- function(slant) {
  if (missing(slant)) {
    .abort_arg(
      "`slant` is required for Skew-Normal effects.",
      "Skew-Normal generation uses the Azzalini slant parameter.",
      "Use `slant = 2` or another finite numeric value."
    )
  }
  slant <- .validate_scalar_number(slant, "slant")
  if (identical(slant, 0)) {
    cli::cli_warn(c(
      "!" = "`slant = 0` is exactly Gaussian.",
      "i" = "Skew-Normal remains valid, but has no skewness at this value.",
      ">" = "Use `true_dist = \"Gaussian\"` when skewness is not needed."
    ))
  }
  if (abs(slant) > 30) {
    cli::cli_warn(c(
      "!" = "`slant = {slant}` is near the half-normal limit.",
      "i" = "Finite-sample moments can be numerically unstable for extreme slant values.",
      ">" = "Use a smaller absolute `slant` unless the half-normal limit is intentional."
    ))
  }
  slant
}
# nolint end
