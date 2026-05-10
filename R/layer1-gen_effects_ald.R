# nolint start: object_name_linter, object_usage_linter
#' Generate asymmetric Laplace latent site effects
#'
#' @encoding UTF-8
#'
#' @description
#' Draw J standardized site effects from the Yu-Zhang asymmetric Laplace
#' distribution and apply the shared Layer 1 location-scale wrapper to
#' produce \eqn{\tau_j = \tau + X_j\boldsymbol{\beta} + \sigma_\tau\,z_j}.
#' Reach for the asymmetric Laplace when you want a sharp peak at zero
#' with exponential tails — useful for a quantile-flavored shape that
#' contrasts with the smoother skew-normal.
#'
#' @details
#' The package draws \eqn{X_j} from the Yu-Zhang asymmetric Laplace with
#' quantile parameter `rho` and converted scale, then standardizes via
#' \eqn{z_j = (X_j - \mu_X) / \sigma_X} where the closed-form moments are
#' \eqn{\mu_X = (1 - 2\rho)/(\rho(1 - \rho))} and
#' \eqn{\sigma_X^2 = (1 - 2\rho + 2\rho^2)/(\rho^2 (1 - \rho)^2)}. The
#' standardized \eqn{z_j} satisfies the unit-variance Layer 1 convention.
#'
#' `rho` is the *quantile* parameter — `rho = 0.5` is the symmetric Laplace,
#' `rho < 0.5` skews left, `rho > 0.5` skews right. The package refuses
#' values outside `(0.05, 0.95)` because the moment standardization
#' becomes numerically unstable near the degenerate boundaries.
#'
#' Requires the \pkg{LaplacesDemon} Suggests dependency. The function calls
#' \code{LaplacesDemon::ralaplace()} and aborts with a friendly install hint
#' if the package is unavailable.
#'
#' For the broader catalog and decision rubric, see the
#' \href{../articles/m2-g-distribution-catalog.html}{G-distribution catalog
#' and standardization} vignette.
#'
#' @param J Integer. Number of sites.
#' @param tau Numeric. Grand mean on the response scale. Default `0`.
#' @param sigma_tau Numeric (\eqn{\ge 0}). Between-site standard deviation
#'   on the response scale. Default `0.20`.
#' @param rho Numeric in `(0.05, 0.95)`. Yu-Zhang quantile / asymmetry
#'   parameter. Required — no default. `rho = 0.5` is symmetric Laplace;
#'   `rho = 0.3` skews left; `rho = 0.7` skews right. Endpoints `0` and `1`
#'   are degenerate; the function refuses values outside `(0.05, 0.95)`.
#' @param formula One-sided formula for site-level covariates, or `NULL`.
#' @param beta Numeric coefficient vector matching `formula`, or `NULL`.
#' @param data A `data.frame` with the predictors named in `formula`, or
#'   `NULL`.
#'
#' @return A tibble with one row per site and columns `site_index` (integer
#'   `1:J`), `z_j` (unit-variance ALD residual), `tau_j` (response-scale
#'   effect), plus any covariate columns from `data`.
#'
#' @family family-effects
#' @seealso
#'   \code{\link{gen_effects}} for the dispatcher and the full eight-shape
#'   catalog;
#'   \code{\link{gen_effects_skewn}} for an alternative asymmetric shape
#'   with smoother (Gaussian-like) tails;
#'   \code{\link[LaplacesDemon]{ralaplace}} for the underlying generator;
#'   the \href{../articles/m2-g-distribution-catalog.html}{M2 G-distribution
#'   catalog} vignette.
#'
#' @references
#' Yu, K., & Zhang, J. (2005). A three-parameter asymmetric Laplace
#' distribution and its extension. \emph{Communications in Statistics —
#' Theory and Methods}, \bold{34}(9-10), 1867--1879.
#' \doi{10.1080/03610920500199018}.
#'
#' Lee, J., Che, J., Rabe-Hesketh, S., Feller, A., & Miratrix, L. (2025).
#' Improving the estimation of site-specific effects and their distribution
#' in multisite trials. \emph{Journal of Educational and Behavioral Statistics},
#' \bold{50}(5), 731--764. \doi{10.3102/10769986241254286}.
#'
#' @examples
#' if (requireNamespace("LaplacesDemon", quietly = TRUE)) {
#'   # Left-skewed (rho < 0.5).
#'   gen_effects_ald(J = 10L, rho = 0.3)
#'
#'   # Symmetric Laplace (rho = 0.5) — sharp peak at zero with exponential tails.
#'   gen_effects_ald(J = 50L, rho = 0.5, sigma_tau = 0.15)
#' }
#' @export
gen_effects_ald <- function(
  J,
  tau = 0,
  sigma_tau = 0.20,
  rho,
  formula = NULL,
  beta = NULL,
  data = NULL
) {
  .require_soft_dependency("LaplacesDemon", "gen_effects_ald")
  rho <- .validate_ald_rho(rho)
  preflight <- .preflight_effects_frame_inputs(
    J = J,
    tau = tau,
    sigma_tau = sigma_tau,
    formula = formula,
    beta = beta,
    data = data
  )
  J <- preflight$J

  kappa <- sqrt(rho / (1 - rho))
  lambda <- sqrt(2 / (rho * (1 - rho)))
  x <- LaplacesDemon::ralaplace(n = J, location = 0, scale = lambda, kappa = kappa)
  mu_x <- (1 - 2 * rho) / (rho * (1 - rho))
  var_x <- (1 - 2 * rho + 2 * rho^2) / (rho^2 * (1 - rho)^2)
  z_j <- (x - mu_x) / sqrt(var_x)

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

.validate_ald_theta <- function(theta_g) {
  .validate_named_list(theta_g, "theta_G")
  if (!"rho" %in% names(theta_g)) {
    .abort_arg(
      "`true_dist = \"ALD\"` requires `theta_G$rho`.",
      "Asymmetric Laplace generation uses the Yu-Zhang quantile parameter.",
      "Use `theta_G = list(rho = 0.3)`."
    )
  }
  .validate_ald_rho(theta_g$rho)
}

.validate_ald_rho <- function(rho) {
  if (missing(rho)) {
    .abort_arg(
      "`rho` is required for ALD effects.",
      "Asymmetric Laplace generation uses the Yu-Zhang quantile parameter.",
      "Use `rho = 0.3` or another value strictly between 0.05 and 0.95."
    )
  }
  rho <- .validate_scalar_number(rho, "rho")
  if (rho <= 0.05 || rho >= 0.95) {
    .abort_arg(
      "`rho` must be strictly between 0.05 and 0.95.",
      "ALD moments become unstable near the degenerate boundary.",
      "Use `rho = 0.25`, `rho = 0.5`, or another interior value."
    )
  }
  rho
}
# nolint end
