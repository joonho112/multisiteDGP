# nolint start: object_name_linter, object_usage_linter
#' Generate Student-t latent site effects
#'
#' @encoding UTF-8
#'
#' @description
#' Draw J standardized site effects from a Student-t distribution with
#' degrees of freedom `nu` (rescaled to unit variance), then apply the shared
#' Layer 1 location-scale wrapper to produce
#' \eqn{\tau_j = \tau + X_j\boldsymbol{\beta} + \sigma_\tau\,z_j}. Reach for
#' Student-t when you want heavier tails than Gaussian — for example, when
#' modeling a robustness check against extreme effects, or when the empirical
#' literature you are calibrating to shows kurtosis above 3.
#'
#' @details
#' The unscaled draw is \eqn{T_j \sim t_\nu}; the package rescales by
#' \eqn{\sqrt{(\nu - 2)/\nu}} so that \eqn{z_j} has unit variance, matching
#' the Layer 1 standardization convention (see \code{\link{gen_effects}}).
#' Degrees of freedom `nu` must be strictly greater than 2 for the rescaling
#' to be well-defined; the function emits a warning when `nu < 4` because
#' kurtosis is non-finite there and Monte Carlo moment checks may be noisy.
#'
#' Tail-heaviness decreases as `nu` grows: `nu = 5` gives kurtosis 6 (modest
#' heavy tails), `nu = 10` gives kurtosis ≈ 3.75 (close to Gaussian),
#' `nu = 30` is essentially Gaussian for practical purposes. Pick `nu` to
#' match the literature you are calibrating to.
#'
#' For the broader catalog and a decision rubric on when to choose a
#' heavier-tailed or asymmetric shape, see the
#' \href{../articles/m2-g-distribution-catalog.html}{G-distribution catalog
#' and standardization} vignette.
#'
#' @param J Integer. Number of sites.
#' @param tau Numeric. Grand mean on the response scale. Default `0`.
#' @param sigma_tau Numeric (\eqn{\ge 0}). Between-site standard deviation
#'   on the response scale. Default `0.20`.
#' @param nu Numeric (> 2). Degrees of freedom. Required — no default.
#'   Typical values: `5` (modest heavy tails), `10` (mild departure from
#'   Gaussian). `nu < 4` triggers a kurtosis-stability warning.
#' @param formula One-sided formula for site-level covariates, or `NULL`.
#' @param beta Numeric coefficient vector matching `formula`, or `NULL`.
#' @param data A `data.frame` with the predictors named in `formula`, or
#'   `NULL`.
#'
#' @return A tibble with one row per site and columns `site_index` (integer
#'   `1:J`), `z_j` (unit-variance Student-t residual), `tau_j` (response-scale
#'   effect), plus any covariate columns from `data`.
#'
#' @family family-effects
#' @seealso
#'   \code{\link{gen_effects}} for the dispatcher and the full eight-shape
#'   catalog;
#'   \code{\link{gen_effects_gaussian}} for the canonical baseline;
#'   \code{\link{gen_effects_skewn}} for asymmetric heavy tails;
#'   \code{\link[stats]{rt}} for the underlying Student-t generator;
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
#' # Modest heavy tails: nu = 5 gives kurtosis 6.
#' gen_effects_studentt(J = 10L, nu = 5)
#'
#' # Larger draw, tighter tails (nu = 10).
#' gen_effects_studentt(J = 50L, tau = 0.2, sigma_tau = 0.15, nu = 10)
#' @export
gen_effects_studentt <- function(
  J,
  tau = 0,
  sigma_tau = 0.20,
  nu,
  formula = NULL,
  beta = NULL,
  data = NULL
) {
  if (missing(nu)) {
    .abort_arg(
      "`nu` is required for Student-t effects.",
      "Student-t standardization requires degrees of freedom greater than 2.",
      "Use `nu = 5` or another finite value greater than 2."
    )
  }
  nu <- .validate_studentt_nu(nu)
  preflight <- .preflight_effects_frame_inputs(
    J = J,
    tau = tau,
    sigma_tau = sigma_tau,
    formula = formula,
    beta = beta,
    data = data
  )
  z_j <- stats::rt(preflight$J, df = nu) * sqrt((nu - 2) / nu)
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

.validate_studentt_nu <- function(nu) {
  theta_G <- list(nu = nu)
  nu <- .validate_studentt_theta(theta_G)
  if (nu < 4) {
    cli::cli_warn(c(
      "!" = "`nu = {nu}` has non-finite kurtosis.",
      "i" = "The Student-t residual variance is finite, but Monte Carlo moment checks may be noisy.",
      ">" = "Use `nu >= 4` for more stable finite-sample variance diagnostics."
    ))
  }
  nu
}
# nolint end
