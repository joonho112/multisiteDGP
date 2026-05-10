# nolint start: object_name_linter, object_usage_linter
#' Generate two-component Gaussian mixture latent site effects
#'
#' @encoding UTF-8
#'
#' @description
#' Draw J standardized site effects from a two-component Gaussian mixture
#' (the legacy JEBS parameterization) and apply the shared Layer 1
#' location-scale wrapper to produce
#' \eqn{\tau_j = \tau + X_j\boldsymbol{\beta} + \sigma_\tau\,z_j}. Reach for
#' the mixture when you expect bimodal effects, contamination, or a
#' subgroup of "outlier" sites whose effect distribution differs from the
#' bulk.
#'
#' @details
#' The mixture model is parameterized so that, before standardization,
#' component 1 has mean \eqn{-\epsilon\delta} and SD 1 and component 2 has
#' mean \eqn{(1 - \epsilon)\delta} and SD `ups`, with mixing weight
#' \eqn{(1 - \epsilon)} on component 1 and \eqn{\epsilon} on component 2.
#' This guarantees the unmixed expectation is zero. The total variance
#' before standardization is
#' \eqn{(1 - \epsilon) + \epsilon\,\mathrm{ups}^2 + \epsilon(1 - \epsilon)\delta^2};
#' the package divides each draw by the square root of that variance to
#' produce unit-variance standardized residuals \eqn{z_j}.
#'
#' This is the parameterization used in the JEBS paper's mixture-shape
#' fixtures; the parameter names (`delta`, `eps`, `ups`) match the JEBS
#' notation. Because of that lock, the returned tibble carries an extra
#' column `latent_component` (integer 1 or 2) recording which component
#' each draw came from — useful for diagnostics and for matching realized
#' draws against intended group memberships.
#'
#' For the broader catalog and decision rubric, see the
#' \href{../articles/m2-g-distribution-catalog.html}{G-distribution catalog
#' and standardization} vignette.
#'
#' @param J Integer. Number of sites.
#' @param tau Numeric. Grand mean on the response scale. Default `0`.
#' @param sigma_tau Numeric (\eqn{\ge 0}). Between-site standard deviation
#'   on the response scale. Default `0.20`.
#' @param delta Numeric (> 0). Component separation. Required — no default.
#'   Larger values produce more bimodal mixtures. Typical applied values:
#'   `delta = 2` (mild bimodality), `delta = 5` (clearly bimodal — the JEBS
#'   fixture).
#' @param eps Numeric in `(0, 1)`. Component-2 mixing weight. Required.
#'   `eps = 0.3` puts 30% of sites in component 2; `eps = 0.5` is balanced.
#' @param ups Numeric (> 0). SD ratio \eqn{\sigma_2 / \sigma_1}. Required.
#'   `ups = 1` gives equal-spread components; `ups = 2` gives a wider
#'   second component (the JEBS fixture).
#' @param formula One-sided formula for site-level covariates, or `NULL`.
#' @param beta Numeric coefficient vector matching `formula`, or `NULL`.
#' @param data A `data.frame` with the predictors named in `formula`, or
#'   `NULL`.
#'
#' @return A tibble with one row per site and columns `site_index` (integer
#'   `1:J`), `z_j` (unit-variance mixture residual), `tau_j` (response-scale
#'   effect), `latent_component` (integer 1 or 2 — which component each draw
#'   came from), plus any covariate columns from `data`.
#'
#' @family family-effects
#' @seealso
#'   \code{\link{gen_effects}} for the dispatcher and the full eight-shape
#'   catalog;
#'   \code{\link{gen_effects_pmslab}} for a related null-spike-plus-slab
#'   shape;
#'   \code{\link{gen_effects_gaussian}} for the unimodal baseline;
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
#' # JEBS fixture: clearly bimodal, 30% in the wider second component.
#' mix <- gen_effects_mixture(J = 50L, delta = 5, eps = 0.3, ups = 2)
#' table(mix$latent_component)  # ~ 35 / 15 split
#'
#' # Mild bimodality with equal-spread components.
#' gen_effects_mixture(J = 50L, delta = 2, eps = 0.5, ups = 1, sigma_tau = 0.15)
#' @export
gen_effects_mixture <- function(
  J,
  tau = 0,
  sigma_tau = 0.20,
  delta,
  eps,
  ups,
  formula = NULL,
  beta = NULL,
  data = NULL
) {
  params <- .validate_mixture_params(delta = delta, eps = eps, ups = ups)
  preflight <- .preflight_effects_frame_inputs(
    J = J,
    tau = tau,
    sigma_tau = sigma_tau,
    formula = formula,
    beta = beta,
    data = data
  )
  J <- preflight$J
  draw <- .legacy_mixture_standardized_draw(
    J = J,
    delta = params$delta,
    eps = params$eps,
    ups = params$ups
  )
  out <- .new_effects_frame(
    z_j = draw$z_j,
    J = J,
    tau = tau,
    sigma_tau = sigma_tau,
    formula = formula,
    beta = beta,
    data = data,
    preflight = preflight
  )
  .append_latent_component(out, draw$latent_component)
}

.validate_mixture_theta <- function(theta_g) {
  .validate_named_list(theta_g, "theta_G")
  missing_params <- setdiff(c("delta", "eps", "ups"), names(theta_g))
  if (length(missing_params) > 0L) {
    .abort_arg(
      "`true_dist = \"Mixture\"` requires `theta_G$delta`, `theta_G$eps`, and `theta_G$ups`.",
      sprintf("Missing parameter(s): %s.", paste(missing_params, collapse = ", ")),
      "Use `theta_G = list(delta = 5, eps = 0.3, ups = 2)`."
    )
  }
  .validate_mixture_params(delta = theta_g$delta, eps = theta_g$eps, ups = theta_g$ups)
}

.validate_mixture_params <- function(delta, eps, ups) {
  if (missing(delta)) {
    .abort_arg(
      "`delta` is required for Mixture effects.",
      "Mixture generation uses `delta` as the positive component separation.",
      "Use `delta = 5` or another positive finite value."
    )
  }
  if (missing(eps)) {
    .abort_arg(
      "`eps` is required for Mixture effects.",
      "Mixture generation uses `eps` as the component-2 probability.",
      "Use `eps = 0.3` or another value strictly between 0 and 1."
    )
  }
  if (missing(ups)) {
    .abort_arg(
      "`ups` is required for Mixture effects.",
      "`ups` is the component-2/component-1 standard-deviation ratio.",
      "Use `ups = 2` or another positive finite value."
    )
  }
  delta <- .validate_scalar_number(delta, "delta")
  eps <- .validate_scalar_number(eps, "eps")
  ups <- .validate_scalar_number(ups, "ups")
  if (delta <= 0) {
    .abort_arg(
      "`delta` must be > 0.",
      "The legacy Mixture parameterization orders components by ascending mean.",
      "Use a positive separation such as `delta = 5`."
    )
  }
  if (eps <= 0 || eps >= 1) {
    .abort_arg(
      "`eps` must be strictly between 0 and 1.",
      "`eps` is the probability of the second mixture component.",
      "Use `eps = 0.3` or another interior probability."
    )
  }
  if (ups <= 0) {
    .abort_arg(
      "`ups` must be > 0.",
      "`ups` is an SD ratio, not a variance ratio or location shift.",
      "Use `ups = 2` for the JEBS fixture or another positive SD ratio."
    )
  }
  list(delta = delta, eps = eps, ups = ups)
}

.legacy_mixture_standardized_draw <- function(J, delta, eps, ups) {
  component <- .legacy_mixture_component_draw(J = J, eps = eps)
  scale <- sqrt(.legacy_mixture_variance(delta = delta, eps = eps, ups = ups))
  component_1 <- stats::rnorm(
    J,
    mean = -eps * delta / scale,
    sd = sqrt(1 / scale^2)
  )
  component_2 <- stats::rnorm(
    J,
    mean = (1 - eps) * delta / scale,
    sd = sqrt(ups^2 / scale^2)
  )
  z_j <- ifelse(component == 1L, component_1, component_2)
  list(z_j = z_j, latent_component = component)
}

.legacy_mixture_raw_draw <- function(delta, eps, ups, J, seed = NULL) {
  params <- .validate_mixture_params(delta = delta, eps = eps, ups = ups)
  J <- .validate_j(J)
  draw <- function() {
    component <- .legacy_mixture_component_draw(J = J, eps = params$eps)
    component_1 <- stats::rnorm(
      J,
      mean = -params$eps * params$delta,
      sd = 1
    )
    component_2 <- stats::rnorm(
      J,
      mean = (1 - params$eps) * params$delta,
      sd = params$ups
    )
    ifelse(component == 1L, component_1, component_2)
  }
  if (is.null(seed)) {
    return(draw())
  }
  seed <- .validate_seed(seed, "seed")
  withr::with_seed(seed, draw())
}

.legacy_mixture_component_draw <- function(J, eps) {
  as.integer(ifelse(stats::runif(J) < (1 - eps), 1L, 2L))
}

.legacy_mixture_variance <- function(delta, eps, ups) {
  (1 - eps) + eps * ups^2 + eps * (1 - eps) * delta^2
}

.append_latent_component <- function(out, latent_component) {
  out$latent_component <- latent_component
  front <- c("site_index", "z_j", "tau_j", "latent_component")
  out[c(front, setdiff(names(out), front))]
}
# nolint end
