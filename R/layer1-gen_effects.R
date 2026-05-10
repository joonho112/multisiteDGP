# nolint start: object_name_linter, object_usage_linter
#' Generate latent site effects
#'
#' @encoding UTF-8
#'
#' @description
#' `gen_effects()` is the Layer 1 entry point of the multisiteDGP pipeline —
#' it draws standardized site effects \eqn{z_j} from one of eight built-in
#' \eqn{G} distributions (or a user callback) and returns a forward-compatible
#' tibble that Layers 2 through 4 can consume. Most users invoke it indirectly
#' through \code{\link{sim_multisite}} or \code{\link{sim_meta}}; call it
#' directly when composing the four layers manually or auditing a single
#' layer in isolation. Shape selection is controlled by `true_dist`,
#' shape-specific parameters travel through `theta_G`, and a user callback
#' `g_fn` overrides the catalog when `true_dist = "User"`.
#'
#' @details
#' The eight built-in \eqn{G} distributions are:
#' \describe{
#'   \item{`"Gaussian"` (\code{\link{gen_effects_gaussian}})}{Standard normal — the canonical baseline. No `theta_G` keys.}
#'   \item{`"StudentT"` (\code{\link{gen_effects_studentt}})}{Standardized Student-\eqn{t} with degrees of freedom `theta_G$nu` (numeric, > 2). Heavier tails than Gaussian.}
#'   \item{`"SkewN"` (\code{\link{gen_effects_skewn}})}{Standardized skew-normal with slant `theta_G$slant` (numeric). Asymmetric shape.}
#'   \item{`"ALD"` (\code{\link{gen_effects_ald}})}{Standardized asymmetric Laplace with asymmetry `theta_G$rho` \eqn{\in (0, 1)}.}
#'   \item{`"Mixture"` (\code{\link{gen_effects_mixture}})}{Two-component normal mixture with `theta_G$delta` (component separation), `theta_G$eps` (mixing weight), `theta_G$ups` (variance ratio). Use for bimodal or contaminated effects.}
#'   \item{`"PointMassSlab"` (\code{\link{gen_effects_pmslab}})}{Point mass at 0 with probability `theta_G$pi0`, plus a continuous slab governed by `theta_G$slab_shape`, `theta_G$mu_slab`, `theta_G$sigma_slab`. Use when a fraction of sites have null effects.}
#'   \item{`"User"` (\code{\link{gen_effects_user}})}{Any user callback `g_fn` returning length-`J` standardized residuals (or raw response-scale effects under `g_returns = "raw"`).}
#'   \item{`"DPM"` (\code{\link{gen_effects_dpm}})}{Dirichlet-process mixture — currently available only via the `g_fn` callback bridge. Direct DPM is unimplemented in the current release.}
#' }
#'
#' \strong{When to call this directly.} For most users,
#' \code{\link{sim_multisite}} or \code{\link{sim_meta}} is the right entry
#' point — direct calls to `gen_effects()` are an advanced surface. The three
#' situations that warrant a direct call are: composing the four layers
#' manually to inspect or modify the Layer 1 → Layer 2 contract; auditing a
#' suspected downstream diagnostic by verifying Layer 1 in isolation; and
#' testing a `g_fn` callback's output before plugging it into the full
#' simulation.
#'
#' \strong{Unit-variance convention.} All eight shapes share a unit-variance
#' standardization: the package draws \eqn{z_j} with \eqn{E[z_j] = 0} and
#' \eqn{\mathrm{Var}(z_j) = 1}, then rescales to
#' \eqn{\tau_j = \tau + X_j\boldsymbol{\beta} + \sigma_\tau\,z_j}. This makes
#' `sigma_tau` a single comparable knob across shapes — heterogeneity targets
#' mean the same thing whether `true_dist = "Gaussian"` or
#' `true_dist = "ALD"`.
#'
#' \strong{Convention A vs Convention B (user callbacks).} Under
#' `g_returns = "standardized"` (Convention A, the default) the callback
#' returns standardized residuals \eqn{z_j}; the package rescales by
#' `sigma_tau` and adds `tau` (and any covariate adjustment) to form `tau_j`.
#' Under `g_returns = "raw"` (Convention B) the callback returns the
#' response-scale effect directly; the package leaves it untouched.
#' Convention A integrates with downstream diagnostics (notably
#' \code{\link{informativeness}} and \code{\link{heterogeneity_ratio}})
#' without further work; Convention B is for callbacks where standardization
#' is meaningless or undesirable. See \code{\link{gen_effects_user}}.
#'
#' \strong{Covariate adjustment.} When `formula` is non-`NULL`, a model
#' matrix \eqn{X} is built from `data` and combined with `beta` to form
#' \eqn{X_j\boldsymbol{\beta}}, which enters the linear predictor for
#' \eqn{\tau_j} additively. The covariate columns from `data` pass through
#' to the returned tibble so downstream layers can recover them.
#'
#' For per-shape derivations and decision rubrics, see the
#' \href{../articles/m2-g-distribution-catalog.html}{G-distribution catalog
#' and standardization} vignette. For the `g_fn` callback contract, see the
#' \href{../articles/m5-custom-g-distributions.html}{Custom G distributions}
#' vignette. For the `formula` / `beta` / `data` covariate surface, see the
#' \href{../articles/a4-covariates-precision-dependence.html}{Covariates and
#' precision dependence} vignette.
#'
#' @param J Integer. Number of sites.
#' @param true_dist Character. One of `"Gaussian"`, `"StudentT"`, `"SkewN"`,
#'   `"ALD"`, `"Mixture"`, `"PointMassSlab"`, `"User"`, or `"DPM"`. Default
#'   `"Gaussian"`. If `g_fn` is supplied without `true_dist`, the package
#'   auto-selects `"User"`.
#' @param tau Numeric. Grand mean on the response scale. Default `0`.
#' @param sigma_tau Numeric (\eqn{\ge 0}). Between-site standard deviation
#'   on the response scale (not variance). Default `0.20`.
#' @param variance Numeric. Legacy Gaussian variance argument. Default `1`.
#'   The unit-variance convention requires `variance = 1`; other shapes
#'   ignore it.
#' @param theta_G Named list of shape-specific parameters. Keys vary by
#'   `true_dist`; see the eight-shape catalog above.
#' @param formula One-sided formula for site-level covariates (e.g.,
#'   `~ x1 + x2`), or `NULL`.
#' @param beta Numeric coefficient vector matching the columns of the model
#'   matrix built from `formula`, or `NULL`.
#' @param data A `data.frame` containing the predictors named in `formula`,
#'   or `NULL`.
#' @param g_fn Optional user callback for `true_dist = "User"` (or for the
#'   `"DPM"` bridge). Receives `J` and returns a length-`J` numeric vector.
#' @param g_returns Character. `"standardized"` (Convention A, default) — the
#'   callback returns standardized residuals \eqn{z_j} and the package
#'   rescales. `"raw"` (Convention B) — the callback returns response-scale
#'   effects and the package does not rescale.
#' @param audit_g Logical. When `g_returns = "standardized"`, validate that
#'   the callback draws meet the unit-moment contract. Default `TRUE`. Has
#'   no effect under `g_returns = "raw"`.
#' @param upstream Reserved for future layer composition. Leave `NULL` (the
#'   default); passing a non-`NULL` value aborts.
#'
#' @return A tibble with one row per site:
#' \describe{
#'   \item{`site_index`}{Integer 1..J — preserved through downstream layers.}
#'   \item{`z_j`}{Standardized residual effect — mean 0, variance 1 by construction.}
#'   \item{`tau_j`}{Response-scale latent effect, \eqn{\tau + X_j\boldsymbol{\beta} + \sigma_\tau\,z_j}.}
#'   \item{`<covariate columns>`}{Pass-through from `data` if `formula` was non-`NULL`.}
#'   \item{`latent_component`}{Character; for `true_dist = "Mixture"`, names which mixture component each row was drawn from. Absent for the other seven shapes.}
#' }
#' The tibble carries no S3 class beyond `tbl_df` — Layer 2 functions add
#' the package's classes on top.
#'
#' @family family-effects
#' @seealso
#'   Shape-specific generators: \code{\link{gen_effects_gaussian}},
#'   \code{\link{gen_effects_studentt}}, \code{\link{gen_effects_skewn}},
#'   \code{\link{gen_effects_ald}}, \code{\link{gen_effects_mixture}},
#'   \code{\link{gen_effects_pmslab}}, \code{\link{gen_effects_user}},
#'   \code{\link{gen_effects_dpm}}.
#'   Wrappers that compose all four layers: \code{\link{sim_multisite}},
#'   \code{\link{sim_meta}}.
#'   The \href{../articles/m2-g-distribution-catalog.html}{M2 G-distribution
#'   catalog} and
#'   \href{../articles/m5-custom-g-distributions.html}{M5 Custom G
#'   distributions} vignettes.
#'
#' @references
#' Lee, J., Che, J., Rabe-Hesketh, S., Feller, A., & Miratrix, L. (2025).
#' Improving the estimation of site-specific effects and their distribution
#' in multisite trials. \emph{Journal of Educational and Behavioral Statistics},
#' \bold{50}(5), 731--764. \doi{10.3102/10769986241254286}.
#'
#' @examples
#' # Gaussian (default — the canonical baseline).
#' gauss <- gen_effects(J = 10L, true_dist = "Gaussian", sigma_tau = 0.2)
#' head(gauss)
#'
#' # Student-t with df = 5 — heavier tails for a robustness check.
#' studentt <- gen_effects(J = 50L, true_dist = "StudentT",
#'                         sigma_tau = 0.2, theta_G = list(nu = 5))
#'
#' # Mixture: two-component bimodal effects.
#' mix <- gen_effects(J = 50L, true_dist = "Mixture",
#'                    sigma_tau = 0.2,
#'                    theta_G = list(delta = 1.0, eps = 0.2, ups = 2.0))
#' table(mix$latent_component)
#'
#' # Covariate-adjusted: tau_j = tau + 0.3 * x_j + sigma_tau * z_j.
#' sites <- data.frame(x = rnorm(20))
#' cov <- gen_effects(J = 20L, true_dist = "Gaussian",
#'                    formula = ~ x, beta = 0.3, data = sites,
#'                    sigma_tau = 0.15)
#'
#' # User callback (Convention A — standardized residuals).
#' my_g <- function(J) rnorm(J)
#' user <- gen_effects(J = 50L, g_fn = my_g)  # auto-selects true_dist = "User"
#' @export
gen_effects <- function(
  J,
  true_dist = c("Gaussian", "StudentT", "SkewN", "ALD", "Mixture", "PointMassSlab", "User", "DPM"),
  tau = 0,
  sigma_tau = 0.20,
  variance = 1,
  theta_G = list(),
  formula = NULL,
  beta = NULL,
  data = NULL,
  g_fn = NULL,
  g_returns = c("standardized", "raw"),
  audit_g = TRUE,
  upstream = NULL
) {
  true_dist_missing <- missing(true_dist)
  if (!is.null(g_fn) && true_dist_missing) {
    true_dist <- "User"
  }
  true_dist <- .match_choice(
    true_dist,
    "true_dist",
    c("Gaussian", "StudentT", "SkewN", "ALD", "Mixture", "PointMassSlab", "User", "DPM")
  )
  g_returns <- .match_choice(g_returns, "g_returns", c("standardized", "raw"))
  audit_g <- .validate_scalar_logical(audit_g, "audit_g")
  theta_G <- .validate_theta_g_container(theta_G)
  g_fn <- .validate_function_or_null(g_fn, "g_fn")
  .validate_g_fn_true_dist(g_fn, true_dist)

  if (!is.null(upstream)) {
    .abort_arg(
      "`upstream` is reserved and is not implemented yet.",
      "Layer 1 starts the multisiteDGP pipeline in v1.",
      "Remove `upstream` for current Layer 1 generators."
    )
  }

  switch(true_dist,
    Gaussian = gen_effects_gaussian(
      J = J,
      tau = tau,
      sigma_tau = sigma_tau,
      variance = variance,
      formula = formula,
      beta = beta,
      data = data
    ),
    StudentT = gen_effects_studentt(
      J = J,
      tau = tau,
      sigma_tau = sigma_tau,
      nu = .studentt_nu_from_theta(theta_G),
      formula = formula,
      beta = beta,
      data = data
    ),
    SkewN = gen_effects_skewn(
      J = J,
      tau = tau,
      sigma_tau = sigma_tau,
      slant = .skewn_slant_from_theta(theta_G),
      formula = formula,
      beta = beta,
      data = data
    ),
    ALD = gen_effects_ald(
      J = J,
      tau = tau,
      sigma_tau = sigma_tau,
      rho = .ald_rho_from_theta(theta_G),
      formula = formula,
      beta = beta,
      data = data
    ),
    Mixture = {
      mixture <- .mixture_params_from_theta(theta_G)
      gen_effects_mixture(
        J = J,
        tau = tau,
        sigma_tau = sigma_tau,
        delta = mixture$delta,
        eps = mixture$eps,
        ups = mixture$ups,
        formula = formula,
        beta = beta,
        data = data
      )
    },
    PointMassSlab = {
      pmslab <- .pmslab_params_from_theta(theta_G)
      gen_effects_pmslab(
        J = J,
        tau = tau,
        sigma_tau = sigma_tau,
        pi0 = pmslab$pi0,
        slab_shape = pmslab$slab_shape,
        mu_slab = pmslab$mu_slab,
        sigma_slab = pmslab$sigma_slab,
        formula = formula,
        beta = beta,
        data = data
      )
    },
    User = gen_effects_user(
      J = J,
      tau = tau,
      sigma_tau = sigma_tau,
      g_fn = g_fn,
      g_returns = g_returns,
      formula = formula,
      beta = beta,
      data = data,
      g_args = theta_G,
      audit_g = audit_g
    ),
    DPM = gen_effects_dpm(
      J = J,
      tau = tau,
      sigma_tau = sigma_tau,
      theta_G = theta_G,
      formula = formula,
      beta = beta,
      data = data,
      g_fn = g_fn,
      g_returns = g_returns,
      audit_g = audit_g
    ),
    .abort_effects_shape_pending(true_dist)
  )
}

.abort_effects_shape_pending <- function(true_dist) {
  .abort_arg(
    sprintf("`true_dist = \"%s\"` is not implemented.", true_dist),
    "Step 3.6 routes all catalog Layer 1 shapes.",
    "Check `true_dist` spelling and use one of the documented catalog shapes."
  )
}

.studentt_nu_from_theta <- function(theta_G) {
  .validate_studentt_theta(theta_G)
}

.skewn_slant_from_theta <- function(theta_G) {
  .validate_skewn_theta(theta_G)
}

.ald_rho_from_theta <- function(theta_G) {
  .validate_ald_theta(theta_G)
}

.mixture_params_from_theta <- function(theta_G) {
  .validate_mixture_theta(theta_G)
}

.pmslab_params_from_theta <- function(theta_G) {
  .validate_pmslab_theta(theta_G)
}
# nolint end
