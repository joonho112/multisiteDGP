# nolint start: object_name_linter, object_usage_linter
#' Generate Dirichlet-process-mixture latent site effects
#'
#' @encoding UTF-8
#'
#' @description
#' Draw J latent site effects from a Dirichlet-process mixture. The current
#' release ships only the user-callback bridge for this shape — to use
#' `true_dist = "DPM"` you must supply a `g_fn` bridge that draws DPM
#' samples (typically fitted by an external package such as \pkg{dirichletprocess}
#' or the JAGS / Stan posterior of a DPM model). Calling without a `g_fn`
#' aborts with an install-hint pointing to the user-callback path.
#'
#' @details
#' DPM as a built-in shape is on the roadmap but currently routes through
#' the user-callback convention. When `g_fn` is supplied, this function
#' delegates to \code{\link{gen_effects_user}} with `theta_G` forwarded as
#' `g_args`. All Convention A / Convention B semantics, the audit pass, and
#' the cross-link to the Custom G vignette apply identically.
#'
#' Typical workflow for the bridge: fit a DPM offline (e.g., via
#' \pkg{dirichletprocess::DirichletProcessGaussian}), wrap a finite-sample
#' draw in a `g_fn(J, ...)` closure that returns standardized residuals
#' (Convention A), and pass that closure to `sim_multisite()` /
#' `sim_meta()` via `g_fn` and `true_dist = "DPM"` (or just `"User"` —
#' the same code path).
#'
#' For the formal callback contract, see the
#' \href{../articles/m5-custom-g-distributions.html}{Custom G distributions}
#' vignette.
#'
#' @param J Integer. Number of sites.
#' @param tau Numeric. Grand mean on the response scale. Default `0`.
#' @param sigma_tau Numeric (\eqn{\ge 0}). Between-site standard deviation
#'   on the response scale. Default `0.20`.
#' @param theta_G Named list. When a `g_fn` bridge is supplied, this list
#'   is forwarded to the bridge as extra arguments (after `J`). Default
#'   `list()`.
#' @param formula One-sided formula for site-level covariates, or `NULL`.
#' @param beta Numeric coefficient vector matching `formula`, or `NULL`.
#' @param data A `data.frame` with the predictors named in `formula`, or
#'   `NULL`.
#' @param g_fn Optional DPM bridge callback with signature
#'   `function(J, ...)`. When `NULL`, the function aborts with an
#'   install-hint pointing to the user-callback route.
#' @param g_returns Character. Bridge convention — `"standardized"`
#'   (Convention A, default) or `"raw"` (Convention B). See
#'   \code{\link{gen_effects_user}} for the contract.
#' @param audit_g Logical. Whether to validate (Convention A only) that
#'   the bridge's draws meet the unit-moment contract. Default `TRUE`.
#'
#' @return A tibble with one row per site and columns `site_index`,
#'   `z_j`, `tau_j`, plus any covariate columns from `data`. Returned only
#'   when a `g_fn` bridge is supplied; otherwise the function aborts.
#'
#' @family family-effects
#' @seealso
#'   \code{\link{gen_effects_user}} for the underlying user-callback
#'   pathway that the bridge routes through;
#'   \code{\link{gen_effects}} for the dispatcher and the full eight-shape
#'   catalog;
#'   the \href{../articles/m5-custom-g-distributions.html}{M5 Custom G
#'   distributions} vignette.
#'
#' @references
#' Lee, J., Che, J., Rabe-Hesketh, S., Feller, A., & Miratrix, L. (2025).
#' Improving the estimation of site-specific effects and their distribution
#' in multisite trials. \emph{Journal of Educational and Behavioral Statistics},
#' \bold{50}(5), 731--764. \doi{10.3102/10769986241254286}.
#'
#' @examples
#' # Bridge a Gaussian DPM draw (placeholder — substitute a real DPM sampler).
#' gen_effects_dpm(
#'   J = 10L,
#'   g_fn = function(J) rnorm(J),
#'   audit_g = FALSE
#' )
#' @export
gen_effects_dpm <- function(
  J,
  tau = 0,
  sigma_tau = 0.20,
  theta_G = list(),
  formula = NULL,
  beta = NULL,
  data = NULL,
  g_fn = NULL,
  g_returns = c("standardized", "raw"),
  audit_g = TRUE
) {
  theta_G <- .validate_theta_g_container(theta_G)
  g_fn <- .validate_function_or_null(g_fn, "g_fn")
  audit_g <- .validate_scalar_logical(audit_g, "audit_g")
  if (!is.null(g_fn)) {
    return(gen_effects_user(
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
    ))
  }
  .abort_dpm_stub()
}

.validate_user_and_dpm_presence <- function(g_fn, true_dist) {
  if (identical(true_dist, "User") && is.null(g_fn)) {
    .abort_arg(
      "`g_fn` is required when `true_dist = \"User\"`.",
      "The User shape has no built-in generator.",
      "Pass `g_fn = function(J, ...) ...` or choose a built-in `true_dist`."
    )
  }
  if (identical(true_dist, "DPM") && is.null(g_fn)) {
    .abort_dpm_stub()
  }
  invisible(TRUE)
}

.abort_dpm_stub <- function() {
  .abort_arg(
    "`true_dist = \"DPM\"` is not implemented in multisiteDGP v1.",
    "Built-in Dirichlet Process Mixture sampling is deferred to v2 or a sibling package.",
    "Use `true_dist = \"User\"` with a custom `g_fn`, or pass `g_fn` as an explicit DPM bridge."
  )
}
# nolint end
