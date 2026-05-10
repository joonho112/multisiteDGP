# nolint start: object_name_linter, object_usage_linter
#' Generate user-supplied latent site effects
#'
#' @encoding UTF-8
#'
#' @description
#' Draw J latent site effects from a user-supplied callback `g_fn(J, ...)`
#' under one of two conventions: Convention A (`g_returns = "standardized"`,
#' the default) — the callback returns mean-0 unit-variance residuals
#' \eqn{z_j} that the package rescales by `sigma_tau` and shifts by
#' `tau` (and any covariate adjustment) to form `tau_j`. Convention B
#' (`g_returns = "raw"`) — the callback returns response-scale effects
#' directly; the package leaves them untouched and back-derives `z_j` for
#' provenance.
#'
#' @details
#' Use the user callback when none of the seven built-in shapes
#' (Gaussian, Student-t, skew-normal, ALD, mixture, point-mass slab, DPM)
#' captures the effect distribution you need. Typical use cases: a DPM
#' draw fitted offline, a custom skew-mixture, a deconvolution-derived
#' empirical \eqn{G}, or a numerical sampler from a non-standard
#' distribution.
#'
#' \strong{Convention A vs B.} Convention A is the default and integrates
#' cleanly with downstream diagnostics
#' (\code{\link{informativeness}}, \code{\link{heterogeneity_ratio}},
#' \code{\link{compute_shrinkage}}) without further work. Convention B is
#' for callbacks where standardization is meaningless or the package's
#' linear rescaling is not what you want; you carry the responsibility for
#' the scale. The package back-derives `z_j = (tau_j - tau - X*beta) / sigma_tau`
#' so downstream Layer 2 / 3 / 4 code still has a `z_j` column to reason
#' about.
#'
#' \strong{Audit.} When `audit_g = TRUE` (default) and
#' `g_returns = "standardized"`, the package draws an extra 100,000-site
#' audit sample from `g_fn` (in a side RNG stream that does not disturb
#' the caller's state) and warns if the empirical mean is more than 0.1
#' off zero or the empirical variance is more than 0.1 off 1. This catches
#' the most common contract violations without aborting the run.
#'
#' For the formal `g_fn` contract and worked examples, see the
#' \href{../articles/m5-custom-g-distributions.html}{Custom G distributions}
#' vignette.
#'
#' @param J Integer. Number of sites.
#' @param tau Numeric. Grand mean on the response scale. Default `0`.
#' @param sigma_tau Numeric (\eqn{\ge 0}). Between-site standard deviation
#'   on the response scale. Default `0.20`.
#' @param g_fn Function. Required user callback with signature
#'   `function(J, ...)`. Must return a finite numeric vector of length `J`.
#'   Under Convention A, the vector should be mean-0 unit-variance; under
#'   Convention B, the vector is the response-scale effect directly.
#' @param g_returns Character. `"standardized"` (Convention A, default) —
#'   the callback returns standardized residuals \eqn{z_j} and the package
#'   rescales. `"raw"` (Convention B) — the callback returns response-scale
#'   `tau_j` directly; the package back-derives `z_j`.
#' @param formula One-sided formula for site-level covariates, or `NULL`.
#' @param beta Numeric coefficient vector matching `formula`, or `NULL`.
#' @param data A `data.frame` with the predictors named in `formula`, or
#'   `NULL`.
#' @param g_args Named list forwarded to `g_fn` as extra arguments after
#'   `J`. Default `list()`.
#' @param audit_g Logical. Whether to validate (Convention A only) that
#'   the callback's draws meet the unit-moment contract. Default `TRUE`.
#'   No-op under `g_returns = "raw"`.
#'
#' @return A tibble with one row per site and columns `site_index`
#'   (integer `1:J`), `z_j` (standardized residual; back-derived under
#'   Convention B), `tau_j` (response-scale effect), plus any covariate
#'   columns from `data`.
#'
#' @family family-effects
#' @seealso
#'   \code{\link{gen_effects}} for the dispatcher and the full eight-shape
#'   catalog;
#'   \code{\link{gen_effects_dpm}} for the bridged DPM route;
#'   \code{\link{gen_effects_gaussian}} and the seven built-in shapes;
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
#' # Convention A: callback returns standardized residuals.
#' my_g <- function(J) rnorm(J)  # mean 0, variance 1 by construction
#' gen_effects_user(J = 50L, g_fn = my_g)
#'
#' # Convention B: callback returns response-scale effects directly.
#' raw_g <- function(J) rnorm(J, mean = 0.3, sd = 0.15)
#' gen_effects_user(J = 50L, g_fn = raw_g, g_returns = "raw",
#'                  tau = 0.3, sigma_tau = 0.15)
#'
#' # Skipping the audit (deterministic callback at small J).
#' gen_effects_user(
#'   J = 10L,
#'   g_fn = function(J) seq(-1, 1, length.out = J),
#'   audit_g = FALSE
#' )
#' @export
gen_effects_user <- function(
  J,
  tau = 0,
  sigma_tau = 0.20,
  g_fn,
  g_returns = c("standardized", "raw"),
  formula = NULL,
  beta = NULL,
  data = NULL,
  g_args = list(),
  audit_g = TRUE
) {
  J <- .validate_j(J)
  g_fn <- .validate_required_g_fn(g_fn)
  g_returns <- .match_choice(g_returns, "g_returns", c("standardized", "raw"))
  g_args <- .validate_list(g_args, "g_args")
  audit_g <- .validate_scalar_logical(audit_g, "audit_g")
  preflight <- .preflight_effects_frame_inputs(
    J = J,
    tau = tau,
    sigma_tau = sigma_tau,
    formula = formula,
    beta = beta,
    data = data
  )
  draw <- .call_user_g_fn(g_fn = g_fn, J = J, g_args = g_args, context = "g_fn")

  if (identical(g_returns, "standardized")) {
    if (isTRUE(audit_g)) {
      .audit_user_g_fn(g_fn = g_fn, g_args = g_args)
    }
    return(.new_effects_frame(
      z_j = draw,
      J = J,
      tau = tau,
      sigma_tau = sigma_tau,
      formula = formula,
      beta = beta,
      data = data,
      preflight = preflight
    ))
  }
  .new_raw_user_effects_frame(
    tau_j = draw,
    J = J,
    tau = tau,
    sigma_tau = sigma_tau,
    formula = formula,
    beta = beta,
    data = data,
    preflight = preflight
  )
}

.validate_required_g_fn <- function(g_fn) {
  if (missing(g_fn) || is.null(g_fn)) {
    .abort_arg(
      "`g_fn` is required for user-supplied effects.",
      "`true_dist = \"User\"` delegates residual generation to a user callback.",
      "Pass `g_fn = function(J, ...) ...` or choose a built-in `true_dist`."
    )
  }
  .validate_function_or_null(g_fn, "g_fn")
}

.call_user_g_fn <- function(g_fn, J, g_args, context) {
  out <- tryCatch(
    do.call(g_fn, c(list(J = J), g_args)),
    error = function(e) {
      .abort_arg(
        sprintf("`%s` failed while generating user-supplied effects.", context),
        conditionMessage(e),
        "Check that the callback accepts `J` and all names supplied in `g_args` or `theta_G`."
      )
    }
  )
  .validate_user_g_output(out, J = J, context = context)
}

.validate_user_g_output <- function(out, J, context) {
  if (!is.numeric(out) || length(out) != J || anyNA(out) || any(!is.finite(out))) {
    .abort_arg(
      sprintf("`%s` must return a finite numeric vector of length `J`.", context),
      sprintf("Expected length %s; got length %s.", J, length(out)),
      "Use one finite numeric value per site."
    )
  }
  as.numeric(out)
}

.audit_user_g_fn <- function(g_fn, g_args) {
  audit_n <- 100000L
  audit_draw <- withr::with_preserve_seed(
    .call_user_g_fn(
      g_fn = g_fn,
      J = audit_n,
      g_args = g_args,
      context = "g_fn audit"
    )
  )
  audit_mean <- mean(audit_draw)
  audit_var <- stats::var(audit_draw)
  if (abs(audit_mean) > 0.1 || abs(audit_var - 1) > 0.1) {
    cli::cli_warn(c(
      "!" = "`g_fn` audit suggests the standardized residual contract may be violated.",
      "i" = sprintf("Audit mean = %.3f; audit variance = %.3f.", audit_mean, audit_var),
      ">" = "Return mean-zero, unit-variance draws or use `g_returns = \"raw\"`."
    ))
  }
  invisible(list(mean = audit_mean, variance = audit_var))
}

.new_raw_user_effects_frame <- function(
  tau_j,
  J,
  tau,
  sigma_tau,
  formula = NULL,
  beta = NULL,
  data = NULL,
  preflight = NULL
) {
  if (is.null(preflight)) {
    preflight <- .preflight_effects_frame_inputs(
      J = J,
      tau = tau,
      sigma_tau = sigma_tau,
      formula = formula,
      beta = beta,
      data = data
    )
  }
  J <- preflight$J
  tau <- preflight$tau
  sigma_tau <- preflight$sigma_tau
  tau_j <- .validate_user_g_output(tau_j, J = J, context = "g_fn")
  covariates <- preflight$covariates
  z_j <- .validate_z_j((tau_j - tau - covariates$effect) / sigma_tau, J = J)
  base <- tibble::tibble(
    site_index = seq_len(J),
    z_j = z_j,
    tau_j = tau_j
  )
  if (is.null(covariates$data)) {
    return(base)
  }
  tibble::as_tibble(c(as.list(base), as.list(covariates$data)))
}
# nolint end
