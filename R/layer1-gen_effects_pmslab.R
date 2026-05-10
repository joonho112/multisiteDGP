# nolint start: object_name_linter, object_usage_linter
#' Generate point-mass-plus-slab latent site effects
#'
#' @encoding UTF-8
#'
#' @description
#' Draw J standardized site effects from a mixed measure: a point mass at
#' zero with probability `pi0` and a zero-centered continuous slab with
#' probability `1 - pi0`. Apply the shared Layer 1 location-scale wrapper to
#' produce \eqn{\tau_j = \tau + X_j\boldsymbol{\beta} + \sigma_\tau\,z_j}.
#' Reach for this shape when a fraction of sites have null effects (no
#' treatment effect, by design or by selection) and the rest have a
#' continuous effect distribution.
#'
#' @details
#' The slab is centered at zero and rescaled so that the total residual
#' variance equals 1: a draw \eqn{z_j} comes from a point mass at 0 with
#' probability `pi0` and from a zero-mean slab with SD
#' \eqn{1/\sqrt{1 - \mathrm{pi0}}} with probability `1 - pi0`. The slab
#' shape is either Gaussian (default) or Laplace, controlled by
#' `slab_shape`. The pre-standardization slab parameters `mu_slab` and
#' `sigma_slab` are reserved for raw-form specification but are normalized
#' to the unit-variance contract before the draw lands in `z_j` —
#' applied users can leave them at their defaults.
#'
#' For the broader catalog and decision rubric, see the
#' \href{../articles/m2-g-distribution-catalog.html}{G-distribution catalog
#' and standardization} vignette.
#'
#' @param J Integer. Number of sites.
#' @param tau Numeric. Grand mean on the response scale. Default `0`.
#' @param sigma_tau Numeric (\eqn{\ge 0}). Between-site standard deviation
#'   on the response scale. Default `0.20`.
#' @param pi0 Numeric in `(0, 1)`. Point-mass probability at zero. Required
#'   — no default. `pi0 = 0.2` puts 20% of sites at exactly zero;
#'   `pi0 = 0.5` is balanced. Endpoints are refused.
#' @param slab_shape Character. Slab family — `"Gaussian"` (default) or
#'   `"Laplace"`. Laplace gives a sharper peak at zero with exponential
#'   tails; Gaussian is smoother.
#' @param mu_slab Numeric. Raw slab location before standardization.
#'   Default `0`. Most users leave this at the default.
#' @param sigma_slab Numeric (> 0). Raw slab SD before standardization.
#'   Default `1`. Most users leave this at the default; the package
#'   automatically rescales to the unit-variance contract.
#' @param formula One-sided formula for site-level covariates, or `NULL`.
#' @param beta Numeric coefficient vector matching `formula`, or `NULL`.
#' @param data A `data.frame` with the predictors named in `formula`, or
#'   `NULL`.
#'
#' @return A tibble with one row per site and columns `site_index` (integer
#'   `1:J`), `z_j` (unit-variance point-mass-slab residual; exactly 0 with
#'   probability `pi0`), `tau_j` (response-scale effect), plus any covariate
#'   columns from `data`.
#'
#' @family family-effects
#' @seealso
#'   \code{\link{gen_effects}} for the dispatcher and the full eight-shape
#'   catalog;
#'   \code{\link{gen_effects_mixture}} for a related two-component mixture
#'   shape (without a point mass);
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
#' # 20% null sites + 80% Gaussian slab.
#' pms <- gen_effects_pmslab(J = 50L, pi0 = 0.2)
#' sum(pms$z_j == 0)  # ~ 10 sites with exactly-zero standardized effect
#'
#' # Sharper peak at zero with Laplace slab; 30% null sites.
#' gen_effects_pmslab(J = 50L, pi0 = 0.3, slab_shape = "Laplace",
#'                    sigma_tau = 0.15)
#' @export
gen_effects_pmslab <- function(
  J,
  tau = 0,
  sigma_tau = 0.20,
  pi0,
  slab_shape = c("Gaussian", "Laplace"),
  mu_slab = 0,
  sigma_slab = 1,
  formula = NULL,
  beta = NULL,
  data = NULL
) {
  params <- .validate_pmslab_params(
    pi0 = pi0,
    slab_shape = slab_shape,
    mu_slab = mu_slab,
    sigma_slab = sigma_slab
  )
  preflight <- .preflight_effects_frame_inputs(
    J = J,
    tau = tau,
    sigma_tau = sigma_tau,
    formula = formula,
    beta = beta,
    data = data
  )
  J <- preflight$J
  z_j <- .draw_pmslab_z(
    J = J,
    pi0 = params$pi0,
    slab_shape = params$slab_shape,
    mu_slab = params$mu_slab,
    sigma_slab = params$sigma_slab
  )
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

.validate_pmslab_theta <- function(theta_g) {
  .validate_named_list(theta_g, "theta_G")
  if (!"pi0" %in% names(theta_g)) {
    .abort_arg(
      "`true_dist = \"PointMassSlab\"` requires `theta_G$pi0`.",
      "`pi0` is the point-mass probability at zero.",
      "Use `theta_G = list(pi0 = 0.5)` or another value strictly between 0 and 1."
    )
  }
  .validate_pmslab_params(
    pi0 = theta_g$pi0,
    slab_shape = .theta_value(theta_g, "slab_shape", "Gaussian"),
    mu_slab = .theta_value(theta_g, "mu_slab", 0),
    sigma_slab = .theta_value(theta_g, "sigma_slab", 1)
  )
}

.validate_pmslab_params <- function(
  pi0,
  slab_shape = c("Gaussian", "Laplace"),
  mu_slab = 0,
  sigma_slab = 1
) {
  if (missing(pi0)) {
    .abort_arg(
      "`pi0` is required for PointMassSlab effects.",
      "`pi0` is the point-mass probability at zero.",
      "Use `pi0 = 0.5` or another value strictly between 0 and 1."
    )
  }
  pi0 <- .validate_scalar_number(pi0, "pi0")
  if (pi0 <= 0 || pi0 >= 1) {
    .abort_arg(
      "`pi0` must be strictly between 0 and 1.",
      "Both endpoints are refused: `pi0 = 0` is all slab; `pi0 = 1` is degenerate.",
      "Use `pi0 = 0.20`, `pi0 = 0.50`, or another interior value."
    )
  }
  slab_shape <- .normalize_pmslab_slab_shape(slab_shape)
  mu_slab <- .validate_scalar_number(mu_slab, "mu_slab")
  sigma_slab <- .validate_scalar_number(sigma_slab, "sigma_slab")
  if (sigma_slab <= 0) {
    .abort_arg(
      "`sigma_slab` must be > 0.",
      "`sigma_slab` is the raw slab standard deviation before unit-variance standardization.",
      "Use `sigma_slab = 1` unless you are matching a custom raw slab scale."
    )
  }
  list(pi0 = pi0, slab_shape = slab_shape, mu_slab = mu_slab, sigma_slab = sigma_slab)
}

.draw_pmslab_z <- function(J, pi0, slab_shape, mu_slab, sigma_slab) {
  is_point_mass <- stats::runif(J) < pi0
  z_j <- numeric(J)
  n_slab <- sum(!is_point_mass)
  if (n_slab == 0L) {
    return(z_j)
  }
  target_slab_sd <- 1 / sqrt(1 - pi0)
  raw <- switch(slab_shape,
    Gaussian = stats::rnorm(n_slab, mean = mu_slab, sd = sigma_slab),
    Laplace = mu_slab + .rlaplace_centered(n_slab, sd = sigma_slab)
  )
  z_j[!is_point_mass] <- ((raw - mu_slab) / sigma_slab) * target_slab_sd
  z_j
}

.rlaplace_centered <- function(n, sd) {
  (stats::rexp(n) - stats::rexp(n)) * (sd / sqrt(2))
}

.normalize_pmslab_slab_shape <- function(slab_shape) {
  if (length(slab_shape) == 1L && is.character(slab_shape)) {
    lower <- tolower(slab_shape)
    if (identical(lower, "gaussian")) {
      slab_shape <- "Gaussian"
    } else if (identical(lower, "laplace")) {
      slab_shape <- "Laplace"
    }
  }
  .match_choice(slab_shape, "slab_shape", c("Gaussian", "Laplace"))
}

.theta_value <- function(theta_g, name, default) {
  if (name %in% names(theta_g)) {
    return(theta_g[[name]])
  }
  default
}
# nolint end
