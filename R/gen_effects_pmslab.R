#' Draw site effects from a point mass and a slab
#'
#' Draws `J` standardized site effects `z_j`, each exactly 0 with probability
#' `pi0` and otherwise drawn from a normal or Laplace slab centered at 0. The
#' slab is scaled so that `z_j` has variance 1. In a design this is
#' `true_dist = "PointMassSlab"` with `theta_G = list(pi0 = )`.
#'
#' @inheritParams gen_effects
#' @param pi0 Number (strictly between 0 and 1), required. Probability that
#'   `z_j` is exactly 0. Then `tau_j = tau`, plus any covariate term.
#' @param slab_shape `"Gaussian"` (the default) or `"Laplace"`. Distribution
#'   of the slab.
#' @param mu_slab,sigma_slab Numbers (defaults 0 and 1). They have no effect
#'   on the distribution, because `z_j` is standardized. Leave them at their
#'   defaults. A scale too small to represent reliably, or too small relative
#'   to the location, is rejected before any draws.
#'
#' @return A tibble with one row per site and columns `site_index`, `z_j`,
#'   and `tau_j`, followed by the columns of `data`, if any.
#'
#' @seealso [gen_effects()], [gen_effects_mixture()],
#'   `vignette("effect-distributions")`.
#' @family site effects
#' @examples
#' set.seed(1)
#' eff <- gen_effects_pmslab(J = 50, pi0 = 0.2)
#' eff
#'
#' # Share of sites in the point mass
#' mean(eff$z_j == 0)
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
    .abort(
      "`true_dist = \"PointMassSlab\"` requires `theta_G$pi0`.",
      fix = "For example, `theta_G = list(pi0 = 0.5)`."
    )
  }
  .validate_pmslab_params(
    pi0 = theta_g$pi0,
    slab_shape = .theta_value(theta_g, "slab_shape", "Gaussian"),
    mu_slab = .theta_value(theta_g, "mu_slab", 0),
    sigma_slab = .theta_value(theta_g, "sigma_slab", 1),
    prefix = "theta_G$"
  )
}

# `prefix` is "theta_G$" when the parameters come from a design, so that the
# messages name them as the user gave them.
.validate_pmslab_params <- function(
  pi0,
  slab_shape = c("Gaussian", "Laplace"),
  mu_slab = 0,
  sigma_slab = 1,
  prefix = ""
) {
  if (missing(pi0)) {
    .abort(
      "`pi0` is required.",
      "It is the probability that `z_j` is exactly 0, strictly between 0 and 1."
    )
  }
  pi0 <- .validate_scalar_number(pi0, paste0(prefix, "pi0"))
  if (pi0 <= 0 || pi0 >= 1) {
    .abort(sprintf("`%spi0` must be strictly between 0 and 1.", prefix), .you_passed(pi0))
  }
  slab_shape <- .normalize_pmslab_slab_shape(slab_shape, arg = paste0(prefix, "slab_shape"))
  mu_slab <- .validate_scalar_number(mu_slab, paste0(prefix, "mu_slab"))
  sigma_slab <- .validate_scalar_number(sigma_slab, paste0(prefix, "sigma_slab"))
  if (sigma_slab <= 0) {
    .abort(sprintf("`%ssigma_slab` must be positive.", prefix), .you_passed(sigma_slab))
  }
  if (sigma_slab < .Machine$double.xmin) {
    .abort(
      sprintf("`%ssigma_slab` is too small for reliable numerical standardization.", prefix),
      .you_passed(sigma_slab),
      "Leave `mu_slab` and `sigma_slab` at their defaults, 0 and 1."
    )
  }
  if (sigma_slab <= abs(mu_slab) * .Machine$double.eps) {
    .abort(
      sprintf(
        "`%ssigma_slab` is too small relative to `%smu_slab` for reliable numerical standardization.",
        prefix, prefix
      ),
      "The slab scale is not resolvable at this location.",
      "Leave `mu_slab` and `sigma_slab` at their defaults, 0 and 1."
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

.normalize_pmslab_slab_shape <- function(slab_shape, arg = "slab_shape") {
  if (length(slab_shape) == 1L && is.character(slab_shape)) {
    lower <- tolower(slab_shape)
    if (identical(lower, "gaussian")) {
      slab_shape <- "Gaussian"
    } else if (identical(lower, "laplace")) {
      slab_shape <- "Laplace"
    }
  }
  .match_choice(slab_shape, arg, c("Gaussian", "Laplace"))
}

.theta_value <- function(theta_g, name, default) {
  if (name %in% names(theta_g)) {
    return(theta_g[[name]])
  }
  default
}
