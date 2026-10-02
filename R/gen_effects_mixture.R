#' Draw site effects from a two-component normal mixture
#'
#' Draws `J` standardized site effects `z_j` from a mixture of two normal
#' distributions, shifted and scaled to mean 0 and variance 1. In a design
#' this is `true_dist = "Mixture"` with
#' `theta_G = list(delta = , eps = , ups = )`.
#'
#' @inheritParams gen_effects
#' @param delta Number (greater than 0), required. Distance between the two
#'   component means, in standard deviations of the first component.
#' @param eps Number (strictly between 0 and 1), required. Probability of the
#'   second component, which has the larger mean.
#' @param ups Number (greater than 0), required. Standard deviation of the
#'   second component divided by that of the first.
#'   Extremely large `delta` or `ups` values are rejected when the mixture
#'   variance cannot be represented numerically.
#'
#' @return A tibble with one row per site and columns `site_index`, `z_j`,
#'   `tau_j`, and `latent_component` (1 or 2, the component of each site),
#'   followed by the columns of `data`, if any.
#'
#' @seealso [gen_effects()], [gen_effects_pmslab()], [presets],
#'   `vignette("effect-distributions")`.
#' @family site effects
#' @examples
#' set.seed(1)
#' eff <- gen_effects_mixture(J = 50, delta = 5, eps = 0.3, ups = 2)
#' eff
#'
#' # Number of sites in each component
#' table(eff$latent_component)
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
    .abort(
      "`true_dist = \"Mixture\"` requires `theta_G$delta`, `theta_G$eps`, and `theta_G$ups`.",
      sprintf("Missing: %s.", paste(missing_params, collapse = ", ")),
      "For example, `theta_G = list(delta = 5, eps = 0.3, ups = 2)`."
    )
  }
  .validate_mixture_params(
    delta = theta_g$delta, eps = theta_g$eps, ups = theta_g$ups,
    prefix = "theta_G$"
  )
}

# `prefix` is "theta_G$" when the parameters come from a design, so that the
# messages name them as the user gave them.
.validate_mixture_params <- function(delta, eps, ups, prefix = "") {
  if (missing(delta)) {
    .abort(
      "`delta` is required.",
      "It is the distance between the two component means, a positive number."
    )
  }
  if (missing(eps)) {
    .abort(
      "`eps` is required.",
      "It is the probability of the second component, strictly between 0 and 1."
    )
  }
  if (missing(ups)) {
    .abort(
      "`ups` is required.",
      "It is the standard deviation of the second component over that of the first, a positive number."
    )
  }
  delta <- .validate_scalar_number(delta, paste0(prefix, "delta"))
  eps <- .validate_scalar_number(eps, paste0(prefix, "eps"))
  ups <- .validate_scalar_number(ups, paste0(prefix, "ups"))
  if (delta <= 0) {
    .abort(sprintf("`%sdelta` must be positive.", prefix), .you_passed(delta))
  }
  if (eps <= 0 || eps >= 1) {
    .abort(sprintf("`%seps` must be strictly between 0 and 1.", prefix), .you_passed(eps))
  }
  if (ups <= 0) {
    .abort(sprintf("`%sups` must be positive.", prefix), .you_passed(ups))
  }
  variance <- .legacy_mixture_variance(delta = delta, eps = eps, ups = ups)
  if (!is.finite(variance)) {
    .abort(
      sprintf(
        "`%sdelta` and `%sups` give a mixture variance that is too large for numerical standardization.",
        prefix, prefix
      ),
      fix = "Use a smaller `delta` or `ups`."
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
