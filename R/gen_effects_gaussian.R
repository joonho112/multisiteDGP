#' Draw site effects from a Gaussian distribution
#'
#' Draws `J` standardized site effects `z_j` from the standard normal
#' distribution, the default of [gen_effects()]. That page describes how `z_j`
#' is scaled by `sigma_tau` and combined with `tau` and the covariates.
#'
#' @inheritParams gen_effects
#'
#' @return A tibble with one row per site and columns `site_index`, `z_j`,
#'   and `tau_j`, followed by the columns of `data`, if any.
#'
#' @seealso [gen_effects()], [gen_effects_studentt()],
#'   `vignette("effect-distributions")`.
#' @family site effects
#' @examples
#' set.seed(1)
#' eff <- gen_effects_gaussian(J = 50, tau = 0.1, sigma_tau = 0.2)
#' eff
#' @export
gen_effects_gaussian <- function(
  J,
  tau = 0,
  sigma_tau = 0.20,
  formula = NULL,
  beta = NULL,
  data = NULL
) {
  preflight <- .preflight_effects_frame_inputs(
    J = J,
    tau = tau,
    sigma_tau = sigma_tau,
    formula = formula,
    beta = beta,
    data = data
  )
  z_j <- stats::rnorm(preflight$J)
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
