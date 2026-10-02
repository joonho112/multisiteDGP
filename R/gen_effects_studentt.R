#' Draw site effects from a Student t distribution
#'
#' Draws `J` standardized site effects `z_j` from a Student t distribution
#' with `nu` degrees of freedom, multiplied by
#' \eqn{\sqrt{(\nu - 2) / \nu}}{sqrt((nu - 2) / nu)} so that they have
#' variance 1. The excess kurtosis is \eqn{6 / (\nu - 4)} for \eqn{\nu > 4}.
#' In a design this is `true_dist = "StudentT"` with `theta_G = list(nu = )`.
#'
#' @inheritParams gen_effects
#' @param nu Degrees of freedom, a single number (greater than 2). Required.
#'   A value of 4 or less gives a warning. The kurtosis is then infinite and
#'   the sample variance of `z_j` is below 1 in most samples.
#'
#' @return A tibble with one row per site and columns `site_index`, `z_j`,
#'   and `tau_j`, followed by the columns of `data`, if any.
#'
#' @seealso [gen_effects()], [gen_effects_gaussian()],
#'   `vignette("effect-distributions")`.
#' @family site effects
#' @examples
#' set.seed(1)
#' eff <- gen_effects_studentt(J = 50, nu = 5)
#' eff
#'
#' # Share of draws beyond 3 standard deviations (0.003 for a normal)
#' z <- gen_effects_studentt(J = 10000, nu = 5)$z_j
#' mean(abs(z) > 3)
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
    .abort(
      "`nu` is required.",
      "It is the degrees of freedom, a number greater than 2."
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
  nu <- .validate_studentt_df(nu, "nu")
  if (nu <= 4) {
    cli::cli_warn(c(
      "!" = sprintf("`nu = %s` gives a distribution with infinite kurtosis.", format(nu)),
      "i" = "The sample variance of `z_j` is then below 1 in most samples.",
      ">" = "Use `nu > 4` for a finite kurtosis."
    ))
  }
  nu
}
