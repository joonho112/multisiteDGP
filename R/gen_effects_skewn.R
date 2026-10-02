#' Draw site effects from a skew-normal distribution
#'
#' Draws `J` standardized site effects `z_j` from the skew-normal distribution
#' of Azzalini (1985) with slant `slant`, shifted and scaled to mean 0 and
#' variance 1. In a design this is `true_dist = "SkewN"` with
#' `theta_G = list(slant = )`.
#'
#' @details
#' A draw is \eqn{\delta |U_0| + \sqrt{1 - \delta^2} U_1}, the representation
#' of Henze (1986), with \eqn{\delta = \alpha / \sqrt{1 + \alpha^2}} and
#' independent standard normal \eqn{U_0} and \eqn{U_1}. The two are consecutive
#' values of one `rnorm()` call, the order that `sn::rsn()` uses (Azzalini,
#' 2026). For a seed the draws equal those of `sn::rsn()`, which version 0.2.0
#' called.
#'
#' @inheritParams gen_effects
#' @param slant Finite number, required. Slant \eqn{\alpha} of the
#'   skew-normal density \eqn{2 \phi(y) \Phi(\alpha y)} before standardization:
#'   positive values give a longer right tail, negative values a longer left
#'   tail. `slant = 0` (the normal distribution) and `abs(slant) > 30` give a
#'   warning. Values too large for numerical standardization are rejected.
#'
#' @return A tibble with one row per site and columns `site_index`, `z_j`,
#'   and `tau_j`, followed by the columns of `data`, if any.
#'
#' @references
#' Azzalini, A. (1985). A class of distributions which includes the normal
#' ones. *Scandinavian Journal of Statistics*, 12(2), 171-178.
#'
#' Azzalini, A. (2026). *The R package sn: The skew-normal and related
#' distributions such as the skew-t and the SUN* (version 2.1.3).
#' <https://cran.r-project.org/package=sn>
#'
#' Henze, N. (1986). A probabilistic representation of the "skew-normal"
#' distribution. *Scandinavian Journal of Statistics*, 13, 271-275.
#'
#' @seealso [gen_effects()], [gen_effects_ald()],
#'   `vignette("effect-distributions")`.
#' @family site effects
#' @examples
#' set.seed(1)
#' eff <- gen_effects_skewn(J = 50, slant = 5)
#' eff
#'
#' # Skewness of the draws, which is 0 for a normal distribution
#' z <- gen_effects_skewn(J = 10000, slant = 5)$z_j
#' mean(z^3)
#' @export
gen_effects_skewn <- function(
  J,
  tau = 0,
  sigma_tau = 0.20,
  slant,
  formula = NULL,
  beta = NULL,
  data = NULL
) {
  slant <- .validate_skewn_slant(slant)
  preflight <- .preflight_effects_frame_inputs(
    J = J,
    tau = tau,
    sigma_tau = sigma_tau,
    formula = formula,
    beta = beta,
    data = data
  )
  J <- preflight$J

  delta <- slant / sqrt(1 + slant^2)
  mu_y <- delta * sqrt(2 / pi)
  sd_y <- sqrt(1 - 2 * delta^2 / pi)
  y <- .rskewnormal(J, alpha = slant)
  z_j <- (y - mu_y) / sd_y

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

# Skew-normal draws from pairs of standard normal draws. Each pair is two
# consecutive values of one rnorm() call, which fixes the result for a seed.
.rskewnormal <- function(n, alpha) {
  delta <- alpha / sqrt(1 + alpha^2)
  z <- matrix(stats::rnorm(2 * n), nrow = 2L)
  delta * abs(z[1L, ]) + sqrt(1 - delta^2) * z[2L, ]
}

.validate_skewn_theta <- function(theta_g) {
  .validate_named_list(theta_g, "theta_G")
  if (!"slant" %in% names(theta_g)) {
    .abort(
      "`true_dist = \"SkewN\"` requires `theta_G$slant`.",
      fix = "For example, `theta_G = list(slant = 2)`."
    )
  }
  .validate_skewn_slant(theta_g$slant, arg = "theta_G$slant")
}

# `arg` is the name under which the user gave the parameter: `slant` in
# gen_effects_skewn() and `theta_G$slant` in a design.
.validate_skewn_slant <- function(slant, arg = "slant") {
  if (missing(slant)) {
    .abort(
      "`slant` is required.",
      "It is the slant of the skew-normal distribution, a finite number."
    )
  }
  slant <- .validate_scalar_number(slant, arg)
  if (!is.finite(slant^2)) {
    .abort(
      sprintf("`%s` is too large for numerical standardization.", arg),
      .you_passed(slant),
      "Use a smaller absolute slant."
    )
  }
  if (identical(slant, 0)) {
    cli::cli_warn(c(
      "!" = sprintf("`%s = 0` gives the normal distribution, which has no skewness.", arg)
    ))
  }
  if (abs(slant) > 30) {
    cli::cli_warn(c(
      "!" = sprintf("`%s` is beyond 30 in absolute value, where the distribution is close to a half-normal.", arg),
      "i" = .you_passed(slant)
    ))
  }
  slant
}
