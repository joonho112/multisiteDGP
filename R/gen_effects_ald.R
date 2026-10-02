#' Draw site effects from an asymmetric Laplace distribution
#'
#' Draws `J` standardized site effects `z_j` from the asymmetric Laplace
#' distribution of Yu and Zhang (2005), indexed by the quantile `rho` and
#' shifted and scaled to mean 0 and variance 1. In a design this is
#' `true_dist = "ALD"` with `theta_G = list(rho = )`.
#'
#' @details
#' A draw is `log(u1^kappa / u2^(1 / kappa)) / sqrt(2)` times a scale, with
#' `kappa = sqrt(rho / (1 - rho))` and two vectors of uniform draws `u1` and
#' `u2`. This is the formula of `LaplacesDemon::ralaplace()`. For a seed the
#' draws equal those of that function, which version 0.2.0 called.
#'
#' @inheritParams gen_effects
#' @param rho Number (strictly between 0.05 and 0.95), required. Probability
#'   that a draw falls below the mode. Before standardization the density
#'   decays exponentially at rate `rho` above the mode and `1 - rho` below it.
#'   `rho = 0.5` gives the symmetric Laplace distribution, smaller values a
#'   longer right tail, larger values a longer left tail.
#'
#' @return A tibble with one row per site and columns `site_index`, `z_j`,
#'   and `tau_j`, followed by the columns of `data`, if any.
#'
#' @references
#' Yu, K., & Zhang, J. (2005). A three-parameter asymmetric Laplace
#' distribution and its extension. *Communications in Statistics - Theory
#' and Methods*, 34(9-10), 1867-1879. \doi{10.1080/03610920500199018}
#'
#' @seealso [gen_effects()], [gen_effects_skewn()],
#'   `vignette("effect-distributions")`.
#' @family site effects
#' @examples
#' set.seed(1)
#' eff <- gen_effects_ald(J = 50, rho = 0.3)
#' eff
#'
#' # With rho below 0.5 the right tail is the longer one
#' z <- gen_effects_ald(J = 10000, rho = 0.3)$z_j
#' c(mean = mean(z), median = median(z))
#' @export
gen_effects_ald <- function(
  J,
  tau = 0,
  sigma_tau = 0.20,
  rho,
  formula = NULL,
  beta = NULL,
  data = NULL
) {
  rho <- .validate_ald_rho(rho)
  preflight <- .preflight_effects_frame_inputs(
    J = J,
    tau = tau,
    sigma_tau = sigma_tau,
    formula = formula,
    beta = beta,
    data = data
  )
  J <- preflight$J

  kappa <- sqrt(rho / (1 - rho))
  lambda <- sqrt(2 / (rho * (1 - rho)))
  x <- .rasymlaplace(J, scale = lambda, kappa = kappa)
  mu_x <- (1 - 2 * rho) / (rho * (1 - rho))
  var_x <- (1 - 2 * rho + 2 * rho^2) / (rho^2 * (1 - rho)^2)
  z_j <- (x - mu_x) / sqrt(var_x)

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

# Asymmetric Laplace draws at location 0, from two uniform draws. The first
# uniform vector is drawn before the second, which fixes the result for a seed.
.rasymlaplace <- function(n, scale, kappa) {
  u1 <- stats::runif(n)
  u2 <- stats::runif(n)
  scale * log(u1^kappa / u2^(1 / kappa)) / sqrt(2)
}

.validate_ald_theta <- function(theta_g) {
  .validate_named_list(theta_g, "theta_G")
  if (!"rho" %in% names(theta_g)) {
    .abort(
      "`true_dist = \"ALD\"` requires `theta_G$rho`.",
      fix = "For example, `theta_G = list(rho = 0.3)`."
    )
  }
  .validate_ald_rho(theta_g$rho, arg = "theta_G$rho")
}

# `arg` is the name under which the user gave the parameter: `rho` in
# gen_effects_ald() and `theta_G$rho` in a design.
.validate_ald_rho <- function(rho, arg = "rho") {
  if (missing(rho)) {
    .abort(
      "`rho` is required.",
      "It is a number strictly between 0.05 and 0.95."
    )
  }
  rho <- .validate_scalar_number(rho, arg)
  if (rho <= 0.05 || rho >= 0.95) {
    .abort(
      sprintf("`%s` must be strictly between 0.05 and 0.95.", arg),
      .you_passed(rho)
    )
  }
  rho
}
