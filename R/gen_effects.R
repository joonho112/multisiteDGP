.builtin_g_shapes <- function() {
  c("Gaussian", "StudentT", "SkewN", "ALD", "Mixture", "PointMassSlab")
}

.generated_g_shapes <- function() {
  c(.builtin_g_shapes(), "User")
}

#' Draw site effects from a chosen distribution
#'
#' Draws `J` standardized site effects \eqn{z_j} from the distribution named
#' by `true_dist` and returns them with the site effects
#' \eqn{\tau_j = \tau + x_j'\beta + \sigma_\tau z_j}. Every built-in
#' distribution is standardized to mean 0 and variance 1 before `sigma_tau`
#' is applied. This is the first step of [sim_multisite()] and [sim_meta()].
#'
#' @details
#' | `true_dist` | Distribution of \eqn{z_j} | Entries of `theta_G` | Function |
#' |---|---|---|---|
#' | `"Gaussian"` | standard normal | none | [gen_effects_gaussian()] |
#' | `"StudentT"` | Student t | `nu` | [gen_effects_studentt()] |
#' | `"SkewN"` | skew normal | `slant` | [gen_effects_skewn()] |
#' | `"ALD"` | asymmetric Laplace | `rho` | [gen_effects_ald()] |
#' | `"Mixture"` | mixture of two normals | `delta`, `eps`, `ups` | [gen_effects_mixture()] |
#' | `"PointMassSlab"` | 0 with probability `pi0`, otherwise normal or Laplace | `pi0`, `slab_shape = "Gaussian"` | [gen_effects_pmslab()] |
#' | `"User"` | set by `g_fn` | arguments of `g_fn` | [gen_effects_user()] |
#'
#' @param J Number of sites, an integer (at least 10).
#' @param true_dist Name of the distribution, a value from the table under
#'   Details. Default `"Gaussian"`, or `"User"` when `g_fn` is given.
#' @param tau Grand mean of the site effects, a single number. Default `0`.
#' @param sigma_tau Cross-site standard deviation of the site effects, a
#'   single number (greater than 0). Default `0.2`.
#' @param theta_G Named list of the parameters of the site-effect
#'   distribution, which the model writes G. Entries shown under Details
#'   without a default are required. Default `list()`.
#' @param formula,beta,data Site-level covariates \eqn{x_j}, or `NULL` (the
#'   default) for none. `formula` is a one-sided formula such as `~ x`, `data`
#'   a data frame with `J` rows that holds its variables, and `beta` the
#'   coefficients of the model matrix columns, in order or by name (an omitted
#'   intercept coefficient is 0). `sigma_tau` is then the standard deviation
#'   of the site effects around \eqn{\tau + x_j'\beta}.
#' @param g_fn Function for `true_dist = "User"`, or `NULL` (the default). It
#'   takes `J` and the entries of `theta_G` and returns `J` finite numbers.
#' @param g_returns What `g_fn` returns: `"standardized"` draws \eqn{z_j} with
#'   mean 0 and variance 1 (the default), or `"raw"` site effects \eqn{\tau_j}.
#' @param audit_g Logical. If `TRUE` (the default) and `g_returns` is
#'   `"standardized"`, the function checks 100,000 further draws from `g_fn`
#'   and warns if their mean or variance is off by more than 0.1.
#'
#' @return A tibble with one row per site and columns `site_index`, `z_j`,
#'   and `tau_j`, followed by the columns of `data`, if any. `"Mixture"` adds
#'   a column `latent_component` (the component, 1 or 2) after `tau_j`.
#'
#' @seealso [gen_site_sizes()], [gen_se_direct()], [multisitedgp_design()],
#'   `vignette("effect-distributions")`.
#' @family site effects
#' @examples
#' set.seed(1)
#' gen_effects(J = 10)
#'
#' # The parameters of a distribution go in theta_G
#' gen_effects(J = 10, true_dist = "Mixture",
#'             theta_G = list(delta = 5, eps = 0.3, ups = 2))
#'
#' # A site-level covariate with coefficient 0.3
#' sites <- data.frame(x = rnorm(10))
#' gen_effects(J = 10, formula = ~ x, beta = 0.3, data = sites)
#' @export
gen_effects <- function(
  J,
  true_dist = c("Gaussian", "StudentT", "SkewN", "ALD", "Mixture", "PointMassSlab", "User"),
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
  true_dist_missing <- missing(true_dist)
  if (!is.null(g_fn) && true_dist_missing) {
    true_dist <- "User"
  }
  true_dist <- .match_choice(
    true_dist,
    "true_dist",
    .generated_g_shapes()
  )
  g_returns <- .match_choice(g_returns, "g_returns", c("standardized", "raw"))
  audit_g <- .validate_scalar_logical(audit_g, "audit_g")
  theta_G <- .validate_theta_g_container(theta_G)
  g_fn <- .validate_function_or_null(g_fn, "g_fn")
  .validate_g_fn_true_dist(g_fn, true_dist)

  switch(true_dist,
    Gaussian = gen_effects_gaussian(
      J = J,
      tau = tau,
      sigma_tau = sigma_tau,
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
    .abort_effects_shape_pending(true_dist)
  )
}

.abort_effects_shape_pending <- function(true_dist) {
  .abort(
    sprintf("`true_dist` must be one of %s.", .format_choices(.generated_g_shapes())),
    .you_passed(true_dist)
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
