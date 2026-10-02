#' Sampling-variance constant kappa
#'
#' Computes \eqn{\kappa = (1/p + 1/(1 - p)) (1 - R^2) \sigma_Y^2}, the
#' constant that links the sampling variance of a site estimate to the site
#' size, \eqn{se_j^2 = \kappa / n_j}. [gen_site_sizes()] uses it to turn site
#' sizes into standard errors.
#'
#' @param p Proportion of individuals assigned to treatment (numeric,
#'   strictly between 0 and 1). Default `0.5`.
#' @param R2 Share of the outcome variance explained by covariates,
#'   \eqn{R^2} (numeric, at least 0 and below 1). Default `0`.
#' @param var_outcome Variance of the outcome, \eqn{\sigma_Y^2} (positive
#'   numeric). Default `1`.
#'
#' @return A numeric vector. Arguments must have length 1 or a common length.
#'
#' @seealso [gen_site_sizes()], [mean_shrinkage()],
#'   `vignette("site-sizes-and-standard-errors")`.
#' @family diagnostics
#' @examples
#' compute_kappa()
#' compute_kappa(R2 = 0.4)
#'
#' # Three allocation proportions at once
#' compute_kappa(p = c(0.3, 0.5, 0.7))
#' @export
compute_kappa <- function(p = 0.5, R2 = 0, var_outcome = 1) {
  p <- .validate_probability_open_vector(p, "p")
  R2 <- .validate_r2_vector(R2, "R2")
  var_outcome <- .validate_positive_numeric_vector(var_outcome, "var_outcome")
  .validate_recyclable_lengths(list(p = p, R2 = R2, var_outcome = var_outcome))

  (1 / p + 1 / (1 - p)) * (1 - R2) * var_outcome
}

#' @rdname informativeness
#' @param se2_j Numeric vector of sampling variances, one per site (positive,
#'   at least two values).
#' @param sigma_tau Cross-site standard deviation of the site effects (a
#'   single number, 0 or larger).
#' @param tau_j Optional numeric vector of site effects, as long as `se2_j`
#'   (default `NULL`). It is not used in the calculation. The argument is kept
#'   from earlier versions and is only checked for its length.
#' @export
compute_I <- function(se2_j, sigma_tau, tau_j = NULL) {
  se2_j <- .validate_se2_vector(se2_j, min_length = 2L)
  sigma_tau <- .validate_sigma_tau_diagnostic(sigma_tau)
  if (!is.null(tau_j)) {
    tau_j <- .validate_numeric_vector(tau_j, "tau_j", min_length = length(se2_j))
    if (length(tau_j) != length(se2_j)) {
      .abort(
        "`tau_j` must have the same length as `se2_j`.",
        sprintf("Their lengths are %s and %s.", length(tau_j), length(se2_j))
      )
    }
  }

  if (identical(sigma_tau, 0)) {
    return(0)
  }
  gm_se2 <- exp(mean(log(se2_j)))
  sigma_tau^2 / (sigma_tau^2 + gm_se2)
}

#' Shrinkage of each site and mean shrinkage
#'
#' `compute_shrinkage()` returns
#' \eqn{S_j = \sigma_\tau^2 / (\sigma_\tau^2 + se_j^2)} for each site.
#' `mean_shrinkage()` is a generic that returns the average of \eqn{S_j} for
#' simulated data or a numeric vector. With no data it returns
#' \eqn{\sigma_\tau^2 / (\sigma_\tau^2 + \kappa / n)}, the shrinkage of a
#' site whose size \eqn{n} is the mean site size `nj_mean`.
#'
#' @details
#' Under a normal prior and normal sampling model, \eqn{S_j} is the weight on
#' the site's estimate in the posterior mean. For other effect distributions
#' it is a Gaussian-model summary, not an exact posterior weight.
#'
#' @param se2_j Numeric vector of sampling variances, one per site (positive).
#' @param sigma_tau Cross-site standard deviation of the site effects (a
#'   single number, 0 or larger).
#' @param monotone Logical. `TRUE` sorts the values in increasing order, and
#'   the default `FALSE` keeps the order of `se2_j`.
#'
#' @return `compute_shrinkage()` returns one value per site. `mean_shrinkage()`
#'   returns a single number, or one value per element of a vector `nj_mean`.
#'
#' @seealso [informativeness()], [compute_kappa()], [scenario_audit()],
#'   `vignette("diagnostics-and-presets")`.
#' @family diagnostics
#' @examples
#' compute_shrinkage(c(0.05, 0.10, 0.20), sigma_tau = 0.2)
#'
#' dat <- sim_multisite(preset_education_modest(), seed = 1)
#' mean_shrinkage(dat)
#' mean_shrinkage(dat$se2_j, sigma_tau = 0.2)
#' # No data: sites of mean size 25, 50, and 100
#' mean_shrinkage(nj_mean = c(25, 50, 100), sigma_tau = 0.2)
#' @export
compute_shrinkage <- function(se2_j, sigma_tau, monotone = FALSE) {
  se2_j <- .validate_se2_vector(se2_j, min_length = 1L)
  sigma_tau <- .validate_sigma_tau_diagnostic(sigma_tau)
  monotone <- .validate_scalar_logical(monotone, "monotone")

  shrinkage <- sigma_tau^2 / (sigma_tau^2 + se2_j)
  if (isTRUE(monotone)) {
    return(sort(shrinkage))
  }
  shrinkage
}

#' Informativeness of the site estimates
#'
#' Computes \eqn{I = \sigma_\tau^2 / (\sigma_\tau^2 + GM)}, where \eqn{GM} is
#' the geometric mean of the sampling variances \eqn{se_j^2}. `I` lies
#' between 0 and 1, and a larger value means that the estimates carry more
#' information about the site effects. `informativeness()` is a generic for
#' simulated data or a numeric vector, and its methods call `compute_I()`.
#'
#' @param x Simulated data from [sim_multisite()] or [sim_meta()], or a
#'   numeric vector of sampling variances.
#' @param ... For a numeric `x`, the named argument `sigma_tau` (required).
#'   Not used for simulated data, where `sigma_tau` comes from the design.
#'
#' @return A single number between 0 and 1. It is 0 when `sigma_tau` is 0.
#'
#' @seealso [heterogeneity_ratio()], [compute_shrinkage()], [sim_meta()],
#'   `vignette("diagnostics-and-presets")`.
#' @family diagnostics
#' @order 1
#' @examples
#' dat <- sim_multisite(preset_education_modest(), seed = 1)
#' informativeness(dat)
#'
#' # From sampling variances, with sigma_tau given by name
#' informativeness(dat$se2_j, sigma_tau = 0.2)
#' compute_I(dat$se2_j, sigma_tau = 0.2)
#' @export
informativeness <- function(x, ...) {
  UseMethod("informativeness")
}

#' @export
informativeness.multisitedgp_data <- function(x, ...) {
  design <- attr(x, "design", exact = TRUE)
  if (is.null(design) || is.null(design$sigma_tau)) {
    .abort(
      "`x` has no `design` attribute to take `sigma_tau` from.",
      fix = "Use `informativeness(x$se2_j, sigma_tau = ...)`."
    )
  }
  compute_I(x$se2_j, design$sigma_tau, tau_j = x$tau_j)
}

#' @export
informativeness.numeric <- function(x, ..., sigma_tau) {
  if (missing(sigma_tau)) {
    .abort(
      "`sigma_tau` is required when `x` is a vector of sampling variances.",
      fix = "For example, `informativeness(se2_j, sigma_tau = 0.2)`."
    )
  }
  compute_I(x, sigma_tau = sigma_tau, ...)
}

#' @export
informativeness.default <- function(x, ...) {
  .abort(
    "`x` must be simulated data or a numeric vector of sampling variances.",
    .you_passed(x)
  )
}

#' @rdname compute_shrinkage
#' @param x Simulated data from [sim_multisite()] or [sim_meta()], a numeric
#'   vector of sampling variances, or `NULL` (the default) for no data.
#' @param ... Named arguments. For a numeric `x`: `sigma_tau`. With no data:
#'   `nj_mean` and `sigma_tau`, and optionally `p`, `R2`, and `varY` (the
#'   outcome variance, called `var_outcome` elsewhere), with the defaults of
#'   [compute_kappa()]. For simulated data: none, because `sigma_tau` comes
#'   from the design.
#' @export
mean_shrinkage <- function(x = NULL, ...) {
  if (missing(x) || is.null(x)) {
    return(mean_shrinkage.default(x = NULL, ...))
  }
  UseMethod("mean_shrinkage")
}

#' @export
mean_shrinkage.multisitedgp_data <- function(x, ...) {
  design <- attr(x, "design", exact = TRUE)
  if (is.null(design) || is.null(design$sigma_tau)) {
    .abort(
      "`x` has no `design` attribute to take `sigma_tau` from.",
      fix = "Use `mean_shrinkage(x$se2_j, sigma_tau = ...)`."
    )
  }
  mean(compute_shrinkage(x$se2_j, sigma_tau = design$sigma_tau))
}

#' @export
mean_shrinkage.numeric <- function(x, ..., sigma_tau) {
  if (missing(sigma_tau)) {
    .abort(
      "`sigma_tau` is required when `x` is a vector of sampling variances.",
      fix = "For example, `mean_shrinkage(se2_j, sigma_tau = 0.2)`."
    )
  }
  mean(compute_shrinkage(x, sigma_tau = sigma_tau))
}

#' @export
mean_shrinkage.default <- function(x = NULL, ..., nj_mean, sigma_tau, varY = 1, p = 0.5, R2 = 0) {
  if (!is.null(x)) {
    .abort(
      "`x` must be simulated data, a numeric vector of sampling variances, or `NULL`.",
      .you_passed(x)
    )
  }
  if (missing(nj_mean) || missing(sigma_tau)) {
    .abort(
      "Without data, `mean_shrinkage()` requires `nj_mean` and `sigma_tau`.",
      fix = "For example, `mean_shrinkage(nj_mean = 50, sigma_tau = 0.2)`."
    )
  }

  nj_mean <- .validate_positive_numeric_vector(nj_mean, "nj_mean")
  sigma_tau <- .validate_nonnegative_numeric_vector(sigma_tau, "sigma_tau")
  p <- .validate_probability_open_vector(p, "p")
  R2 <- .validate_r2_vector(R2, "R2")
  varY <- .validate_positive_numeric_vector(varY, "varY")
  .validate_recyclable_lengths(list(nj_mean = nj_mean, sigma_tau = sigma_tau, p = p, R2 = R2, varY = varY))

  se2 <- compute_kappa(p = p, R2 = R2, var_outcome = varY) / nj_mean
  sigma_tau^2 / (sigma_tau^2 + se2)
}

#' Default thresholds for scenario_audit()
#'
#' Returns the three thresholds that [scenario_audit()] applies to the
#' diagnostics of each design. A name ending in `_min` is a lower limit and a
#' name ending in `_max` an upper limit. To change a threshold, pass a
#' modified list to `scenario_audit()` as `thresholds`, or only the entries to
#' change, as in `thresholds = list(R_max = 10)`.
#'
#' @details
#' | Threshold | Applies to | Group | Default |
#' |---|---|---|---|
#' | `mean_shrinkage_min` | [mean_shrinkage()] | precision | 0.3 |
#' | `R_max` | [heterogeneity_ratio()] | scale | 30 |
#' | `ks_scaled_max` | [ks_distance()] times `sqrt(J)` | shape | 1.63 |
#'
#' The first two are working limits of the package, not published cutoffs.
#' The third is the 1 percent critical value of the Kolmogorov-Smirnov test.
#' When the site effects come from the distribution of the design, the
#' distance times `sqrt(J)` has nearly the same distribution for every number
#' of sites `J`: a median near 0.8, a 95th percentile between 1.3 and 1.4, and
#' a value above 1.63 in about 1 data set in 100. `summary()` of simulated
#' data does not use these thresholds.
#'
#' @return A named list of three numbers.
#'
#' @seealso [scenario_audit()], [design_grid()],
#'   `vignette("diagnostics-and-presets")`.
#' @family diagnostics
#' @examples
#' str(default_thresholds())
#'
#' # Require a mean shrinkage of at least 0.4 (M is small to keep this fast)
#' th <- default_thresholds()
#' th$mean_shrinkage_min <- 0.4
#' grid <- design_grid(J = 200, sigma_tau = c(0.2, 0.3), seed_root = 1)
#' audit <- scenario_audit(grid, M = 5, thresholds = th)
#' audit[, c("sigma_tau", "status", "fail_reasons")]
#' @export
default_thresholds <- function() {
  list(
    mean_shrinkage_min = 0.30,
    R_max = 30.0,
    ks_scaled_max = 1.63
  )
}

.validate_se2_vector <- function(se2_j, min_length = 1L) {
  .validate_positive_numeric_vector(se2_j, "se2_j", min_length = min_length)
}

.validate_numeric_vector <- function(x, arg, min_length = 1L) {
  if (!is.numeric(x) || length(x) < min_length || anyNA(x) || any(!is.finite(x))) {
    .abort(
      if (min_length > 1L) {
        sprintf("`%s` must be a numeric vector of at least %s finite values.", arg, min_length)
      } else {
        sprintf("`%s` must be a numeric vector of finite values.", arg)
      },
      if (!is.numeric(x)) {
        .you_passed(x)
      } else if (length(x) < min_length) {
        sprintf("It has length %s.", length(x))
      } else {
        "It has missing or infinite values."
      }
    )
  }
  as.numeric(x)
}

.validate_positive_numeric_vector <- function(x, arg, min_length = 1L) {
  x <- .validate_numeric_vector(x, arg, min_length = min_length)
  if (any(x <= 0)) {
    .abort(
      sprintf("`%s` must be positive.", arg),
      .you_passed_values(x)
    )
  }
  x
}

.validate_nonnegative_numeric_vector <- function(x, arg, min_length = 1L) {
  x <- .validate_numeric_vector(x, arg, min_length = min_length)
  if (any(x < 0)) {
    .abort(
      sprintf("`%s` must be 0 or larger.", arg),
      .you_passed_values(x)
    )
  }
  x
}

.validate_sigma_tau_diagnostic <- function(sigma_tau) {
  sigma_tau <- .validate_nonnegative_numeric_vector(sigma_tau, "sigma_tau")
  if (length(sigma_tau) != 1L) {
    .abort(
      "`sigma_tau` must be a single number.",
      sprintf("It has length %s.", length(sigma_tau))
    )
  }
  sigma_tau
}

.validate_probability_open_vector <- function(x, arg) {
  x <- .validate_numeric_vector(x, arg)
  if (any(x <= 0 | x >= 1)) {
    .abort(
      sprintf("`%s` must be strictly between 0 and 1.", arg),
      .you_passed_values(x)
    )
  }
  x
}

.validate_r2_vector <- function(x, arg) {
  x <- .validate_numeric_vector(x, arg)
  if (any(x < 0 | x >= 1)) {
    .abort(
      sprintf("`%s` must be at least 0 and below 1.", arg),
      .you_passed_values(x)
    )
  }
  x
}

.validate_recyclable_lengths <- function(x) {
  lengths <- vapply(x, length, integer(1))
  max_length <- max(lengths)
  bad <- lengths != 1L & lengths != max_length
  if (any(bad)) {
    .abort(
      sprintf(
        "%s must have length 1 or a common length.",
        paste(sprintf("`%s`", names(lengths)), collapse = ", ")
      ),
      sprintf("Their lengths are %s.", paste(lengths, collapse = ", "))
    )
  }
  invisible(TRUE)
}
