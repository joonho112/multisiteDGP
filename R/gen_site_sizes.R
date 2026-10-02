#' Generate site sizes and standard errors
#'
#' Draws a site size `n_j` for each site, with a given mean, coefficient of
#' variation, and minimum, and sets the sampling variance `se2_j` to
#' `kappa / n_j`. [sim_multisite()] runs this step after [gen_effects()]. Use
#' [gen_se_direct()] to set the sampling variances directly instead.
#'
#' @details
#' The truncated-gamma generator draws from a gamma distribution truncated at
#' `nj_min` whose mean and coefficient of variation after truncation are
#' `nj_mean` and `cv`, and rounds each draw down or up at random. The rounding
#' keeps the mean and adds variance, so the site sizes vary slightly more than
#' `cv`, most visibly when `cv` is small. The function stops with an error
#' when `cv` is too large for the ratio `nj_min / nj_mean`. The limit is about
#' 1.4 at a ratio of 0.1, 1.1 at 0.2, and 0.57 at 0.5.
#'
#' The censored generator draws from a gamma distribution with mean `nj_mean`
#' and coefficient of variation `cv`, raises values below `nj_min` to `nj_min`,
#' and rounds to the nearest integer.
#'
#' @param upstream The data from the previous step: a data frame with columns
#'   `site_index`, `z_j`, and `tau_j`, usually from [gen_effects()]. It must
#'   not have `n_j`, `se2_j`, or `se_j`.
#' @param J Integer number of sites (at least 10), equal to `nrow(upstream)`.
#' @param nj_mean,cv,nj_min Mean (above `nj_min`, default `50`), coefficient of
#'   variation (at least 0, default `0.5`), and integer minimum (at least 1,
#'   default `5`) of the site sizes. With `cv = 0` every site has size
#'   `nj_mean`, which must then be a whole number and may equal `nj_min`.
#' @param p,R2,var_outcome Proportion treated (above 0, below 1, default `0.5`),
#'   share of outcome variance explained by covariates (at least 0, below 1,
#'   default `0`), and outcome variance (above 0, default `1`).
#'   [compute_kappa()] turns them into `kappa`.
#' @param engine Site-size generator: `"truncated_gamma"` (the default), or
#'   `"censored"` to reproduce the procedure of Lee et al. (2025).
#' @return The input data as a tibble, with the columns `n_j` (integer site
#'   size), `se2_j` (sampling variance), and `se_j` (standard error) added. The
#'   generator and `kappa` are stored as the attributes `engine` and `kappa`.
#' @references Lee, J., Che, J., Rabe-Hesketh, S., Feller, A., & Miratrix, L.
#'   (2025). Improving the estimation of site-specific effects and their
#'   distribution in multisite trials. *Journal of Educational and Behavioral
#'   Statistics*, 50(5), 731-764. \doi{10.3102/10769986241254286}
#' @seealso [compute_kappa()], [sim_multisite()],
#'   `vignette("site-sizes-and-standard-errors")`.
#' @family site sizes
#' @examples
#' set.seed(1)
#' effects <- gen_effects(J = 10)
#' gen_site_sizes(effects, J = 10, nj_mean = 40, cv = 0.5, nj_min = 10)
#'
#' # The censored generator of Lee et al. (2025)
#' sizes <- gen_site_sizes(effects, J = 10, nj_mean = 40, engine = "censored")
#' sizes$n_j
#' @export
gen_site_sizes <- function(
  upstream,
  J,
  nj_mean = 50,
  cv = 0.50,
  nj_min = 5L,
  p = 0.5,
  R2 = 0,
  var_outcome = 1,
  engine = c("truncated_gamma", "censored")
) {
  if (missing(upstream)) {
    .abort(
      "`upstream` is required.",
      "It is the result of `gen_effects()`, with one row per site."
    )
  }
  if (missing(J)) {
    .abort(
      "`J` is required.",
      "It is the number of sites, `nrow(upstream)`."
    )
  }

  J <- .validate_j(J)
  upstream <- .validate_effects_upstream(upstream, J, caller = "gen_site_sizes")
  engine <- .match_choice(engine, "engine", c("truncated_gamma", "censored"))
  cv <- .validate_scalar_number(cv, "cv")
  nj <- .validate_nj_mean_nj_min(nj_mean, nj_min, allow_equal = .is_zero_cv(cv))
  if (cv < 0) {
    .abort("`cv` must be at least 0.", .you_passed(cv))
  }
  .validate_public_deterministic_site_size(nj$nj_mean, cv)
  p <- .validate_scalar_number(p, "p")
  R2 <- .validate_scalar_number(R2, "R2")
  var_outcome <- .validate_scalar_number(var_outcome, "var_outcome")

  kappa <- compute_kappa(p = p, R2 = R2, var_outcome = var_outcome)
  sizes <- switch(
    engine,
    truncated_gamma = .site_sizes_truncated_gamma(J = J, nj_mean = nj$nj_mean, cv = cv, nj_min = nj$nj_min),
    censored = .site_sizes_censored(J = J, nj_mean = nj$nj_mean, cv = cv, nj_min = nj$nj_min)
  )
  n_j <- .materialize_public_n_j(sizes$n_j, nj_min = nj$nj_min, engine = engine)
  se2_j <- as.numeric(kappa / n_j)
  out <- dplyr::mutate(
    upstream,
    n_j = n_j,
    se2_j = se2_j,
    se_j = sqrt(se2_j)
  )
  attr(out, "engine") <- engine
  attr(out, "kappa") <- kappa
  out
}

.validate_effects_upstream <- function(upstream, J, caller = "gen_site_sizes") {
  takes <- sprintf("`%s()` takes the result of `gen_effects()`.", caller)
  if (!inherits(upstream, "data.frame")) {
    .abort(
      "`upstream` must be a data frame.",
      .you_passed(upstream),
      takes
    )
  }
  if (nrow(upstream) != J) {
    .abort(
      "`upstream` must have `J` rows.",
      sprintf("`nrow(upstream)` is %s and `J` is %s.", nrow(upstream), J)
    )
  }
  required <- c("site_index", "z_j", "tau_j")
  missing_required <- setdiff(required, names(upstream))
  if (length(missing_required) > 0L) {
    .abort(
      "`upstream` must have the columns `site_index`, `z_j`, and `tau_j`.",
      sprintf("Missing columns: %s.", paste(missing_required, collapse = ", ")),
      takes
    )
  }
  reserved <- intersect(names(upstream), c("n_j", "se2_j", "se_j"))
  if (length(reserved) > 0L) {
    .abort(
      sprintf("`upstream` must not have the columns `n_j`, `se2_j`, and `se_j`, which `%s()` adds.", caller),
      sprintf("It has: %s.", paste(reserved, collapse = ", "))
    )
  }
  tibble::as_tibble(upstream)
}

.validate_public_deterministic_site_size <- function(nj_mean, cv) {
  if (.is_zero_cv(cv) && nj_mean != floor(nj_mean)) {
    .abort(
      "`nj_mean` must be a whole number when `cv = 0`.",
      sprintf("You passed `nj_mean = %s`. With `cv = 0` every site has size `nj_mean`.", format(nj_mean))
    )
  }
  invisible(TRUE)
}

.is_zero_cv <- function(cv) {
  identical(cv, 0) || isTRUE(all.equal(cv, 0))
}

.materialize_public_n_j <- function(n_j, nj_min, engine) {
  if (!is.numeric(n_j) || anyNA(n_j) || any(!is.finite(n_j))) {
    .abort("The site-size generator returned values that are not finite numbers.")
  }
  if (identical(engine, "censored")) {
    n_j <- as.integer(n_j)
  } else {
    n_j <- .stochastic_round_site_sizes(n_j)
  }
  n_j <- as.integer(pmax(nj_min, n_j))
  if (any(!is.finite(n_j)) || any(n_j < nj_min)) {
    .abort(sprintf("The site-size generator returned a site size below `nj_min = %s`.", nj_min))
  }
  n_j
}

.stochastic_round_site_sizes <- function(x) {
  lower <- floor(x)
  fractional <- x - lower
  needs_round <- fractional > .Machine$double.eps
  if (!any(needs_round)) {
    return(as.integer(lower))
  }
  increments <- integer(length(x))
  increments[needs_round] <- as.integer(stats::runif(sum(needs_round)) < fractional[needs_round])
  as.integer(lower + increments)
}
