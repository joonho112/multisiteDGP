.site_sizes_censored <- function(J, nj_mean = 50, cv = 0.50, nj_min = 5L) {
  J <- .validate_j(J)
  cv <- .validate_scalar_number(cv, "cv")
  nj <- .validate_nj_mean_nj_min(nj_mean, nj_min, allow_equal = .is_zero_cv(cv))
  if (cv < 0) {
    .abort("`cv` must be at least 0.", .you_passed(cv))
  }

  if (identical(cv, 0) || isTRUE(all.equal(cv, 0))) {
    if (nj$nj_mean != floor(nj$nj_mean)) {
      .abort(
        "`nj_mean` must be a whole number when `cv = 0`.",
        sprintf("You passed `nj_mean = %s`. With `cv = 0` every site has size `nj_mean`.", format(nj$nj_mean))
      )
    }
    return(tibble::tibble(n_j = rep(as.integer(nj$nj_mean), J)))
  }

  params <- .legacy_jebs_gamma_params(nj_mean = nj$nj_mean, cv = cv)
  n_j_raw <- stats::rgamma(n = J, shape = params$shape, rate = params$rate)
  tibble::tibble(n_j = as.integer(round(pmax(nj$nj_min, n_j_raw), 0)))
}

.legacy_jebs_gamma_params <- function(nj_mean, cv) {
  list(
    shape = 1 / cv^2,
    rate = 1 / (cv^2 * nj_mean)
  )
}
