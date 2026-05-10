# nolint start: object_name_linter, object_usage_linter
engine_legacy_jebs_censor_round <- function(J, nj_mean = 50, cv = 0.50, nj_min = 5L) {
  J <- .validate_j(J)
  cv <- .validate_scalar_number(cv, "cv")
  nj <- .validate_nj_mean_nj_min(nj_mean, nj_min, allow_equal = .is_zero_cv(cv))
  if (cv < 0) {
    .abort_arg(
      "`cv` must be >= 0.",
      "The legacy JEBS site-size engine uses `cv` as a Gamma coefficient of variation.",
      "Use `cv = 0` for deterministic site sizes or a positive value such as `cv = 0.5`."
    )
  }

  if (identical(cv, 0) || isTRUE(all.equal(cv, 0))) {
    if (nj$nj_mean != floor(nj$nj_mean)) {
      .abort_arg(
        "`nj_mean` must be integer-like when `cv = 0` in Engine A1.",
        "The deterministic legacy branch returns one integer site size per site.",
        "Pass an integer-like value such as `nj_mean = 50` or use `cv > 0`."
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
# nolint end
