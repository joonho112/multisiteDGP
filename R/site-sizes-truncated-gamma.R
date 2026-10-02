.site_sizes_truncated_gamma <- function(
  J,
  nj_mean = 50,
  cv = 0.50,
  nj_min = 5L,
  tol = 1e-6,
  max_starts = 5L,
  max_iter = 100L
) {
  J <- .validate_j(J)
  cv <- .validate_scalar_number(cv, "cv")
  nj <- .validate_nj_mean_nj_min(nj_mean, nj_min, allow_equal = .is_zero_cv(cv))
  if (cv < 0) {
    .abort("`cv` must be at least 0.", .you_passed(cv))
  }
  if (.is_zero_cv(cv)) {
    return(tibble::tibble(n_j = rep(nj$nj_mean, J)))
  }

  solution <- .solve_truncated_gamma(
    n_bar = nj$nj_mean,
    cv = cv,
    n_min = nj$nj_min,
    tol = tol,
    max_starts = max_starts,
    max_iter = max_iter
  )
  tibble::tibble(n_j = .rtrunc_gamma(J, solution$alpha, solution$beta, nj$nj_min))
}

# `n_bar` and `n_min` are the `nj_mean` and `nj_min` of gen_site_sizes(), and
# the messages use those names.
.solve_truncated_gamma <- function(
  n_bar,
  cv,
  n_min,
  tol = 1e-6,
  max_starts = 5L,
  max_iter = 100L
) {
  n_bar <- .validate_scalar_number(n_bar, "nj_mean")
  n_min <- .validate_scalar_integer(n_min, "nj_min")
  if (n_min < 1L || n_bar <= n_min) {
    .abort(
      "`nj_mean` must be larger than `nj_min`, and `nj_min` must be at least 1.",
      sprintf("You passed `nj_mean = %s` and `nj_min = %s`.", format(n_bar), n_min)
    )
  }
  cv <- .validate_scalar_number(cv, "cv")
  if (cv < 0) {
    .abort("`cv` must be at least 0.", .you_passed(cv))
  }
  tol <- .validate_scalar_number(tol, "tol")
  if (tol <= 0) {
    .abort("The solver tolerance `tol` must be positive.", .you_passed(tol))
  }
  max_starts <- .validate_scalar_integer(max_starts, "max_starts")
  if (max_starts < 1L) {
    .abort("The number of solver starts `max_starts` must be at least 1.", .you_passed(max_starts))
  }
  max_iter <- .validate_scalar_integer(max_iter, "max_iter")
  if (max_iter < 1L) {
    .abort("The solver iteration limit `max_iter` must be at least 1.", .you_passed(max_iter))
  }
  if (identical(cv, 0) || isTRUE(all.equal(cv, 0))) {
    return(list(
      alpha = Inf,
      beta = Inf,
      mean = n_bar,
      sd = 0,
      cv = 0,
      residual = c(mean = 0, sd = 0),
      start = NA_integer_,
      termcd = NA_integer_,
      message = "deterministic"
    ))
  }

  .warn_trunc_gamma_conditioning(n_bar = n_bar, cv = cv, n_min = n_min)
  starts <- .trunc_gamma_starts(n_bar = n_bar, cv = cv, max_starts = max_starts)
  fits <- Map(
    f = .fit_trunc_gamma_start,
    start = starts,
    start_id = seq_along(starts),
    MoreArgs = list(n_bar = n_bar, cv = cv, n_min = n_min, max_iter = max_iter)
  )
  verified <- Filter(function(fit) isTRUE(fit$verified), fits)
  if (length(verified) == 0L) {
    .abort_cv_unreachable(n_bar = n_bar, cv = cv, n_min = n_min)
  }

  # The scores are rounded before the best start is chosen. Two starts that
  # fit equally well would otherwise be ordered by rounding error, which
  # differs between platforms, and the site sizes would differ with it.
  scores <- signif(
    vapply(verified, `[[`, numeric(1), "residual_norm"),
    .TRUNC_GAMMA_SELECTION_DIGITS
  )
  best <- verified[[which.min(scores)]]
  moments <- .trunc_gamma_moments(best$alpha, best$beta, n_min)
  sd_floor <- moments$sd_relative_error_bound
  if (!is.finite(sd_floor) || sd_floor > .TRUNC_GAMMA_MAX_SD_RELAXATION) {
    .abort_solver(
      sprintf(
        "The site-size distribution for `cv = %s`, `nj_mean = %s`, and `nj_min = %s` cannot be checked.",
        format(cv), format(n_bar), n_min
      ),
      sprintf(
        "Its standard deviation has a relative rounding error of %s, above the limit of %s.",
        format(sd_floor, digits = 3),
        format(.TRUNC_GAMMA_MAX_SD_RELAXATION, digits = 3)
      ),
      "Change `cv`, or move `nj_mean` further above `nj_min`."
    )
  }
  tol_effective <- c(mean = tol, sd = max(tol, sd_floor))
  failed_mean <- abs(best$residual[["mean"]]) > tol_effective[["mean"]]
  failed_sd <- abs(best$residual[["sd"]]) > tol_effective[["sd"]]
  if (failed_mean || failed_sd) {
    .abort_cv_unreachable(n_bar = n_bar, cv = cv, n_min = n_min)
  }

  list(
    alpha = best$alpha,
    beta = best$beta,
    mean = moments$mean,
    sd = moments$sd,
    cv = moments$cv,
    residual = best$residual,
    tol_effective = tol_effective,
    start = best$start,
    termcd = best$termcd,
    message = best$message
  )
}

# No gamma distribution truncated at `nj_min` has mean `nj_mean` and the
# requested `cv`. The largest `cv` falls as `nj_min / nj_mean` rises.
.abort_cv_unreachable <- function(n_bar, cv, n_min) {
  .abort_solver(
    sprintf(
      "`cv = %s` cannot be reached with `nj_mean = %s` and `nj_min = %s`.",
      format(cv), format(n_bar), n_min
    ),
    sprintf(
      "`cv` is too large for the ratio `nj_min / nj_mean`, which is %s.",
      format(n_min / n_bar, digits = 3)
    ),
    "Use a smaller `cv` or a smaller `nj_min`."
  )
}

# The standard deviation of the fitted distribution is checked to `tol`, or to
# the estimated rounding error of its computation if that is larger. An
# estimate above this limit is an error, because the fit cannot be checked.
.TRUNC_GAMMA_MAX_SD_RELAXATION <- 1e-3

# The rounding error of a sum is taken to be this multiple of the machine
# epsilon times the sum of the absolute terms.
.TRUNC_GAMMA_ROUNDOFF_SCALE <- 32

# Significant digits kept from the solver's answer. The answer is determined
# to about 12 digits, so rounding to 10 gives the same value on every platform.
.TRUNC_GAMMA_SOLUTION_DIGITS <- 10L

# Significant digits of the score used to compare starts. Rounding error
# cannot change the order at 6 digits, and a better fit still wins.
.TRUNC_GAMMA_SELECTION_DIGITS <- 6L

.trunc_gamma_moments <- function(alpha, beta, n_min) {
  alpha <- .validate_positive_scalar_number(alpha, "alpha")
  beta <- .validate_positive_scalar_number(beta, "beta")
  n_min <- .validate_scalar_integer(n_min, "nj_min")
  if (n_min < 1L) {
    .abort("`nj_min` must be at least 1.", .you_passed(n_min))
  }

  components <- .trunc_gamma_recurrence_components(alpha, beta, n_min)
  raw_first <- components$mean_numerator / beta
  variance_numerator <- components$variance_numerator
  roundoff_bound <- components$variance_roundoff_bound
  if (variance_numerator < 0 && abs(variance_numerator) <= roundoff_bound) {
    variance_numerator <- 0
  }
  if (!is.finite(variance_numerator) || variance_numerator < 0) {
    variance <- NaN
  } else {
    variance <- variance_numerator / beta^2
  }
  sd <- sqrt(variance)
  raw_second <- variance + raw_first^2
  sd_relative_error_bound <- if (variance_numerator > 0) {
    max(
      .TRUNC_GAMMA_ROUNDOFF_SCALE * .Machine$double.eps,
      0.5 * roundoff_bound / variance_numerator
    )
  } else {
    Inf
  }

  list(
    mean = raw_first,
    variance = variance,
    sd = sd,
    cv = sd / raw_first,
    raw_second = raw_second,
    survival = exp(components$log_survival),
    hazard_adjustment = components$hazard_adjustment,
    sd_relative_error_bound = sd_relative_error_bound
  )
}

# The mean and the variance of a gamma variable truncated below `n_min`, in
# units of `1 / beta`, written with the hazard term `h`. This form avoids the
# subtraction of nearly equal numbers that a difference of raw moments needs.
.trunc_gamma_recurrence_components <- function(alpha, beta, n_min) {
  log_survival <- .trunc_gamma_log_survival(alpha, beta, n_min)
  z <- beta * n_min
  log_h <- alpha * log(z) - z - lgamma(alpha) - log_survival
  h <- if (is.finite(log_h) && log_h > log(.Machine$double.xmin)) {
    exp(log_h)
  } else if (is.finite(log_h) && log_h <= log(.Machine$double.xmin)) {
    0
  } else {
    NaN
  }
  variance_terms <- c(
    alpha,
    (1 + z - alpha) * h,
    -(h^2)
  )
  list(
    log_survival = log_survival,
    hazard_adjustment = h,
    mean_numerator = alpha + h,
    variance_numerator = sum(variance_terms),
    variance_roundoff_bound = .TRUNC_GAMMA_ROUNDOFF_SCALE *
      .Machine$double.eps * sum(abs(variance_terms))
  )
}

.rtrunc_gamma <- function(n, alpha, beta, n_min) {
  n <- .validate_scalar_integer(n, "n")
  if (n < 1L) {
    .abort("`n` must be at least 1.", .you_passed(n))
  }
  alpha <- .validate_positive_scalar_number(alpha, "alpha")
  beta <- .validate_positive_scalar_number(beta, "beta")
  n_min <- .validate_scalar_integer(n_min, "nj_min")
  if (n_min < 1L) {
    .abort("`nj_min` must be at least 1.", .you_passed(n_min))
  }

  fitted <- sprintf("The fitted gamma distribution has shape %s and rate %s.", format(alpha), format(beta))
  log_survival <- .trunc_gamma_log_survival(alpha, beta, n_min)
  if (!is.finite(log_survival)) {
    .abort_solver(
      sprintf("The probability that a site size exceeds `nj_min = %s` could not be computed.", n_min),
      fitted
    )
  }
  survival <- exp(log_survival)
  if (!is.finite(survival) || survival <= 0) {
    .abort_solver(
      sprintf("The probability that a site size exceeds `nj_min = %s` is too small to draw from.", n_min),
      fitted
    )
  }

  tail_prob <- stats::runif(n, min = 0, max = survival)
  tail_prob <- pmax(tail_prob, .Machine$double.xmin)
  draws <- stats::qgamma(tail_prob, shape = alpha, rate = beta, lower.tail = FALSE)
  if (any(!is.finite(draws))) {
    .abort_solver(
      "The site-size draws are not all finite.",
      fitted
    )
  }
  pmax(n_min, draws)
}

.trunc_gamma_log_survival <- function(alpha, beta, n_min) {
  stats::pgamma(n_min, shape = alpha, rate = beta, lower.tail = FALSE, log.p = TRUE)
}

.trunc_gamma_residual <- function(log_par, n_bar, cv, n_min) {
  alpha <- exp(log_par[[1L]])
  beta <- exp(log_par[[2L]])
  if (!is.finite(alpha) || !is.finite(beta) || alpha <= 0 || beta <= 0) {
    return(c(mean = .Machine$double.xmax, sd = .Machine$double.xmax))
  }
  moments <- .trunc_gamma_moments(alpha, beta, n_min)
  target_sd <- cv * n_bar
  residual <- c(
    mean = moments$mean / n_bar - 1,
    sd = moments$sd / target_sd - 1
  )
  if (any(!is.finite(residual))) {
    return(c(mean = .Machine$double.xmax, sd = .Machine$double.xmax))
  }
  residual
}

.trunc_gamma_starts <- function(n_bar, cv, max_starts) {
  alpha0 <- 1 / cv^2
  beta0 <- alpha0 / n_bar
  starts <- list(
    c(alpha0, beta0),
    c(1.5 * alpha0, 1.5 * beta0),
    c(0.5 * alpha0, 0.5 * beta0),
    c(2, 2 / n_bar),
    c(10, 10 / n_bar)
  )
  starts[seq_len(min(max_starts, length(starts)))]
}

.fit_trunc_gamma_start <- function(start, start_id, n_bar, cv, n_min, max_iter) {
  fit <- tryCatch(
    .nleqslv_solve(
      x = log(start),
      fn = .trunc_gamma_residual,
      n_bar = n_bar,
      cv = cv,
      n_min = n_min,
      control = list(ftol = 1e-12, xtol = 1e-12, maxit = max_iter)
    ),
    error = function(err) err
  )
  if (inherits(fit, "error")) {
    return(.failed_trunc_gamma_fit(message = conditionMessage(fit), start_id = start_id))
  }

  # The solver stops within 1e-12 of a root, and where it stops inside that
  # range depends on the platform. Rounding gives the same shape and rate, and
  # so the same site sizes, on every platform.
  alpha <- signif(exp(fit$x[[1L]]), .TRUNC_GAMMA_SOLUTION_DIGITS)
  beta <- signif(exp(fit$x[[2L]]), .TRUNC_GAMMA_SOLUTION_DIGITS)

  # The residual is computed from the rounded values, because those are the
  # ones returned.
  residual <- .trunc_gamma_residual(
    c(log(alpha), log(beta)),
    n_bar = n_bar, cv = cv, n_min = n_min
  )
  list(
    alpha = alpha,
    beta = beta,
    residual = residual,
    residual_norm = max(abs(residual)),
    verified = all(is.finite(c(alpha, beta, residual))),
    start = start_id,
    termcd = fit$termcd,
    message = fit$message
  )
}

.failed_trunc_gamma_fit <- function(message, start_id) {
  list(
    alpha = NA_real_,
    beta = NA_real_,
    residual = c(mean = Inf, sd = Inf),
    residual_norm = Inf,
    verified = FALSE,
    start = start_id,
    termcd = NA_integer_,
    message = message
  )
}

.nleqslv_solve <- function(...) {
  nleqslv::nleqslv(...)
}

.warn_trunc_gamma_conditioning <- function(n_bar, cv, n_min) {
  if (cv > 1.5) {
    cli::cli_warn(c(
      "!" = "`cv` is above 1.5, where the site-size distribution is hard to compute accurately.",
      "i" = .you_passed(cv)
    ))
  }
  if (n_bar < 2 * n_min) {
    cli::cli_warn(c(
      "!" = "`nj_mean` is less than twice `nj_min`, so only a small `cv` can be reached.",
      "i" = sprintf("You passed `nj_mean = %s` and `nj_min = %s`.", format(n_bar), n_min)
    ))
  }
  invisible(TRUE)
}

.validate_positive_scalar_number <- function(x, arg) {
  x <- .validate_scalar_number(x, arg)
  if (x <= 0) {
    .abort(sprintf("`%s` must be positive.", arg), .you_passed(x))
  }
  invisible(x)
}
