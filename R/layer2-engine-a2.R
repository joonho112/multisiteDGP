# nolint start: object_usage_linter
engine_trunc_gamma_moment <- function(
    J,
    nj_mean = 50,
    cv = 0.50,
    nj_min = 5L,
    tol = 1e-6,
    max_starts = 5L,
  max_iter = 100L) {
  J <- .validate_j(J)
  cv <- .validate_scalar_number(cv, "cv")
  nj <- .validate_nj_mean_nj_min(nj_mean, nj_min, allow_equal = .is_zero_cv(cv))
  if (cv < 0) {
    .abort_arg(
      sprintf("`cv` must be >= 0; you passed %s.", cv),
      "`cv` is the target conditional coefficient of variation for site sizes.",
      "Use `cv = 0` for deterministic site sizes or a positive value such as `0.50`."
    )
  }
  if (.is_zero_cv(cv)) {
    return(tibble::tibble(n_j = rep(nj$nj_mean, J)))
  }

  solution <- solve_trunc_gamma(
    n_bar = nj$nj_mean,
    cv = cv,
    n_min = nj$nj_min,
    tol = tol,
    max_starts = max_starts,
    max_iter = max_iter
  )
  tibble::tibble(n_j = rtrunc_gamma(J, solution$alpha, solution$beta, nj$nj_min))
}

solve_trunc_gamma <- function(
    n_bar,
    cv,
    n_min,
    tol = 1e-6,
    max_starts = 5L,
    max_iter = 100L) {
  n_bar <- .validate_scalar_number(n_bar, "n_bar")
  n_min <- .validate_scalar_integer(n_min, "n_min")
  if (n_min < 1L || n_bar <= n_min) {
    .abort_arg(
      sprintf("`n_bar` (%s) must exceed `n_min` (%s).", n_bar, n_min),
      "The lower-truncated Gamma family is used for positive site sizes.",
      "Try `n_bar = 50, n_min = 5L` or another pair with `n_bar > n_min`."
    )
  }
  cv <- .validate_scalar_number(cv, "cv")
  if (cv < 0) {
    .abort_arg(
      sprintf("`cv` must be >= 0; you passed %s.", cv),
      "`cv` is the target conditional coefficient of variation.",
      "Use `cv = 0` for the deterministic path or a positive value such as `0.50`."
    )
  }
  tol <- .validate_scalar_number(tol, "tol")
  if (tol <= 0) {
    .abort_arg(
      sprintf("`tol` must be > 0; you passed %s.", tol),
      "`tol` controls post-solve moment verification.",
      "Use `tol = 1e-6` for the blueprint default."
    )
  }
  max_starts <- .validate_scalar_integer(max_starts, "max_starts")
  if (max_starts < 1L) {
    .abort_arg(
      sprintf("`max_starts` must be >= 1; you passed %s.", max_starts),
      "Engine A2 requires at least one numerical solver start.",
      "Use `max_starts = 5L` for the blueprint default."
    )
  }
  max_iter <- .validate_scalar_integer(max_iter, "max_iter")
  if (max_iter < 1L) {
    .abort_arg(
      sprintf("`max_iter` must be >= 1; you passed %s.", max_iter),
      "`max_iter` is passed to the A2 numerical solver.",
      "Use `max_iter = 100L` for the blueprint default."
    )
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
    .abort_solver(
      "Engine A2 truncated-Gamma solver did not find a verified solution.",
      sprintf("Tried %s start(s) for n_bar = %s, cv = %s, n_min = %s.", length(starts), n_bar, cv, n_min),
      "Try a less extreme site-size design or increase `max_starts`/`max_iter`."
    )
  }

  best <- verified[[which.min(vapply(verified, `[[`, numeric(1), "residual_norm"))]]
  tol_effective <- max(tol, .trunc_gamma_residual_floor(best$alpha, cv))
  if (any(abs(best$residual) > tol_effective)) {
    .abort_solver(
      "Engine A2 truncated-Gamma post-solve verification failed.",
      sprintf(
        # Rounded on purpose. Full-precision doubles in a message make the
        # error snapshot platform dependent, which is the very problem the
        # adaptive tolerance exists to remove.
        "Maximum scaled residual was %s against an effective tolerance of %s (requested %s, evaluation noise floor %s).",
        format(max(abs(best$residual)), digits = 3),
        format(tol_effective, digits = 3),
        format(tol, digits = 3),
        format(.trunc_gamma_residual_floor(best$alpha, cv), digits = 3)
      ),
      "Try a less extreme site-size design or increase `tol` slightly."
    )
  }

  moments <- trunc_gamma_moments(best$alpha, best$beta, n_min)
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

# Smallest scaled residual that `.trunc_gamma_residual()` can actually resolve
# at a given solution. Below this, a residual comparison is measuring floating
# point noise rather than fit quality, and the verdict becomes platform
# dependent (defect ledger D-024).
#
# Two cancellations set the floor.
#
#   1. Raw moments come from `lgamma(alpha + k) - lgamma(alpha)`. For large
#      `alpha` this subtracts two large numbers to get a small one, so the log
#      moment carries absolute error of order `eps * lgamma(alpha)`. After
#      `exp()` that becomes a relative error of the same order.
#
#   2. `sd^2 = E[X^2] - E[X]^2` cancels again. Since `E[X^2] ~ mean^2` and
#      `sd^2 = cv^2 * mean^2`, the relative error of `sd^2` is inflated by
#      `1 / cv^2`, and `sd` by half that.
#
# Hence `eps * lgamma(alpha) / (2 * cv^2)`, plus a plain machine-epsilon term
# that dominates once `cv` is large enough for the cancellations to vanish.
#
# The constants were fitted to a measured (n_bar, cv, n_min) grid: over the
# cancellation-dominated region the empirical floor ran 0.73x to 3.05x the
# analytic prediction, so 8x leaves roughly a factor of three of headroom.
# Measurement: log/log-version-up-2026-08-14/evidence/phase04/residual-noise-floor.csv
.TRUNC_GAMMA_FLOOR_SCALE <- 8
.TRUNC_GAMMA_FLOOR_BASE <- 256

.trunc_gamma_residual_floor <- function(alpha, cv) {
  if (!is.finite(alpha) || !is.finite(cv) || cv <= 0) {
    return(.TRUNC_GAMMA_FLOOR_BASE * .Machine$double.eps)
  }
  cancellation <- .TRUNC_GAMMA_FLOOR_SCALE * .Machine$double.eps *
    max(lgamma(alpha), 1) / (2 * cv^2)
  cancellation + .TRUNC_GAMMA_FLOOR_BASE * .Machine$double.eps
}

trunc_gamma_moments <- function(alpha, beta, n_min) {
  alpha <- .validate_positive_scalar_number(alpha, "alpha")
  beta <- .validate_positive_scalar_number(beta, "beta")
  n_min <- .validate_scalar_integer(n_min, "n_min")
  if (n_min < 1L) {
    .abort_arg(
      sprintf("`n_min` must be >= 1; you passed %s.", n_min),
      "Truncated-Gamma moments are defined here for positive site-size lower bounds.",
      "Use `n_min = 5L` or another positive integer."
    )
  }

  log_survival <- .trunc_gamma_log_survival(alpha, beta, n_min)
  raw_first <- .trunc_gamma_raw_moment(1, alpha, beta, n_min, log_survival)
  raw_second <- .trunc_gamma_raw_moment(2, alpha, beta, n_min, log_survival)
  variance <- raw_second - raw_first^2
  variance <- max(variance, 0)
  sd <- sqrt(variance)

  list(
    mean = raw_first,
    variance = variance,
    sd = sd,
    cv = sd / raw_first,
    raw_second = raw_second,
    survival = exp(log_survival)
  )
}

rtrunc_gamma <- function(n, alpha, beta, n_min) {
  n <- .validate_scalar_integer(n, "n")
  if (n < 1L) {
    .abort_arg(
      sprintf("`n` must be >= 1; you passed %s.", n),
      "The truncated-Gamma sampler must draw at least one value.",
      "Use `n = 10L` or another positive integer."
    )
  }
  alpha <- .validate_positive_scalar_number(alpha, "alpha")
  beta <- .validate_positive_scalar_number(beta, "beta")
  n_min <- .validate_scalar_integer(n_min, "n_min")
  if (n_min < 1L) {
    .abort_arg(
      sprintf("`n_min` must be >= 1; you passed %s.", n_min),
      "The truncated-Gamma sampler uses a positive site-size lower bound.",
      "Use `n_min = 5L` or another positive integer."
    )
  }

  log_survival <- .trunc_gamma_log_survival(alpha, beta, n_min)
  if (!is.finite(log_survival)) {
    .abort_solver(
      "Engine A2 truncated-Gamma sampler could not evaluate the upper tail.",
      sprintf("alpha = %s, beta = %s, n_min = %s produced a non-finite survival probability.", alpha, beta, n_min),
      "Try a less extreme site-size design."
    )
  }
  survival <- exp(log_survival)
  if (!is.finite(survival) || survival <= 0) {
    .abort_solver(
      "Engine A2 truncated-Gamma sampler upper tail underflowed.",
      sprintf("alpha = %s, beta = %s, n_min = %s produced survival = %s.", alpha, beta, n_min, survival),
      "Try a less extreme site-size design."
    )
  }

  tail_prob <- stats::runif(n, min = 0, max = survival)
  tail_prob <- pmax(tail_prob, .Machine$double.xmin)
  draws <- stats::qgamma(tail_prob, shape = alpha, rate = beta, lower.tail = FALSE)
  if (any(!is.finite(draws))) {
    .abort_solver(
      "Engine A2 truncated-Gamma sampler produced non-finite draws.",
      "The inverse-CDF transform hit a numerical boundary.",
      "Try a less extreme site-size design."
    )
  }
  pmax(n_min, draws)
}

.trunc_gamma_raw_moment <- function(k, alpha, beta, n_min, log_survival = NULL) {
  if (is.null(log_survival)) {
    log_survival <- .trunc_gamma_log_survival(alpha, beta, n_min)
  }
  log_shifted_survival <- .trunc_gamma_log_survival(alpha + k, beta, n_min)
  log_moment <- lgamma(alpha + k) - lgamma(alpha) - k * log(beta) +
    log_shifted_survival - log_survival
  if (!is.finite(log_moment)) {
    return(NaN)
  }
  exp(log_moment)
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
  moments <- trunc_gamma_moments(alpha, beta, n_min)
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

  alpha <- exp(fit$x[[1L]])
  beta <- exp(fit$x[[2L]])
  residual <- .trunc_gamma_residual(fit$x, n_bar = n_bar, cv = cv, n_min = n_min)
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

# Relative error in the realized site-size SD that the caller deserves to hear
# about. Once the residual evaluation cannot resolve better than this, the fit
# is accepted but its quality is no longer verifiable to a useful precision.
.TRUNC_GAMMA_WEAK_VERIFICATION <- 1e-3

.warn_trunc_gamma_conditioning <- function(n_bar, cv, n_min) {
  # The old form of this warning fired on `cv < 1e-3` and described the
  # solution as "mathematically allowed but may be numerically delicate". Both
  # halves were wrong (defect ledger D-022): the real failure boundary sat near
  # `cv = 0.005`, so designs at `cv = 0.002` aborted with no warning at all,
  # while everything that did warn failed outright rather than being merely
  # delicate.
  #
  # The condition is now the measured one — how precisely the post-solve
  # residual can be evaluated at the shape this design implies. `alpha` is not
  # known before solving, but for a Gamma `cv = 1 / sqrt(alpha)`, which is
  # accurate enough to decide whether to warn.
  approx_alpha <- 1 / cv^2
  floor_estimate <- .trunc_gamma_residual_floor(approx_alpha, cv)
  if (floor_estimate > .TRUNC_GAMMA_WEAK_VERIFICATION) {
    cli::cli_warn(c(
      "!" = "Engine A2 cannot verify this site-size fit to a useful precision.",
      "i" = paste0(
        "At `cv = ", format(cv, digits = 3), "` the truncated-Gamma moment ",
        "evaluation cancels to about ", format(floor_estimate, digits = 2),
        " relative error, so the realized SD is only checkable to that level."
      ),
      ">" = "Use a larger `cv`, or treat the realized site-size SD as approximate."
    ))
  }
  if (cv > 1.5) {
    cli::cli_warn(c(
      "!" = "Engine A2 received a large `cv`.",
      "i" = "Heavy-tailed site-size designs can be numerically delicate under lower truncation."
    ))
  }
  if (n_bar < 2 * n_min) {
    cli::cli_warn(c(
      "!" = "`n_bar` is close to the lower truncation point.",
      "i" = "The blueprint recommends `n_bar >= 2 * n_min` when feasible."
    ))
  }
  invisible(TRUE)
}

.validate_positive_scalar_number <- function(x, arg) {
  x <- .validate_scalar_number(x, arg)
  if (x <= 0) {
    .abort_arg(
      sprintf("`%s` must be > 0; you passed %s.", arg, x),
      sprintf("`%s` is a positive truncated-Gamma parameter.", arg),
      sprintf("Pass `%s` as one positive finite numeric value.", arg)
    )
  }
  invisible(x)
}
# nolint end
