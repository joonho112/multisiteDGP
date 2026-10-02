#' Generate site effects from a user-supplied function
#'
#' Draws the site effects with a user-supplied function `g_fn`, for a
#' distribution that is not built in. [gen_effects()] calls this function
#' when `true_dist = "User"`.
#'
#' @details
#' With `g_returns = "standardized"`, `g_fn` returns draws `z_j` from a
#' distribution with mean 0 and variance 1. The site effects are then
#' `tau_j = tau + sigma_tau * z_j`, plus the covariate term if `formula` is
#' given. With `g_returns = "raw"`, `g_fn` returns the site effects `tau_j`
#' on the response scale, and they are used as they are. A covariate term
#' from `formula` and `beta` is not added to them. In that case `tau`,
#' `sigma_tau`, and the covariate term serve only to compute `z_j` from the
#' same equation.
#'
#' @inheritParams gen_effects
#' @param g_fn Function called with `J = J` and the elements of `g_args`. It
#'   must return a finite numeric vector of length `J`.
#' @param g_returns Character. What `g_fn` returns: `"standardized"` (the
#'   default) or `"raw"`, as described in Details.
#' @param g_args List of further arguments for `g_fn`. Default `list()`. In
#'   [gen_effects()] and in a design, they are given as `theta_G`.
#' @param audit_g Logical. If `TRUE` (the default) and `g_returns` is
#'   `"standardized"`, the function calls `g_fn` once more with `J = 100000`,
#'   without advancing the random number stream. It warns if the mean of that
#'   sample is more than 0.1 from 0 or its variance more than 0.1 from 1.
#'
#' @return A tibble with one row per site and columns `site_index`, `z_j`
#'   (standardized site effect), and `tau_j` (site effect), followed by the
#'   columns of `data`, if any.
#'
#' @seealso [sim_multisite()], [multisitedgp_design()],
#'   `vignette("effect-distributions")`.
#' @family site effects
#' @examples
#' set.seed(1)
#' # Standardized draws from a uniform distribution, mean 0 and variance 1
#' g_unif <- function(J) runif(J, -sqrt(3), sqrt(3))
#' gen_effects_user(J = 10, g_fn = g_unif)
#'
#' # Effects on the response scale: gamma with mean 0.3 and SD 0.15
#' g_gamma <- function(J, shape) rgamma(J, shape, rate = shape / 0.3)
#' gen_effects_user(J = 10, tau = 0.3, sigma_tau = 0.15, g_fn = g_gamma,
#'                  g_returns = "raw", g_args = list(shape = 4))
#' @export
gen_effects_user <- function(
  J,
  tau = 0,
  sigma_tau = 0.20,
  g_fn,
  g_returns = c("standardized", "raw"),
  formula = NULL,
  beta = NULL,
  data = NULL,
  g_args = list(),
  audit_g = TRUE
) {
  J <- .validate_j(J)
  g_fn <- .validate_required_g_fn(g_fn)
  g_returns <- .match_choice(g_returns, "g_returns", c("standardized", "raw"))
  g_args <- .validate_list(g_args, "g_args")
  audit_g <- .validate_scalar_logical(audit_g, "audit_g")
  preflight <- .preflight_effects_frame_inputs(
    J = J,
    tau = tau,
    sigma_tau = sigma_tau,
    formula = formula,
    beta = beta,
    data = data
  )
  draw <- .call_user_g_fn(g_fn = g_fn, J = J, g_args = g_args)

  if (identical(g_returns, "standardized")) {
    if (isTRUE(audit_g)) {
      .audit_user_g_fn(g_fn = g_fn, g_args = g_args)
    }
    return(.new_effects_frame(
      z_j = draw,
      J = J,
      tau = tau,
      sigma_tau = sigma_tau,
      formula = formula,
      beta = beta,
      data = data,
      preflight = preflight
    ))
  }
  .new_raw_user_effects_frame(
    tau_j = draw,
    J = J,
    tau = tau,
    sigma_tau = sigma_tau,
    formula = formula,
    beta = beta,
    data = data,
    preflight = preflight
  )
}

.validate_required_g_fn <- function(g_fn) {
  if (missing(g_fn) || is.null(g_fn)) {
    .abort(
      "`g_fn` is required.",
      "It is a function that takes `J` and returns `J` draws."
    )
  }
  .validate_function_or_null(g_fn, "g_fn")
}

.validate_user_g_fn <- function(g_fn, true_dist) {
  if (identical(true_dist, "User") && is.null(g_fn)) {
    .abort(
      "`g_fn` is required when `true_dist = \"User\"`.",
      "It is a function that takes `J` and returns `J` draws."
    )
  }
  invisible(TRUE)
}

.call_user_g_fn <- function(g_fn, J, g_args) {
  out <- tryCatch(
    do.call(g_fn, c(list(J = J), g_args)),
    error = function(e) {
      .abort(
        sprintf("`g_fn` stopped with an error when called with `J = %s`.", J),
        conditionMessage(e)
      )
    }
  )
  .validate_user_g_output(out, J = J)
}

.validate_user_g_output <- function(out, J) {
  if (!is.numeric(out) || length(out) != J || anyNA(out) || any(!is.finite(out))) {
    .abort(
      "`g_fn` must return `J` finite numbers.",
      sprintf(
        "Called with `J = %s`, it returned %s.",
        J, .describe_returned(out, J, "missing or infinite values")
      )
    )
  }
  as.numeric(out)
}

.audit_user_g_fn <- function(g_fn, g_args) {
  audit_n <- 100000L
  # The audit draw must not move the caller's random number stream, so the
  # state found here is put back on exit.
  caller_has_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  caller_seed <- if (caller_has_seed) {
    get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  } else {
    NULL
  }
  on.exit({
    if (caller_has_seed) {
      assign(".Random.seed", caller_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  audit_draw <- .call_user_g_fn(g_fn = g_fn, J = audit_n, g_args = g_args)
  audit_mean <- mean(audit_draw)
  audit_var <- stats::var(audit_draw)
  if (abs(audit_mean) > 0.1 || abs(audit_var - 1) > 0.1) {
    cli::cli_warn(c(
      "!" = "`g_fn` does not seem to return draws with mean 0 and variance 1.",
      "i" = sprintf(
        "In %d further draws the mean is %.3f and the variance is %.3f.",
        audit_n, audit_mean, audit_var
      ),
      ">" = "Use `g_returns = \"raw\"` if `g_fn` returns the site effects themselves."
    ))
  }
  invisible(list(mean = audit_mean, variance = audit_var))
}

.new_raw_user_effects_frame <- function(
  tau_j,
  J,
  tau,
  sigma_tau,
  formula = NULL,
  beta = NULL,
  data = NULL,
  preflight = NULL
) {
  if (is.null(preflight)) {
    preflight <- .preflight_effects_frame_inputs(
      J = J,
      tau = tau,
      sigma_tau = sigma_tau,
      formula = formula,
      beta = beta,
      data = data
    )
  }
  J <- preflight$J
  tau <- preflight$tau
  sigma_tau <- preflight$sigma_tau
  tau_j <- .validate_user_g_output(tau_j, J = J)
  covariates <- preflight$covariates
  z_j <- .validate_z_j((tau_j - tau - covariates$effect) / sigma_tau, J = J)
  base <- tibble::tibble(
    site_index = seq_len(J),
    z_j = z_j,
    tau_j = tau_j
  )
  if (is.null(covariates$data)) {
    return(base)
  }
  tibble::as_tibble(c(as.list(base), as.list(covariates$data)))
}
