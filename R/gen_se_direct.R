#' Set standard errors directly from informativeness and heterogeneity ratio
#'
#' Sets the sampling variances `se2_j` without site sizes, so that their
#' informativeness is `I` and their heterogeneity ratio (largest over smallest)
#' is `R`. [sim_meta()] runs this step after [gen_effects()]. Use
#' [gen_site_sizes()] to derive the sampling variances from site sizes instead.
#'
#' @details
#' Without `se_fn`, the sampling variances are `J` values equally spaced on the
#' log scale. Their geometric mean is `sigma_tau^2 * (1 - I) / I`, which makes
#' the informativeness equal to `I`, and the largest is `R` times the smallest.
#' It stops before shuffling if these variances cannot be represented as
#' positive finite numbers.
#'
#' @param upstream The data from the previous step: a data frame with columns
#'   `site_index`, `z_j`, and `tau_j`, usually from [gen_effects()]. It must
#'   not have `n_j`, `se2_j`, or `se_j`.
#' @param J Integer number of sites (at least 10), equal to `nrow(upstream)`.
#' @param I Target informativeness (above 0, below 1). It is required even
#'   with `se_fn`, because the diagnostics of the simulated data compare the
#'   realized informativeness with it.
#' @param R Target heterogeneity ratio (at least 1). Default `1`.
#' @param shuffle Logical. `TRUE` (the default) assigns the sampling variances
#'   to the sites in random order. `FALSE` assigns them in increasing order.
#' @param sigma_tau Cross-site standard deviation of the site effects in
#'   `upstream` (above 0). Default `0.2`.
#' @param se_fn Function that supplies the sampling variances, or `NULL` (the
#'   default). It is called as `se_fn(J = J, ...)` and must return a list or
#'   data frame with `se2_j` (`J` positive numbers) and, optionally, `n_j`.
#'   If supplied, `n_j` must contain one positive whole number per site, at
#'   most 2147483647.
#'   `I`, `R`, `sigma_tau`, and `shuffle` then have no effect on the variances.
#' @param se_args Named list of further arguments for `se_fn`. Default `list()`.
#' @return The input data as a tibble, with the columns `n_j` (`NA`, or the
#'   site sizes from `se_fn`), `se2_j` (sampling variance), and `se_j` (standard
#'   error) added. The targets are kept as the attributes `I` and `R`.
#' @seealso [sim_meta()], [informativeness()], [heterogeneity_ratio()],
#'   `vignette("site-sizes-and-standard-errors")`.
#' @family site sizes
#' @examples
#' set.seed(1)
#' effects <- gen_effects(J = 10)
#' dat <- gen_se_direct(effects, J = 10, I = 0.3, R = 4, shuffle = FALSE)
#' dat
#' informativeness(dat$se2_j, sigma_tau = 0.2)
#' heterogeneity_ratio(dat$se2_j)
#'
#' # Sampling variances from a user-supplied function
#' se_fn <- function(J) list(se2_j = runif(J, 0.02, 0.2))
#' gen_se_direct(effects, J = 10, I = 0.3, se_fn = se_fn)$se2_j
#' @export
gen_se_direct <- function(
  upstream,
  J,
  I,
  R = 1,
  shuffle = TRUE,
  sigma_tau = 0.20,
  se_fn = NULL,
  se_args = list()
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
  if (missing(I)) {
    .abort(
      "`I` is required.",
      "It is the target informativeness, a number strictly between 0 and 1."
    )
  }

  J <- .validate_j(J)
  upstream <- .validate_effects_upstream(upstream, J, caller = "gen_se_direct")
  I <- .validate_direct_i(I)
  R <- .validate_direct_r(R)
  shuffle <- .validate_scalar_logical(shuffle, "shuffle")
  sigma_tau <- .validate_sigma_tau(sigma_tau)
  se_fn <- .validate_function_or_null(se_fn, "se_fn")
  se_args <- .validate_direct_se_args(se_args)
  .warn_direct_conditioning(I = I, R = R)

  if (!is.null(se_fn)) {
    return(.gen_se_direct_custom(
      upstream = upstream,
      J = J,
      I = I,
      R = R,
      se_fn = se_fn,
      se_args = se_args
    ))
  }

  se2_j <- .direct_se2_grid(J = J, I = I, R = R, sigma_tau = sigma_tau)
  if (any(!is.finite(se2_j)) || any(se2_j <= 0)) {
    .abort(
      "`I`, `R`, and `sigma_tau` imply sampling variances outside the positive finite range.",
      "Choose values that give representable sampling variances."
    )
  }
  if (isTRUE(shuffle) && R > 1) {
    se2_j <- sample(se2_j, size = length(se2_j), replace = FALSE)
  }
  out <- dplyr::mutate(
    upstream,
    n_j = rep(NA_integer_, J),
    se2_j = se2_j,
    se_j = sqrt(se2_j)
  )
  attr(out, "engine") <- "direct"
  attr(out, "I") <- I
  attr(out, "R") <- R
  attr(out, "direct_se_diagnostics") <- list(
    method = "grid",
    exact_grid = TRUE,
    shuffle = shuffle,
    rng_draws = if (isTRUE(shuffle) && R > 1) 1L else 0L
  )
  out
}

.gen_se_direct_custom <- function(upstream, J, I, R, se_fn, se_args) {
  returned <- do.call(se_fn, c(list(J = J), se_args))
  raw <- if (inherits(returned, "data.frame")) as.list(returned) else returned
  if (!is.list(raw) || is.null(names(raw)) || !"se2_j" %in% names(raw)) {
    .abort(
      "`se_fn` must return a list or data frame with an element `se2_j`.",
      sprintf("It returned %s.", .describe_value(returned))
    )
  }

  se2_j <- raw$se2_j
  usable <- is.numeric(se2_j) && length(se2_j) == J && !anyNA(se2_j) &&
    all(is.finite(se2_j)) && all(se2_j > 0)
  if (!usable) {
    .abort(
      "`se_fn` must return `se2_j` as one positive finite number per site.",
      sprintf(
        "For %s sites it returned %s.",
        J, .describe_returned(se2_j, J, "values that are missing, infinite, or not positive")
      )
    )
  }
  se2_j <- as.numeric(se2_j)

  n_j <- raw$n_j
  if (is.null(n_j)) {
    n_j <- rep(NA_integer_, J)
  } else {
    n_j <- .validate_direct_custom_n_j(n_j, J = J)
  }

  out <- dplyr::mutate(
    upstream,
    n_j = n_j,
    se2_j = se2_j,
    se_j = sqrt(se2_j)
  )
  attr(out, "engine") <- "direct_custom"
  attr(out, "I") <- I
  attr(out, "R") <- R
  attr(out, "direct_se_diagnostics") <- list(method = "custom", se_fn = TRUE)
  out
}

.validate_direct_se_args <- function(se_args) {
  se_args <- .validate_list(se_args, "se_args")
  if (length(se_args) == 0L) {
    return(se_args)
  }
  if (is.null(names(se_args)) || anyNA(names(se_args)) || any(!nzchar(names(se_args)))) {
    .abort(
      "Every entry of `se_args` must be named.",
      "The entries are passed to `se_fn` by name."
    )
  }
  if ("J" %in% names(se_args)) {
    .abort(
      "`se_args` must not have an entry `J`.",
      "`se_fn` is called with `J` set to the number of sites."
    )
  }
  se_args
}

.validate_direct_custom_n_j <- function(n_j, J) {
  usable <- is.numeric(n_j) && length(n_j) == J &&
    all(is.finite(n_j) & n_j == floor(n_j) & n_j > 0 & n_j <= .Machine$integer.max)
  if (!usable) {
    flaw <- sprintf(
      "values that are missing, infinite, not positive whole numbers, or above %d",
      .Machine$integer.max
    )
    .abort(
      "`se_fn` must return `n_j` as one positive whole number per site, or leave it out.",
      sprintf("For %s sites it returned %s.", J, .describe_returned(n_j, J, flaw))
    )
  }
  as.integer(n_j)
}

.direct_se2_grid <- function(J, I, R, sigma_tau) {
  gm <- sigma_tau^2 * (1 - I) / I
  if (identical(R, 1) || isTRUE(all.equal(R, 1))) {
    return(rep(gm, J))
  }
  exponent <- seq(0, 1, length.out = J) - 0.5
  as.numeric(gm * R^exponent)
}

.validate_direct_i <- function(I) {
  I <- .validate_scalar_number(I, "I")
  if (I <= 0 || I >= 1) {
    .abort("`I` must be strictly between 0 and 1.", .you_passed(I))
  }
  I
}

.validate_direct_r <- function(R) {
  R <- .validate_scalar_number(R, "R")
  if (R < 1) {
    .abort("`R` must be at least 1.", .you_passed(R))
  }
  R
}

.warn_direct_conditioning <- function(I, R) {
  if (I < 0.01) {
    cli::cli_warn(c(
      "!" = "`I` is below 0.01, which asks for sampling variances about 100 times `sigma_tau^2` or larger.",
      "i" = .you_passed(I)
    ))
  }
  if (I > 0.95) {
    cli::cli_warn(c(
      "!" = "`I` is above 0.95, which asks for sampling variances about `sigma_tau^2 / 19` or smaller.",
      "i" = .you_passed(I)
    ))
  }
  if (R > 100) {
    cli::cli_warn(c(
      "!" = "`R` is above 100, which asks for a largest sampling variance over 100 times the smallest.",
      "i" = .you_passed(R)
    ))
  }
  invisible(TRUE)
}
