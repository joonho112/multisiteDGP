# nolint start: object_name_linter, object_usage_linter
#' Generate direct sampling variances from informativeness and heterogeneity targets
#'
#' @encoding UTF-8
#'
#' @description
#' Build a deterministic grid of per-site sampling variances
#' \eqn{\widehat{se}_j^2} that exactly hits user-specified informativeness
#' \eqn{I = \sigma_\tau^2 / (\sigma_\tau^2 + \mathrm{GM}(\widehat{se}_j^2))}
#' and heterogeneity-ratio
#' \eqn{R = \max \widehat{se}^2 / \min \widehat{se}^2} targets, then append
#' `n_j = NA_integer_`, `se2_j`, and `se_j` columns to an upstream Layer 1
#' frame. This is the Layer 2 margin generator for the *direct-precision*
#' path (Paradigm B in the blueprint) — call it directly when composing
#' the four layers manually, or let \code{\link{sim_meta}} call it for you.
#'
#' @details
#' Under the default deterministic-grid mode (`se_fn = NULL`), the returned
#' SE values exactly hit the targets: the geometric mean of \eqn{\widehat{se}_j^2}
#' is \eqn{\sigma_\tau^2 (1 - I)/I} and the max/min ratio equals `R`.
#' Setting `shuffle = TRUE` randomly permutes the assignment of grid values
#' to sites so the multiset of SE values is preserved (the targets remain
#' exact under permutation) but the order changes — useful before
#' downstream rank-correlation alignment.
#'
#' \strong{Custom `se_fn` extensibility.} Supply `se_fn(J, ...)` returning a
#' named list with at least `se2_j` (length-`J`) and optionally `n_j` to
#' replace the deterministic grid with a user-supplied SE distribution.
#' Under custom-`se_fn` the deterministic-target invariant no longer
#' applies — the realized `I` and `R` are reported in the diagnostics but
#' not constrained to match the inputs.
#'
#' For the formal Paradigm A vs Paradigm B contrast and the grid
#' derivation, see the
#' \href{../articles/m3-margin-se-models.html}{Margin and SE models —
#' site-size and direct-precision paths} vignette.
#'
#' @section RNG policy:
#' `shuffle = TRUE` uses R's active `sample()` / `sample.int()` RNG policy
#' when `R > 1`; fixed-seed permutations therefore require the same R
#' sampling-kind policy. The homogeneous `R = 1` path and the
#' `shuffle = FALSE` path consume no RNG.
#'
#' @param upstream Data frame with exactly `J` rows. Typically the output of
#'   \code{\link{gen_effects}}; must contain the canonical Layer 1 columns
#'   `site_index`, `z_j`, `tau_j`. Layer 2 columns must NOT be present yet.
#' @param J Integer. Number of sites — must equal `nrow(upstream)`.
#' @param I Numeric in `(0, 1)`. Target mean informativeness. Required.
#'   Typical values: `0.10` (low precision), `0.30` (modest), `0.50`
#'   (Lord-Wallace), `0.80` (high). Endpoints are degenerate.
#' @param R Numeric (\eqn{\ge 1}). Target heterogeneity ratio
#'   \eqn{\widehat{se}^2_{\max} / \widehat{se}^2_{\min}}. Default `1`
#'   (homogeneous precision). Larger values widen the precision spread.
#' @param shuffle Logical. If `TRUE` (default), randomly permute the
#'   deterministic SE grid; `R = 1` is unaffected. The `(I, R)` targets
#'   remain exact under permutation.
#' @param sigma_tau Numeric (\eqn{\ge 0}). Between-site standard deviation
#'   on the response scale. Default `0.20`. Used to back-compute
#'   \eqn{\bar{\widehat{se}^2} = \sigma_\tau^2 (1 - I)/I}.
#' @param se_fn Optional callback `function(J, ...)` returning a named list
#'   with at least `se2_j`. When non-`NULL`, replaces the deterministic
#'   grid; the `(I, R)` targets are no longer guaranteed exact.
#' @param se_args Named list forwarded to `se_fn` as extra arguments after
#'   `J`. Default `list()`.
#'
#' @return The upstream tibble with three appended columns: `n_j` (always
#'   `NA_integer_` — no site-size margin under the direct path), `se2_j`
#'   (numeric sampling variance), `se_j` (numeric SE \eqn{\sqrt{se2_j}}).
#'   Attributes attached: `engine` (`"paradigm_B_deterministic"` or
#'   `"paradigm_B_custom"`), `I`, `R`, `direct_se_diagnostics`.
#'
#' @family family-margins
#' @seealso
#'   \code{\link{gen_site_sizes}} for the site-size-driven (Paradigm A)
#'   counterpart that builds SE from sample sizes;
#'   \code{\link{sim_meta}} for the wrapper that calls this in the
#'   four-layer pipeline;
#'   \code{\link{compute_I}} and \code{\link{informativeness}} for reading
#'   the realized informativeness from the result;
#'   the \href{../articles/m3-margin-se-models.html}{M3 Margin and SE
#'   models} vignette.
#'
#' @references
#' Lee, J., Che, J., Rabe-Hesketh, S., Feller, A., & Miratrix, L. (2025).
#' Improving the estimation of site-specific effects and their distribution
#' in multisite trials. \emph{Journal of Educational and Behavioral Statistics},
#' \bold{50}(5), 731--764. \doi{10.3102/10769986241254286}.
#'
#' @examples
#' # Compose Layer 1 + Layer 2 (direct-precision) manually.
#' effects <- gen_effects_gaussian(J = 10L)
#' gen_se_direct(effects, J = 10L, I = 0.30, R = 2, shuffle = FALSE)
#'
#' # Larger draw with R = 4 — wide precision spread.
#' effects50 <- gen_effects_gaussian(J = 50L, sigma_tau = 0.15)
#' direct <- gen_se_direct(effects50, J = 50L, I = 0.5, R = 4)
#' summary(direct$se_j)
#' attr(direct, "engine")  # "paradigm_B_deterministic"
#' @export
gen_se_direct <- function(
    upstream,
    J,
    I,
    R = 1,
    shuffle = TRUE,
    sigma_tau = 0.20,
    se_fn = NULL,
    se_args = list()) {
  if (missing(upstream)) {
    .abort_arg(
      "`upstream` is required.",
      "`gen_se_direct()` appends Paradigm B Layer 2 columns to a Layer 1 data frame.",
      "Pass the output of `gen_effects()` or another data frame with one row per site."
    )
  }
  if (missing(J)) {
    .abort_arg(
      "`J` is required.",
      "`gen_se_direct()` validates that upstream rows align with the design.",
      "Pass `J = nrow(upstream)` or the matching design value."
    )
  }
  if (missing(I)) {
    .abort_arg(
      "`I` is required.",
      "Paradigm B directly specifies the target informativeness.",
      "Pass `I = 0.30`, `I = 0.50`, or another value in `(0, 1)`."
    )
  }

  J <- .validate_j(J)
  upstream <- .validate_l2_upstream(upstream, J, caller = "gen_se_direct")
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
  if (isTRUE(shuffle) && R > 1) {
    se2_j <- sample(se2_j, size = length(se2_j), replace = FALSE)
  }
  out <- dplyr::mutate(
    upstream,
    n_j = rep(NA_integer_, J),
    se2_j = se2_j,
    se_j = sqrt(se2_j)
  )
  attr(out, "engine") <- "paradigm_B_deterministic"
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
  raw <- do.call(se_fn, c(list(J = J), se_args))
  if (inherits(raw, "data.frame")) {
    raw <- as.list(raw)
  }
  if (!is.list(raw) || is.null(names(raw)) || !"se2_j" %in% names(raw)) {
    .abort_arg(
      "`se_fn` must return a named list containing `se2_j`.",
      "`gen_se_direct()` uses `se2_j` to append direct sampling variances.",
      "Return `list(se2_j = numeric(J), n_j = NULL)` or a data frame with `se2_j`."
    )
  }

  se2_j <- .validate_se2_vector(raw$se2_j, min_length = J)
  if (length(se2_j) != J) {
    .abort_arg(
      "`se_fn()` returned the wrong number of `se2_j` values.",
      sprintf("Got length %s for J = %s.", length(se2_j), J),
      "Return exactly one positive `se2_j` value per site."
    )
  }

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
  attr(out, "engine") <- "paradigm_B_custom"
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
    .abort_arg(
      "`se_args` must be a named list.",
      "`gen_se_direct()` forwards `se_args` by name to `se_fn`.",
      "Pass `se_args = list(scale = 0.3)` or leave it empty."
    )
  }
  if ("J" %in% names(se_args)) {
    .abort_arg(
      "`se_args` must not contain `J`.",
      "`gen_se_direct()` supplies `J` from the validated design.",
      "Remove `J` from `se_args` and pass `J` to `gen_se_direct()` directly."
    )
  }
  se_args
}

.validate_direct_custom_n_j <- function(n_j, J) {
  if (!is.numeric(n_j) || length(n_j) != J || anyNA(n_j) || any(!is.finite(n_j)) || any(n_j != floor(n_j))) {
    .abort_arg(
      "`se_fn()` returned invalid `n_j` values.",
      "`n_j` must be integer-like with one value per site, or `NULL` for Paradigm B.",
      "Return `n_j = NULL` or an integer vector of length J."
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
    .abort_arg(
      sprintf("`I` must be strictly between 0 and 1; you passed %s.", I),
      "Direct designs use `I` as an informativeness proportion; endpoints are degenerate.",
      "Try `I = 0.30`, `I = 0.50`, or another value in `(0, 1)`."
    )
  }
  I
}

.validate_direct_r <- function(R) {
  R <- .validate_scalar_number(R, "R")
  if (R < 1) {
    .abort_arg(
      sprintf("`R` must be >= 1; you passed %s.", R),
      "`R` is the max/min sampling-variance ratio.",
      "Use `R = 1` for homogeneous precision or a value greater than 1."
    )
  }
  R
}

.warn_direct_conditioning <- function(I, R) {
  if (I < 0.01) {
    cli::cli_warn(c(
      "!" = "Paradigm B received very low informativeness.",
      "i" = "Near-zero `I` implies very large sampling variances.",
      ">" = "Use this only for boundary-breakdown studies; otherwise try a larger `I`."
    ))
  }
  if (I > 0.95) {
    cli::cli_warn(c(
      "!" = "Paradigm B received very high informativeness.",
      "i" = "Near-one `I` implies very small sampling variances.",
      ">" = "Use this only for near-trivial precision studies; otherwise try a smaller `I`."
    ))
  }
  if (R > 100) {
    cli::cli_warn(c(
      "!" = "Paradigm B received a very large heterogeneity ratio.",
      "i" = "Extreme max/min sampling-variance ratios can dominate diagnostics.",
      ">" = "Try `R <= 100` unless extreme precision heterogeneity is intentional."
    ))
  }
  invisible(TRUE)
}
# nolint end
