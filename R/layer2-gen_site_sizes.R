# nolint start: object_name_linter, object_usage_linter
#' Generate site sizes and sampling variances
#'
#' @encoding UTF-8
#'
#' @description
#' Draw J integer site sizes \eqn{n_j} from a target mean and coefficient of
#' variation, compute the per-site Neyman sampling variance
#' \eqn{\widehat{se}_j^2 = \kappa / n_j}, and append `n_j`, `se2_j`, and
#' `se_j` columns to an upstream Layer 1 frame. This is the Layer 2 margin
#' generator for the *site-size-driven* path (Paradigm A in the blueprint)
#' — call it directly when composing the four layers manually, or let
#' \code{\link{sim_multisite}} call it for you.
#'
#' @details
#' \strong{Engine choice.} Two engines back the site-size draw:
#' \describe{
#'   \item{`"A2_modern"` (default — recommended for new work)}{Lower-truncated Gamma on the continuous target scale, then stochastic rounding to integer `n_j`. Preserves the target mean in expectation and matches `cv` exactly on the underlying continuous draw.}
#'   \item{`"A1_legacy"`}{The JEBS paper's censor-then-round procedure. Preserved for bit-identical reproduction of the JEBS reference design and its replication fixtures. Can inflate the empirical mean near `nj_min` through censoring; not recommended for new work.}
#' }
#'
#' Pick A2 unless you are explicitly trying to reproduce a JEBS fixture. The
#' A1 engine is also restricted: combining A1 with non-trivial precision
#' dependence (`dependence != "none"`) is refused by
#' \code{\link{validate_multisitedgp_design}} — A1 is for legacy
#' reproduction only.
#'
#' \strong{Sampling variance.} The per-site Neyman variance is
#' \eqn{\kappa / n_j} with
#' \eqn{\kappa = \mathrm{var\_outcome}(1 - R^2) / (p (1 - p))},
#' the standard Neyman-allocation precision constant. Pass `p`, `R2`, and
#' `var_outcome` to control \eqn{\kappa} explicitly; defaults
#' (`p = 0.5`, `R2 = 0`, `var_outcome = 1`) give \eqn{\kappa = 4}, the
#' baseline used in the JEBS paper.
#'
#' For the formal Paradigm A vs Paradigm B contrast and the engine
#' derivation, see the
#' \href{../articles/m3-margin-se-models.html}{Margin and SE models —
#' site-size and direct-precision paths} vignette.
#'
#' @section RNG policy:
#' Stochastic rounding (A2 only) consumes one `runif()` draw for each
#' non-integer engine output. All-integer engine output, including the
#' `cv = 0` deterministic path, consumes no rounding RNG. The engine itself
#' (Gamma draw under A2 or A1) consumes the usual `rgamma()` stream.
#'
#' @param upstream Data frame with exactly `J` rows. Typically the output of
#'   \code{\link{gen_effects}}; must contain the canonical Layer 1 columns
#'   `site_index`, `z_j`, `tau_j`. Layer 2 columns (`n_j`, `se_j`, `se2_j`)
#'   must NOT be present yet.
#' @param J Integer. Number of sites — must equal `nrow(upstream)`.
#' @param nj_mean Numeric (\eqn{\ge \mathrm{nj\_min}}). Target site-size
#'   mean on the engine scale. Default `50`. Typical applied range: 20–500.
#' @param cv Numeric (\eqn{\ge 0}). Target site-size coefficient of variation.
#'   Default `0.50`. Use `cv = 0` for equal-size sites; `cv = 0.5` for the
#'   JEBS reference range. Larger `cv` produces more heterogeneous sites.
#' @param nj_min Integer (\eqn{\ge 1}). Lower bound for public site sizes.
#'   Default `5`. The engine output is floored at this bound.
#' @param p Numeric in `(0, 1)`. Treatment-assignment proportion. Default
#'   `0.5` (balanced). Affects \eqn{\kappa} through Neyman allocation.
#' @param R2 Numeric in `[0, 1)`. Covariate-explained variance share at the
#'   site level. Default `0`. Decreases \eqn{\kappa} and improves precision
#'   through the multiplier \eqn{1 - R^2}.
#' @param var_outcome Numeric (> 0). Outcome variance. Default `1`. Scales
#'   \eqn{\kappa} linearly.
#' @param engine Character. `"A2_modern"` (default — recommended) or
#'   `"A1_legacy"` (JEBS bit-parity reproduction only).
#'
#' @return The upstream tibble with three appended columns: `n_j`
#'   (integer site size), `se2_j` (numeric sampling variance
#'   \eqn{\kappa / n_j}), and `se_j` (numeric SE \eqn{\sqrt{se2_j}}). Two
#'   attributes are attached: `engine` (the resolved engine name) and
#'   `kappa` (the Neyman precision constant).
#'
#' @family family-margins
#' @seealso
#'   \code{\link{gen_se_direct}} for the direct-precision (Paradigm B)
#'   counterpart that takes precision targets directly;
#'   \code{\link{sim_multisite}} for the wrapper that calls this in the
#'   four-layer pipeline;
#'   \code{\link{compute_kappa}} for the underlying Neyman precision
#'   constant;
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
#' # Compose Layer 1 + Layer 2 manually.
#' effects <- gen_effects_gaussian(J = 10L)
#' gen_site_sizes(effects, J = 10L, nj_mean = 40, cv = 0.2)
#'
#' # Larger draw with the JEBS reference cv = 0.5 and Neyman defaults.
#' effects50 <- gen_effects_gaussian(J = 50L, sigma_tau = 0.15)
#' sized <- gen_site_sizes(effects50, J = 50L, nj_mean = 50, cv = 0.5)
#' summary(sized$n_j)
#' summary(sized$se_j)
#'
#' # JEBS bit-parity reproduction — engine A1.
#' a1 <- gen_site_sizes(effects, J = 10L, nj_mean = 40, cv = 0.5,
#'                      engine = "A1_legacy")
#' attr(a1, "engine")  # "A1_legacy"
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
    engine = c("A2_modern", "A1_legacy")) {
  if (missing(upstream)) {
    .abort_arg(
      "`upstream` is required.",
      "`gen_site_sizes()` appends Layer 2 columns to a Layer 1 data frame.",
      "Pass the output of `gen_effects()` or another data frame with one row per site."
    )
  }
  if (missing(J)) {
    .abort_arg(
      "`J` is required.",
      "`gen_site_sizes()` validates that upstream rows align with the design.",
      "Pass `J = nrow(upstream)` or the matching design value."
    )
  }

  J <- .validate_j(J)
  upstream <- .validate_l2_upstream(upstream, J, caller = "gen_site_sizes")
  engine <- .match_choice(engine, "engine", c("A2_modern", "A1_legacy"))
  cv <- .validate_scalar_number(cv, "cv")
  nj <- .validate_nj_mean_nj_min(nj_mean, nj_min, allow_equal = .is_zero_cv(cv))
  if (cv < 0) {
    .abort_arg(
      sprintf("`cv` must be >= 0; you passed %s.", cv),
      "`cv` is the target site-size coefficient of variation.",
      "Use `cv = 0` for deterministic site sizes or a positive value such as `0.50`."
    )
  }
  .validate_public_deterministic_site_size(nj$nj_mean, cv)
  p <- .validate_scalar_number(p, "p")
  R2 <- .validate_scalar_number(R2, "R2")
  var_outcome <- .validate_scalar_number(var_outcome, "var_outcome")

  kappa <- compute_kappa(p = p, R2 = R2, var_outcome = var_outcome)
  engine_out <- switch(
    engine,
    A2_modern = engine_trunc_gamma_moment(J = J, nj_mean = nj$nj_mean, cv = cv, nj_min = nj$nj_min),
    A1_legacy = engine_legacy_jebs_censor_round(J = J, nj_mean = nj$nj_mean, cv = cv, nj_min = nj$nj_min)
  )
  n_j <- .materialize_public_n_j(engine_out$n_j, nj_min = nj$nj_min, engine = engine)
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

.validate_l2_upstream <- function(upstream, J, caller = "Layer 2 generator") {
  if (!inherits(upstream, "data.frame")) {
    .abort_arg(
      "`upstream` must be a data frame.",
      sprintf("`%s()` appends columns using tibble semantics.", caller),
      "Pass the output of `gen_effects()` or another data frame with one row per site."
    )
  }
  if (nrow(upstream) != J) {
    .abort_arg(
      "`upstream` must have exactly `J` rows.",
      sprintf("Got %s row(s) in `upstream` for J = %s.", nrow(upstream), J),
      "Use one upstream row per site."
    )
  }
  required <- c("site_index", "z_j", "tau_j")
  missing_required <- setdiff(required, names(upstream))
  if (length(missing_required) > 0L) {
    .abort_arg(
      "`upstream` is missing required Layer 1 columns.",
      sprintf("Missing columns: %s.", paste(missing_required, collapse = ", ")),
      "Pass the output of `gen_effects()` or include the canonical Layer 1 columns."
    )
  }
  reserved <- intersect(names(upstream), c("n_j", "se2_j", "se_j"))
  if (length(reserved) > 0L) {
    .abort_arg(
      "Layer 2 columns are already present in `upstream`.",
      sprintf("Existing Layer 2 columns: %s.", paste(reserved, collapse = ", ")),
      "Remove existing margin columns before calling `gen_site_sizes()`."
    )
  }
  tibble::as_tibble(upstream)
}

.validate_public_deterministic_site_size <- function(nj_mean, cv) {
  if (.is_zero_cv(cv) && nj_mean != floor(nj_mean)) {
    .abort_arg(
      "`nj_mean` must be integer-like when `cv = 0`.",
      "Public site-size output stores deterministic site sizes as integer `n_j`.",
      "Use an integer `nj_mean`, or set a positive `cv`."
    )
  }
  invisible(TRUE)
}

.is_zero_cv <- function(cv) {
  identical(cv, 0) || isTRUE(all.equal(cv, 0))
}

.materialize_public_n_j <- function(n_j, nj_min, engine) {
  if (!is.numeric(n_j) || anyNA(n_j) || any(!is.finite(n_j))) {
    .abort_arg(
      "`n_j` engine output must be finite numeric values.",
      "Layer 2 converts engine output to the public integer site-size column.",
      "Use a built-in engine or return finite positive site sizes."
    )
  }
  if (identical(engine, "A1_legacy")) {
    n_j <- as.integer(n_j)
  } else {
    n_j <- .stochastic_round_site_sizes(n_j)
  }
  n_j <- as.integer(pmax(nj_min, n_j))
  if (any(!is.finite(n_j)) || any(n_j < nj_min)) {
    .abort_arg(
      "`n_j` engine output violated the lower site-size bound.",
      sprintf("All public site sizes must be at least nj_min = %s.", nj_min),
      "Use a built-in engine or increase `nj_min`."
    )
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
# nolint end
