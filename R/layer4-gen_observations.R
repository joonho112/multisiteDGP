# nolint start: object_usage_linter
#' Generate observed site-level effect estimates
#'
#' @encoding UTF-8
#'
#' @description
#' Draw observed site-level estimates
#' \eqn{\widehat{\tau}_j \sim \mathcal{N}(\tau_j,\, \widehat{se}_j^2)} from
#' the upstream Layer 2 / Layer 3 frame and append them as a new
#' `tau_j_hat` column. This is the final layer of the four-layer pipeline
#' — it is conceptually simple (one Gaussian draw per site, conditional on
#' the latent effect and sampling variance) but the `obs_fn` extensibility
#' hook lets you swap in a non-Gaussian observation model when needed
#' (e.g., a Poisson-rate observation, a heavy-tailed alternative, or a
#' bootstrap from real data).
#'
#' @details
#' \strong{Default Gaussian path.} For each site, draw
#' \eqn{\widehat{\tau}_j \sim \mathcal{N}(\tau_j, \widehat{se}_j^2)} using
#' base R's `rnorm()`. This matches the standard Stage-1 sampling
#' assumption in the JEBS paper and most multisite-trial / meta-analysis
#' literature.
#'
#' \strong{Custom `obs_fn` extensibility.} Pass a function with signature
#' `obs_fn(tau_j, se2_j, ...)` returning a finite numeric vector of length
#' `J`. Use this to swap in non-Gaussian observation models —
#' Poisson-rate, Student-t with heavy tails, or a non-parametric bootstrap
#' from a real dataset. The callback owns its own RNG.
#'
#' \strong{Legacy A1 shuffle.} When `upstream` was produced by
#' \code{\link{gen_site_sizes}} with `engine = "A1_legacy"` and the default
#' Gaussian path is used, the function first applies the JEBS-paper
#' observation-stage precision shuffle (one `sample.int(J)` call) before
#' drawing the Gaussian observations. This preserves bit-identical
#' reproduction of the JEBS reference design. Custom `obs_fn` callbacks do
#' NOT trigger the legacy shuffle (they're assumed to be modern code).
#'
#' @section RNG policy:
#' The default Gaussian path consumes exactly one `rnorm()` draw per site.
#' Under engine A1 with the default observation path, one extra
#' `sample.int(J)` call is consumed for the legacy shuffle (before the
#' `rnorm()` draws). User-supplied `obs_fn` hooks own their own RNG
#' behavior and do not trigger the legacy shuffle. All RNG is wrapped by
#' the caller's seed when invoked through \code{\link{sim_multisite}} /
#' \code{\link{sim_meta}}.
#'
#' @param upstream A Layer 2 or Layer 3 data frame with the canonical
#'   columns `site_index, z_j, tau_j, n_j, se2_j, se_j`. Typically the
#'   output of \code{\link{gen_site_sizes}}, \code{\link{gen_se_direct}},
#'   or one of the Layer 3 dependence aligners.
#' @param obs_fn Optional callback. If supplied, called as
#'   `obs_fn(tau_j = upstream$tau_j, se2_j = upstream$se2_j, ...)` and
#'   must return a finite numeric vector of length `J`.
#' @param ... Additional arguments forwarded to `obs_fn`.
#'
#' @return The upstream tibble with one appended numeric column,
#'   `tau_j_hat`. An attribute `observation_diagnostics` is attached, with
#'   entries `method` (`"gaussian"` or `"custom"`), `legacy_a1_shuffle`
#'   (logical), and `rng_draws` (integer or `NA`).
#'
#' @family family-observations
#' @seealso
#'   \code{\link{sim_multisite}} and \code{\link{sim_meta}} for the
#'   wrappers that compose this in the four-layer pipeline;
#'   \code{\link{gen_site_sizes}} and \code{\link{gen_se_direct}} for
#'   the upstream Layer 2 generators;
#'   \code{\link{align_hybrid_corr}} and the other Layer 3 aligners for
#'   the precision-dependence step that typically precedes this.
#'
#' @references
#' Lee, J., Che, J., Rabe-Hesketh, S., Feller, A., & Miratrix, L. (2025).
#' Improving the estimation of site-specific effects and their distribution
#' in multisite trials. \emph{Journal of Educational and Behavioral Statistics},
#' \bold{50}(5), 731--764. \doi{10.3102/10769986241254286}.
#'
#' @examples
#' # Default Gaussian observation path.
#' effects <- gen_effects_gaussian(J = 10L)
#' margins <- gen_site_sizes(effects, J = 10L, nj_mean = 40, cv = 0.2)
#' obs <- gen_observations(margins)
#' obs[, c("site_index", "tau_j", "tau_j_hat", "se_j")]
#'
#' # Custom obs_fn — heavy-tailed Student-t observation noise.
#' heavy_obs <- function(tau_j, se2_j, df = 5) {
#'   se_j <- sqrt(se2_j)
#'   tau_j + se_j * stats::rt(length(tau_j), df = df) * sqrt((df - 2) / df)
#' }
#' obs_t <- gen_observations(margins, obs_fn = heavy_obs, df = 5)
#' attr(obs_t, "observation_diagnostics")$method  # "custom"
#' @export
gen_observations <- function(upstream, obs_fn = NULL, ...) {
  if (missing(upstream)) {
    .abort_arg(
      "`upstream` is required.",
      "`gen_observations()` appends Layer 4 observations to Layer 2 or Layer 3 output.",
      "Pass the output of `gen_site_sizes()`, `gen_se_direct()`, or a dependence aligner."
    )
  }
  obs_fn <- .validate_function_or_null(obs_fn, "obs_fn")
  upstream <- .validate_l4_upstream(upstream, caller = "gen_observations")

  observation_permutation <- NULL
  if (is.null(obs_fn) && .uses_a1_legacy_observation_shuffle(upstream)) {
    observation_permutation <- base::sample.int(nrow(upstream), size = nrow(upstream))
    upstream <- .apply_l4_precision_permutation(upstream, observation_permutation)
  }

  tau_j_hat <- if (is.null(obs_fn)) {
    stats::rnorm(nrow(upstream), mean = upstream$tau_j, sd = sqrt(upstream$se2_j))
  } else {
    out <- obs_fn(tau_j = upstream$tau_j, se2_j = upstream$se2_j, ...)
    .validate_obs_fn_output(out, J = nrow(upstream))
  }

  out <- dplyr::mutate(upstream, tau_j_hat = as.numeric(tau_j_hat))
  out <- .restore_custom_attrs(out, upstream)
  attr(out, "observation_diagnostics") <- list(
    method = if (is.null(obs_fn)) "gaussian" else "custom",
    legacy_a1_shuffle = !is.null(observation_permutation),
    rng_draws = if (is.null(obs_fn)) nrow(out) else NA_integer_
  )
  if (!is.null(observation_permutation)) {
    attr(out, "observation_permutation_perm") <- observation_permutation
  }
  out
}

.validate_l4_upstream <- function(upstream, caller = "Layer 4 observation") {
  if (!inherits(upstream, "data.frame")) {
    .abort_arg(
      "`upstream` must be a data frame.",
      sprintf("`%s()` appends observations using tibble semantics.", caller),
      "Pass the output of `gen_site_sizes()`, `gen_se_direct()`, or a dependence aligner."
    )
  }
  if ("tau_j_hat" %in% names(upstream)) {
    .abort_arg(
      "`upstream` already contains `tau_j_hat`.",
      "Layer 4 appends observed estimates exactly once.",
      "Remove the existing `tau_j_hat` column before calling `gen_observations()`."
    )
  }

  out <- .validate_l3_upstream(upstream, caller = caller)
  out <- .restore_custom_attrs(out, upstream)
  .validate_numeric_vector(out$tau_j, "upstream$tau_j", min_length = nrow(out))
  out
}

.validate_obs_fn_output <- function(out, J) {
  if (!is.numeric(out) || length(out) != J || anyNA(out) || any(!is.finite(out))) {
    .abort_arg(
      "`obs_fn` must return finite numeric values of length J.",
      sprintf("Got type `%s` and length %s; expected numeric length %s.", typeof(out), length(out), J),
      "Use `obs_fn = function(tau_j, se2_j, ...) stats::rnorm(length(tau_j), tau_j, sqrt(se2_j))`."
    )
  }
  as.numeric(out)
}

.uses_a1_legacy_observation_shuffle <- function(upstream) {
  identical(attr(upstream, "engine", exact = TRUE), "A1_legacy")
}

.apply_l4_precision_permutation <- function(upstream, perm) {
  perm <- .validate_permutation(perm, nrow(upstream), "perm")
  out <- tibble::as_tibble(upstream)
  out <- .restore_custom_attrs(out, upstream)
  for (column in c("n_j", "se2_j", "se_j")) {
    out[[column]] <- out[[column]][perm]
  }
  out
}

.restore_custom_attrs <- function(out, source) {
  custom_attrs <- setdiff(names(attributes(source)), c("names", "row.names", "class"))
  for (attr_name in custom_attrs) {
    attr(out, attr_name) <- attr(source, attr_name, exact = TRUE)
  }
  out
}
# nolint end
