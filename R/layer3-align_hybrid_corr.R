# nolint start: object_usage_linter
#' Align precision ranks using hybrid copula initialization and rank polishing
#'
#' @encoding UTF-8
#'
#' @description
#' The recommended default Layer 3 dependence injector. `align_hybrid_corr()`
#' combines fast empirical-rank Gaussian-copula initialization with an
#' optional hill-climb polish, getting the best of both worlds: the smooth
#' joint distribution of the copula path and the tight target-realized
#' agreement of the hill-climb. It preserves the empirical precision
#' multiset exactly via explicit permutation.
#'
#' @details
#' \strong{Two-stage algorithm.} Stage 1 — initialization. By default,
#' run \code{\link{align_copula_corr}} with `pearson_corr` chosen so the
#' implied Spearman matches `rank_corr`. (Alternative: `init = "rank"`
#' starts from a partial-sort permutation seeded by ordered indexing.)
#' Stage 2 — polish. By default, run a hill-climb on the Stage 1 output
#' until the realized Spearman lands within `tol` of `rank_corr` or
#' `max_iter` swaps have been proposed. Stage 2 can be skipped with
#' `polish = "none"` for a copula-only result.
#'
#' \strong{Why it's the default.} Pure copula (Stage 1 only) is fast but
#' has finite-J slack on the realized Spearman; pure hill-climb is exact
#' but slow to converge from an arbitrary starting permutation, especially
#' near \eqn{|rank_{corr}| \to 1}. The hybrid uses copula to land in the
#' right neighborhood and hill-climb to clean up — typically converges in
#' far fewer swaps than pure rank, with comparable target accuracy.
#'
#' \strong{Custom `dependence_fn` extensibility.} See the same contract
#' documented in \code{\link{align_rank_corr}}.
#'
#' For the formal contrast among the three injection methods and a
#' decision rubric, see the
#' \href{../articles/m4-precision-dependence-theory.html}{Precision
#' dependence — three injection methods} vignette.
#'
#' @section RNG policy:
#' Stage 1 (copula init) consumes one `rnorm()` draw per site at finite
#' `pearson_corr`. Stage 2 (hill-climb polish) proposes swaps via
#' `sample.int()` and accepts deterministically; consumes the active
#' `sample.int()` RNG. Both stages are wrapped in the caller's seed when
#' invoked through \code{\link{sim_multisite}} / \code{\link{sim_meta}}.
#'
#' @param upstream A Layer 2 data frame with canonical columns
#'   `site_index, z_j, tau_j, se_j, se2_j, n_j`.
#' @param rank_corr Numeric in `[-1, 1]`. Target residual Spearman
#'   correlation. Default `0`. Typical applied values: `0.3` (moderate
#'   positive), `-0.5` (selection-on-precision in meta-analysis).
#' @param init Character. Initialization mode — `"copula"` (default,
#'   recommended) or `"rank"` (partial-sort start).
#' @param polish Character. Polishing mode — `"hill_climb"` (default,
#'   recommended) or `"none"` (copula-only).
#' @param max_iter Integer (\eqn{\ge 100}). Maximum swap proposals during
#'   the polish stage. Default `20000L`.
#' @param tol Numeric (> 0). Absolute tolerance for the realized residual
#'   Spearman. Default `0.02`.
#' @param dependence_fn Optional callback. See \code{\link{align_rank_corr}}
#'   for the contract.
#' @param ... Additional arguments forwarded to `dependence_fn`.
#'
#' @return The upstream tibble with paired precision columns permuted, plus
#'   attributes `permutation_perm` and `dependence_diagnostics` (with
#'   stage-specific entries: `init_method`, `polish_method`, `init_realized`,
#'   `polish_realized`).
#'
#' @family family-dependence
#' @seealso
#'   \code{\link{align_rank_corr}} for the pure hill-climb;
#'   \code{\link{align_copula_corr}} for the pure copula path;
#'   the \href{../articles/m4-precision-dependence-theory.html}{M4
#'   Precision dependence theory} vignette.
#'
#' @references
#' Lee, J., Che, J., Rabe-Hesketh, S., Feller, A., & Miratrix, L. (2025).
#' Improving the estimation of site-specific effects and their distribution
#' in multisite trials. \emph{Journal of Educational and Behavioral Statistics},
#' \bold{50}(5), 731--764. \doi{10.3102/10769986241254286}.
#'
#' @examples
#' # Recommended default: hybrid copula + hill-climb polish.
#' effects <- gen_effects_gaussian(J = 12L)
#' margins <- gen_site_sizes(effects, J = 12L, nj_mean = 40, cv = 0.2)
#' aligned <- align_hybrid_corr(margins, rank_corr = 0.3, max_iter = 1000L)
#' cor(aligned$z_j, aligned$se2_j, method = "spearman")
#'
#' # Copula-only (skip the polish stage).
#' copula_only <- align_hybrid_corr(margins, rank_corr = 0.3, polish = "none")
#' @export
align_hybrid_corr <- function(
  upstream,
  rank_corr = 0,
  init = c("copula", "rank"),
  polish = c("hill_climb", "none"),
  max_iter = 20000L,
  tol = 0.02,
  dependence_fn = NULL,
  ...
) {
  upstream <- .validate_l3_upstream(upstream, caller = "align_hybrid_corr")
  rank_corr <- .validate_scalar_number(rank_corr, "rank_corr")
  if (abs(rank_corr) > 1) {
    .abort_arg(
      "`rank_corr` must be in [-1, 1].",
      "Hybrid dependence targets residual Spearman correlation.",
      "Pass `rank_corr = 0.3`, `rank_corr = 0`, or another value in [-1, 1]."
    )
  }
  init <- .match_choice(init, "init", c("copula", "rank"))
  polish <- .match_choice(polish, "polish", c("hill_climb", "none"))
  max_iter <- .validate_scalar_integer(max_iter, "max_iter")
  if (max_iter < 100L) {
    .abort_arg(
      "`max_iter` must be at least 100.",
      "Hybrid rank polishing uses the same minimal iteration budget as `align_rank_corr()`.",
      "Use `max_iter = 20000L` unless you have a reason to tune it."
    )
  }
  tol <- .validate_scalar_number(tol, "tol")
  if (tol <= 0) {
    .abort_arg(
      "`tol` must be > 0.",
      "Hybrid dependence requires a positive residual Spearman tolerance.",
      "Use `tol = 0.02` for the default continuous-target tolerance."
    )
  }
  dependence_fn <- .validate_function_or_null(dependence_fn, "dependence_fn")
  if (!is.null(dependence_fn)) {
    return(.apply_custom_dependence_fn(
      upstream = upstream,
      dependence_fn = dependence_fn,
      target = rank_corr,
      target_type = "residual_spearman",
      caller = "align_hybrid_corr",
      ...
    ))
  }

  if (identical(rank_corr, 0) || isTRUE(all.equal(rank_corr, 0))) {
    return(.hybrid_identity(upstream, rank_corr = rank_corr, init = init, polish = polish, tol = tol))
  }

  if (identical(init, "rank")) {
    if (identical(polish, "none")) {
      return(.hybrid_identity(upstream, rank_corr = rank_corr, init = init, polish = polish, tol = tol))
    }
    out <- align_rank_corr(upstream, rank_corr = rank_corr, max_iter = max_iter, tol = tol)
    rank_diag <- attr(out, "dependence_diagnostics", exact = TRUE)
    attr(out, "dependence_diagnostics") <- .hybrid_diagnostics(
      target = rank_corr,
      pearson_init = NA_real_,
      achieved = rank_diag$achieved,
      converged = rank_diag$converged,
      iterations = rank_diag$iterations,
      tol = tol,
      init = init,
      polish = polish,
      init_diagnostics = NULL,
      polish_diagnostics = rank_diag,
      rng_draws = 0L
    )
    return(out)
  }

  pearson_init <- spearman_to_pearson(rank_corr)
  initialized <- align_copula_corr(upstream, pearson_corr = pearson_init)
  init_diag <- attr(initialized, "dependence_diagnostics", exact = TRUE)
  init_perm <- attr(initialized, "permutation_perm", exact = TRUE)

  if (identical(polish, "none")) {
    achieved <- .realized_spearman(initialized$z_j, initialized$se2_j)
    attr(initialized, "dependence_diagnostics") <- .hybrid_diagnostics(
      target = rank_corr,
      pearson_init = pearson_init,
      achieved = achieved,
      converged = NA,
      iterations = 0L,
      tol = tol,
      init = init,
      polish = polish,
      init_diagnostics = init_diag,
      polish_diagnostics = NULL,
      rng_draws = init_diag$rng_draws
    )
    attr(initialized, "permutation_perm") <- init_perm
    return(initialized)
  }

  polish_iter <- max(100L, max_iter %/% 4L)
  polished <- align_rank_corr(initialized, rank_corr = rank_corr, max_iter = polish_iter, tol = tol)
  polish_diag <- attr(polished, "dependence_diagnostics", exact = TRUE)
  polish_perm <- attr(polished, "permutation_perm", exact = TRUE)
  composed_perm <- init_perm[polish_perm]
  attr(polished, "permutation_perm") <- composed_perm
  attr(polished, "dependence_diagnostics") <- .hybrid_diagnostics(
    target = rank_corr,
    pearson_init = pearson_init,
    achieved = polish_diag$achieved,
    converged = polish_diag$converged,
    iterations = polish_diag$iterations,
    tol = tol,
    init = init,
    polish = polish,
    init_diagnostics = init_diag,
    polish_diagnostics = polish_diag,
    rng_draws = init_diag$rng_draws
  )
  polished
}

spearman_to_pearson <- function(rank_corr) {
  rank_corr <- .validate_scalar_number(rank_corr, "rank_corr")
  if (abs(rank_corr) > 1) {
    .abort_arg(
      "`rank_corr` must be in [-1, 1].",
      "The Gaussian Spearman-to-Pearson mapping is defined only on valid correlations.",
      "Pass `rank_corr = 0.3`, `rank_corr = 0`, or another value in [-1, 1]."
    )
  }
  2 * sin(pi * rank_corr / 6)
}

.hybrid_identity <- function(upstream, rank_corr, init, polish, tol) {
  out <- .apply_l3_permutation(upstream, seq_len(nrow(upstream)), caller = "align_hybrid_corr")
  achieved <- .realized_spearman(out$z_j, out$se2_j)
  attr(out, "dependence_diagnostics") <- .hybrid_diagnostics(
    target = rank_corr,
    pearson_init = if (identical(init, "copula")) 0 else NA_real_,
    achieved = achieved,
    converged = is.finite(achieved) && abs(achieved) <= tol,
    iterations = 0L,
    tol = tol,
    init = init,
    polish = polish,
    init_diagnostics = NULL,
    polish_diagnostics = NULL,
    rng_draws = 0L
  )
  out
}

.hybrid_diagnostics <- function(
  target,
  pearson_init,
  achieved,
  converged,
  iterations,
  tol,
  init,
  polish,
  init_diagnostics,
  polish_diagnostics,
  rng_draws
) {
  list(
    method = "hybrid",
    mode = "empirical_rank_pairing",
    preservation = "empirical_multiset",
    target_type = "residual_spearman",
    target = unname(target),
    pearson_init = unname(pearson_init),
    achieved = unname(achieved),
    achieved_spearman = unname(achieved),
    residual = unname(achieved - target),
    converged = if (is.na(converged)) NA else isTRUE(converged),
    iterations = as.integer(iterations),
    tol = unname(tol),
    ties_method = "average",
    init = init,
    polish = polish,
    rng_draws = as.integer(rng_draws),
    init_diagnostics = init_diagnostics,
    polish_diagnostics = polish_diagnostics
  )
}
# nolint end
