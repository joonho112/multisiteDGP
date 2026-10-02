#' Reorder standard errors with a copula draw and swaps
#'
#' Reorders the standard errors by a Gaussian copula draw and then swaps pairs
#' of sites until the Spearman correlation between the standardized site
#' effects `z_j` and the sampling variances `se2_j` is within `tol` of
#' `rank_corr`. The effects and the set of standard errors do not change. With
#' covariates the target refers to `z_j`, not `tau_j`.
#'
#' @details
#' The copula draw is [align_copula_corr()] with `pearson_corr` set to
#' `2 * sin(pi * rank_corr / 6)`, the correlation of a bivariate normal
#' distribution whose Spearman correlation is `rank_corr`. The swaps are those
#' of [align_rank_corr()], limited to `max(100, max_iter %/% 4)` after a copula
#' draw, and the function warns if they end more than `tol` from the target.
#' With `polish = "none"` no swaps run, and the correlation varies from draw
#' to draw as in [align_copula_corr()]. With `rank_corr = 0`, or with both
#' stages skipped, the standard errors keep their order. Warnings and errors
#' are those of the two stages.
#'
#' @inheritParams align_rank_corr
#' @param dependence_fn Function used in place of the built-in method, or
#'   `NULL` (the default). The details of [align_rank_corr()] say what it must
#'   accept and return.
#' @param init First stage: `"copula"` (the default) runs the copula draw and
#'   `"rank"` skips it. In [multisitedgp_design()] it is `hybrid_init`.
#' @param polish Second stage: `"hill_climb"` (the default) runs the swaps
#'   and `"none"` skips them. In [multisitedgp_design()] it is `hybrid_polish`.
#'
#' @return The input data as a tibble with `se_j`, `se2_j`, and `n_j`
#'   reordered. The attributes `permutation_perm` and `dependence_diagnostics`
#'   hold the permutation and a list with `target`, `achieved`, `converged`,
#'   and the diagnostics of each stage.
#'
#' @seealso [multisitedgp_design()], [realized_rank_corr()],
#'   `vignette("effect-precision-dependence")`.
#' @family dependence
#' @examples
#' set.seed(1)
#' dat <- gen_site_sizes(gen_effects(J = 50), J = 50)
#' out <- align_hybrid_corr(dat, rank_corr = 0.4)
#' cor(out$z_j, out$se2_j, method = "spearman")
#' # The same step inside a simulation
#' design <- preset_education_modest(dependence = "hybrid", rank_corr = 0.4)
#' sim_multisite(design, seed = 1)
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
  upstream <- .validate_precision_upstream(upstream, caller = "align_hybrid_corr")
  rank_corr <- .validate_scalar_number(rank_corr, "rank_corr")
  if (abs(rank_corr) > 1) {
    .abort("`rank_corr` must be between -1 and 1.", .you_passed(rank_corr))
  }
  init <- .match_choice(init, "init", c("copula", "rank"))
  polish <- .match_choice(polish, "polish", c("hill_climb", "none"))
  max_iter <- .validate_scalar_integer(max_iter, "max_iter")
  if (max_iter < 100L) {
    .abort("`max_iter` must be at least 100.", .you_passed(max_iter))
  }
  tol <- .validate_scalar_number(tol, "tol")
  if (tol <= 0) {
    .abort("`tol` must be positive.", .you_passed(tol))
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

  pearson_init <- .spearman_to_pearson(rank_corr)
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

.spearman_to_pearson <- function(rank_corr) {
  rank_corr <- .validate_scalar_number(rank_corr, "rank_corr")
  if (abs(rank_corr) > 1) {
    .abort("`rank_corr` must be between -1 and 1.", .you_passed(rank_corr))
  }
  2 * sin(pi * rank_corr / 6)
}

.hybrid_identity <- function(upstream, rank_corr, init, polish, tol) {
  out <- .apply_dependence_permutation(upstream, seq_len(nrow(upstream)), caller = "align_hybrid_corr")
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
