#' Reorder standard errors to reach a target rank correlation
#'
#' Swaps standard errors between sites until the Spearman correlation between
#' the standardized site effects `z_j` and the sampling variances `se2_j` is
#' within `tol` of `rank_corr`. The effects and the set of standard errors do
#' not change. With covariates the target refers to `z_j`, not `tau_j`.
#'
#' @details
#' The search starts from the order in `upstream`, or from the perfectly
#' correlated order if that is closer to the target. Each step examines every
#' pair of sites and makes the swap that brings the correlation closest to the
#' target, so a search over 1,000 sites takes many seconds. The result usually
#' differs from the starting order in a few pairs. The function warns if the
#' search ends more than `tol` from the target or if `abs(rank_corr) > 0.95`.
#' It stops with an error if `z_j` or `se2_j` is constant or the sign is wrong.
#'
#' `dependence_fn(z_j, se2_j, target, ...)` replaces the search and must return
#' `list(se2_j = se2_j[perm], perm = perm)`, where `perm` is a permutation of
#' `seq_along(se2_j)`. Its argument `target` is the target correlation.
#'
#' @param upstream The data from the previous step: a data frame with columns
#'   `site_index`, `z_j`, `tau_j`, `se_j`, `se2_j`, `n_j`, as returned by
#'   [gen_site_sizes()] or [gen_se_direct()].
#' @param rank_corr Target Spearman correlation between `z_j` and `se2_j`
#'   (-1 to 1, default `0`). A positive value pairs larger effects with larger
#'   standard errors.
#' @param max_iter,tol The search ends when the correlation is within `tol` of
#'   the target (positive number, default `0.02`), after `max_iter` swaps
#'   (integer, at least 100, default `20000`), or when no swap brings it closer.
#' @param dependence_fn Function used in place of the built-in search, or
#'   `NULL` (the default). Details say what it must accept and return.
#' @param ... Arguments passed to `dependence_fn`.
#'
#' @return The input data as a tibble with `se_j`, `se2_j`, and `n_j`
#'   reordered. The attributes `permutation_perm` and `dependence_diagnostics`
#'   hold the permutation and a list with `target`, `achieved`, and `converged`.
#'
#' @seealso [multisitedgp_design()], [realized_rank_corr()],
#'   `vignette("effect-precision-dependence")`.
#' @family dependence
#' @examples
#' set.seed(1)
#' dat <- gen_site_sizes(gen_effects(J = 50), J = 50)
#' out <- align_rank_corr(dat, rank_corr = 0.4)
#' cor(out$z_j, out$se2_j, method = "spearman")
#' # A user-supplied function: extreme effects get the largest standard errors
#' out <- align_rank_corr(dat, dependence_fn = function(z_j, se2_j, target) {
#'   perm <- order(se2_j)[rank(abs(z_j), ties.method = "first")]
#'   list(se2_j = se2_j[perm], perm = perm)
#' })
#' cor(abs(out$z_j), out$se2_j, method = "spearman")
#' @export
align_rank_corr <- function(
  upstream,
  rank_corr = 0,
  max_iter = 20000L,
  tol = 0.02,
  dependence_fn = NULL,
  ...
) {
  upstream <- .validate_precision_upstream(upstream, caller = "align_rank_corr")
  rank_corr <- .validate_scalar_number(rank_corr, "rank_corr")
  if (abs(rank_corr) > 1) {
    .abort("`rank_corr` must be between -1 and 1.", .you_passed(rank_corr))
  }
  max_iter <- .validate_scalar_integer(max_iter, "max_iter")
  if (max_iter < 100L) {
    .abort("`max_iter` must be at least 100.", .you_passed(max_iter))
  }
  tol <- .validate_scalar_number(tol, "tol")
  if (tol <= 0) {
    .abort("`tol` must be positive.", .you_passed(tol))
  }
  if (abs(rank_corr) > 0.95) {
    cli::cli_warn(c(
      "!" = "`rank_corr` is beyond 0.95 in absolute value, and the swaps may not reach it.",
      "i" = .you_passed(rank_corr)
    ))
  }
  dependence_fn <- .validate_function_or_null(dependence_fn, "dependence_fn")
  if (!is.null(dependence_fn)) {
    return(.apply_custom_dependence_fn(
      upstream = upstream,
      dependence_fn = dependence_fn,
      target = rank_corr,
      target_type = "residual_spearman",
      caller = "align_rank_corr",
      ...
    ))
  }

  fit <- .hill_climb_spearman(
    z_j = upstream$z_j,
    se2_j = upstream$se2_j,
    target = rank_corr,
    max_iter = max_iter,
    tol = tol
  )
  .check_rank_fit(fit, target = rank_corr, tol = tol, J = nrow(upstream))
  out <- .apply_dependence_permutation(upstream, fit$perm, caller = "align_rank_corr")
  attr(out, "dependence_diagnostics") <- .rank_dependence_diagnostics(
    target = rank_corr,
    achieved = fit$achieved,
    converged = fit$converged,
    iterations = fit$iterations,
    tol = tol
  )
  out
}

.hill_climb_spearman <- function(z_j, se2_j, target, max_iter, tol) {
  ranks_z <- .mdgp_rank(z_j)
  ranks_se2 <- .mdgp_rank(se2_j)
  rank_state <- .spearman_rank_state(ranks_z, ranks_se2)
  candidates <- list(
    .reorder_for_spearman(z_j, se2_j, target_corr = target),
    seq_along(z_j)
  )
  scores <- vapply(candidates, .spearman_for_perm, numeric(1), rank_state = rank_state)
  errors <- abs(scores - target)
  best_idx <- which.min(.na_to_inf(errors))
  perm <- candidates[[best_idx]]
  achieved <- scores[[best_idx]]
  best_error <- errors[[best_idx]]

  iterations <- 0L
  while (iterations < max_iter && is.finite(best_error) && best_error > tol) {
    swap <- .best_spearman_swap(perm, rank_state, target, best_error, achieved)
    if (is.null(swap)) {
      break
    }
    perm[c(swap$i, swap$j)] <- perm[c(swap$j, swap$i)]
    achieved <- swap$score
    best_error <- abs(achieved - target)
    iterations <- iterations + 1L
  }

  list(
    perm = .validate_permutation(perm, length(z_j), "perm"),
    achieved = unname(achieved),
    residual = unname(achieved - target),
    converged = is.finite(best_error) && best_error <= tol,
    iterations = iterations
  )
}

.best_spearman_swap <- function(perm, rank_state, target, current_error, current_score) {
  J <- length(perm)
  best <- NULL
  best_error <- current_error
  for (i in seq_len(J - 1L)) {
    for (j in seq.int(i + 1L, J)) {
      score <- .spearman_swap_score(
        perm = perm,
        i = i,
        j = j,
        rank_state = rank_state,
        current_score = current_score
      )
      error <- abs(score - target)
      if (is.finite(error) && error + .Machine$double.eps < best_error) {
        best <- list(i = i, j = j, score = score)
        best_error <- error
      }
    }
  }
  best
}

.spearman_rank_state <- function(ranks_z, ranks_y) {
  centered_z <- ranks_z - mean(ranks_z)
  centered_y <- ranks_y - mean(ranks_y)
  denom <- sqrt(sum(centered_z^2) * sum(centered_y^2))
  list(centered_z = centered_z, centered_y = centered_y, denom = denom)
}

.spearman_for_perm <- function(perm, rank_state) {
  if (!is.finite(rank_state$denom) || rank_state$denom <= 0) {
    return(NaN)
  }
  sum(rank_state$centered_z * rank_state$centered_y[perm]) / rank_state$denom
}

.spearman_swap_score <- function(perm, i, j, rank_state, current_score) {
  if (!is.finite(rank_state$denom) || rank_state$denom <= 0) {
    return(NaN)
  }
  delta <- (rank_state$centered_z[[i]] - rank_state$centered_z[[j]]) *
    (rank_state$centered_y[[perm[[j]]]] - rank_state$centered_y[[perm[[i]]]])
  current_score + delta / rank_state$denom
}

.na_to_inf <- function(x) {
  x[!is.finite(x)] <- Inf
  x
}

.check_rank_fit <- function(fit, target, tol, J) {
  achieved <- fit$achieved
  if (!is.finite(achieved)) {
    .abort_solver(
      "The rank correlation between `z_j` and `se2_j` is not defined.",
      "One of the two columns is constant."
    )
  }
  if (abs(target) > tol && !identical(sign(achieved), sign(target))) {
    .abort_solver(
      sprintf(
        "With %s sites the swaps ended at a rank correlation of %s, the wrong sign for the target %s.",
        J, format(achieved, digits = 3L), format(target)
      )
    )
  }
  if (!isTRUE(fit$converged)) {
    cli::cli_warn(c(
      "!" = "The rank correlation did not come within `tol` of the target.",
      "i" = sprintf(
        "The target is %s, the swaps reached %s, and `tol` is %s.",
        format(target), format(achieved, digits = 3L), format(tol)
      )
    ))
  }
  invisible(TRUE)
}

.rank_dependence_diagnostics <- function(target, achieved, converged, iterations, tol) {
  list(
    method = "rank",
    target_type = "residual_spearman",
    target = unname(target),
    achieved = unname(achieved),
    residual = unname(achieved - target),
    converged = isTRUE(converged),
    iterations = as.integer(iterations),
    tol = unname(tol)
  )
}
