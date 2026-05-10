# nolint start: object_usage_linter
#' Align precision ranks to a target Spearman correlation
#'
#' @encoding UTF-8
#'
#' @description
#' Permute the paired precision columns (`se_j`, `se2_j`, `n_j`) of a Layer 2
#' frame so the realized Spearman correlation between the standardized
#' residual effect \eqn{z_j} and the sampling variance \eqn{\widehat{se}_j^2}
#' approximates a target value `rank_corr`. The permutation preserves both
#' marginals exactly — only the *assignment* of precision values to sites
#' changes — and adds an `attr(., "permutation_perm")` integer vector
#' recording the realized assignment.
#'
#' @details
#' \strong{Hill-climb algorithm.} The package starts from the upstream
#' (Layer 2) ordering of `(se2_j, se_j, n_j)` and proposes random pair
#' swaps. A swap is accepted if it brings the realized residual Spearman
#' correlation closer to `rank_corr`; otherwise it is rejected. The search
#' continues until the realized correlation lands within `tol` of the
#' target or until `max_iter` swaps have been proposed. The result is the
#' best-effort permutation found within budget — for moderate
#' \eqn{|rank_{corr}| \le 0.7} and `max_iter = 20,000` (the default) the
#' algorithm converges reliably; near \eqn{|rank_{corr}| \approx 1} the
#' search may stall before convergence.
#'
#' \strong{Why "exact marginal preservation" matters.} Because the
#' algorithm only reorders the existing values, the marginals of `se2_j`
#' and \eqn{z_j} are bit-identical to their Layer 2 inputs. Diagnostics
#' computed on the marginals (informativeness, heterogeneity ratio,
#' shrinkage) are unchanged by Layer 3; only the joint distribution and
#' rank correlation move. If you need to *introduce* dependence by
#' shifting marginals, use \code{\link{align_copula_corr}} (Gaussian
#' copula) or \code{\link{align_hybrid_corr}} (the recommended default,
#' which combines both).
#'
#' \strong{Custom `dependence_fn` extensibility.} Pass a `dependence_fn`
#' callback to bypass the built-in hill-climb. The callback receives
#' `z_j`, `se2_j`, `target`, and `...`, and must return
#' `list(se2_j = <length-J numeric>, perm = <length-J integer>)`. The
#' returned `se2_j` must be a permutation of the upstream `se2_j`
#' multiset. The callback owns its own RNG.
#'
#' For the formal contrast among the three injection methods (rank,
#' copula, hybrid) and a decision rubric on when to choose each, see the
#' \href{../articles/m4-precision-dependence-theory.html}{Precision
#' dependence — three injection methods} vignette.
#'
#' @section RNG policy:
#' Built-in hill-climb proposes swaps via `sample.int()` and accepts /
#' rejects deterministically based on the resulting correlation. The
#' active `sample.int()` RNG is consumed; under a wrapper seed
#' (`sim_multisite()` / `sim_meta()` with `seed`), runs are bit-identical.
#' Custom `dependence_fn` callbacks own their own RNG.
#'
#' @param upstream A Layer 2 data frame with canonical columns
#'   `site_index, z_j, tau_j, se_j, se2_j, n_j` (typically the output of
#'   \code{\link{gen_site_sizes}} or \code{\link{gen_se_direct}}).
#' @param rank_corr Numeric in `[-1, 1]`. Target Spearman correlation
#'   between `z_j` and `se2_j`. Default `0` (independence). Typical applied
#'   values: `0.3` (moderate positive — small sites tend to have larger
#'   effects), `-0.5` (moderate negative — selection-on-effect-size in
#'   meta-analysis). `|rank_corr| > 0.95` triggers a near-boundary warning.
#' @param max_iter Integer (\eqn{\ge 100}). Maximum number of swap
#'   proposals. Default `20000L`.
#' @param tol Numeric (> 0). Absolute tolerance for the realized residual
#'   Spearman correlation. Default `0.02`.
#' @param dependence_fn Optional callback. See Details for the contract.
#' @param ... Additional arguments forwarded to `dependence_fn`.
#'
#' @return The upstream tibble with the paired precision columns
#'   (`se_j`, `se2_j`, `n_j`) permuted to approximate the target. Two
#'   attributes are attached: `permutation_perm` (length-`J` integer
#'   vector — the realized site-to-grid permutation) and
#'   `dependence_diagnostics` (a named list with `method`, `target`,
#'   `realized_residual`, `realized_marginal`, `iterations`, `converged`).
#'
#' @family family-dependence
#' @seealso
#'   \code{\link{align_copula_corr}} for the Gaussian-copula alternative;
#'   \code{\link{align_hybrid_corr}} for the recommended default that
#'   combines copula initialization with hill-climb polish;
#'   \code{\link{realized_rank_corr}} for the realized Spearman after
#'   alignment;
#'   \code{\link{sim_multisite}} for the wrapper that calls this in the
#'   four-layer pipeline;
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
#' # Compose Layer 1 + 2 + 3 manually with a moderate positive target.
#' effects <- gen_effects_gaussian(J = 12L)
#' margins <- gen_site_sizes(effects, J = 12L, nj_mean = 40, cv = 0.2)
#' aligned <- align_rank_corr(margins, rank_corr = 0.3, max_iter = 1000L)
#' cor(aligned$z_j, aligned$se2_j, method = "spearman")
#'
#' # Negative target — common in meta-analytic selection-on-precision scenarios.
#' aligned_neg <- align_rank_corr(margins, rank_corr = -0.5)
#' attr(aligned_neg, "dependence_diagnostics")
#' @export
align_rank_corr <- function(
  upstream,
  rank_corr = 0,
  max_iter = 20000L,
  tol = 0.02,
  dependence_fn = NULL,
  ...
) {
  upstream <- .validate_l3_upstream(upstream, caller = "align_rank_corr")
  rank_corr <- .validate_scalar_number(rank_corr, "rank_corr")
  if (abs(rank_corr) > 1) {
    .abort_arg(
      "`rank_corr` must be in [-1, 1].",
      "Spearman correlation targets outside [-1, 1] are not valid.",
      "Pass `rank_corr = 0.3`, `rank_corr = 0`, or another value in [-1, 1]."
    )
  }
  max_iter <- .validate_scalar_integer(max_iter, "max_iter")
  if (max_iter < 100L) {
    .abort_arg(
      "`max_iter` must be at least 100.",
      "Rank alignment uses a greedy swap search with a minimum iteration budget.",
      "Use `max_iter = 20000L` unless you have a reason to tune it."
    )
  }
  tol <- .validate_scalar_number(tol, "tol")
  if (tol <= 0) {
    .abort_arg(
      "`tol` must be > 0.",
      "Rank alignment requires a positive convergence tolerance.",
      "Use `tol = 0.02` for the default continuous-target tolerance."
    )
  }
  if (abs(rank_corr) > 0.95) {
    cli::cli_warn(c(
      "!" = "`rank_corr` is near the boundary; hill-climb may not converge.",
      "i" = sprintf("Target residual Spearman is %s.", rank_corr),
      ">" = "Consider a less extreme target or a copula-based aligner."
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
  out <- .apply_l3_permutation(upstream, fit$perm, caller = "align_rank_corr")
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
    reorder_for_spearman(z_j, se2_j, target_corr = target),
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
    .abort_dependence_solver(
      "Rank alignment could not compute a finite achieved correlation.",
      "At least one rank vector is constant, so Spearman correlation is undefined.",
      "Use more variable effects and precision values, or set `dependence = \"none\"`."
    )
  }
  if (abs(target) > tol && !identical(sign(achieved), sign(target))) {
    .abort_dependence_solver(
      sprintf("Hill-climb sign gate failed: target %s, achieved %s.", target, achieved),
      sprintf("Combinatorial discreteness blocks this target at J = %s.", J),
      "Try a larger `J`, a less extreme target, or a copula-based aligner."
    )
  }
  if (!isTRUE(fit$converged)) {
    cli::cli_warn(c(
      "!" = "Rank alignment did not converge within tolerance.",
      "i" = sprintf("Target %s, achieved %s, tolerance %s.", target, achieved, tol),
      ">" = "Inspect `attr(x, \"dependence_diagnostics\")` before treating the target as exact."
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
# nolint end
