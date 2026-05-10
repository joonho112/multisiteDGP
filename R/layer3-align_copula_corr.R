# nolint start: object_usage_linter
#' Align precision ranks using a Gaussian copula pairing
#'
#' @encoding UTF-8
#'
#' @description
#' Permute the paired precision columns (`se_j`, `se2_j`, `n_j`) of a Layer 2
#' frame so the realized rank correlation between \eqn{z_j} and
#' \eqn{\widehat{se}_j^2} approximates a target *latent Gaussian copula*
#' correlation `pearson_corr`. Unlike the hill-climb in
#' \code{\link{align_rank_corr}}, the copula aligner draws a correlated
#' latent score per site and pairs the empirical `se2_j` multiset by that
#' order in one shot — fast, no iterative search.
#'
#' @details
#' \strong{Mechanism.} The package converts \eqn{z_j} to rank-normal scores,
#' draws a correlated latent score from a bivariate Gaussian copula with
#' Pearson correlation `pearson_corr`, and assigns the empirical `se2_j`
#' multiset by the order of the latent score. Because the assignment uses
#' only the order of the latent draw, the empirical SE marginal is
#' preserved exactly — only the joint distribution moves.
#'
#' \strong{Tradeoff vs hill-climb.} The copula path is fast and produces a
#' clean joint distribution that follows a Gaussian-copula form, but the
#' realized residual Spearman correlation is only *approximately* equal to
#' `pearson_corr` — the conversion is exact in the limit \eqn{J \to \infty}
#' and approximate at finite \eqn{J}. For tight tolerance on the realized
#' Spearman, use \code{\link{align_hybrid_corr}} (the recommended default —
#' copula init + hill-climb polish) or \code{\link{align_rank_corr}}
#' directly.
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
#' For `abs(pearson_corr) < 1`, the aligner consumes exactly one `rnorm()`
#' draw per site (the latent score). For `pearson_corr = -1` or
#' `pearson_corr = 1`, the endpoint path is deterministic and consumes
#' no RNG.
#'
#' @param upstream A Layer 2 data frame with canonical columns
#'   `site_index, z_j, tau_j, se_j, se2_j, n_j`.
#' @param pearson_corr Numeric in `[-1, 1]`. Target latent Gaussian-copula
#'   correlation. Default `0` (independence). Realized Spearman \eqn{\rho_S}
#'   is approximately \eqn{(6/\pi)\,\arcsin(\mathrm{pearson\_corr}/2)} —
#'   close to the input value over the typical applied range. Near-boundary
#'   `|pearson_corr| > 0.95` triggers a stability warning.
#' @param dependence_fn Optional callback. See \code{\link{align_rank_corr}}
#'   for the contract.
#' @param ... Additional arguments forwarded to `dependence_fn`.
#'
#' @return The upstream tibble with the paired precision columns permuted,
#'   plus attributes `permutation_perm` (the realized assignment) and
#'   `dependence_diagnostics`.
#'
#' @family family-dependence
#' @seealso
#'   \code{\link{align_rank_corr}} for the iterative hill-climb that hits
#'   the realized Spearman exactly within tolerance;
#'   \code{\link{align_hybrid_corr}} for the recommended default that
#'   combines copula init with hill-climb polish;
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
#' # Compose Layer 1 + 2 + 3 with copula injection (small J — noisy realization).
#' effects <- gen_effects_gaussian(J = 12L)
#' margins <- gen_site_sizes(effects, J = 12L, nj_mean = 40, cv = 0.2)
#' aligned <- align_copula_corr(margins, pearson_corr = 0.3)
#' cor(aligned$z_j, aligned$se2_j, method = "spearman")
#'
#' # At larger J the realized Spearman tracks the target closely.
#' effects100 <- gen_effects_gaussian(J = 100L)
#' margins100 <- gen_site_sizes(effects100, J = 100L, nj_mean = 40, cv = 0.2)
#' aligned100 <- align_copula_corr(margins100, pearson_corr = 0.5)
#' cor(aligned100$z_j, aligned100$se2_j, method = "spearman")
#' @export
align_copula_corr <- function(upstream, pearson_corr = 0, dependence_fn = NULL, ...) {
  upstream <- .validate_l3_upstream(upstream, caller = "align_copula_corr")
  pearson_corr <- .validate_scalar_number(pearson_corr, "pearson_corr")
  if (abs(pearson_corr) > 1) {
    .abort_arg(
      "`pearson_corr` must be in [-1, 1].",
      "Gaussian copula correlations outside [-1, 1] are not valid.",
      "Pass `pearson_corr = 0.3`, `pearson_corr = 0`, or another value in [-1, 1]."
    )
  }
  if (abs(pearson_corr) > 0.95) {
    cli::cli_warn(c(
      "!" = "`pearson_corr` is near the boundary; copula pairing may be nearly deterministic.",
      "i" = sprintf("Target latent Gaussian correlation is %s.", pearson_corr),
      ">" = "Inspect achieved rank correlation before treating the sample as exact."
    ))
  }
  dependence_fn <- .validate_function_or_null(dependence_fn, "dependence_fn")
  if (!is.null(dependence_fn)) {
    return(.apply_custom_dependence_fn(
      upstream = upstream,
      dependence_fn = dependence_fn,
      target = pearson_corr,
      target_type = "latent_gaussian_pearson",
      caller = "align_copula_corr",
      ...
    ))
  }

  target_score <- .rank_normal_scores(upstream$z_j, "upstream$z_j")
  .rank_normal_scores(upstream$se2_j, "upstream$se2_j")
  rng_draws <- if (identical(abs(pearson_corr), 1) || isTRUE(all.equal(abs(pearson_corr), 1))) {
    0L
  } else {
    nrow(upstream)
  }
  precision_score <- .copula_precision_score(target_score, pearson_corr)
  perm <- .copula_empirical_perm(precision_score, upstream$se2_j)
  out <- .apply_l3_permutation(upstream, perm, caller = "align_copula_corr")
  achieved_spearman <- .realized_spearman(out$z_j, out$se2_j)
  achieved_latent <- .latent_pearson(target_score, precision_score)

  attr(out, "dependence_diagnostics") <- list(
    method = "copula",
    mode = "empirical_rank_pairing",
    preservation = "empirical_multiset",
    target_type = "latent_gaussian_pearson",
    target = unname(pearson_corr),
    achieved = unname(achieved_latent),
    achieved_latent_pearson = unname(achieved_latent),
    achieved_spearman = unname(achieved_spearman),
    residual = unname(achieved_latent - pearson_corr),
    ties_method = "average",
    rng_draws = rng_draws,
    converged = NA,
    iterations = 0L,
    tol = NA_real_
  )
  out
}

.rank_normal_scores <- function(x, arg = "x") {
  ranks <- .mdgp_rank(x)
  n <- length(ranks)
  if (n < 2L || length(unique(ranks)) < 2L) {
    .abort_dependence_solver(
      sprintf("Copula alignment requires nonconstant ranks for `%s`.", arg),
      sprintf("A constant `%s` vector cannot define empirical rank-normal scores.", arg),
      "Use more variable effects and precision values, or set `dependence = \"none\"`."
    )
  }
  scores <- stats::qnorm((ranks - 0.5) / n)
  scores <- scores - mean(scores)
  score_sd <- stats::sd(scores)
  if (!is.finite(score_sd) || score_sd <= 0) {
    .abort_dependence_solver(
      sprintf("Copula alignment requires nonconstant rank-normal scores for `%s`.", arg),
      "The empirical rank-normal transform has zero variance.",
      "Use more variable effects and precision values, or set `dependence = \"none\"`."
    )
  }
  scores / score_sd
}

.copula_precision_score <- function(target_score, pearson_corr) {
  if (identical(abs(pearson_corr), 1) || isTRUE(all.equal(abs(pearson_corr), 1))) {
    return(sign(pearson_corr) * target_score)
  }
  pearson_corr * target_score + sqrt(1 - pearson_corr^2) * stats::rnorm(length(target_score))
}

.copula_empirical_perm <- function(precision_score, se2_j) {
  J <- length(precision_score)
  row_order <- order(precision_score, seq_len(J))
  se2_order <- order(se2_j, seq_len(J))
  perm <- integer(J)
  perm[row_order] <- se2_order
  .validate_permutation(perm, J, "perm")
}

.latent_pearson <- function(target_score, precision_score) {
  if (stats::sd(target_score) == 0 || stats::sd(precision_score) == 0) {
    return(NaN)
  }
  stats::cor(target_score, precision_score, method = "pearson")
}
# nolint end
