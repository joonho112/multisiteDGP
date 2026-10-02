#' Reorder standard errors with a Gaussian copula
#'
#' Draws a score for each site, correlated `pearson_corr` with the normal
#' score of its standardized site effect `z_j`, and assigns the sampling
#' variances `se2_j` in the order of the scores. The effects and the set of
#' standard errors do not change. With covariates the correlation refers to
#' `z_j`, not `tau_j`. Use [align_hybrid_corr()] for a target rank correlation.
#'
#' @details
#' The drawn score is `pearson_corr * s + sqrt(1 - pearson_corr^2) * e`, where
#' `s` is the normal score of `z_j`, `qnorm((rank(z_j) - 0.5) / J)` scaled to
#' unit variance, and `e` is standard normal noise. For continuous margins
#' without ties, the Spearman correlation between `z_j` and `se2_j` varies
#' from draw to draw, with a mean near `(6 / pi) * asin(pearson_corr / 2)`.
#' Ties and finite samples can change this relationship. The function stops
#' with an error if `z_j` or `se2_j` is constant and warns if
#' `abs(pearson_corr) > 0.95`.
#'
#' @inheritParams align_rank_corr
#' @param dependence_fn Function used in place of the built-in method, or
#'   `NULL` (the default). The details of [align_rank_corr()] say what it must
#'   accept and return.
#' @param pearson_corr Correlation between the drawn score and the normal score
#'   of `z_j` (-1 to 1, default `0`). A positive value pairs larger effects
#'   with larger standard errors.
#'
#' @return The input data as a tibble with `se_j`, `se2_j`, and `n_j`
#'   reordered. The attributes `permutation_perm` and `dependence_diagnostics`
#'   hold the permutation and a list with the correlation realized between the
#'   scores (`achieved`) and the Spearman correlation reached
#'   (`achieved_spearman`).
#'
#' @seealso [multisitedgp_design()], [realized_rank_corr()],
#'   `vignette("effect-precision-dependence")`.
#' @family dependence
#' @examples
#' set.seed(1)
#' dat <- gen_site_sizes(gen_effects(J = 50), J = 50)
#' out <- align_copula_corr(dat, pearson_corr = 0.5)
#' cor(out$z_j, out$se2_j, method = "spearman")
#' # The rank correlation varies from draw to draw
#' summary(replicate(100, {
#'   out <- align_copula_corr(dat, pearson_corr = 0.5)
#'   cor(out$z_j, out$se2_j, method = "spearman")
#' }))
#' @export
align_copula_corr <- function(upstream, pearson_corr = 0, dependence_fn = NULL, ...) {
  upstream <- .validate_precision_upstream(upstream, caller = "align_copula_corr")
  pearson_corr <- .validate_scalar_number(pearson_corr, "pearson_corr")
  if (abs(pearson_corr) > 1) {
    .abort("`pearson_corr` must be between -1 and 1.", .you_passed(pearson_corr))
  }
  if (abs(pearson_corr) > 0.95) {
    cli::cli_warn(c(
      "!" = paste(
        "`pearson_corr` is beyond 0.95 in absolute value, so the site effects",
        "nearly fix the order of the standard errors."
      ),
      "i" = .you_passed(pearson_corr)
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

  target_score <- .rank_normal_scores(upstream$z_j, "z_j")
  .rank_normal_scores(upstream$se2_j, "se2_j")
  rng_draws <- if (identical(abs(pearson_corr), 1) || isTRUE(all.equal(abs(pearson_corr), 1))) {
    0L
  } else {
    nrow(upstream)
  }
  precision_score <- .copula_precision_score(target_score, pearson_corr)
  perm <- .copula_empirical_perm(precision_score, upstream$se2_j)
  out <- .apply_dependence_permutation(upstream, perm, caller = "align_copula_corr")
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
    .abort_solver(
      sprintf("Column `%s` is constant, so the copula step cannot order the sites by it.", arg)
    )
  }
  scores <- stats::qnorm((ranks - 0.5) / n)
  scores <- scores - mean(scores)
  score_sd <- stats::sd(scores)
  if (!is.finite(score_sd) || score_sd <= 0) {
    .abort_solver(
      sprintf("The normal scores of column `%s` do not vary, so the copula step cannot use them.", arg)
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
