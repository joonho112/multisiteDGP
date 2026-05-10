# nolint start: object_usage_linter, object_name_linter, function_body_linter
#' Compute realized standard-error heterogeneity ratio
#'
#' @encoding UTF-8
#'
#' @description
#' Compute the realized heterogeneity ratio
#' \eqn{\widehat{R} = \max \widehat{se}_j^2 / \min \widehat{se}_j^2} (or
#' the trimmed-percentile variant). **Group A diagnostic** (precision and
#' feasibility — the first of Dr. Chen's four questions): "Are my sites
#' similarly precise, or does one site dominate?"
#'
#' @details
#' Default `trimmed = FALSE` uses raw min/max; for a more robust summary
#' on long-tailed precision distributions, set `trimmed = TRUE` to use the
#' 95th/5th percentile ratio. Reading the ratio: `R = 1` is homogeneous;
#' `R = 2` means the noisiest site has twice the variance of the most
#' precise; `R > 10` indicates a small subset of sites dominates.
#'
#' @param se2_j Numeric vector (length \eqn{\ge 2}) of positive sampling
#'   variances, or a `multisitedgp_data` object.
#' @param trimmed Logical. `FALSE` (default) for raw max/min;
#'   `TRUE` for 95th/5th percentile ratio.
#'
#' @return A scalar double \eqn{\ge 1}.
#' @family family-diagnostics
#' @seealso
#'   \code{\link{compute_I}}, \code{\link{informativeness}},
#'   \code{\link{feasibility_index}} for companion Group A diagnostics;
#'   the \href{../articles/a3-diagnostics-in-practice.html}{A3 Diagnostics
#'   in practice} vignette.
#' @references
#' Lee, J., Che, J., Rabe-Hesketh, S., Feller, A., & Miratrix, L. (2025).
#' Improving the estimation of site-specific effects and their distribution
#' in multisite trials. \emph{Journal of Educational and Behavioral Statistics},
#' \bold{50}(5), 731--764. \doi{10.3102/10769986241254286}.
#' @examples
#' heterogeneity_ratio(c(0.05, 0.08, 0.20))   # = 4
#' heterogeneity_ratio(c(0.001, 0.05, 0.08, 0.20, 5), trimmed = FALSE)
#' @export
heterogeneity_ratio <- function(se2_j, trimmed = FALSE) {
  if (is_multisitedgp_data(se2_j)) {
    se2_j <- se2_j$se2_j
  }
  se2_j <- .validate_se2_vector(se2_j, min_length = 2L)
  trimmed <- .validate_scalar_logical(trimmed, "trimmed")
  if (isTRUE(trimmed)) {
    if (length(se2_j) < 20L) {
      cli::cli_warn(c(
        "!" = "Trimmed heterogeneity ratio is unstable for small J.",
        "i" = sprintf("Got J = %s; the 5th/95th percentile ratio leaves too few sites in each tail.", length(se2_j)),
        ">" = "Use `trimmed = FALSE` or inspect the full SE distribution."
      ))
    }
    qs <- stats::quantile(se2_j, c(0.05, 0.95), names = FALSE, type = 7)
    return(unname(qs[[2L]] / qs[[1L]]))
  }
  max(se2_j) / min(se2_j)
}

#' Compute realized residual-scale Spearman rank correlation
#'
#' @encoding UTF-8
#'
#' @description
#' Compute the realized Spearman rank correlation between the latent
#' effect column and the per-site sampling variance. **Group B diagnostic**
#' (realized dependence): "Did the simulated dataset reproduce the
#' precision-dependence pattern I asked for?"
#'
#' @details
#' `on = "residual"` (default) uses `z_j` — the design target of Layer 3
#' aligners. `on = "marginal"` uses `tau_j` — important when site-level
#' covariates contribute to the marginal correlation without affecting
#' the residual.
#'
#' @param x A `multisitedgp_data` object.
#' @param on Character. `"residual"` (default) uses `z_j`; `"marginal"`
#'   uses `tau_j`.
#'
#' @return A scalar double in `[-1, 1]`, or `NA_real_` when either margin
#'   is constant.
#' @family family-diagnostics
#' @seealso
#'   \code{\link{realized_rank_corr_marginal}} for the marginal-scale
#'   alias; \code{\link{align_rank_corr}}, \code{\link{align_copula_corr}},
#'   \code{\link{align_hybrid_corr}} for Layer 3 aligners this verifies;
#'   the \href{../articles/a3-diagnostics-in-practice.html}{A3 Diagnostics
#'   in practice} vignette.
#' @references
#' Lee, J., Che, J., Rabe-Hesketh, S., Feller, A., & Miratrix, L. (2025).
#' Improving the estimation of site-specific effects and their distribution
#' in multisite trials. \emph{Journal of Educational and Behavioral Statistics},
#' \bold{50}(5), 731--764. \doi{10.3102/10769986241254286}.
#' @examples
#' dat <- sim_multisite(J = 50L, dependence = "rank", rank_corr = 0.3, seed = 1L)
#' realized_rank_corr(dat)
#' realized_rank_corr(dat, on = "marginal")
#' @export
realized_rank_corr <- function(x, on = c("residual", "marginal")) {
  .validate_diagnostic_data(x, "realized_rank_corr")
  on <- .match_choice(on, "on", c("residual", "marginal"))
  effect <- switch(on,
    residual = x$z_j,
    marginal = x$tau_j
  )
  .safe_cor(effect, x$se2_j, method = "spearman")
}

#' Compute realized marginal-scale Spearman rank correlation
#'
#' @encoding UTF-8
#'
#' @description
#' Convenience alias for `realized_rank_corr(x, on = "marginal")`.
#' Computes the realized Spearman rank correlation between the
#' response-scale latent effect `tau_j` and the per-site sampling
#' variance `se2_j`. **Group B diagnostic** (realized dependence — Dr.
#' Chen's question 2: "Did the simulated dataset reproduce the
#' precision-dependence pattern I asked for, on the marginal scale?").
#'
#' @details
#' Use this when you care about the dependence on the response-scale
#' latent effect after any covariate-adjusted shift, not just the
#' standardized residual `z_j`. When site-level covariates are present
#' (\eqn{X\boldsymbol{\beta}} entering `tau_j`), residual-scale and
#' marginal-scale Spearman can diverge — this alias surfaces the
#' marginal view.
#'
#' For the residual-scale view (which matches the design target of
#' Layer 3 aligners), use \code{\link{realized_rank_corr}} with
#' `on = "residual"` (the default).
#'
#' For the four-question diagnostic walkthrough see the
#' \href{../articles/a3-diagnostics-in-practice.html}{A3 \enc{·}{.}
#' Diagnostics in practice} vignette.
#'
#' @param x A `multisitedgp_data` object.
#' @param ... Reserved for future extensions.
#'
#' @return A scalar double in `[-1, 1]`, or `NA_real_` when either
#'   margin is constant.
#'
#' @family family-diagnostics
#' @seealso
#'   \code{\link{realized_rank_corr}} (the underlying generic) for the
#'   residual-vs-marginal contrast;
#'   \code{\link{align_rank_corr}}, \code{\link{align_copula_corr}},
#'   \code{\link{align_hybrid_corr}} for the Layer 3 aligners this
#'   diagnostic verifies;
#'   the \href{../articles/a3-diagnostics-in-practice.html}{A3 \enc{·}{.}
#'   Diagnostics in practice} vignette.
#'
#' @references
#' Lee, J., Che, J., Rabe-Hesketh, S., Feller, A., & Miratrix, L. (2025).
#' Improving the estimation of site-specific effects and their distribution
#' in multisite trials. \emph{Journal of Educational and Behavioral Statistics},
#' \bold{50}(5), 731--764. \doi{10.3102/10769986241254286}.
#'
#' @examples
#' # Marginal-scale realized correlation between tau_j and se2_j.
#' dat <- sim_multisite(J = 50L, seed = 1L)
#' realized_rank_corr_marginal(dat)
#' @export
realized_rank_corr_marginal <- function(x, ...) {
  realized_rank_corr(x, on = "marginal")
}

#' Compute Bhattacharyya coefficient between empirical distributions
#'
#' @encoding UTF-8
#'
#' @description
#' Compute the Bhattacharyya coefficient
#' \eqn{\mathrm{BC}(p, q) = \sum_b \sqrt{p_b\,q_b}} — a similarity measure
#' in `[0, 1]` where 1 is identical-shape and 0 is disjoint-support.
#' **Group C diagnostic** (distributional fit — the third of Dr. Chen's
#' four questions): "Does the realized effect distribution match the
#' target G shape?"
#'
#' @details
#' For `multisitedgp_data` inputs, the realized `z_j` is compared against
#' a deterministic reference quantile grid for `attr(x, "design")$true_dist`.
#' Higher BC = closer agreement. Default-threshold rubric flags BC < 0.85
#' as a fit warning at the typical applied setting (\eqn{J = 50},
#' `bins = 50`).
#'
#' @param x,y Numeric vectors. If `x` is a `multisitedgp_data` object and
#'   `y` is `NULL`, `x$z_j` is compared with the target G's quantile grid.
#' @param bins Integer. Number of histogram bins. Default `50L`.
#'
#' @return A scalar double in `[0, 1]`.
#' @family family-diagnostics
#' @seealso
#'   \code{\link{ks_distance}} for a tail-sensitive Group C complement;
#'   \code{\link{scenario_audit}} for the top-of-funnel sweep; the
#'   \href{../articles/a3-diagnostics-in-practice.html}{A3 Diagnostics
#'   in practice} vignette.
#' @references
#' Lee, J., Che, J., Rabe-Hesketh, S., Feller, A., & Miratrix, L. (2025).
#' Improving the estimation of site-specific effects and their distribution
#' in multisite trials. \emph{Journal of Educational and Behavioral Statistics},
#' \bold{50}(5), 731--764. \doi{10.3102/10769986241254286}.
#' @examples
#' bhattacharyya_coef(rnorm(200), rnorm(200))    # ≈ 1
#' dat <- sim_multisite(J = 50L, true_dist = "Gaussian", seed = 1L)
#' bhattacharyya_coef(dat)
#' @export
bhattacharyya_coef <- function(x, y = NULL, bins = 50L) {
  pair <- .resolve_distribution_pair(x, y, caller = "bhattacharyya_coef")
  x <- pair$x
  y <- pair$y
  bins <- .validate_bins(bins)

  range_xy <- range(c(x, y))
  if (identical(unname(range_xy[[1L]]), unname(range_xy[[2L]]))) {
    return(1)
  }
  breaks <- seq(range_xy[[1L]], range_xy[[2L]], length.out = bins + 1L)
  px <- graphics::hist(x, breaks = breaks, plot = FALSE)$counts / length(x)
  py <- graphics::hist(y, breaks = breaks, plot = FALSE)$counts / length(y)
  unname(sum(sqrt(px * py)))
}

#' Compute Kolmogorov-Smirnov distance between empirical distributions
#'
#' @encoding UTF-8
#'
#' @description
#' Compute the Kolmogorov-Smirnov distance
#' \eqn{D = \sup_t |F_x(t) - F_y(t)|} — a tail-sensitive complement to the
#' Bhattacharyya coefficient. **Group C diagnostic** (distributional fit):
#' use KS when worst single-quantile mismatch matters; use BC for overall
#' shape.
#'
#' @details
#' Lower KS = closer agreement. KS < 0.10 corresponds to two-sample KS
#' p > 0.05 at \eqn{J = 50}, the package's default flag threshold.
#' For `multisitedgp_data` inputs, `z_j` is compared against the target G's
#' deterministic reference quantile grid (same protocol as
#' \code{\link{bhattacharyya_coef}}).
#'
#' @param x,y Numeric vectors. If `x` is a `multisitedgp_data` object and
#'   `y` is `NULL`, `x$z_j` is compared with the target G's quantile grid.
#'
#' @return A scalar double in `[0, 1]`.
#' @family family-diagnostics
#' @seealso
#'   \code{\link{bhattacharyya_coef}} for the shape-similarity Group C
#'   companion; the \href{../articles/a3-diagnostics-in-practice.html}{A3
#'   Diagnostics in practice} vignette.
#' @references
#' Lee, J., Che, J., Rabe-Hesketh, S., Feller, A., & Miratrix, L. (2025).
#' Improving the estimation of site-specific effects and their distribution
#' in multisite trials. \emph{Journal of Educational and Behavioral Statistics},
#' \bold{50}(5), 731--764. \doi{10.3102/10769986241254286}.
#' @examples
#' ks_distance(rnorm(200), rnorm(200))    # small
#' dat <- sim_multisite(J = 50L, seed = 1L)
#' ks_distance(dat)
#' @export
ks_distance <- function(x, y = NULL) {
  pair <- .resolve_distribution_pair(x, y, caller = "ks_distance")
  unname(suppressWarnings(stats::ks.test(pair$x, pair$y)$statistic))
}

.attach_core_diagnostics_table <- function(x) {
  diagnostics <- attr(x, "diagnostics", exact = TRUE)
  diagnostics$target_vs_realized <- .apply_diagnostic_thresholds(.compute_diagnostics_table(x), x = x)
  attr(x, "diagnostics") <- diagnostics
  x
}

.core_diagnostics_table <- function(x) {
  .compute_diagnostics_table(x)
}

.compute_diagnostics_table <- function(x, design = attr(x, "design", exact = TRUE)) {
  .validate_diagnostic_data(x, ".compute_diagnostics_table")
  design <- attr(x, "design", exact = TRUE)
  diagnostics <- attr(x, "diagnostics", exact = TRUE)
  sigma_tau <- .diagnostic_sigma_tau(x)
  target_i <- .target_i(design, diagnostics)
  target_r <- .target_r(design, diagnostics)
  gm_target <- if (is.finite(target_i$value)) {
    sigma_tau^2 * (1 - target_i$value) / target_i$value
  } else {
    NA_real_
  }
  target_rank <- .target_rank_corr(design, diagnostics)
  target_pearson <- .target_pearson_corr(design, diagnostics)

  table <- list(
    .diagnostic_row("A", "informativeness", "overall", target_i$value, compute_I(x$se2_j, sigma_tau = sigma_tau, tau_j = x$tau_j), target_i$source, "compute_I", "API026", NA_real_, ""),
    .diagnostic_row("A", "heterogeneity_ratio", "overall", target_r$value, heterogeneity_ratio(x$se2_j), target_r$source, "heterogeneity_ratio", "API030", NA_real_, ""),
    .diagnostic_row("A", "sigma_tau", "residual", sigma_tau, stats::sd(x$z_j) * sigma_tau, "design", "stats::sd", NA_character_, NA_real_, "residual scale"),
    .diagnostic_row("A", "GM_se2", "overall", gm_target, exp(mean(log(x$se2_j))), .gm_target_source(target_i), "exp_mean_log", NA_character_, NA_real_, ""),
    .diagnostic_row("B", "realized_rank_corr", "residual", target_rank$value, realized_rank_corr(x, on = "residual"), target_rank$source, "realized_rank_corr", "API034", NA_real_, target_rank$note),
    .diagnostic_row("B", "realized_rank_corr", "marginal", NA_real_, realized_rank_corr(x, on = "marginal"), "not_available", "realized_rank_corr", "API034", NA_real_, "marginal reporting diagnostic"),
    .diagnostic_row("B", "realized_pearson_corr", "residual", target_pearson$value, .safe_cor(x$z_j, x$se2_j, method = "pearson"), target_pearson$source, "stats::cor", NA_character_, NA_real_, target_pearson$note),
    .diagnostic_row("B", "realized_pearson_corr", "marginal", NA_real_, .safe_cor(x$tau_j, x$se2_j, method = "pearson"), "not_available", "stats::cor", NA_character_, NA_real_, "marginal reporting diagnostic"),
    .diagnostic_row("C", "ks_distance", "target_G", 0, .safe_distribution_distance(ks_distance, x), "ideal", "ks_distance", "API039", .safe_distribution_p_value(x), ""),
    .diagnostic_row("C", "bhattacharyya_coef", "target_G", 1, .safe_distribution_distance(bhattacharyya_coef, x), "ideal", "bhattacharyya_coef", "API038", NA_real_, ""),
    .diagnostic_row("C", "qq_residuals", "target_G", 0, .safe_distribution_distance(.qq_residual_max_abs, x), "ideal", ".qq_residual_max_abs", NA_character_, NA_real_, "max absolute standardized quantile residual"),
    .diagnostic_row("D", "mean_shrinkage", "overall", NA_real_, mean_shrinkage(x), "not_available", "mean_shrinkage", "API033", NA_real_, ""),
    .diagnostic_row("D", "average_moe", "overall", NA_real_, stats::qnorm(0.975) * mean(x$se_j), "not_available", "stats::qnorm", NA_character_, NA_real_, "95 percent average margin of error"),
    .diagnostic_row("D", "feasibility_index", "efron", NA_real_, feasibility_index(x$se2_j, sigma_tau = sigma_tau, warn = FALSE), "not_available", "feasibility_index", "API028", NA_real_, "")
  )

  out <- dplyr::bind_rows(table)
  out$delta <- out$realized - out$target
  out$rel_delta <- .relative_delta(out$realized, out$target)
  tibble::as_tibble(out[c(
    "group", "diagnostic", "basis", "target", "realized", "delta", "rel_delta",
    "target_source", "helper", "api_id", "p_value", "note"
  )])
}

.apply_diagnostic_thresholds <- function(tab, x) {
  design <- attr(x, "design", exact = TRUE)
  sigma_tau <- if (is.null(design) || is.null(design$sigma_tau)) NA_real_ else design$sigma_tau
  specs <- .diagnostic_threshold_rubric()
  out <- tab
  out$status <- NA_character_
  out$threshold_metric <- NA_character_
  out$pass_rule <- NA_character_
  out$warn_rule <- NA_character_
  out$fail_rule <- NA_character_
  out$threshold_source <- NA_character_
  out$threshold_note <- NA_character_

  for (idx in seq_len(nrow(out))) {
    spec <- .threshold_spec_for_row(out[idx, ], specs, design = design, J = nrow(x))
    if (is.null(spec)) {
      next
    }
    out$status[[idx]] <- .evaluate_threshold_status(out[idx, ], spec, sigma_tau = sigma_tau)
    out$threshold_metric[[idx]] <- spec$metric
    out$pass_rule[[idx]] <- spec$pass_rule
    out$warn_rule[[idx]] <- spec$warn_rule
    out$fail_rule[[idx]] <- spec$fail_rule
    out$threshold_source[[idx]] <- spec$source
    out$threshold_note[[idx]] <- spec$note
  }
  out
}

.diagnostic_threshold_rubric <- function() {
  list(
    informativeness = .threshold_spec("abs_delta", "abs(delta) < 0.02", "0.02 <= abs(delta) < 0.05", "abs(delta) >= 0.05", "derived"),
    heterogeneity_ratio = .threshold_spec("rel_delta_abs", "abs(rel_delta) < 0.10", "0.10 <= abs(rel_delta) < 0.25", "abs(rel_delta) >= 0.25", "derived"),
    sigma_tau = .threshold_spec("rel_delta_abs", "abs(rel_delta) < 0.05", "0.05 <= abs(rel_delta) < 0.15", "abs(rel_delta) >= 0.15", "derived"),
    GM_se2 = .threshold_spec("rel_delta_abs", "abs(rel_delta) < 0.05", "0.05 <= abs(rel_delta) < 0.15", "abs(rel_delta) >= 0.15", "derived"),
    realized_rank_corr = .threshold_spec("abs_delta", "abs(delta) < 0.02", "0.02 <= abs(delta) < 0.05", "abs(delta) >= 0.05", "simulation-calibrated", "residual target rows only"),
    realized_pearson_corr = .threshold_spec("abs_delta", "abs(delta) < 0.03", "0.03 <= abs(delta) < 0.08", "abs(delta) >= 0.08", "heuristic", "residual target rows only"),
    ks_distance = .threshold_spec("p_value", "p_value > 0.05", "0.01 < p_value <= 0.05", "p_value <= 0.01", "simulation-calibrated"),
    bhattacharyya_coef = .threshold_spec("realized", "realized >= 0.90", "0.80 <= realized < 0.90", "realized < 0.80", "heuristic"),
    qq_residuals = .threshold_spec("deferred", "band metadata available", "band metadata near boundary", "band metadata fail", "derived", "order-statistic band metadata deferred"),
    mean_shrinkage = .threshold_spec("realized_range", "0.10 <= realized <= 0.95", "0.05 <= realized < 0.10 or 0.95 < realized <= 0.99", "realized < 0.05 or realized > 0.99", "heuristic"),
    average_moe = .threshold_spec("sigma_tau_multiple", "realized <= 2 * sigma_tau", "2 * sigma_tau < realized <= 4 * sigma_tau", "realized > 4 * sigma_tau", "heuristic"),
    feasibility_index = .threshold_spec("realized", "realized >= 30", "5 <= realized < 30", "realized < 5", "simulation-calibrated")
  )
}

.threshold_spec <- function(metric, pass_rule, warn_rule, fail_rule, source, note = "") {
  list(
    metric = metric,
    pass_rule = pass_rule,
    warn_rule = warn_rule,
    fail_rule = fail_rule,
    source = source,
    note = note
  )
}

.threshold_spec_for_row <- function(row, specs, design = NULL, J = NA_integer_) {
  diagnostic <- row$diagnostic[[1L]]
  if (!diagnostic %in% names(specs)) {
    return(NULL)
  }
  spec <- specs[[diagnostic]]
  if (diagnostic %in% c("realized_rank_corr", "realized_pearson_corr") && !is.finite(row$target[[1L]])) {
    spec$note <- paste(c(spec$note, "no finite target; status not assigned"), collapse = "; ")
    spec$metric <- "not_applicable"
    return(spec)
  }
  if (identical(diagnostic, "realized_rank_corr") && identical(row$basis[[1L]], "residual")) {
    spec <- .rank_corr_threshold_spec(spec, row = row, design = design, J = J)
  }
  spec
}

.rank_corr_threshold_spec <- function(spec, row, design, J) {
  method <- if (!is.null(design) && is.list(design$dependence_spec)) {
    design$dependence_spec$method
  } else {
    NA_character_
  }
  if (method %in% c("none", "rank", "hybrid") && isTRUE(all.equal(row$target[[1L]], 0))) {
    cutoff <- .sampling_drift_cutoffs(J)
    spec$metric <- "sampling_drift"
    spec$pass_rule <- "abs(realized) <= 2 / sqrt(J - 1)"
    spec$warn_rule <- "2 / sqrt(J - 1) < abs(realized) <= 3 / sqrt(J - 1)"
    spec$fail_rule <- "abs(realized) > 3 / sqrt(J - 1)"
    spec$source <- "sampling-theory"
    spec$note <- sprintf(
      "%s; finite-J sampling drift bands use J = %s",
      if (identical(method, "none")) "independence baseline" else paste(method, "zero target"),
      if (is.finite(J)) J else "unknown"
    )
    spec$pass_cutoff <- cutoff[["pass"]]
    spec$warn_cutoff <- cutoff[["warn"]]
    return(spec)
  }
  if (method %in% c("rank", "hybrid")) {
    tol <- design$dependence_spec$tol
    if (is.finite(tol) && tol > 0) {
      pass_cutoff <- max(0.02, tol)
      warn_cutoff <- max(0.05, 2 * tol)
      spec$metric <- "rank_corr_tolerance"
      spec$pass_rule <- sprintf("abs(delta) <= max(0.02, design tol = %.3f)", tol)
      spec$warn_rule <- sprintf("max(0.02, design tol) < abs(delta) <= max(0.05, 2 * design tol = %.3f)", 2 * tol)
      spec$fail_rule <- "abs(delta) exceeds tolerance-aware warning band"
      spec$note <- sprintf("residual target rows only; design tolerance = %.3f", tol)
      spec$pass_cutoff <- pass_cutoff
      spec$warn_cutoff <- warn_cutoff
    }
  }
  spec
}

.evaluate_threshold_status <- function(row, spec, sigma_tau) {
  realized <- row$realized[[1L]]
  target <- row$target[[1L]]
  delta <- row$delta[[1L]]
  rel_delta <- row$rel_delta[[1L]]
  p_value <- row$p_value[[1L]]
  metric <- spec$metric

  if (identical(metric, "not_applicable") || identical(metric, "deferred")) {
    return(NA_character_)
  }
  if (metric %in% c("realized", "realized_range") && !is.finite(realized)) {
    return(NA_character_)
  }
  if (metric %in% c("abs_delta", "rel_delta_abs") && (!is.finite(target) || !is.finite(realized))) {
    return(NA_character_)
  }

  switch(metric,
    abs_delta = .status_by_abs_delta(abs(delta), row$diagnostic[[1L]]),
    rank_corr_tolerance = .status_by_cutoffs(abs(delta), spec$pass_cutoff, spec$warn_cutoff),
    sampling_drift = .status_by_cutoffs(abs(realized), spec$pass_cutoff, spec$warn_cutoff),
    rel_delta_abs = .status_by_rel_delta(abs(rel_delta), row$diagnostic[[1L]]),
    p_value = .status_ks(p_value),
    realized = .status_by_realized(realized, row$diagnostic[[1L]]),
    realized_range = .status_mean_shrinkage(realized),
    sigma_tau_multiple = .status_average_moe(realized, sigma_tau),
    NA_character_
  )
}

.sampling_drift_cutoffs <- function(J) {
  if (!is.finite(J) || J < 2L) {
    return(c(pass = NA_real_, warn = NA_real_))
  }
  se <- 1 / sqrt(J - 1)
  c(pass = 2 * se, warn = 3 * se)
}

.status_by_cutoffs <- function(value, pass_cutoff, warn_cutoff) {
  if (!is.finite(value) || !is.finite(pass_cutoff) || !is.finite(warn_cutoff)) {
    return(NA_character_)
  }
  if (value <= pass_cutoff) {
    "PASS"
  } else if (value <= warn_cutoff) {
    "WARN"
  } else {
    "FAIL"
  }
}

.status_by_abs_delta <- function(abs_delta, diagnostic) {
  if (!is.finite(abs_delta)) {
    return(NA_character_)
  }
  cutoffs <- switch(diagnostic,
    informativeness = c(pass = 0.02, fail = 0.05),
    realized_rank_corr = c(pass = 0.02, fail = 0.05),
    realized_pearson_corr = c(pass = 0.03, fail = 0.08),
    c(pass = NA_real_, fail = NA_real_)
  )
  if (abs_delta < cutoffs[["pass"]]) {
    "PASS"
  } else if (abs_delta < cutoffs[["fail"]]) {
    "WARN"
  } else {
    "FAIL"
  }
}

.status_by_rel_delta <- function(abs_rel_delta, diagnostic) {
  if (!is.finite(abs_rel_delta)) {
    return(NA_character_)
  }
  cutoffs <- switch(diagnostic,
    heterogeneity_ratio = c(pass = 0.10, fail = 0.25),
    sigma_tau = c(pass = 0.05, fail = 0.15),
    GM_se2 = c(pass = 0.05, fail = 0.15),
    c(pass = NA_real_, fail = NA_real_)
  )
  if (abs_rel_delta < cutoffs[["pass"]]) {
    "PASS"
  } else if (abs_rel_delta < cutoffs[["fail"]]) {
    "WARN"
  } else {
    "FAIL"
  }
}

.status_ks <- function(p_value) {
  if (!is.finite(p_value)) {
    return(NA_character_)
  }
  if (p_value > 0.05) {
    "PASS"
  } else if (p_value > 0.01) {
    "WARN"
  } else {
    "FAIL"
  }
}

.status_by_realized <- function(realized, diagnostic) {
  switch(diagnostic,
    bhattacharyya_coef = .status_bhattacharyya(realized),
    feasibility_index = .feasibility_status(realized),
    NA_character_
  )
}

.status_bhattacharyya <- function(realized) {
  if (realized >= 0.90) {
    "PASS"
  } else if (realized >= 0.80) {
    "WARN"
  } else {
    "FAIL"
  }
}

.status_mean_shrinkage <- function(realized) {
  if (realized >= 0.10 && realized <= 0.95) {
    "PASS"
  } else if ((realized >= 0.05 && realized < 0.10) || (realized > 0.95 && realized <= 0.99)) {
    "WARN"
  } else {
    "FAIL"
  }
}

.status_average_moe <- function(realized, sigma_tau) {
  if (!is.finite(realized) || !is.finite(sigma_tau) || sigma_tau < 0) {
    return(NA_character_)
  }
  if (realized <= 2 * sigma_tau) {
    "PASS"
  } else if (realized <= 4 * sigma_tau) {
    "WARN"
  } else {
    "FAIL"
  }
}

.feasibility_status <- function(realized) {
  if (!is.finite(realized)) {
    return(NA_character_)
  }
  if (realized >= 30) {
    "PASS"
  } else if (realized >= 5) {
    "WARN"
  } else {
    "FAIL"
  }
}

.warn_feasibility_fail <- function(value) {
  cli::cli_warn(c(
    "!" = "Feasibility index = FAIL -- partial-pooling estimator may be degenerate.",
    "i" = sprintf("Efron effective information is %.3f; FAIL threshold is < 5.", value),
    ">" = "Try larger `J`, larger `nj_mean`, or larger `sigma_tau`."
  ))
}

.diagnostic_row <- function(group, diagnostic, basis, target, realized, target_source, helper, api_id, p_value, note) {
  tibble::tibble(
    group = group,
    diagnostic = diagnostic,
    basis = basis,
    target = as.numeric(target),
    realized = as.numeric(realized),
    target_source = target_source,
    helper = helper,
    api_id = api_id,
    p_value = as.numeric(p_value),
    note = note
  )
}

.validate_diagnostic_data <- function(x, caller) {
  if (!is_multisitedgp_data(x)) {
    .abort_arg(
      sprintf("`%s()` requires a multisitedgp_data object.", caller),
      sprintf("Received object with class: %s.", paste(class(x), collapse = "/")),
      "Pass the output of `sim_multisite()` or `sim_meta()`."
    )
  }
  missing <- setdiff(.canonical_data_columns(), names(x))
  if (length(missing) > 0L) {
    .abort_arg(
      sprintf("`%s()` requires canonical simulation columns.", caller),
      sprintf("Missing columns: %s.", paste(missing, collapse = ", ")),
      "Pass an unmodified multisiteDGP simulation output."
    )
  }
  invisible(x)
}

.diagnostic_sigma_tau <- function(x) {
  design <- attr(x, "design", exact = TRUE)
  if (is.null(design) || is.null(design$sigma_tau)) {
    .abort_arg(
      "Diagnostics require `sigma_tau`.",
      "`sigma_tau` is read from `attr(x, \"design\")`.",
      "Pass a simulation object with an attached design."
    )
  }
  design$sigma_tau
}

.target_i <- function(design, diagnostics) {
  if (!is.null(design) && identical(design$paradigm, "direct") && !is.null(design$I)) {
    return(list(value = design$I, source = "direct_design"))
  }
  if (is.list(diagnostics) && !is.null(diagnostics$target_I)) {
    return(list(value = diagnostics$target_I, source = "diagnostics"))
  }
  list(value = NA_real_, source = "not_available")
}

.target_r <- function(design, diagnostics) {
  if (!is.null(design) && identical(design$paradigm, "direct") && !is.null(design$R)) {
    return(list(value = design$R, source = "direct_design"))
  }
  if (is.list(diagnostics) && !is.null(diagnostics$target_R)) {
    return(list(value = diagnostics$target_R, source = "diagnostics"))
  }
  list(value = NA_real_, source = "not_available")
}

.target_rank_corr <- function(design, diagnostics) {
  if (!is.null(design) && is.list(design$dependence_spec)) {
    if (design$dependence_spec$method %in% c("rank", "hybrid")) {
      return(list(value = design$dependence_spec$rank_corr, source = "design", note = design$dependence_spec$method))
    }
    if (identical(design$dependence_spec$method, "none")) {
      return(list(value = 0, source = "design", note = "independence baseline"))
    }
  }
  if (is.list(diagnostics) && !is.null(diagnostics$target_rank_corr)) {
    return(list(value = diagnostics$target_rank_corr, source = "diagnostics", note = "diagnostics"))
  }
  list(value = NA_real_, source = "not_available", note = "no rank-correlation target")
}

.target_pearson_corr <- function(design, diagnostics) {
  if (!is.null(design) && is.list(design$dependence_spec)) {
    if (identical(design$dependence_spec$method, "copula")) {
      return(list(value = design$dependence_spec$pearson_corr, source = "design", note = "latent Gaussian copula target"))
    }
    if (identical(design$dependence_spec$method, "none")) {
      return(list(value = 0, source = "design", note = "independence baseline"))
    }
  }
  if (is.list(diagnostics) && !is.null(diagnostics$target_pearson_corr)) {
    return(list(value = diagnostics$target_pearson_corr, source = "diagnostics", note = "diagnostics"))
  }
  list(value = NA_real_, source = "not_available", note = "no Pearson target for this dependence mode")
}

.gm_target_source <- function(target_i) {
  if (is.finite(target_i$value)) {
    return("derived_expectation")
  }
  "not_available"
}

.relative_delta <- function(value, target) {
  out <- rep(NA_real_, length(value))
  ok <- is.finite(value) & is.finite(target) & target != 0
  out[ok] <- (value[ok] - target[ok]) / abs(target[ok])
  out
}

.safe_distribution_distance <- function(fn, x) {
  tryCatch(
    fn(x),
    error = function(e) NA_real_
  )
}

.safe_distribution_p_value <- function(x) {
  pair <- tryCatch(
    .resolve_distribution_pair(x, NULL, caller = "ks_distance"),
    error = function(e) NULL
  )
  if (is.null(pair)) {
    return(NA_real_)
  }
  unname(suppressWarnings(stats::ks.test(pair$x, pair$y)$p.value))
}

.resolve_distribution_pair <- function(x, y = NULL, caller) {
  if (is_multisitedgp_data(x) && is.null(y)) {
    .validate_diagnostic_data(x, caller)
    return(list(x = .validate_numeric_vector(x$z_j, "x$z_j", min_length = 2L), y = .reference_z_quantiles(x)))
  }
  if (is_multisitedgp_data(x) && !is.null(y)) {
    x <- x$z_j
  }
  list(
    x = .validate_numeric_vector(x, "x", min_length = 2L),
    y = .validate_numeric_vector(y, "y", min_length = 2L)
  )
}

.reference_z_quantiles <- function(x) {
  design <- attr(x, "design", exact = TRUE)
  if (is.null(design) || is.null(design$true_dist)) {
    .abort_arg(
      "Distribution-distance diagnostics require a design.",
      "The target distribution is read from `attr(x, \"design\")$true_dist`.",
      "Pass `y` explicitly or use a simulation object with an attached design."
    )
  }
  p <- stats::ppoints(nrow(x))
  switch(design$true_dist,
    Gaussian = stats::qnorm(p),
    StudentT = {
      nu <- design$theta_G$nu
      stats::qt(p, df = nu) * sqrt((nu - 2) / nu)
    },
    .abort_arg(
      "Automatic reference distances are implemented for Gaussian and StudentT targets only.",
      sprintf("Got `true_dist = \"%s\"`.", design$true_dist),
      "Pass an explicit numeric `y` reference vector for this distribution."
    )
  )
}

.qq_residual_max_abs <- function(x) {
  pair <- .resolve_distribution_pair(x, NULL, caller = ".qq_residual_max_abs")
  max(abs(sort(pair$x) - sort(pair$y)))
}

.validate_bins <- function(bins) {
  bins <- .validate_scalar_integer(bins, "bins")
  if (bins < 2L) {
    .abort_arg(
      "`bins` must be at least 2.",
      "Bhattacharyya overlap uses a shared histogram grid.",
      "Use `bins = 50L` or another integer at least 2."
    )
  }
  bins
}
# nolint end
