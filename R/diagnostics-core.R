#' Heterogeneity ratio of the sampling variances
#'
#' Computes the heterogeneity ratio `R`, the largest sampling variance
#' divided by the smallest: `max(se2_j) / min(se2_j)`. This is the `R` that
#' [sim_meta()] takes as a target.
#'
#' @param se2_j Numeric vector of sampling variances (positive, at least two
#'   values), or simulated data from [sim_multisite()] or [sim_meta()], whose
#'   `se2_j` column is used.
#' @param trimmed Logical. If `TRUE`, the function returns the ratio of the
#'   95th to the 5th percentile of `se2_j` instead, and warns when there are
#'   fewer than 20 values. Default `FALSE`.
#'
#' @return A single number, at least 1.
#'
#' @seealso [informativeness()], [gen_se_direct()],
#'   `vignette("diagnostics-and-presets")`.
#' @family diagnostics
#' @examples
#' heterogeneity_ratio(c(0.05, 0.08, 0.20))
#'
#' dat <- sim_multisite(preset_education_modest(), seed = 1)
#' heterogeneity_ratio(dat)
#' heterogeneity_ratio(dat, trimmed = TRUE)
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
        "!" = "The trimmed heterogeneity ratio is unstable with fewer than 20 values.",
        "i" = sprintf(
          "With %s values the 5th and 95th percentiles are close to the smallest and the largest value.",
          length(se2_j)
        )
      ))
    }
    qs <- stats::quantile(se2_j, c(0.05, 0.95), names = FALSE, type = 7)
    return(unname(qs[[2L]] / qs[[1L]]))
  }
  max(se2_j) / min(se2_j)
}

#' Realized rank correlation between site effects and sampling variances
#'
#' Computes the Spearman correlation between the site effects and the sampling
#' variances `se2_j` of simulated data. In a design with `dependence = "rank"`
#' or `"hybrid"`, `rank_corr` is the target for the correlation with the
#' standardized site effects. `realized_rank_corr_marginal(x)` is short for
#' `realized_rank_corr(x, on = "marginal")`.
#'
#' @param x Simulated data from [sim_multisite()] or [sim_meta()].
#' @param on Which effects to use: `"residual"` (the default) for the
#'   standardized site effects `z_j`, or `"marginal"` for the site effects
#'   `tau_j`. The two give the same value unless the design has covariates.
#' @param ... Not used.
#'
#' @return A single number between -1 and 1, or `NA` when `se2_j` or the
#'   effects are constant.
#'
#' @seealso [align_rank_corr()], [align_hybrid_corr()], [plot_dependence()],
#'   `vignette("effect-precision-dependence")`.
#' @family diagnostics
#' @examples
#' design <- preset_education_modest(dependence = "rank", rank_corr = 0.3)
#' dat <- sim_multisite(design, seed = 1)
#' realized_rank_corr(dat)
#'
#' # With a site-level covariate the two correlations differ
#' design <- update_multisitedgp_design(
#'   design, formula = ~x, beta = 0.3, data = data.frame(x = rep(0:1, 25))
#' )
#' dat <- sim_multisite(design, seed = 1)
#' realized_rank_corr(dat)
#' realized_rank_corr_marginal(dat)
#' @export
realized_rank_corr <- function(x, on = c("residual", "marginal")) {
  .validate_diagnostic_data(x)
  on <- .match_choice(on, "on", c("residual", "marginal"))
  effect <- switch(on,
    residual = x$z_j,
    marginal = x$tau_j
  )
  .safe_cor(effect, x$se2_j, method = "spearman")
}

#' @rdname realized_rank_corr
#' @export
realized_rank_corr_marginal <- function(x, ...) {
  realized_rank_corr(x, on = "marginal")
}

#' Bhattacharyya coefficient between two distributions
#'
#' Computes the Bhattacharyya coefficient, the overlap of two histograms on a
#' common grid: \eqn{\sum_b \sqrt{p_b q_b}}{sum(sqrt(p_b * q_b))}, where
#' \eqn{p_b} and \eqn{q_b} are the proportions of the two samples in cell
#' \eqn{b}. Given simulated data alone, the function compares the standardized
#' site effects `z_j` with the distribution the design asked for, standardized
#' to mean 0 and variance 1.
#'
#' @details
#' The value depends on `bins` and on the number of values. With 50 cells,
#' simulated Gaussian data give about 0.77 for 50 sites and 0.95 for 200.
#'
#' @param x Numeric vector, or simulated data from [sim_multisite()] or
#'   [sim_meta()], whose `z_j` column is used.
#' @param y Numeric vector to compare with `x`. If `NULL` (the default), `x`
#'   must be simulated data with `true_dist = "Gaussian"` or `"StudentT"`, and
#'   `z_j` is compared with the quantiles of that distribution at `ppoints(J)`.
#' @param bins Integer number of cells (at least 2), of equal width over the
#'   range of both samples. Default `50`.
#'
#' @return A single number between 0 (no overlap) and 1 (equal histograms).
#'
#' @seealso [ks_distance()], [scenario_audit()],
#'   `vignette("diagnostics-and-presets")`.
#' @family diagnostics
#' @examples
#' dat <- sim_multisite(preset_education_modest(), seed = 1)
#' bhattacharyya_coef(dat)
#' bhattacharyya_coef(dat, bins = 10)
#'
#' # A mixture design has no built-in comparison, so pass a reference sample
#' mix <- sim_multisite(preset_jebs_paper(), seed = 1)
#' ref <- sim_multisite(preset_jebs_paper(J = 5000), seed = 2)
#' bhattacharyya_coef(mix, ref$z_j)
#' @export
bhattacharyya_coef <- function(x, y = NULL, bins = 50L) {
  pair <- .resolve_distribution_pair(x, y)
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

#' Kolmogorov-Smirnov distance between two distributions
#'
#' Computes the Kolmogorov-Smirnov statistic, the largest absolute difference
#' between two cumulative distribution functions. Given simulated data alone,
#' the function compares the standardized site effects `z_j` with the
#' distribution the design asked for, standardized to mean 0 and variance 1.
#'
#' @param x Numeric vector, or simulated data from [sim_multisite()] or
#'   [sim_meta()], whose `z_j` column is used.
#' @param y Numeric vector to compare with `x`. If `NULL` (the default), `x`
#'   must be simulated data with `true_dist = "Gaussian"` or `"StudentT"`, and
#'   `z_j` is compared with the distribution function of that distribution.
#'
#' @return A single number between 0 and 1, the statistic of
#'   [stats::ks.test()].
#'
#' @seealso [bhattacharyya_coef()], [scenario_audit()],
#'   `vignette("diagnostics-and-presets")`.
#' @family diagnostics
#' @examples
#' dat <- sim_multisite(preset_education_modest(), seed = 1)
#' ks_distance(dat)
#'
#' # A mixture design has no built-in comparison, so pass a reference sample
#' mix <- sim_multisite(preset_jebs_paper(), seed = 1)
#' ref <- sim_multisite(preset_jebs_paper(J = 5000), seed = 2)
#' ks_distance(mix, ref$z_j)
#' @export
ks_distance <- function(x, y = NULL) {
  if (is_multisitedgp_data(x) && is.null(y)) {
    .validate_diagnostic_data(x)
    cdf <- .reference_z_cdf(x)
    return(unname(suppressWarnings(stats::ks.test(x$z_j, cdf)$statistic)))
  }
  pair <- .resolve_distribution_pair(x, y)
  unname(suppressWarnings(stats::ks.test(pair$x, pair$y)$statistic))
}

.attach_core_diagnostics_table <- function(x) {
  diagnostics <- attr(x, "diagnostics", exact = TRUE)
  diagnostics$target_vs_realized <- .apply_diagnostic_thresholds(.compute_diagnostics_table(x), x = x)
  attr(x, "diagnostics") <- diagnostics
  x
}

.compute_diagnostics_table <- function(x, design = attr(x, "design", exact = TRUE)) {
  .validate_diagnostic_data(x)
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
  achieved_latent_pearson <- .achieved_latent_pearson(diagnostics)

  table <- list(
    .diagnostic_row(
      "scale", "informativeness", "overall",
      target_i$value, compute_I(x$se2_j, sigma_tau = sigma_tau, tau_j = x$tau_j),
      target_i$source, NA_real_, ""
    ),
    .diagnostic_row(
      "scale", "heterogeneity_ratio", "overall",
      target_r$value, heterogeneity_ratio(x$se2_j),
      target_r$source, NA_real_, ""
    ),
    .diagnostic_row(
      "scale", "sigma_tau", "residual",
      sigma_tau, stats::sd(x$z_j) * sigma_tau,
      "design", NA_real_, "residual scale"
    ),
    .diagnostic_row(
      "scale", "GM_se2", "overall",
      gm_target, exp(mean(log(x$se2_j))),
      .gm_target_source(target_i), NA_real_, ""
    ),
    .diagnostic_row(
      "dependence", "realized_rank_corr", "residual",
      target_rank$value, realized_rank_corr(x, on = "residual"),
      target_rank$source, NA_real_, target_rank$note
    ),
    .diagnostic_row(
      "dependence", "realized_rank_corr", "marginal",
      NA_real_, realized_rank_corr(x, on = "marginal"),
      "not_available", NA_real_, "marginal reporting diagnostic"
    ),
    .diagnostic_row(
      "dependence", "realized_pearson_corr", "copula_latent",
      target_pearson$value, achieved_latent_pearson,
      target_pearson$source, NA_real_, target_pearson$note
    ),
    .diagnostic_row(
      "dependence", "raw_pearson_corr", "residual",
      NA_real_, .safe_cor(x$z_j, x$se2_j, method = "pearson"),
      "not_targeted", NA_real_, "descriptive raw-margin Pearson; not the Gaussian-copula target"
    ),
    .diagnostic_row(
      "dependence", "raw_pearson_corr", "marginal",
      NA_real_, .safe_cor(x$tau_j, x$se2_j, method = "pearson"),
      "not_targeted", NA_real_, "descriptive marginal Pearson"
    ),
    .diagnostic_row(
      "shape", "ks_distance", "target_G",
      0, .safe_distribution_distance(ks_distance, x),
      "ideal", .safe_distribution_p_value(x), "one-sample KS against analytic target CDF"
    ),
    .diagnostic_row(
      "shape", "bhattacharyya_coef", "target_G",
      1, .safe_distribution_distance(bhattacharyya_coef, x),
      "ideal", NA_real_, ""
    ),
    .diagnostic_row(
      "shape", "qq_residuals", "target_G",
      0, .safe_distribution_distance(.qq_residual_max_abs, x),
      "ideal", NA_real_, "max absolute standardized quantile residual"
    ),
    .diagnostic_row(
      "precision", "mean_shrinkage", "overall",
      NA_real_, mean_shrinkage(x),
      "not_available", NA_real_, ""
    ),
    .diagnostic_row(
      "precision", "average_moe", "overall",
      NA_real_, stats::qnorm(0.975) * mean(x$se_j),
      "not_available", NA_real_, "95 percent average margin of error"
    )
  )

  out <- dplyr::bind_rows(table)
  out$delta <- out$realized - out$target
  out$rel_delta <- .relative_delta(out$realized, out$target)
  tibble::as_tibble(out[c(
    "group", "diagnostic", "basis", "target", "realized", "delta", "rel_delta",
    "target_source", "p_value", "note"
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
    informativeness = .threshold_spec(
      "abs_delta",
      "abs(delta) < 0.02", "0.02 <= abs(delta) < 0.05", "abs(delta) >= 0.05",
      "derived"
    ),
    heterogeneity_ratio = .threshold_spec(
      "rel_delta_abs",
      "abs(rel_delta) < 0.10", "0.10 <= abs(rel_delta) < 0.25", "abs(rel_delta) >= 0.25",
      "derived"
    ),
    # The limits of this row and of the copula row are multiples of a standard
    # error that depends on the number of sites. .threshold_spec_for_row()
    # fills in the standard error.
    sigma_tau = .threshold_spec(
      "sampling_se",
      "abs(delta) <= 2 * SE", "2 * SE < abs(delta) <= 3 * SE", "abs(delta) > 3 * SE",
      "sampling-theory"
    ),
    GM_se2 = .threshold_spec(
      "rel_delta_abs",
      "abs(rel_delta) < 0.05", "0.05 <= abs(rel_delta) < 0.15", "abs(rel_delta) >= 0.15",
      "derived"
    ),
    realized_rank_corr = .threshold_spec(
      "abs_delta",
      "abs(delta) < 0.02", "0.02 <= abs(delta) < 0.05", "abs(delta) >= 0.05",
      "simulation-calibrated", "residual target rows only"
    ),
    realized_pearson_corr = .threshold_spec(
      "sampling_se",
      "abs(delta) <= 2 * SE", "2 * SE < abs(delta) <= 3 * SE", "abs(delta) > 3 * SE",
      "sampling-theory", "Gaussian-copula latent scale only"
    ),
    ks_distance = .threshold_spec(
      "p_value",
      "p_value > 0.05", "0.01 < p_value <= 0.05", "p_value <= 0.01",
      "analytic-one-sample"
    ),
    mean_shrinkage = .threshold_spec(
      "realized_range",
      "0.10 <= realized <= 0.95",
      "0.05 <= realized < 0.10 or 0.95 < realized <= 0.99",
      "realized < 0.05 or realized > 0.99",
      "heuristic"
    ),
    average_moe = .threshold_spec(
      "sigma_tau_multiple",
      "realized <= 2 * sigma_tau", "2 * sigma_tau < realized <= 4 * sigma_tau", "realized > 4 * sigma_tau",
      "heuristic"
    )
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
  if (identical(diagnostic, "sigma_tau")) {
    sigma_tau <- row$target[[1L]]
    se <- .sigma_tau_standard_error(sigma_tau, design = design, J = J)
    # The standard deviation is never negative. Once 2 standard errors reach
    # sigma_tau, no realized value is too small to pass, so the row gets no
    # status.
    if (.is_finite_number(se) && 2 * se >= sigma_tau) {
      return(.without_status(spec, "standard error is half of sigma_tau or more; status not assigned"))
    }
    spec <- .with_standard_error(spec, se)
  }
  if (identical(diagnostic, "realized_pearson_corr")) {
    spec <- .with_standard_error(spec, (1 - row$target[[1L]]^2) / sqrt(J - 1))
  }
  spec
}

# Limits of 2 and 3 standard errors. Without a standard error the row gets no
# status. The floor keeps a difference that is zero up to rounding within the
# limits when the standard error is zero, as for a copula correlation of 1.
.with_standard_error <- function(spec, se) {
  if (!.is_finite_number(se) || se < 0) {
    return(.without_status(spec, "no standard error; status not assigned"))
  }
  floor <- sqrt(.Machine$double.eps)
  spec$pass_cutoff <- max(2 * se, floor)
  spec$warn_cutoff <- max(3 * se, floor)
  spec$note <- paste(c(spec$note[nzchar(spec$note)], sprintf("SE = %.3g", se)), collapse = "; ")
  spec
}

.without_status <- function(spec, reason) {
  spec$metric <- "not_applicable"
  spec$note <- paste(c(spec$note[nzchar(spec$note)], reason), collapse = "; ")
  spec
}

# The sample variance of J draws with variance sigma^2 has variance
# sigma^4 * ((kurtosis - 1) / J + 2 / (J * (J - 1))). The standard deviation
# has about half its relative standard error. For a kurtosis of 3 this is
# sigma_tau / sqrt(2 * (J - 1)).
.sigma_tau_standard_error <- function(sigma_tau, design, J) {
  kurtosis <- .z_kurtosis(design)
  if (!.is_finite_number(kurtosis) || !.is_finite_number(sigma_tau) || !is.finite(J) || J < 2L) {
    return(NA_real_)
  }
  sigma_tau * sqrt((kurtosis - 1 + 2 / (J - 1)) / (4 * J))
}

# The fourth moment of the standardized site effects under the design. It is
# NA for a user-supplied distribution, whose moments are unknown, and for a
# Student t distribution with 4 or fewer degrees of freedom, which has none.
.z_kurtosis <- function(design) {
  if (is.null(design) || is.null(design$true_dist)) {
    return(NA_real_)
  }
  theta <- design$theta_G
  kurtosis <- switch(design$true_dist,
    Gaussian = 3,
    StudentT = if (isTRUE(theta$nu > 4)) 3 + 6 / (theta$nu - 4) else NA_real_,
    SkewN = {
      delta_sq <- theta$slant^2 / (1 + theta$slant^2)
      3 + 2 * (pi - 3) * (2 * delta_sq / pi)^2 / (1 - 2 * delta_sq / pi)^2
    },
    ALD = 3 + 6 * (theta$rho^4 + (1 - theta$rho)^4) / (theta$rho^2 + (1 - theta$rho)^2)^2,
    Mixture = .mixture_kurtosis(theta$delta, theta$eps, theta$ups),
    PointMassSlab = {
      # The generator takes the slab shape in any letter case.
      slab_shape <- .theta_value(theta, "slab_shape", "Gaussian")
      slab_kurtosis <- if (is.character(slab_shape) && identical(tolower(slab_shape), "laplace")) 6 else 3
      slab_kurtosis / (1 - theta$pi0)
    },
    NA_real_
  )
  if (length(kurtosis) == 1L && is.numeric(kurtosis)) kurtosis else NA_real_
}

# Components N(0, 1) with probability 1 - eps and N(delta, ups^2) with
# probability eps: the fourth central moment over the squared variance.
.mixture_kurtosis <- function(delta, eps, ups) {
  center <- eps * delta
  d1 <- -center
  d2 <- delta - center
  variance <- (1 - eps) * (1 + d1^2) + eps * (ups^2 + d2^2)
  fourth <- (1 - eps) * (d1^4 + 6 * d1^2 + 3) + eps * (d2^4 + 6 * d2^2 * ups^2 + 3 * ups^4)
  fourth / variance^2
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

  if (identical(metric, "not_applicable")) {
    return(NA_character_)
  }
  if (identical(metric, "realized_range") && !is.finite(realized)) {
    return(NA_character_)
  }
  if (identical(metric, "sampling_se") && (!is.finite(target) || !is.finite(realized))) {
    return(NA_character_)
  }
  if (metric %in% c("abs_delta", "rel_delta_abs") && (!is.finite(target) || !is.finite(realized))) {
    return(NA_character_)
  }

  switch(metric,
    abs_delta = .status_by_abs_delta(abs(delta), row$diagnostic[[1L]]),
    rank_corr_tolerance = .status_by_cutoffs(abs(delta), spec$pass_cutoff, spec$warn_cutoff),
    sampling_se = .status_by_cutoffs(abs(delta), spec$pass_cutoff, spec$warn_cutoff),
    sampling_drift = .status_by_cutoffs(abs(realized), spec$pass_cutoff, spec$warn_cutoff),
    rel_delta_abs = .status_by_rel_delta(abs(rel_delta), row$diagnostic[[1L]]),
    p_value = .status_ks(p_value),
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
    "pass"
  } else if (value <= warn_cutoff) {
    "warn"
  } else {
    "fail"
  }
}

.status_by_abs_delta <- function(abs_delta, diagnostic) {
  if (!is.finite(abs_delta)) {
    return(NA_character_)
  }
  cutoffs <- switch(diagnostic,
    informativeness = c(pass = 0.02, fail = 0.05),
    realized_rank_corr = c(pass = 0.02, fail = 0.05),
    c(pass = NA_real_, fail = NA_real_)
  )
  if (abs_delta < cutoffs[["pass"]]) {
    "pass"
  } else if (abs_delta < cutoffs[["fail"]]) {
    "warn"
  } else {
    "fail"
  }
}

.status_by_rel_delta <- function(abs_rel_delta, diagnostic) {
  if (!is.finite(abs_rel_delta)) {
    return(NA_character_)
  }
  cutoffs <- switch(diagnostic,
    heterogeneity_ratio = c(pass = 0.10, fail = 0.25),
    GM_se2 = c(pass = 0.05, fail = 0.15),
    c(pass = NA_real_, fail = NA_real_)
  )
  if (abs_rel_delta < cutoffs[["pass"]]) {
    "pass"
  } else if (abs_rel_delta < cutoffs[["fail"]]) {
    "warn"
  } else {
    "fail"
  }
}

.status_ks <- function(p_value) {
  if (!is.finite(p_value)) {
    return(NA_character_)
  }
  if (p_value > 0.05) {
    "pass"
  } else if (p_value > 0.01) {
    "warn"
  } else {
    "fail"
  }
}

.status_mean_shrinkage <- function(realized) {
  if (realized >= 0.10 && realized <= 0.95) {
    "pass"
  } else if ((realized >= 0.05 && realized < 0.10) || (realized > 0.95 && realized <= 0.99)) {
    "warn"
  } else {
    "fail"
  }
}

.status_average_moe <- function(realized, sigma_tau) {
  if (!is.finite(realized) || !is.finite(sigma_tau) || sigma_tau < 0) {
    return(NA_character_)
  }
  if (realized <= 2 * sigma_tau) {
    "pass"
  } else if (realized <= 4 * sigma_tau) {
    "warn"
  } else {
    "fail"
  }
}

.diagnostic_row <- function(group, diagnostic, basis, target, realized, target_source, p_value, note) {
  tibble::tibble(
    group = group,
    diagnostic = diagnostic,
    basis = basis,
    target = as.numeric(target),
    realized = as.numeric(realized),
    target_source = target_source,
    p_value = as.numeric(p_value),
    note = note
  )
}

.validate_diagnostic_data <- function(x) {
  if (!is_multisitedgp_data(x)) {
    .abort_not_simulated_data(x)
  }
  missing <- setdiff(.data_columns(), names(x))
  if (length(missing) > 0L) {
    .abort(
      "`x` must have the columns that `sim_multisite()` and `sim_meta()` return.",
      sprintf("Missing columns: %s.", paste(missing, collapse = ", "))
    )
  }
  invisible(x)
}

.diagnostic_sigma_tau <- function(x) {
  design <- attr(x, "design", exact = TRUE)
  if (is.null(design) || is.null(design$sigma_tau)) {
    .abort("`x` has no `design` attribute to take `sigma_tau` from.")
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
    return(list(
      value = NA_real_,
      source = "not_available",
      note = "realized Spearman is descriptive for Gaussian-copula mode"
    ))
  }
  if (is.list(diagnostics) && !is.null(diagnostics$target_rank_corr)) {
    return(list(value = diagnostics$target_rank_corr, source = "diagnostics", note = "diagnostics"))
  }
  list(value = NA_real_, source = "not_available", note = "no rank-correlation target")
}

.target_pearson_corr <- function(design, diagnostics) {
  if (!is.null(design) && is.list(design$dependence_spec)) {
    if (identical(design$dependence_spec$method, "copula")) {
      return(list(
        value = design$dependence_spec$pearson_corr,
        source = "design",
        note = "latent Gaussian copula target"
      ))
    }
    return(list(
      value = NA_real_,
      source = "not_available",
      note = "no latent Pearson target for this dependence mode"
    ))
  }
  if (is.list(diagnostics) && !is.null(diagnostics$target_pearson_corr)) {
    return(list(value = diagnostics$target_pearson_corr, source = "diagnostics", note = "diagnostics"))
  }
  list(value = NA_real_, source = "not_available", note = "no Pearson target for this dependence mode")
}

.achieved_latent_pearson <- function(diagnostics) {
  if (!is.list(diagnostics) || !identical(diagnostics$dependence_method, "copula")) {
    return(NA_real_)
  }
  dependence <- diagnostics$dependence_diagnostics
  if (!is.list(dependence) || is.null(dependence$achieved_latent_pearson)) {
    return(NA_real_)
  }
  value <- dependence$achieved_latent_pearson
  if (!is.numeric(value) || length(value) != 1L || !is.finite(value)) {
    return(NA_real_)
  }
  unname(value)
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
  test <- tryCatch(
    {
      .validate_diagnostic_data(x)
      stats::ks.test(x$z_j, .reference_z_cdf(x))
    },
    error = function(e) NULL
  )
  if (is.null(test)) {
    return(NA_real_)
  }
  unname(suppressWarnings(test$p.value))
}

.reference_z_cdf <- function(x) {
  design <- attr(x, "design", exact = TRUE)
  if (is.null(design) || is.null(design$true_dist)) {
    .abort_no_reference(design)
  }
  switch(design$true_dist,
    Gaussian = stats::pnorm,
    StudentT = {
      nu <- design$theta_G$nu
      scale <- sqrt((nu - 2) / nu)
      function(q) stats::pt(q / scale, df = nu)
    },
    .abort_no_reference(design)
  )
}

.resolve_distribution_pair <- function(x, y = NULL) {
  if (is_multisitedgp_data(x) && is.null(y)) {
    .validate_diagnostic_data(x)
    return(list(x = .validate_numeric_vector(x$z_j, "x$z_j", min_length = 2L), y = .reference_z_quantiles(x)))
  }
  if (is_multisitedgp_data(x) && !is.null(y)) {
    x <- x$z_j
  }
  x <- .validate_numeric_vector(x, "x", min_length = 2L)
  if (is.null(y)) {
    .abort("`y` is required when `x` is a numeric vector.")
  }
  list(x = x, y = .validate_numeric_vector(y, "y", min_length = 2L))
}

.auto_reference_shapes <- function() {
  c("Gaussian", "StudentT")
}

# Lets scenario_audit() ask, before it computes anything, whether the design's
# distribution has a reference distribution for the shape diagnostics.
.has_auto_reference <- function(design) {
  !is.null(design) && !is.null(design$true_dist) &&
    design$true_dist %in% .auto_reference_shapes()
}

# For ks_distance() and bhattacharyya_coef() called without `y`. The
# distribution to compare with then comes from the design.
.abort_no_reference <- function(design) {
  if (is.null(design) || is.null(design$true_dist)) {
    .abort(
      "`x` has no `design` attribute, so there is no distribution to compare `z_j` with.",
      fix = "Pass a comparison sample as `y`."
    )
  }
  .abort(
    "Without `y`, the comparison is available only for `true_dist = \"Gaussian\"` and `\"StudentT\"`.",
    sprintf("The design has `true_dist = \"%s\"`.", design$true_dist),
    "Pass a comparison sample as `y`."
  )
}

.reference_z_quantiles <- function(x) {
  design <- attr(x, "design", exact = TRUE)
  if (is.null(design) || is.null(design$true_dist)) {
    .abort_no_reference(design)
  }
  p <- stats::ppoints(nrow(x))
  switch(design$true_dist,
    Gaussian = stats::qnorm(p),
    StudentT = {
      nu <- design$theta_G$nu
      stats::qt(p, df = nu) * sqrt((nu - 2) / nu)
    },
    .abort_no_reference(design)
  )
}

.qq_residual_max_abs <- function(x) {
  pair <- .resolve_distribution_pair(x, NULL)
  max(abs(sort(pair$x) - sort(pair$y)))
}

.validate_bins <- function(bins) {
  bins <- .validate_scalar_integer(bins, "bins")
  if (bins < 2L) {
    .abort("`bins` must be at least 2.", .you_passed(bins))
  }
  bins
}
