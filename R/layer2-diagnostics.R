# nolint start: object_name_linter, object_usage_linter
#' Compute the Neyman sampling-variance constant kappa
#'
#' @encoding UTF-8
#'
#' @description
#' Compute the Neyman precision constant
#' \eqn{\kappa = (1/p + 1/(1 - p))(1 - R^2)\,\mathrm{Var}(Y)} that maps
#' per-site sample size to per-site sampling variance through
#' \eqn{\widehat{se}_j^2 = \kappa / n_j}. Use this to inspect or sweep
#' \eqn{\kappa} for a planned design — `compute_kappa()` is also called
#' internally by \code{\link{gen_site_sizes}} when constructing the
#' Layer 2 SE column.
#'
#' @details
#' The defaults (`p = 0.5`, `R2 = 0`, `var_outcome = 1`) give
#' \eqn{\kappa = 4}, matching the JEBS reference design. Increasing
#' `R2` shrinks \eqn{\kappa} (covariates buy precision); imbalanced `p`
#' inflates it.
#'
#' @param p Numeric in `(0, 1)`. Treatment-assignment proportion. Default
#'   `0.5` (balanced).
#' @param R2 Numeric in `[0, 1)`. Covariate-explained variance share at the
#'   site level. Default `0`.
#' @param var_outcome Numeric (> 0). Outcome variance. Default `1`.
#'
#' @return A numeric vector. Inputs are recycled by base R rules — pass
#'   scalars for a scalar `\kappa`, or matched vectors to sweep a grid.
#'
#' @family family-diagnostics
#' @seealso
#'   \code{\link{gen_site_sizes}} for the Layer 2 generator that consumes
#'   `\kappa`;
#'   \code{\link{compute_I}} for the realized informativeness diagnostic.
#'
#' @references
#' Lee, J., Che, J., Rabe-Hesketh, S., Feller, A., & Miratrix, L. (2025).
#' Improving the estimation of site-specific effects and their distribution
#' in multisite trials. \emph{Journal of Educational and Behavioral Statistics},
#' \bold{50}(5), 731--764. \doi{10.3102/10769986241254286}.
#'
#' @examples
#' # JEBS baseline: balanced assignment, no covariates, unit outcome variance.
#' compute_kappa()  # 4
#'
#' # Covariates buy precision: kappa shrinks with R^2.
#' compute_kappa(p = 0.5, R2 = 0.2, var_outcome = 1)  # 3.2
#'
#' # Imbalanced assignment inflates kappa.
#' compute_kappa(p = 0.3, R2 = 0, var_outcome = 1)  # ~ 4.76
#' @export
compute_kappa <- function(p = 0.5, R2 = 0, var_outcome = 1) {
  p <- .validate_probability_open_vector(p, "p")
  R2 <- .validate_r2_vector(R2, "R2")
  var_outcome <- .validate_positive_numeric_vector(var_outcome, "var_outcome")
  .validate_recyclable_lengths(list(p = p, R2 = R2, var_outcome = var_outcome))

  (1 / p + 1 / (1 - p)) * (1 - R2) * var_outcome
}

#' Compute realized informativeness
#'
#' @encoding UTF-8
#'
#' @description
#' Compute the realized mean informativeness
#' \eqn{\widehat{I} = \sigma_\tau^2 / (\sigma_\tau^2 + \mathrm{GM}(\widehat{se}_j^2))},
#' where `GM` is the geometric mean of the per-site sampling variances.
#' Higher \eqn{\widehat{I}} indicates more precise per-site estimates
#' relative to the between-site heterogeneity scale; a feasibility index
#' near 1 means each site's estimate alone identifies the latent effect.
#'
#' @details
#' This is the realized counterpart to the design-target \eqn{I} fed to
#' \code{\link{sim_meta}} or used in \code{\link{gen_se_direct}}. Under
#' the deterministic-grid direct-precision path, the realized and target
#' \eqn{I} match exactly; under the site-size-driven path or under a
#' custom `se_fn`, the realized value is what you read off the simulation.
#'
#' \code{tau_j} is accepted for data-method signature compatibility (so
#' the same call shape works for `multisitedgp_data` objects and bare
#' numeric vectors) but does not enter the statistic.
#'
#' @param se2_j Numeric vector (length \eqn{\ge 2}). Per-site sampling
#'   variances \eqn{\widehat{se}_j^2}. Must be strictly positive.
#' @param sigma_tau Numeric (\eqn{\ge 0}). Between-site SD. `0` returns
#'   `0` (no heterogeneity → no informativeness to recover).
#' @param tau_j Optional numeric vector matching `se2_j` length. Kept for
#'   signature compatibility with the S3 method; not used in the
#'   computation.
#'
#' @return A scalar double in `[0, 1)`.
#'
#' @family family-diagnostics
#' @seealso
#'   \code{\link{informativeness}} for the S3 generic with methods for
#'   `multisitedgp_data` and numeric `se2_j` inputs;
#'   \code{\link{compute_kappa}} for the precision constant input to the
#'   site-size-driven path;
#'   \code{\link{heterogeneity_ratio}} for the realized \eqn{R}.
#'
#' @references
#' Lee, J., Che, J., Rabe-Hesketh, S., Feller, A., & Miratrix, L. (2025).
#' Improving the estimation of site-specific effects and their distribution
#' in multisite trials. \emph{Journal of Educational and Behavioral Statistics},
#' \bold{50}(5), 731--764. \doi{10.3102/10769986241254286}.
#'
#' @examples
#' se2 <- c(0.05, 0.08, 0.10)
#' compute_I(se2, sigma_tau = 0.20)
#' @export
compute_I <- function(se2_j, sigma_tau, tau_j = NULL) {
  se2_j <- .validate_se2_vector(se2_j, min_length = 2L)
  sigma_tau <- .validate_sigma_tau_diagnostic(sigma_tau)
  if (!is.null(tau_j)) {
    tau_j <- .validate_numeric_vector(tau_j, "tau_j", min_length = length(se2_j))
    if (length(tau_j) != length(se2_j)) {
      .abort_arg(
        "`tau_j` must have the same length as `se2_j` when supplied.",
        "`tau_j` is an optional diagnostic cross-check vector.",
        "Pass matching site-level vectors or omit `tau_j`."
      )
    }
  }

  if (identical(sigma_tau, 0)) {
    return(0)
  }
  gm_se2 <- exp(mean(log(se2_j)))
  sigma_tau^2 / (sigma_tau^2 + gm_se2)
}

#' Compute per-site empirical-Bayes shrinkage proportions
#'
#' @encoding UTF-8
#'
#' @description
#' Compute the classical empirical-Bayes shrinkage weight per site,
#' \eqn{S_j = \sigma_\tau^2 / (\sigma_\tau^2 + \widehat{se}_j^2)} —
#' the fraction by which a partial-pooling estimator pulls each site's
#' estimate toward the grand mean. **Group D diagnostic** (downstream
#' shrinkage — Dr. Chen's question 4: "How much does empirical Bayes
#' pool toward the mean?").
#'
#' @details
#' \eqn{S_j} ranges over `[0, 1)`. \eqn{S_j \to 1} means "the prior
#' dominates" — site \eqn{j} has so much sampling noise that
#' partial-pooling collapses its estimate toward the overall mean.
#' \eqn{S_j \to 0} means "the data dominate" — the site identifies its
#' own latent effect cleanly. Sites with intermediate \eqn{S_j} are
#' where empirical-Bayes pooling does real work.
#' \eqn{\sigma_\tau = 0} returns zero for every site (no heterogeneity
#' to recover).
#'
#' \strong{Reading guide.} For diagnostic plots — funnel- or
#' caterpillar-style traces of shrinkage against site precision — pass
#' `monotone = TRUE` so the returned vector is sorted ascending and
#' renders as a smooth curve. For computing means or downstream
#' summaries, leave `monotone = FALSE` so each \eqn{S_j} aligns with
#' its site index.
#'
#' For the four-question diagnostic walkthrough see the
#' \href{../articles/a3-diagnostics-in-practice.html}{A3 \enc{·}{.}
#' Diagnostics in practice} vignette.
#'
#' @param se2_j Numeric vector (length \eqn{\ge 1}, strictly positive).
#'   Per-site sampling variances \eqn{\widehat{se}_j^2}.
#' @param sigma_tau Numeric scalar (\eqn{\ge 0}). Between-site SD on
#'   the response scale. `sigma_tau = 0` returns zeros (no
#'   heterogeneity to shrink toward).
#' @param monotone Logical scalar. If `TRUE`, return `sort(shrinkage)`
#'   for plotting purposes. Default `FALSE` preserves site-index
#'   alignment.
#'
#' @return A numeric vector of length `length(se2_j)`, each entry in
#'   `[0, 1)`. Sorted ascending when `monotone = TRUE`.
#'
#' @family family-diagnostics
#' @seealso
#'   \code{\link{compute_I}} for the geometric-mean aggregate
#'   \eqn{\widehat{I}} that uses the same shrinkage kernel;
#'   \code{\link{mean_shrinkage}} for the across-site mean of \eqn{S_j};
#'   \code{\link{feasibility_index}} for \eqn{\sum_j S_j} (Efron) or
#'   \eqn{\sum_j (1 - S_j)} (Morris);
#'   \code{\link{scenario_audit}} for the quality gates that consume
#'   these diagnostics;
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
#' # Two sites, the second twice as noisy as the first.
#' compute_shrinkage(c(0.05, 0.10), sigma_tau = 0.20)
#' # First site keeps more of its own information; second is shrunk harder.
#'
#' # Sorted view — useful for a per-site shrinkage curve plot.
#' compute_shrinkage(c(0.10, 0.04, 0.07), sigma_tau = 0.20, monotone = TRUE)
#'
#' # No heterogeneity collapses every site to zero shrinkage.
#' compute_shrinkage(c(0.05, 0.10), sigma_tau = 0)
#' @export
compute_shrinkage <- function(se2_j, sigma_tau, monotone = FALSE) {
  se2_j <- .validate_se2_vector(se2_j, min_length = 1L)
  sigma_tau <- .validate_sigma_tau_diagnostic(sigma_tau)
  monotone <- .validate_scalar_logical(monotone, "monotone")

  shrinkage <- sigma_tau^2 / (sigma_tau^2 + se2_j)
  if (isTRUE(monotone)) {
    return(sort(shrinkage))
  }
  shrinkage
}

#' Compute realized informativeness from data or a sampling-variance vector
#'
#' @encoding UTF-8
#'
#' @description
#' Compute the realized informativeness scalar
#' \eqn{\widehat{I} = \sigma_\tau^2 / (\sigma_\tau^2 + \mathrm{GM}(\widehat{se}_j^2))},
#' where \eqn{\mathrm{GM}} is the geometric mean across sites.
#' **Group A diagnostic** (precision and feasibility — Dr. Chen's
#' question 1: "Is the design precise enough at the site level for the
#' between-site signal to be recoverable?"). `informativeness()` is the
#' user-facing S3 wrapper around \code{\link{compute_I}}.
#'
#' @details
#' \eqn{\widehat{I}} reads as the shrinkage at a representative
#' (geometric-mean-precision) site: \eqn{\widehat{I} \to 1} means
#' site-level estimates dominate the prior; \eqn{\widehat{I} \to 0}
#' means the prior dominates and site-specific identification is weak.
#' Under the direct-precision path with a deterministic
#' \eqn{\widehat{se}_j^2} grid, \eqn{\widehat{I}} matches the design
#' target exactly; under the site-size-driven path or any custom
#' `se_fn`, the realized value is what the simulation produces.
#'
#' \strong{Method dispatch.} The `multisitedgp_data` method reads
#' `sigma_tau` from `attr(x, "design")$sigma_tau` and forwards `x$se2_j`
#' (and `x$tau_j`, kept for signature compatibility) to
#' \code{\link{compute_I}}. The `numeric` method requires an explicit
#' `sigma_tau` argument and treats `x` as the \eqn{\widehat{se}_j^2}
#' vector. The default method errors with a hint pointing to either
#' entry.
#'
#' For the full Group A/B/C/D diagnostic workflow see the
#' \href{../articles/a3-diagnostics-in-practice.html}{A3 \enc{·}{.}
#' Diagnostics in practice} vignette.
#'
#' @param x A `multisitedgp_data` object (data method) or a positive
#'   numeric `se2_j` vector (numeric method).
#' @param ... Method-specific named arguments. The numeric method
#'   requires `sigma_tau` (numeric scalar \eqn{\ge 0}, between-site SD
#'   on the response scale; the diagnostic is scale-aware and cannot
#'   be computed from sampling variances alone). The data method reads
#'   `sigma_tau` from the attached design and ignores any explicit
#'   value here. The numeric method also accepts `tau_j` for signature
#'   compatibility; it is not used in the computation. All other
#'   `...` are forwarded to \code{\link{compute_I}}.
#'
#' @return A scalar double in `[0, 1)`. Returns `0` exactly when
#'   `sigma_tau = 0`.
#'
#' @family family-diagnostics
#' @seealso
#'   \code{\link{compute_I}} for the underlying scalar formula;
#'   \code{\link{compute_shrinkage}} for the per-site \eqn{S_j} the
#'   geometric mean aggregates;
#'   \code{\link{compute_kappa}} for the precision constant on the
#'   site-size-driven path;
#'   \code{\link{heterogeneity_ratio}} for the Group A companion
#'   diagnostic \eqn{R};
#'   \code{\link{feasibility_index}} for the additive feasibility
#'   variants;
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
#' # Data method: sigma_tau is read from the attached design.
#' dat <- sim_multisite(J = 10L, seed = 1L)
#' informativeness(dat)
#'
#' # Numeric method: sigma_tau must be supplied explicitly.
#' informativeness(dat$se2_j, sigma_tau = 0.20)
#'
#' # Compare realized I against the design target.
#' attr(dat, "design")$sigma_tau
#' @export
informativeness <- function(x, ...) {
  UseMethod("informativeness")
}

#' @export
informativeness.multisitedgp_data <- function(x, ...) {
  design <- attr(x, "design", exact = TRUE)
  if (is.null(design) || is.null(design$sigma_tau)) {
    .abort_arg(
      "`informativeness()` requires `sigma_tau` for `multisitedgp_data` objects.",
      "The data method reads `sigma_tau` from `attr(x, \"design\")`.",
      "Pass a valid design or call `informativeness(x$se2_j, sigma_tau = ...)`."
    )
  }
  compute_I(x$se2_j, design$sigma_tau, tau_j = x$tau_j)
}

#' @export
informativeness.numeric <- function(x, ..., sigma_tau) {
  if (missing(sigma_tau)) {
    .abort_arg(
      "`sigma_tau` is required when `x` is a numeric `se2_j` vector.",
      "Informativeness is scale-aware and cannot be computed from SEs alone.",
      "Pass `sigma_tau`, for example `informativeness(se2_j, sigma_tau = 0.20)`."
    )
  }
  compute_I(x, sigma_tau = sigma_tau, ...)
}

#' @export
informativeness.default <- function(x, ...) {
  .abort_arg(
    "`informativeness()` requires a `multisitedgp_data` object or numeric `se2_j` vector.",
    sprintf("Received object with class: %s.", paste(class(x), collapse = "/")),
    "Pass a simulation data object or call `informativeness(se2_j, sigma_tau = ...)`."
  )
}

#' Compute mean empirical-Bayes shrinkage
#'
#' @encoding UTF-8
#'
#' @description
#' Compute the across-site mean of the empirical-Bayes shrinkage,
#' \eqn{\bar{S} = J^{-1} \sum_{j=1}^{J} \sigma_\tau^2 / (\sigma_\tau^2 + \widehat{se}_j^2)},
#' from a simulated dataset, a bare \eqn{\widehat{se}_j^2} vector, or a
#' closed-form approximation that uses only design parameters.
#' **Group D diagnostic** (downstream shrinkage — Dr. Chen's question 4
#' on average pooling toward the overall mean).
#'
#' @details
#' \eqn{\bar{S}} is the natural single-number summary of how much
#' partial-pooling work a hierarchical model will do across the design.
#' Values near 0 indicate the prior dominates almost everywhere; values
#' near 1 indicate near-no pooling. The canonical quality gate is
#' `mean_shrinkage_min = 0.30` (see \code{\link{default_thresholds}}).
#'
#' \strong{Method dispatch.} `mean_shrinkage()` resolves to one of
#' three paths:
#'
#' \itemize{
#'   \item \strong{Data method} (`multisitedgp_data` input): reads
#'         `sigma_tau` from `attr(x, "design")$sigma_tau` and returns
#'         `mean(compute_shrinkage(x$se2_j, sigma_tau))`.
#'   \item \strong{Numeric method} (numeric `se2_j` vector): requires
#'         `sigma_tau` explicitly and returns
#'         `mean(compute_shrinkage(x, sigma_tau))`.
#'   \item \strong{Closed-form path} (`x = NULL`, named arguments
#'         only): approximates
#'         \eqn{\widehat{se}^2 \approx \kappa(p, R^2, \mathrm{Var}(Y)) / \bar{n}}
#'         and returns
#'         \eqn{\sigma_\tau^2 / (\sigma_\tau^2 + \widehat{se}^2)}.
#'         Use this at the design stage to plan \eqn{\bar{S}} before
#'         any simulation has been drawn — it answers "at this
#'         heterogeneity scale and average site size, what shrinkage
#'         should I expect?"
#' }
#'
#' The closed-form path vectorizes over its arguments via R recycling,
#' so passing a vector `nj_mean` produces a vector \eqn{\bar{S}}. The
#' two empirical paths return scalars.
#'
#' For the design-stage planning workflow see the
#' \href{../articles/a3-diagnostics-in-practice.html}{A3 \enc{·}{.}
#' Diagnostics in practice} vignette.
#'
#' @param x Optional. A `multisitedgp_data` object (data method), a
#'   positive numeric `se2_j` vector (numeric method), or `NULL` to
#'   take the closed-form path. Default `NULL`.
#' @param ... Method-specific named arguments. The numeric method and
#'   the closed-form path require `sigma_tau` (numeric \eqn{\ge 0},
#'   between-site SD; auto-read for the data method). The closed-form
#'   path also accepts `nj_mean` (numeric \eqn{>} 0, mean per-site
#'   sample size used in \eqn{\widehat{se}^2 = \kappa / \bar{n}});
#'   `varY` (numeric \eqn{>} 0, outcome variance fed into
#'   \code{\link{compute_kappa}}; default `1`); `p` (numeric in
#'   `(0, 1)`, treatment-assignment proportion; default `0.5`); and
#'   `R2` (numeric in `[0, 1)`, site-level covariate-explained
#'   variance share; default `0`). All other `...` are forwarded to
#'   the chosen method.
#'
#' @return A scalar double in `[0, 1)` for the data and numeric
#'   methods; a numeric vector (recycled over the closed-form
#'   arguments) for the closed-form path.
#'
#' @family family-diagnostics
#' @seealso
#'   \code{\link{compute_shrinkage}} for the per-site \eqn{S_j} kernel;
#'   \code{\link{compute_kappa}} for the closed-form precision constant
#'   \eqn{\kappa};
#'   \code{\link{informativeness}} and \code{\link{compute_I}} for the
#'   Group A counterpart;
#'   \code{\link{feasibility_index}} for the additive Efron / Morris
#'   forms;
#'   \code{\link{default_thresholds}} for the `mean_shrinkage_min` gate;
#'   \code{\link{scenario_audit}} for the audit pipeline;
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
#' # Data method: sigma_tau is read from the attached design.
#' dat <- sim_multisite(J = 10L, seed = 1L)
#' mean_shrinkage(dat)
#'
#' # Numeric method: explicit sigma_tau on a bare se2_j vector.
#' mean_shrinkage(dat$se2_j, sigma_tau = 0.20)
#'
#' # Closed-form (design-stage) path: no simulated data needed.
#' mean_shrinkage(nj_mean = 50, sigma_tau = 0.20)
#'
#' # Closed-form sweep: how does mean shrinkage vary with average site size?
#' mean_shrinkage(nj_mean = c(25, 50, 100, 200), sigma_tau = 0.20)
#' @export
mean_shrinkage <- function(x = NULL, ...) {
  if (missing(x) || is.null(x)) {
    return(mean_shrinkage.default(x = NULL, ...))
  }
  UseMethod("mean_shrinkage")
}

#' @export
mean_shrinkage.multisitedgp_data <- function(x, ...) {
  design <- attr(x, "design", exact = TRUE)
  if (is.null(design) || is.null(design$sigma_tau)) {
    .abort_arg(
      "`mean_shrinkage()` requires `sigma_tau` for `multisitedgp_data` objects.",
      "The data method reads `sigma_tau` from `attr(x, \"design\")`.",
      "Pass a valid design or call `mean_shrinkage(x$se2_j, sigma_tau = ...)`."
    )
  }
  mean(compute_shrinkage(x$se2_j, sigma_tau = design$sigma_tau))
}

#' @export
mean_shrinkage.numeric <- function(x, ..., sigma_tau) {
  if (missing(sigma_tau)) {
    .abort_arg(
      "`sigma_tau` is required when `x` is a numeric `se2_j` vector.",
      "Mean shrinkage is scale-aware and cannot be computed from SEs alone.",
      "Pass `sigma_tau`, for example `mean_shrinkage(se2_j, sigma_tau = 0.20)`."
    )
  }
  mean(compute_shrinkage(x, sigma_tau = sigma_tau))
}

#' @export
mean_shrinkage.default <- function(x = NULL, ..., nj_mean, sigma_tau, varY = 1, p = 0.5, R2 = 0) {
  if (!is.null(x)) {
    .abort_arg(
      "`mean_shrinkage()` received an unsupported `x`.",
      sprintf("Received object with class: %s.", paste(class(x), collapse = "/")),
      "Pass a `multisitedgp_data` object, numeric `se2_j` vector, or use named closed-form arguments."
    )
  }
  if (missing(nj_mean) || missing(sigma_tau)) {
    .abort_arg(
      "Closed-form `mean_shrinkage()` requires `nj_mean` and `sigma_tau`.",
      "The closed-form path approximates `se2 = kappa / nj_mean`.",
      "Call `mean_shrinkage(nj_mean = 50, sigma_tau = 0.20)`."
    )
  }

  nj_mean <- .validate_positive_numeric_vector(nj_mean, "nj_mean")
  sigma_tau <- .validate_nonnegative_numeric_vector(sigma_tau, "sigma_tau")
  p <- .validate_probability_open_vector(p, "p")
  R2 <- .validate_r2_vector(R2, "R2")
  varY <- .validate_positive_numeric_vector(varY, "varY")
  .validate_recyclable_lengths(list(nj_mean = nj_mean, sigma_tau = sigma_tau, p = p, R2 = R2, varY = varY))

  se2 <- compute_kappa(p = p, R2 = R2, var_outcome = varY) / nj_mean
  sigma_tau^2 / (sigma_tau^2 + se2)
}

#' Compute the additive feasibility index (Efron or Morris)
#'
#' @encoding UTF-8
#'
#' @description
#' Compute one of two additive feasibility statistics built on the
#' per-site shrinkage
#' \eqn{S_j = \sigma_\tau^2 / (\sigma_\tau^2 + \widehat{se}_j^2)}:
#' the **Efron** form \eqn{\sum_{j=1}^{J} S_j} (total effective
#' information; default) or the **Morris** form
#' \eqn{\sum_{j=1}^{J} (1 - S_j)} (total residual variation).
#' **Group A diagnostic** (top-of-funnel feasibility — Dr. Chen's
#' question 1: "Is there enough effective site-level information for
#' partial pooling to do real work?"). Run this *before* committing a
#' long simulation to disk.
#'
#' @details
#' \strong{Two conventions.} `kind = "efron"` returns \eqn{\sum_j S_j},
#' interpretable as "the equivalent number of perfectly-informative
#' sites": a design with \eqn{J = 50} and \eqn{\sum_j S_j = 8} has the
#' effective information of about eight noise-free sites.
#' `kind = "morris"` returns \eqn{\sum_j (1 - S_j)}, the
#' residual-variation form used in Morris-style empirical-Bayes
#' diagnostics. The two are linked by
#' \eqn{\sum_j S_j + \sum_j (1 - S_j) = J}.
#'
#' \strong{Reading guide.} The canonical quality gate is the Efron form
#' \eqn{\sum_j S_j \ge 5} (see
#' `default_thresholds()$feasibility_min = 5.0`). Designs that fall
#' below this threshold are unlikely to support partial-pooling
#' estimators well — heterogeneity cannot be reliably recovered from
#' the available site-level information. With `warn = TRUE` (default),
#' the function emits a warning when the Efron statistic crosses into
#' the FAIL band.
#'
#' \strong{Method dispatch.} When `se2_j` is a `multisitedgp_data`
#' object, `sigma_tau` is auto-read from
#' `attr(se2_j, "design")$sigma_tau` if not supplied; the function then
#' operates on `se2_j$se2_j`. When `se2_j` is a bare numeric vector,
#' `sigma_tau` is required.
#'
#' For how feasibility interacts with the other Group A/B/C/D gates see
#' the \href{../articles/a3-diagnostics-in-practice.html}{A3 \enc{·}{.}
#' Diagnostics in practice} vignette.
#'
#' @param se2_j A positive numeric \eqn{\widehat{se}_j^2} vector, or a
#'   `multisitedgp_data` object whose `se2_j` column will be used.
#' @param sigma_tau Numeric scalar (\eqn{\ge 0}). Between-site SD.
#'   Required when `se2_j` is numeric; optional when `se2_j` is a data
#'   object (auto-read from the attached design when `NULL`). Default
#'   `NULL`.
#' @param kind Character. Feasibility convention. `"efron"` (default)
#'   returns \eqn{\sum_j S_j}; `"morris"` returns
#'   \eqn{\sum_j (1 - S_j)}.
#' @param warn Logical. If `TRUE` (default), emit a warning when the
#'   Efron statistic lands in the FAIL band defined by
#'   `default_thresholds()$feasibility_min`. Ignored for
#'   `kind = "morris"`.
#'
#' @return A scalar double. For `"efron"`, in `[0, J]`. For `"morris"`,
#'   in `[0, J]` complementary.
#'
#' @family family-diagnostics
#' @seealso
#'   \code{\link{compute_shrinkage}} for the per-site \eqn{S_j} the
#'   index sums;
#'   \code{\link{informativeness}} for the geometric-mean Group A
#'   companion;
#'   \code{\link{mean_shrinkage}} for the Group D mean of \eqn{S_j};
#'   \code{\link{default_thresholds}} for the `feasibility_min` gate;
#'   \code{\link{scenario_audit}} for the audit pipeline;
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
#' dat <- sim_multisite(J = 10L, seed = 1L)
#'
#' # Efron form (default): sum of S_j; equivalent number of perfectly
#' # informative sites.
#' feasibility_index(dat, warn = FALSE)
#'
#' # Morris form: sum of (1 - S_j); total residual variation.
#' feasibility_index(dat, kind = "morris", warn = FALSE)
#'
#' # The two conventions sum to J.
#' feasibility_index(dat, kind = "efron", warn = FALSE) +
#'   feasibility_index(dat, kind = "morris", warn = FALSE)
#'
#' # Numeric path: pass se2_j and sigma_tau directly.
#' feasibility_index(dat$se2_j, sigma_tau = 0.20, warn = FALSE)
#' @export
feasibility_index <- function(se2_j, sigma_tau = NULL, kind = c("efron", "morris"), warn = TRUE) {
  kind <- .match_choice(kind, "kind", c("efron", "morris"))
  warn <- .validate_scalar_logical(warn, "warn")
  if (is_multisitedgp_data(se2_j)) {
    design <- attr(se2_j, "design", exact = TRUE)
    if (is.null(sigma_tau)) {
      if (is.null(design) || is.null(design$sigma_tau)) {
        .abort_arg(
          "`feasibility_index()` requires `sigma_tau` for data objects without a design.",
          "The data path reads `sigma_tau` from `attr(x, \"design\")`.",
          "Pass `sigma_tau` explicitly or use an unmodified multisiteDGP simulation object."
        )
      }
      sigma_tau <- design$sigma_tau
    }
    se2_j <- se2_j$se2_j
  } else if (is.null(sigma_tau)) {
    .abort_arg(
      "`sigma_tau` is required when `se2_j` is numeric.",
      "Feasibility is scale-aware and cannot be computed from sampling variances alone.",
      "Pass `sigma_tau`, for example `feasibility_index(se2_j, sigma_tau = 0.20)`."
    )
  }

  S_j <- compute_shrinkage(se2_j, sigma_tau = sigma_tau)
  out <- switch(kind,
    efron = sum(S_j),
    morris = sum(1 - S_j)
  )
  if (isTRUE(warn) && identical(kind, "efron") && .feasibility_status(out) == "FAIL") {
    .warn_feasibility_fail(out)
  }
  out
}

#' Default thresholds for diagnostic-grid audits
#'
#' @encoding UTF-8
#'
#' @description
#' Return the canonical quality-gate thresholds consumed by
#' \code{\link{scenario_audit}} when scoring a design grid against
#' Dr. Chen's four-question diagnostic rubric. Each gate maps a Group
#' A/B/C/D diagnostic to a PASS / WARN / FAIL band, so the audit
#' output is a single calibrated verdict rather than a raw diagnostic
#' table.
#'
#' @details
#' \strong{Reading guide.} The five named entries cover three
#' diagnostic groups:
#'
#' \itemize{
#'   \item \strong{Group A — precision and feasibility.}
#'         `feasibility_min = 5.0` for the Efron
#'         \code{\link{feasibility_index}}; `R_max = 30.0` for the
#'         realized \code{\link{heterogeneity_ratio}}.
#'   \item \strong{Group C — distribution recovery.}
#'         `bhattacharyya_min = 0.85` for
#'         \code{\link{bhattacharyya_coef}}; `ks_max = 0.10` for
#'         \code{\link{ks_distance}}.
#'   \item \strong{Group D — downstream shrinkage.}
#'         `mean_shrinkage_min = 0.30` for
#'         \code{\link{mean_shrinkage}}.
#' }
#'
#' Group B diagnostics (realized rank correlation) are tracked
#' qualitatively in \code{\link{scenario_audit}} rather than via a
#' fixed numerical threshold and so do not appear here. Override the
#' defaults by editing the returned list and passing it to
#' `scenario_audit()`; the returned audit object always records which
#' thresholds were used so the verdict is reproducible.
#'
#' For the calibration rationale behind each numeric value see the
#' \href{../articles/a3-diagnostics-in-practice.html}{A3 \enc{·}{.}
#' Diagnostics in practice} vignette.
#'
#' @return A named list of five scalar threshold gates:
#'   \describe{
#'     \item{`mean_shrinkage_min`}{Numeric `0.30`. Minimum
#'           \eqn{\bar{S}} for Group D PASS — designs below this floor
#'           do too little partial-pooling work to benefit from a
#'           hierarchical estimator.}
#'     \item{`feasibility_min`}{Numeric `5.0`. Minimum Efron
#'           feasibility \eqn{\sum_j S_j} for Group A PASS —
#'           corresponds to the equivalent of about five
#'           perfectly-informative sites.}
#'     \item{`R_max`}{Numeric `30.0`. Maximum realized heterogeneity
#'           ratio \eqn{R} for Group A PASS — a ceiling on the
#'           ill-conditioning of the precision-to-heterogeneity
#'           scale.}
#'     \item{`bhattacharyya_min`}{Numeric `0.85`. Minimum Bhattacharyya
#'           coefficient between target and realized \eqn{G} for
#'           Group C PASS — high-overlap shape recovery.}
#'     \item{`ks_max`}{Numeric `0.10`. Maximum Kolmogorov-Smirnov
#'           distance between target and realized \eqn{G} for
#'           Group C PASS — tail-sensitive complement to the
#'           Bhattacharyya gate.}
#'   }
#'
#' @family family-diagnostics
#' @seealso
#'   \code{\link{scenario_audit}} for the audit consumer;
#'   \code{\link{mean_shrinkage}} for the Group D diagnostic;
#'   \code{\link{feasibility_index}} for the Group A feasibility
#'   diagnostic;
#'   \code{\link{heterogeneity_ratio}} for the Group A \eqn{R}
#'   diagnostic;
#'   \code{\link{bhattacharyya_coef}} and \code{\link{ks_distance}}
#'   for the Group C distribution-recovery diagnostics;
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
#' # Inspect the canonical gates.
#' default_thresholds()
#'
#' # Tighten the Group D gate before passing to the audit.
#' th <- default_thresholds()
#' th$mean_shrinkage_min <- 0.40
#' th
#' @export
default_thresholds <- function() {
  list(
    mean_shrinkage_min = 0.30,
    feasibility_min = 5.0,
    R_max = 30.0,
    bhattacharyya_min = 0.85,
    ks_max = 0.10
  )
}

.validate_se2_vector <- function(se2_j, min_length = 1L) {
  .validate_positive_numeric_vector(se2_j, "se2_j", min_length = min_length)
}

.validate_numeric_vector <- function(x, arg, min_length = 1L) {
  if (!is.numeric(x) || length(x) < min_length || anyNA(x) || any(!is.finite(x))) {
    .abort_arg(
      sprintf("`%s` must be a finite numeric vector.", arg),
      sprintf("`%s` has length %s; minimum required length is %s.", arg, length(x), min_length),
      sprintf("Pass finite numeric values for `%s`.", arg)
    )
  }
  as.numeric(x)
}

.validate_positive_numeric_vector <- function(x, arg, min_length = 1L) {
  x <- .validate_numeric_vector(x, arg, min_length = min_length)
  if (any(x <= 0)) {
    .abort_arg(
      sprintf("`%s` must be positive.", arg),
      sprintf("`%s` is used in a variance or rate denominator.", arg),
      sprintf("Pass `%s` values greater than zero.", arg)
    )
  }
  x
}

.validate_nonnegative_numeric_vector <- function(x, arg, min_length = 1L) {
  x <- .validate_numeric_vector(x, arg, min_length = min_length)
  if (any(x < 0)) {
    .abort_arg(
      sprintf("`%s` must be nonnegative.", arg),
      sprintf("`%s = 0` is allowed for diagnostic zero-information edge cases.", arg),
      sprintf("Pass `%s` values greater than or equal to zero.", arg)
    )
  }
  x
}

.validate_sigma_tau_diagnostic <- function(sigma_tau) {
  sigma_tau <- .validate_nonnegative_numeric_vector(sigma_tau, "sigma_tau")
  if (length(sigma_tau) != 1L) {
    .abort_arg(
      "`sigma_tau` must be a scalar for this diagnostic.",
      "Realized diagnostics pair one between-site scale with the supplied site-level vector.",
      "Pass one `sigma_tau` value."
    )
  }
  sigma_tau
}

.validate_probability_open_vector <- function(x, arg) {
  x <- .validate_numeric_vector(x, arg)
  if (any(x <= 0 | x >= 1)) {
    .abort_arg(
      sprintf("`%s` must be strictly between 0 and 1.", arg),
      sprintf("`%s` appears in the treatment-allocation denominator.", arg),
      sprintf("Pass `%s = 0.5` for balanced assignment or another interior probability.", arg)
    )
  }
  x
}

.validate_r2_vector <- function(x, arg) {
  x <- .validate_numeric_vector(x, arg)
  if (any(x < 0 | x >= 1)) {
    .abort_arg(
      sprintf("`%s` must be in [0, 1).", arg),
      "`R2 = 1` makes the sampling-variance scale degenerate.",
      sprintf("Pass `%s = 0` for no covariate adjustment or a value less than 1.", arg)
    )
  }
  x
}

.validate_recyclable_lengths <- function(x) {
  lengths <- vapply(x, length, integer(1))
  max_length <- max(lengths)
  bad <- lengths != 1L & lengths != max_length
  if (any(bad)) {
    .abort_arg(
      "Vectorized diagnostic arguments have incompatible lengths.",
      sprintf("Lengths: %s.", paste(sprintf("%s=%s", names(lengths), lengths), collapse = ", ")),
      "Use length-one scalars or vectors with the same length."
    )
  }
  invisible(TRUE)
}
# nolint end
