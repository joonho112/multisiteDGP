# nolint start: object_usage_linter
#' Simulate a direct-precision meta-analysis data-generating process
#'
#' @encoding UTF-8
#'
#' @description
#' `sim_meta()` is the unified interface for the four-layer multisite-trial
#' data-generating process under direct precision-scale targets. Given a
#' meta-analytic design — site count \eqn{J}, latent-effect distribution,
#' mean informativeness
#' \eqn{I = \sigma_\tau^2 / (\sigma_\tau^2 + \mathrm{GM}(\widehat{se}_j^2))},
#' and heterogeneity ratio \eqn{R = \max \widehat{se}^2 / \min \widehat{se}^2}
#' — it composes the latent-effects, direct-precision-margin,
#' dependence-alignment, and observation layers in one call and returns a
#' `multisitedgp_meta` tibble with `diagnostics` and `provenance`
#' attributes; the canonical hash is stored at
#' `attr(x, "provenance")$canonical_hash` (not as a top-level attribute).
#' \code{\link{sim_multisite}} is the sister *site-size-driven*
#' (Paradigm A) interface; `sim_meta()` covers the *direct-precision*
#' (Paradigm B) front door, where precision targets are supplied directly
#' rather than derived from per-site sample sizes.
#'
#' @details
#' The simulation runs four generative layers in order:
#' \describe{
#'   \item{\strong{Layer 1 — latent effects} (\code{\link{gen_effects}})}{Draws standardized site effects \eqn{z_j} from one of eight built-in \eqn{G} distributions and rescales to \eqn{\tau_j = \tau + \sigma_\tau\,z_j}.}
#'   \item{\strong{Layer 2 — direct precision margin} (\code{\link{gen_se_direct}})}{Builds the per-site sampling variance \eqn{\widehat{se}_j^2} as a deterministic grid that exactly hits the user-specified \eqn{(I, R)} targets. The site-size column \eqn{n_j} is `NA_integer_` because no site-size margin is involved.}
#'   \item{\strong{Layer 3 — precision dependence} (\code{\link{align_rank_corr}}, \code{\link{align_copula_corr}}, \code{\link{align_hybrid_corr}})}{Optionally aligns \eqn{\widehat{se}_j^2} against \eqn{\tau_j} to a target Spearman correlation, preserving both marginals exactly.}
#'   \item{\strong{Layer 4 — observation draws} (\code{\link{gen_observations}})}{Draws \eqn{\widehat{\tau}_j \sim \mathcal{N}(\tau_j,\, \widehat{se}_j^2)}.}
#' }
#'
#' The direct-precision path is the formal counterpart of the site-size-driven
#' path covered by \code{\link{sim_multisite}}: where the site-size path
#' derives precision from sample sizes \eqn{n_j}, the direct-precision path
#' supplies the precision targets and the package generates a deterministic
#' SE grid to match exactly. Set `shuffle = TRUE` to randomize the SE-to-site
#' assignment within the wrapped seed; the multiset of \eqn{\widehat{se}_j^2}
#' values is preserved, so the `(I, R)` targets remain exact under
#' permutation but the assignment to sites changes. Supply a custom `se_fn`
#' to plug in a non-grid SE distribution; when `se_fn` is non-`NULL` the
#' deterministic-grid invariant no longer applies and the diagnostic
#' `I_error` / `R_error` slots become `NA_real_`.
#'
#' Site-size arguments (`nj_mean`, `cv`, `nj_min`, `engine`, `n_per_site`)
#' are rejected with a coherence error pointing the caller to
#' \code{\link{sim_multisite}}; they belong to Paradigm A, not the
#' direct-precision path.
#'
#' For a workflow walkthrough see the
#' \href{../articles/a7-case-study-meta-analysis.html}{meta-analysis case
#' study} vignette. For the formal contract on direct-precision SE
#' generation, see \href{../articles/m3-margin-se-models.html}{Margin and SE
#' models — site-size and direct-precision paths}.
#'
#' @section RNG policy:
#' If `seed` is `NULL`, the pipeline runs under the caller's active RNG state.
#' If `seed` is a single integer, the full pipeline is wrapped in
#' \code{\link[withr]{with_seed}}, so the caller's global RNG state is
#' restored on exit. The direct SE grid is deterministic except for
#' `shuffle = TRUE` with `R > 1`, which uses the active sample RNG inside
#' the same wrapper seed.
#'
#' @param design Optional `multisitedgp_design` with `paradigm = "direct"`.
#'   If `NULL`, `...` is forwarded to \code{\link{multisitedgp_design}} with
#'   `paradigm = "direct"` automatically locked. Construct a design once
#'   when reusing across multiple `sim_meta()` calls or a
#'   \code{\link{design_grid}} sweep.
#' @param ... Flat direct-precision design arguments used only when
#'   `design = NULL`. All must be named (positional `...` is rejected). The
#'   key direct-path arguments are `J` (number of studies), `I` (mean
#'   informativeness target, \eqn{0 < I < 1}), `R` (heterogeneity ratio
#'   target, \eqn{R \ge 1}), `tau` (grand mean), `sigma_tau` (between-study
#'   SD), `true_dist` (one of `"Gaussian"`, `"StudentT"`, `"SkewN"`, `"ALD"`,
#'   `"Mixture"`, `"PointMassSlab"`, `"User"`, `"DPM"`), `shuffle`,
#'   `dependence`, `se_fn`, `se_args`. See \code{\link{multisitedgp_design}}
#'   for the full list. Passing `paradigm` is an error (the wrapper locks
#'   it). Site-size arguments (`nj_mean`, `cv`, `engine`, `n_per_site`)
#'   trigger a coherence error pointing to \code{\link{sim_multisite}}.
#' @param seed Optional integer seed override. When supplied, replaces
#'   `design$seed` and gives bit-identical reruns. Use a small integer (e.g.
#'   `1L`) for examples; use a 9-digit integer in production for cross-run
#'   uniqueness.
#'
#' @return A `multisitedgp_meta` tibble (which inherits from
#' `multisitedgp_data`) with one row per study and columns:
#' \describe{
#'   \item{`site_index`}{Integer study identifier \eqn{j = 1, \ldots, J}.}
#'   \item{`z_j`}{Standardized residual effect (mean 0, variance 1).}
#'   \item{`tau_j`}{Latent study-level effect on the response scale, \eqn{\tau + \sigma_\tau\,z_j}.}
#'   \item{`tau_j_hat`}{Observed study-level estimate \eqn{\widehat{\tau}_j}.}
#'   \item{`se_j`, `se2_j`}{Study-level SE and sampling variance \eqn{\widehat{se}_j^2} from the direct grid (or `se_fn` output).}
#'   \item{`n_j`}{Always `NA_integer_` — the direct-precision path has no site-size margin.}
#' }
#' Plus the following attributes:
#' \describe{
#'   \item{`design`}{The locked `multisitedgp_design` with `paradigm = "direct"`.}
#'   \item{`diagnostics`}{Group A / B / C / D diagnostics — `I_hat`, `R_hat`, realized Spearman / Pearson correlations, plus the meta-specific extras `target_I`, `target_R`, `I_error`, `R_error` (zero under the deterministic grid; `NA_real_` under custom `se_fn`), `I_exact`, `R_exact` (logical exactness flags), `shuffle`, `direct_se_method` (`"grid"` or `"custom"`), `direct_se_diagnostics`, `margin_engine`. See \code{\link{compute_I}}.}
#'   \item{`provenance`}{Package version, R version, platform, resolved seed, `canonical_hash`, `design_hash`, and the call expression.}
#'   \item{`multisitedgp_version`, `paradigm`}{Convenience copies for quick attribute lookup.}
#' }
#'
#' @family family-wrappers
#' @seealso
#'   \code{\link{sim_multisite}} for the site-size-driven (Paradigm A)
#'   sister wrapper;
#'   \code{\link{multisitedgp_design}} for explicit design construction and
#'   validation;
#'   \code{\link{preset_meta_modest}} for a defensible meta-analysis
#'   starting design;
#'   \code{\link{gen_se_direct}} for the underlying Layer 2 direct-precision
#'   generator;
#'   \code{\link{design_grid}} for scenario-grid sweeps;
#'   the \href{../articles/a7-case-study-meta-analysis.html}{meta-analysis
#'   case study} vignette.
#'
#' @references
#' Lee, J., Che, J., Rabe-Hesketh, S., Feller, A., & Miratrix, L. (2025).
#' Improving the estimation of site-specific effects and their distribution
#' in multisite trials. \emph{Journal of Educational and Behavioral Statistics},
#' \bold{50}(5), 731--764. \doi{10.3102/10769986241254286}.
#'
#' @examples
#' # Minimal: a defensible meta-analysis preset, one call, read realized I.
#' dat <- sim_meta(design = preset_meta_modest(), seed = 1L)
#' attr(dat, "diagnostics")$I_hat
#'
#' # Explicit direct-precision design via flat (J, I, R) targets.
#' dat <- sim_meta(J = 12L, I = 0.30, R = 2, sigma_tau = 0.20, seed = 1L)
#' summary(dat)
#'
#' # The grid generator is exact: I_error and R_error are zero.
#' diag <- attr(dat, "diagnostics")
#' diag$I_error  # 0
#' diag$R_error  # 0
#'
#' \dontrun{
#'   # Hand off to a meta-analytic estimator (requires {metafor}).
#'   # `as_metafor()` renames the canonical columns to metafor's (yi, vi, sei).
#'   metafor_obj <- as_metafor(dat)
#'   metafor::rma(yi = yi, vi = vi, data = metafor_obj)
#' }
#' @export
sim_meta <- function(design = NULL, ..., seed = NULL) {
  dots <- list(...)
  design <- .resolve_sim_meta_design(design = design, dots = dots, seed = seed)

  run_layer_pipeline <- function() {
    out <- gen_effects(
      J = design$J,
      true_dist = design$true_dist,
      tau = design$tau,
      sigma_tau = design$sigma_tau,
      variance = design$variance,
      theta_G = design$theta_G,
      formula = design$formula,
      beta = design$beta,
      data = design$data,
      g_fn = design$g_fn,
      g_returns = design$g_returns
    )
    out <- .apply_direct_precision(out, design = design)
    out <- .apply_design_dependence(out, design = design)
    gen_observations(out, obs_fn = design$obs_spec$obs_fn)
  }

  out <- if (is.null(design$seed)) {
    run_layer_pipeline()
  } else {
    withr::with_seed(design$seed, run_layer_pipeline())
  }

  diagnostics <- .initial_meta_diagnostics(out, design = design)
  out <- .new_multisitedgp_meta(.order_sim_columns(out, design = design), design = design, diagnostics = diagnostics)
  out <- .attach_core_diagnostics_table(out)
  .finalize_provenance_hash(out)
}

.resolve_sim_meta_design <- function(design, dots, seed = NULL) {
  if (is.null(design)) {
    .preflight_sim_meta_dots(dots)
    design <- do.call(multisitedgp_design, c(list(paradigm = "direct"), dots))
  } else {
    .validate_empty_wrapper_dots(dots, caller = "sim_meta")
    if (!is_multisitedgp_design(design)) {
      .abort_arg(
        "`design` must be a multisitedgp_design object.",
        sprintf("Got object with class: %s.", paste(class(design), collapse = "/")),
        "Use `multisitedgp_design(paradigm = \"direct\", I = ...)` or pass flat arguments directly to `sim_meta()`."
      )
    }
    validate_multisitedgp_design(design)
    if (!identical(design$paradigm, "direct")) {
      .abort_coherence(
        "`sim_meta()` requires `paradigm = \"direct\"`.",
        sprintf("Got `design$paradigm = \"%s\"`.", design$paradigm),
        "Use `sim_multisite()` for site-size designs."
      )
    }
  }

  if (!is.null(seed)) {
    design <- update_multisitedgp_design(design, seed = seed)
  }
  design
}

.preflight_sim_meta_dots <- function(dots) {
  if (length(dots) == 0L) {
    return(invisible(TRUE))
  }
  supplied <- names(dots)
  if (is.null(supplied)) {
    supplied <- rep("", length(dots))
  }
  supplied[is.na(supplied)] <- ""
  supplied[!nzchar(supplied)] <- "<unnamed>"

  if ("<unnamed>" %in% supplied) {
    .abort_arg(
      "`sim_meta()` requires named arguments.",
      "The wrapper forwards flat names to `multisitedgp_design(paradigm = \"direct\")`.",
      "Pass arguments such as `I = 0.5`, `R = 3`, and `J = 50L`."
    )
  }

  if ("set_seed" %in% supplied) {
    .abort_arg(
      "`set_seed` is not supported by `sim_meta()`.",
      "`sim_meta()` uses a single optional `seed` argument for the full pipeline.",
      "Use `seed = <integer>` or remove `set_seed`."
    )
  }

  wrong_door <- intersect(
    supplied,
    c("nj_mean", "cv", "nj_min", "p", "R2", "var_outcome", "engine", "n_j", "n_per_site")
  )
  if (length(wrong_door) > 0L) {
    .abort_coherence(
      "`sim_meta()` received site-size argument(s).",
      sprintf("Wrong-door argument(s): %s.", paste(wrong_door, collapse = ", ")),
      "Use `sim_multisite()` for site-size designs or remove those arguments."
    )
  }

  if ("paradigm" %in% supplied) {
    .abort_coherence(
      "`sim_meta()` does not accept `paradigm`.",
      "The wrapper is the fixed front door for `paradigm = \"direct\"` designs.",
      "Use `sim_multisite()` for site-size designs, or remove `paradigm`."
    )
  }

  invisible(TRUE)
}

.apply_direct_precision <- function(upstream, design) {
  gen_se_direct(
    upstream,
    J = design$J,
    I = design$I,
    R = design$R,
    shuffle = design$shuffle,
    sigma_tau = design$sigma_tau,
    se_fn = design$se_fn,
    se_args = design$se_args
  )
}

.initial_meta_diagnostics <- function(x, design) {
  diagnostics <- .initial_sim_diagnostics(x, design = design)
  direct_se_diagnostics <- attr(x, "direct_se_diagnostics", exact = TRUE)
  margin_engine <- attr(x, "engine", exact = TRUE)
  if (!is.null(margin_engine)) {
    diagnostics$engine <- margin_engine
    diagnostics$margin_engine <- margin_engine
  }
  diagnostics$target_I <- design$I
  diagnostics$target_R <- design$R
  diagnostics$shuffle <- design$shuffle
  diagnostics$direct_se_method <- if (is.null(design$se_fn)) "grid" else "custom"
  diagnostics$I_error <- if (identical(diagnostics$direct_se_method, "grid")) {
    unname(diagnostics$I_hat - design$I)
  } else {
    NA_real_
  }
  diagnostics$R_error <- if (identical(diagnostics$direct_se_method, "grid")) {
    unname(diagnostics$R_hat - design$R)
  } else {
    NA_real_
  }
  diagnostics$I_exact <- if (identical(diagnostics$direct_se_method, "grid")) {
    is.finite(diagnostics$I_error) && abs(diagnostics$I_error) <= .Machine$double.eps^0.5
  } else {
    NA
  }
  diagnostics$R_exact <- if (identical(diagnostics$direct_se_method, "grid")) {
    is.finite(diagnostics$R_error) && abs(diagnostics$R_error) <= .Machine$double.eps^0.5
  } else {
    NA
  }
  diagnostics$direct_se_diagnostics <- direct_se_diagnostics
  diagnostics
}
# nolint end
