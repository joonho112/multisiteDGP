# nolint start: object_usage_linter
#' Simulate a multisite trial data-generating process
#'
#' @encoding UTF-8
#'
#' @description
#' `sim_multisite()` is the unified interface for the four-layer
#' multisite-trial data-generating process. Given a multisite design — site
#' count, per-site sizes, latent-effect distribution, and optional precision
#' dependence — it composes the latent-effects, site-size-margin,
#' dependence-alignment, and observation layers in one call and returns a
#' `multisitedgp_data` tibble with diagnostics, provenance, and a canonical
#' hash. This is the *site-size-driven* path (Paradigm A in the blueprint),
#' in which sampling variances are induced from a site-size margin
#' \eqn{n_j}; the sister \code{\link{sim_meta}} covers the
#' *direct-precision* path (Paradigm B), in which \eqn{\widehat{se}_j^2} is
#' specified directly.
#'
#' @details
#' The simulation runs four generative layers in order:
#' \describe{
#'   \item{\strong{Layer 1 — latent effects} (\code{\link{gen_effects}})}{Draws standardized site effects \eqn{z_j} from one of eight built-in \eqn{G} distributions and rescales to \eqn{\tau_j = \tau + \sigma_\tau\,z_j}.}
#'   \item{\strong{Layer 2 — site-level precision} (\code{\link{gen_site_sizes}})}{Builds the per-site sampling variance \eqn{\widehat{se}_j^2 = \kappa / n_j} from generated site sizes \eqn{n_j}.}
#'   \item{\strong{Layer 3 — precision dependence} (\code{\link{align_rank_corr}}, \code{\link{align_copula_corr}}, \code{\link{align_hybrid_corr}})}{Optionally aligns \eqn{\widehat{se}_j^2} against \eqn{\tau_j} to a target Spearman correlation, preserving both marginals exactly.}
#'   \item{\strong{Layer 4 — observation draws} (\code{\link{gen_observations}})}{Draws the observed estimate \eqn{\widehat{\tau}_j \sim \mathcal{N}(\tau_j,\, \widehat{se}_j^2)}.}
#' }
#'
#' The `multisitedgp_design` is validated and frozen at entry, then attached
#' to the returned `multisitedgp_data` alongside the `diagnostics` and
#' `provenance` attributes. The canonical hash is stored at
#' `attr(x, "provenance")$canonical_hash` (not as a top-level attribute) and
#' is the cross-machine reproducibility identifier — two machines producing
#' the same hash will have generated bit-identical site-level tibbles.
#'
#' For a workflow walkthrough see the
#' \href{../articles/a1-getting-started.html}{Getting started} vignette. For
#' the formal two-stage DGP specification, see
#' \href{../articles/m1-statistical-dgp.html}{The two-stage DGP — formal specification}.
#'
#' @section RNG policy:
#' If `seed` is `NULL`, the pipeline runs under the caller's active RNG state
#' and consumes the ordinary Layer 1/2/3/4 draws. No seed is manufactured. If
#' `seed` is a single integer, the full pipeline is wrapped in
#' \code{\link[withr]{with_seed}}, so the caller's global RNG state is
#' restored on exit. The resolved seed is recorded in the `provenance`
#' attribute.
#'
#' @param design Optional `multisitedgp_design`. If `NULL`, `...` is forwarded
#'   to \code{\link{multisitedgp_design}} with `paradigm = "site_size"`.
#'   Construct a design once with \code{\link{multisitedgp_design}} or a
#'   \code{\link{presets}} when reusing across multiple calls or a
#'   \code{\link{design_grid}} sweep.
#' @param ... Flat design arguments used only when `design = NULL`. See
#'   \code{\link{multisitedgp_design}} for the full parameter list. Note that
#'   `paradigm` cannot be passed here — the wrapper locks
#'   `paradigm = "site_size"`; use \code{\link{sim_meta}} for direct-precision
#'   designs.
#' @param seed Optional integer seed override. When supplied, replaces
#'   `design$seed` and gives bit-identical reruns. Use a small integer (e.g.
#'   `1L`) for examples; use a 9-digit integer in production for cross-run
#'   uniqueness.
#'
#' @return A `multisitedgp_data` tibble with one row per site and columns:
#' \describe{
#'   \item{`site_index`}{Integer site identifier \eqn{j = 1, \ldots, J} — preserved through the pipeline (Layer 3 permutes the `(se_j, se2_j, n_j)` triple, never `site_index`).}
#'   \item{`z_j`}{Standardized residual effect (mean 0, variance 1).}
#'   \item{`tau_j`}{Latent site-level effect on the response scale, \eqn{\tau + \sigma_\tau\,z_j}.}
#'   \item{`tau_j_hat`}{Observed site-level estimate \eqn{\widehat{\tau}_j}.}
#'   \item{`se_j`, `se2_j`}{Site-level SE and sampling variance \eqn{\widehat{se}_j^2 = \kappa / n_j}.}
#'   \item{`n_j`}{Site size from the Layer 2 margin.}
#' }
#' Plus the following attributes:
#' \describe{
#'   \item{`design`}{The locked `multisitedgp_design` object.}
#'   \item{`diagnostics`}{Group A / B / C / D diagnostics — `I_hat`, `R_hat`, realized Spearman and Pearson correlations (residual and marginal), `sigma_tau` realized vs. target, dependence and observation diagnostics; see \code{\link{compute_I}} and \code{\link{informativeness}}.}
#'   \item{`provenance`}{Package version, R version, platform, resolved seed, `canonical_hash`, `design_hash`, and the call expression.}
#'   \item{`multisitedgp_version`, `paradigm`}{Convenience copies for quick attribute lookup.}
#' }
#'
#' @family family-wrappers
#' @seealso
#'   \code{\link{sim_meta}} for the direct-precision (Paradigm B) sister
#'   wrapper that takes precision targets in place of a site-size margin;
#'   \code{\link{multisitedgp_design}} for explicit design construction and
#'   validation;
#'   the \code{\link{presets}} family for defensible starting designs;
#'   \code{\link{design_grid}} for scenario-grid sweeps;
#'   \code{\link{gen_effects}}, \code{\link{gen_site_sizes}},
#'   \code{\link{align_hybrid_corr}}, \code{\link{gen_observations}} for the
#'   four layers exposed individually;
#'   the \href{../articles/a1-getting-started.html}{Getting started} vignette.
#'
#' @references
#' Lee, J., Che, J., Rabe-Hesketh, S., Feller, A., & Miratrix, L. (2025).
#' Improving the estimation of site-specific effects and their distribution
#' in multisite trials. \emph{Journal of Educational and Behavioral Statistics},
#' \bold{50}(5), 731--764. \doi{10.3102/10769986241254286}.
#'
#' @examples
#' # Minimal usage: a defensible preset, one call, read realized informativeness.
#' dat <- sim_multisite(preset_education_modest(), seed = 1L)
#' attr(dat, "diagnostics")$I_hat
#'
#' # Full diagnostic report — realized vs. intended on every dimension.
#' summary(dat)
#'
#' # Provenance travels with the object for reproducibility audits.
#' attr(dat, "provenance")$canonical_hash
#'
#' \dontrun{
#'   # Hand off to a meta-analytic estimator (requires {metafor}).
#'   # `as_metafor()` renames the canonical columns to metafor's (yi, vi, sei).
#'   metafor_obj <- as_metafor(dat)
#'   metafor::rma(yi = yi, vi = vi, data = metafor_obj)
#' }
#' @export
sim_multisite <- function(design = NULL, ..., seed = NULL) {
  dots <- list(...)
  design <- .resolve_sim_multisite_design(design = design, dots = dots, seed = seed)

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
    out <- gen_site_sizes(
      out,
      J = design$J,
      nj_mean = design$nj_mean,
      cv = design$cv,
      nj_min = design$nj_min,
      p = design$p,
      R2 = design$R2,
      var_outcome = design$var_outcome,
      engine = design$engine
    )
    out <- .apply_design_dependence(out, design = design)
    gen_observations(out, obs_fn = design$obs_spec$obs_fn)
  }

  out <- if (is.null(design$seed)) {
    run_layer_pipeline()
  } else {
    withr::with_seed(design$seed, run_layer_pipeline())
  }

  diagnostics <- .initial_sim_diagnostics(out, design = design)
  out <- .new_multisitedgp_data(.order_sim_columns(out, design = design), design = design, diagnostics = diagnostics)
  out <- .attach_core_diagnostics_table(out)
  .finalize_provenance_hash(out)
}

.resolve_sim_multisite_design <- function(design, dots, seed = NULL) {
  if (is.null(design)) {
    .preflight_sim_multisite_dots(dots)
    design <- do.call(multisitedgp_design, c(list(paradigm = "site_size"), dots))
  } else {
    .validate_empty_wrapper_dots(dots, caller = "sim_multisite")
    if (!is_multisitedgp_design(design)) {
      .abort_arg(
        "`design` must be a multisitedgp_design object.",
        sprintf("Got object with class: %s.", paste(class(design), collapse = "/")),
        "Use `multisitedgp_design()` or pass flat arguments directly to `sim_multisite()`."
      )
    }
    validate_multisitedgp_design(design)
    if (!identical(design$paradigm, "site_size")) {
      .abort_coherence(
        "`sim_multisite()` requires `paradigm = \"site_size\"`.",
        sprintf("Got `design$paradigm = \"%s\"`.", design$paradigm),
        "Use `sim_meta()` for direct `(I, R)` meta-analysis designs."
      )
    }
  }

  if (!is.null(seed)) {
    design <- update_multisitedgp_design(design, seed = seed)
  }
  design
}

.preflight_sim_multisite_dots <- function(dots) {
  if (length(dots) == 0L) {
    return(invisible(TRUE))
  }
  supplied <- names(dots)
  if (is.null(supplied)) {
    supplied <- rep("", length(dots))
  }
  supplied[is.na(supplied)] <- ""
  supplied[!nzchar(supplied)] <- "<unnamed>"

  if ("set_seed" %in% supplied) {
    .abort_arg(
      "`set_seed` is not supported by `sim_multisite()`.",
      "`sim_multisite()` uses a single optional `seed` argument for the full pipeline.",
      "Use `seed = <integer>` or remove `set_seed`."
    )
  }

  wrong_door <- intersect(supplied, c("I", "R", "shuffle", "se_fn", "se_args"))
  if (length(wrong_door) > 0L) {
    .abort_coherence(
      "`sim_multisite()` received direct meta-analysis argument(s).",
      sprintf("Wrong-door argument(s): %s.", paste(wrong_door, collapse = ", ")),
      "Use `sim_meta()` for `(I, R)` or direct-SE designs."
    )
  }

  if ("paradigm" %in% supplied) {
    .abort_coherence(
      "`sim_multisite()` does not accept `paradigm`.",
      "The wrapper is the fixed front door for `paradigm = \"site_size\"` designs.",
      "Use `sim_meta()` for direct designs, or remove `paradigm`."
    )
  }

  invisible(TRUE)
}

.validate_empty_wrapper_dots <- function(dots, caller) {
  if (length(dots) > 0L) {
    supplied <- names(dots)
    supplied[!nzchar(supplied)] <- "<unnamed>"
    .abort_arg(
      sprintf("`...` must be empty when `design` is supplied to `%s()`.", caller),
      sprintf("Received extra argument(s): %s.", paste(supplied, collapse = ", ")),
      "Use `update_multisitedgp_design()` first or call the wrapper with flat arguments only."
    )
  }
  invisible(TRUE)
}

.apply_design_dependence <- function(upstream, design) {
  spec <- design$dependence_spec
  switch(spec$method,
    none = upstream,
    rank = align_rank_corr(
      upstream,
      rank_corr = spec$rank_corr,
      max_iter = spec$max_iter,
      tol = spec$tol,
      dependence_fn = spec$dependence_fn
    ),
    copula = align_copula_corr(
      upstream,
      pearson_corr = spec$pearson_corr,
      dependence_fn = spec$dependence_fn
    ),
    hybrid = align_hybrid_corr(
      upstream,
      rank_corr = spec$rank_corr,
      init = spec$hybrid_init,
      polish = spec$hybrid_polish,
      max_iter = spec$max_iter,
      tol = spec$tol,
      dependence_fn = spec$dependence_fn
    )
  )
}

.order_sim_columns <- function(x, design = NULL) {
  x <- tibble::as_tibble(x)
  if (!is.null(design) && is.null(design$formula) && "tau_j" %in% names(x)) {
    # Decision B/JEBS fixtures keep z_j as the canonical residual effect.
    x$z_j <- (x$tau_j - design$tau) / design$sigma_tau
  }
  leading <- .canonical_data_columns()
  leading <- leading[leading %in% names(x)]
  trailing <- setdiff(names(x), c(leading, "latent_component"))
  x[c(leading, trailing)]
}

.initial_sim_diagnostics <- function(x, design) {
  dependence_diagnostics <- attr(x, "dependence_diagnostics", exact = TRUE)
  observation_diagnostics <- attr(x, "observation_diagnostics", exact = TRUE)
  perm <- attr(x, "permutation_perm", exact = TRUE)

  rho_s_residual <- .safe_cor(x$z_j, x$se2_j, method = "spearman")
  rho_s_marginal <- .safe_cor(x$tau_j, x$se2_j, method = "spearman")
  rho_p_residual <- .safe_cor(x$z_j, x$se2_j, method = "pearson")
  rho_p_marginal <- .safe_cor(x$tau_j, x$se2_j, method = "pearson")

  list(
    J = nrow(x),
    paradigm = design$paradigm,
    true_dist = design$true_dist,
    engine = design$engine,
    framing = design$framing,
    seed = design$seed,
    I_hat = compute_I(x$se2_j, sigma_tau = design$sigma_tau, tau_j = x$tau_j),
    R_hat = max(x$se2_j) / min(x$se2_j),
    GM_se2 = exp(mean(log(x$se2_j))),
    rho_S_residual = unname(rho_s_residual),
    rho_S_marginal = unname(rho_s_marginal),
    rho_P_residual = unname(rho_p_residual),
    rho_P_marginal = unname(rho_p_marginal),
    target_sigma_tau_resid = design$sigma_tau,
    sigma_tau_resid = if (nrow(x) > 1L) stats::sd(x$z_j) * design$sigma_tau else NA_real_,
    sigma_tau_marg = if (nrow(x) > 1L) stats::sd(x$tau_j) else NA_real_,
    dependence_method = design$dependence_spec$method,
    target_rank_corr = design$dependence_spec$rank_corr,
    target_pearson_corr = design$dependence_spec$pearson_corr,
    dependence_diagnostics = dependence_diagnostics,
    observation_diagnostics = observation_diagnostics,
    permutation = .diagnostic_permutation(perm, design = design)
  )
}

.finalize_provenance_hash <- function(x) {
  provenance <- attr(x, "provenance", exact = TRUE)
  if (is.list(provenance)) {
    provenance$canonical_hash <- canonical_hash(x)
    attr(x, "provenance") <- provenance
  }
  x
}

.diagnostic_permutation <- function(perm, design) {
  if (identical(design$dependence_spec$method, "none")) {
    return("identity")
  }
  if (isTRUE(design$record_permutation)) {
    return(perm)
  }
  "not_recorded"
}

.safe_cor <- function(x, y, method) {
  out <- suppressWarnings(stats::cor(x, y, method = method))
  if (length(out) != 1L || !is.finite(out)) {
    return(NA_real_)
  }
  out
}
# nolint end
