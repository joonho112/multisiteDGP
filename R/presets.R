# nolint start: object_usage_linter
#' Preset simulation designs — defensible starting points for common multisite scenarios
#'
#' @encoding UTF-8
#'
#' @description
#' Presets package a defensible starting design — site count, latent-effect
#' shape, heterogeneity, sample-size or precision targets — for a common
#' multisite-trial or meta-analysis scenario. Each preset returns a locked
#' `multisitedgp_design` object whose parameter values are anchored to a
#' published reference, so a reviewer can defend the choice. Pass any
#' named `multisitedgp_design()` argument through `...` to override the
#' locked defaults.
#'
#' @details
#' \strong{Decision table — pick by trial type.}
#' \describe{
#'   \item{Education multisite trial}{
#'     \code{\link{preset_education_small}} (\eqn{\sigma_\tau = 0.05}) /
#'     \code{\link{preset_education_modest}} (\eqn{\sigma_\tau = 0.20}) /
#'     \code{\link{preset_education_substantial}} (\eqn{\sigma_\tau = 0.30}).
#'     Calibrated to Weiss et al. (2017) cross-site benchmarks from 16
#'     multisite RCTs.
#'   }
#'   \item{JEBS reference design (Mixture, engine A1)}{
#'     \code{\link{preset_jebs_paper}} (UX-anchored, \eqn{\sigma_\tau = 0.20})
#'     for general use; \code{\link{preset_jebs_strict}}
#'     (\eqn{\sigma_\tau = 0.15}, J = 100) for paper-grid bit-identical
#'     reproduction. Both use A1 legacy engine — the JEBS Mixture fixture.
#'   }
#'   \item{Empirical-Bayes school value-added}{
#'     \code{\link{preset_walters_2024}} — J = 46 schools, Boston charter
#'     value-added, calibrated to the Walters (2024) Handbook chapter
#'     reanalysis of the BPS dataset.
#'   }
#'   \item{Deconvolution / shape-recovery benchmark}{
#'     \code{\link{preset_twin_towers}} — bimodal mixture, J = 1000,
#'     equal-sample-size sites; the canonical deconvolution-method test
#'     bed (Efron-style benchmark).
#'   }
#'   \item{Meta-analysis (direct precision)}{
#'     \code{\link{preset_meta_modest}} — J = 50, \eqn{I = 0.30},
#'     \eqn{R = 1.5}, modest-informativeness meta-analysis design.
#'   }
#'   \item{Small-area estimation (direct precision)}{
#'     \code{\link{preset_small_area_estimation}} — J = 30,
#'     \eqn{I = 0.20}, \eqn{R = 3.0}, Fay-Herriot small-area design.
#'   }
#' }
#'
#' \strong{Override pattern.} Every preset accepts `...` for named
#' overrides; the `\code{\link{multisitedgp_design}}` argument set is the
#' valid name space. To use a preset's heterogeneity scale but a different
#' site count: `preset_education_modest(J = 100L)`. To use a preset's
#' calibration but switch on rank-correlated dependence:
#' `preset_education_modest(dependence = "rank", rank_corr = 0.3)`.
#'
#' \strong{Citing presets in published work.} Each preset locks parameter
#' values from a published reference; cite that reference, not the
#' preset name. The relevant citations are listed in
#' \code{\link{multisiteDGP}} package help and in
#' \code{citation("multisiteDGP")}.
#'
#' For a workflow walkthrough see the
#' \href{../articles/a2-choosing-a-preset.html}{Choosing a preset}
#' vignette.
#'
#' @param ... Named overrides passed to \code{\link{multisitedgp_design}}.
#'   Override field names must match `multisitedgp_design()` arguments;
#'   unknown names trigger an informative error.
#'
#' @return A `multisitedgp_design` object with `paradigm = "site_size"`
#'   or `paradigm = "direct"` depending on the preset. Pass to
#'   \code{\link{sim_multisite}} or \code{\link{sim_meta}} respectively.
#'
#' @family family-presets
#' @seealso
#'   \code{\link{multisitedgp_design}} for the constructor that backs
#'   every preset; \code{\link{sim_multisite}} and \code{\link{sim_meta}}
#'   for the wrappers that consume preset designs;
#'   \code{\link{design_grid}} for sweeping multiple presets in a grid;
#'   the \href{../articles/a2-choosing-a-preset.html}{A2 Choosing a
#'   preset} vignette.
#'
#' @references
#' Lee, J., Che, J., Rabe-Hesketh, S., Feller, A., & Miratrix, L. (2025).
#' Improving the estimation of site-specific effects and their distribution
#' in multisite trials. \emph{Journal of Educational and Behavioral Statistics},
#' \bold{50}(5), 731--764. \doi{10.3102/10769986241254286}.
#'
#' Walters, C. (2024). Empirical Bayes methods in labor economics. In
#' \emph{Handbook of Labor Economics} (Vol. 5, pp. 183--260). Elsevier.
#' \doi{10.1016/bs.heslab.2024.11.001}.
#'
#' Weiss, M. J., Bloom, H. S., Verbitsky-Savitz, N., Gupta, H., Vigil, A. E.,
#' & Cullinan, D. N. (2017). How much do the effects of education and training
#' programs vary across sites? Evidence from past multisite randomized trials.
#' \emph{Journal of Research on Educational Effectiveness}, \bold{10}(4),
#' 843--876. \doi{10.1080/19345747.2017.1300719}.
#'
#' @name presets
#' @examples
#' # The default applied starting point.
#' design <- preset_education_modest()
#' sim_multisite(design, seed = 1L)
#'
#' # Override the site count while keeping the calibrated heterogeneity.
#' large <- preset_education_modest(J = 200L)
#'
#' # Switch on rank-correlated precision dependence.
#' dep <- preset_education_modest(dependence = "rank", rank_corr = 0.3)
NULL

#' @describeIn presets Education multisite trial with small effect-size heterogeneity (\eqn{\sigma_\tau = 0.05}). Calibrated to Weiss et al. (2017) education programs at the small end of the cross-site benchmark range. J = 50, sample sizes ~ 40 per site.
#' @export
preset_education_small <- function(...) {
  .preset_design(
    list(
      J = 50L,
      paradigm = "site_size",
      true_dist = "Gaussian",
      tau = 0,
      sigma_tau = 0.05,
      nj_mean = 40,
      cv = 0.50,
      nj_min = 5L,
      p = 0.5,
      R2 = 0,
      engine = "A2_modern",
      framing = "superpopulation"
    ),
    list(...)
  )
}

#' @describeIn presets Education multisite trial with moderate effect-size heterogeneity (\eqn{\sigma_\tau = 0.20}). The package's default applied starting point; J = 50, sample sizes ~ 50 per site, calibrated to the Weiss et al. (2017) middle of the cross-site range and used as the JEBS UX anchor.
#' @export
preset_education_modest <- function(...) {
  .preset_design(
    list(
      J = 50L,
      paradigm = "site_size",
      true_dist = "Gaussian",
      tau = 0,
      sigma_tau = 0.20,
      nj_mean = 50,
      cv = 0.50,
      nj_min = 10L,
      p = 0.5,
      R2 = 0,
      engine = "A2_modern",
      framing = "superpopulation"
    ),
    list(...)
  )
}

#' @describeIn presets Education multisite trial with substantial effect-size heterogeneity (\eqn{\sigma_\tau = 0.30}). Larger trial (J = 100, sample sizes ~ 80) calibrated to the upper end of the Weiss et al. (2017) cross-site benchmarks.
#' @export
preset_education_substantial <- function(...) {
  .preset_design(
    list(
      J = 100L,
      paradigm = "site_size",
      true_dist = "Gaussian",
      tau = 0,
      sigma_tau = 0.30,
      nj_mean = 80,
      cv = 0.50,
      nj_min = 10L,
      p = 0.5,
      R2 = 0,
      engine = "A2_modern",
      framing = "superpopulation"
    ),
    list(...)
  )
}

#' @describeIn presets JEBS Mixture reference design with the UX-anchored heterogeneity (\eqn{\sigma_\tau = 0.20}). Uses the legacy A1 site-size engine for bit-identical reproduction of the JEBS paper's general-use Mixture fixture (J = 50, mixture parameters \eqn{\delta = 5}, \eqn{\epsilon = 0.3}, \eqn{\Upsilon = 2}).
#' @export
preset_jebs_paper <- function(...) {
  .preset_design(
    list(
      J = 50L,
      paradigm = "site_size",
      true_dist = "Mixture",
      theta_G = list(delta = 5, eps = 0.3, ups = 2),
      variance = 1,
      tau = 0,
      sigma_tau = 0.20,
      nj_mean = 40,
      cv = 0.50,
      nj_min = 5L,
      p = 0.5,
      R2 = 0,
      engine = "A1_legacy",
      dependence = "none",
      framing = "superpopulation"
    ),
    list(...)
  )
}

#' @describeIn presets JEBS strict paper-grid Mixture (J = 100, \eqn{\sigma_\tau = 0.15}). Use this for bit-identical reproduction of the JEBS paper's strict simulation grid (the T1a invariant). Engine A1 legacy.
#' @export
preset_jebs_strict <- function(...) {
  .preset_design(
    list(
      J = 100L,
      paradigm = "site_size",
      true_dist = "Mixture",
      theta_G = list(delta = 5, eps = 0.3, ups = 2),
      variance = 1,
      tau = 0,
      sigma_tau = 0.15,
      nj_mean = 80,
      cv = 0.50,
      nj_min = 4L,
      p = 0.5,
      R2 = 0,
      engine = "A1_legacy",
      dependence = "none",
      framing = "superpopulation"
    ),
    list(...)
  )
}

#' @describeIn presets Boston charter / BPS value-added empirical-Bayes design from Walters (2024) Handbook of Labor Economics chapter. J = 46 schools, \eqn{\sigma_\tau \approx 0.197}, mean per-school sample 240, with \eqn{R^2 = 0.40} from covariate adjustment. Use when calibrating to school-effect or teacher-effect Empirical Bayes literature.
#' @export
preset_walters_2024 <- function(...) {
  .preset_design(
    list(
      J = 46L,
      paradigm = "site_size",
      true_dist = "Gaussian",
      tau = 0,
      sigma_tau = 0.197,
      nj_mean = 240,
      cv = 0.30,
      nj_min = 50L,
      p = 0.5,
      R2 = 0.40,
      engine = "A2_modern",
      dependence = "none",
      framing = "superpopulation"
    ),
    list(...)
  )
}

#' @describeIn presets Twin-towers bimodal mixture benchmark for deconvolution and shape-recovery method evaluation. J = 1000 sites with equal sample sizes; mixture parameters \eqn{\delta = 4}, \eqn{\epsilon = 0.5}, \eqn{\Upsilon = 1}. Standard test bed for nonparametric Empirical Bayes methods.
#' @export
preset_twin_towers <- function(...) {
  .preset_design(
    list(
      J = 1000L,
      paradigm = "site_size",
      true_dist = "Mixture",
      theta_G = list(delta = 4, eps = 0.5, ups = 1),
      tau = 0,
      sigma_tau = 2.0,
      nj_mean = 100,
      cv = 0,
      nj_min = 100L,
      p = 0.5,
      R2 = 0,
      engine = "A2_modern",
      dependence = "none",
      framing = "superpopulation"
    ),
    list(...)
  )
}

#' @describeIn presets Direct-precision (Paradigm B) meta-analysis design with moderate informativeness (\eqn{I = 0.30}, \eqn{R = 1.5}). J = 50 studies. Use with \code{\link{sim_meta}} for meta-analysis simulations where study-level standard errors are specified directly.
#' @export
preset_meta_modest <- function(...) {
  .preset_design(
    list(
      J = 50L,
      paradigm = "direct",
      true_dist = "Gaussian",
      tau = 0,
      sigma_tau = 0.20,
      I = 0.30,
      R = 1.5,
      shuffle = TRUE,
      dependence = "none",
      framing = "superpopulation"
    ),
    list(...)
  )
}

#' @describeIn presets Direct-precision Fay-Herriot small-area estimation design (J = 30, \eqn{I = 0.20}, \eqn{R = 3.0}). Wider precision spread than the meta-analysis preset; reflects the heterogeneous-precision character of small-area survey data. Use with \code{\link{sim_meta}}.
#' @export
preset_small_area_estimation <- function(...) {
  .preset_design(
    list(
      J = 30L,
      paradigm = "direct",
      true_dist = "Gaussian",
      tau = 0,
      sigma_tau = 0.20,
      I = 0.20,
      R = 3.0,
      shuffle = TRUE,
      dependence = "none",
      framing = "superpopulation"
    ),
    list(...)
  )
}

.preset_design <- function(base, overrides) {
  if (length(overrides) == 0L) {
    return(do.call(multisitedgp_design, base))
  }

  supplied <- names(overrides)
  if (is.null(supplied)) {
    supplied <- rep("", length(overrides))
  }
  supplied[is.na(supplied)] <- ""
  if (any(!nzchar(supplied))) {
    .abort_arg(
      "Preset overrides in `...` must be named.",
      "Preset wrappers forward overrides to `multisitedgp_design()` by argument name.",
      "Use named overrides such as `J = 100L` or `sigma_tau = 0.25`."
    )
  }

  bad <- setdiff(supplied, names(formals(multisitedgp_design)))
  if (length(bad) > 0L) {
    .abort_arg(
      "Unknown preset override field.",
      sprintf("Override fields must match `multisitedgp_design()` arguments; got: %s.", paste(bad, collapse = ", ")),
      "Use flat argument names such as `J`, `sigma_tau`, `dependence`, or `rank_corr`."
    )
  }

  base[supplied] <- overrides
  do.call(multisitedgp_design, base)
}
# nolint end
