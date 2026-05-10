#' multisiteDGP: Data-generating processes for multisite trial simulations
#'
#' @encoding UTF-8
#'
#' @description
#' Generate a complete multisite-trial summary dataset — latent site effects,
#' sampling variances, optional precision dependence between effects and
#' precisions, and observed site estimates — in one call. Use this when you
#' are designing a multisite-trial power analysis, choosing between
#' meta-analytic estimators, or sweeping a scenario grid before committing
#' to a long simulation run. The eight built-in effect distributions and the
#' literature-calibrated presets let you anchor a simulation to the
#' applied evidence base, and bundled diagnostics, plots, and adapters
#' carry the result through to downstream analysis.
#'
#' @details
#' The generative model is a transparent four-layer pipeline. Each layer
#' has a single responsibility and forwards a documented schema downstream:
#' \describe{
#'   \item{Layer 1 — latent effects (\code{\link{gen_effects}})}{Draws
#'     standardized site effects \eqn{z_j} from one of eight built-in
#'     \eqn{G} distributions — Gaussian, Student-t, skew-normal,
#'     asymmetric Laplace, two-component mixture, point-mass slab, a user
#'     callback, and a Dirichlet-process-mixture bridge — and rescales to
#'     the response-scale latent effect
#'     \eqn{\tau_j = \tau + \sigma_\tau\,z_j}.}
#'   \item{Layer 2 — site-level precision
#'     (\code{\link{gen_site_sizes}}, \code{\link{gen_se_direct}})}{Builds
#'     each site's sampling variance \eqn{\widehat{se}_j^2} either from
#'     generated site sizes \eqn{n_j} (the site-size-driven path) or from
#'     direct precision-scale targets — mean \eqn{\widehat{se}^2} and the
#'     heterogeneity ratio \eqn{R} — for the direct-precision path.}
#'   \item{Layer 3 — precision dependence
#'     (\code{\link{align_rank_corr}}, \code{\link{align_copula_corr}},
#'     \code{\link{align_hybrid_corr}})}{Optionally aligns
#'     \eqn{\widehat{se}_j^2} against \eqn{\tau_j} to a target Spearman
#'     correlation through one of three injection methods (rank
#'     hill-climb, Gaussian copula, or hybrid), preserving both
#'     marginals exactly.}
#'   \item{Layer 4 — observation draws
#'     (\code{\link{gen_observations}})}{Draws the observed estimate
#'     \eqn{\widehat{\tau}_j \mid \tau_j, \widehat{se}_j^2 \sim
#'     \mathcal{N}(\tau_j, \widehat{se}_j^2)}.}
#' }
#'
#' Two front doors compose the four layers in one call.
#' \code{\link{sim_multisite}} drives the precision margin from site
#' sizes — the site-size-driven path (Paradigm A in the blueprint) — and
#' matches how applied multisite-trial designs are usually specified.
#' \code{\link{sim_meta}} takes the precision targets directly — the
#' direct-precision path (Paradigm B) — and matches how meta-analysis
#' simulations are usually specified. Both return a
#' \code{multisitedgp_data} tibble built from an immutable
#' \code{\link{multisitedgp_design}}, with `diagnostics` and
#' `provenance` attached as attributes; the canonical hash is stored
#' inside `attr(x, "provenance")$canonical_hash`.
#'
#' On top of the pipeline, the package ships diagnostics covering Dr.
#' Chen's four questions (precision and feasibility, realized
#' dependence, distributional fit, downstream shrinkage), presets
#' calibrated to published education and labor-economics evidence, plots
#' for caterpillar / funnel / dependence views, adapters that emit data
#' ready for \pkg{metafor}, \pkg{baggr}, and \pkg{multisitepower}, and
#' reproducibility helpers (\code{\link{canonical_hash}},
#' \code{\link{provenance_string}}) for cross-machine replication.
#'
#' @section Vignettes:
#' \describe{
#'   \item{Applied Track}{
#'     \itemize{
#'       \item \href{../articles/a1-getting-started.html}{A1 \enc{·}{.} Getting started}
#'       \item \href{../articles/a2-choosing-a-preset.html}{A2 \enc{·}{.} Choosing a preset}
#'       \item \href{../articles/a3-diagnostics-in-practice.html}{A3 \enc{·}{.} Diagnostics in practice}
#'       \item \href{../articles/a4-covariates-precision-dependence.html}{A4 \enc{·}{.} Covariates and precision dependence}
#'       \item \href{../articles/a5-calibrating-real-data.html}{A5 \enc{·}{.} Calibrating to real data}
#'       \item \href{../articles/a6-case-study-multisite.html}{A6 \enc{·}{.} Case study \enc{—}{--} multisite trial}
#'       \item \href{../articles/a7-case-study-meta-analysis.html}{A7 \enc{·}{.} Case study \enc{—}{--} meta-analysis}
#'       \item \href{../articles/a8-cookbook.html}{A8 \enc{·}{.} Cookbook}
#'     }
#'   }
#'   \item{Methodological Track}{
#'     \itemize{
#'       \item \href{../articles/m1-statistical-dgp.html}{M1 \enc{·}{.} The two-stage DGP \enc{—}{--} formal specification}
#'       \item \href{../articles/m2-g-distribution-catalog.html}{M2 \enc{·}{.} G-distribution catalog and standardization}
#'       \item \href{../articles/m3-margin-se-models.html}{M3 \enc{·}{.} Margin and SE models \enc{—}{--} site-size and direct-precision paths}
#'       \item \href{../articles/m4-precision-dependence-theory.html}{M4 \enc{·}{.} Precision dependence \enc{—}{--} three injection methods}
#'       \item \href{../articles/m5-custom-g-distributions.html}{M5 \enc{·}{.} Custom G distributions}
#'       \item \href{../articles/m6-adapters-downstream.html}{M6 \enc{·}{.} Adapters and downstream packages}
#'       \item \href{../articles/m7-reproducibility-provenance.html}{M7 \enc{·}{.} Reproducibility and provenance}
#'       \item \href{../articles/m8-migration-from-siteBayes2.html}{M8 \enc{·}{.} Migration from siteBayes2}
#'     }
#'   }
#' }
#'
#' @section Function families:
#' \describe{
#'   \item{Front doors}{\code{\link{sim_multisite}}, \code{\link{sim_meta}}, \code{\link{design_grid}}.}
#'   \item{Design and data objects}{\code{\link{multisitedgp_design}} constructor with \code{\link{validate_multisitedgp_design}}, \code{\link{update_multisitedgp_design}}, \code{\link{is_multisitedgp_design}}, \code{\link{is_multisitedgp_data}}, and \code{\link{as_tibble.multisitedgp_data}}.}
#'   \item{Effect distributions (Layer 1)}{\code{\link{gen_effects}} dispatcher and the eight shape-specific generators \code{\link{gen_effects_gaussian}}, \code{\link{gen_effects_studentt}}, \code{\link{gen_effects_skewn}}, \code{\link{gen_effects_ald}}, \code{\link{gen_effects_mixture}}, \code{\link{gen_effects_pmslab}}, \code{\link{gen_effects_user}}, \code{\link{gen_effects_dpm}}.}
#'   \item{Margin / SE models (Layer 2)}{\code{\link{gen_site_sizes}} for the site-size margin; \code{\link{gen_se_direct}} for the direct standard-error margin.}
#'   \item{Precision dependence (Layer 3)}{\code{\link{align_rank_corr}}, \code{\link{align_copula_corr}}, \code{\link{align_hybrid_corr}}.}
#'   \item{Observation draws (Layer 4)}{\code{\link{gen_observations}}.}
#'   \item{Diagnostics}{Group A precision and feasibility scalars (\code{\link{compute_kappa}}, \code{\link{compute_I}}, \code{\link{informativeness}}, \code{\link{compute_shrinkage}}, \code{\link{mean_shrinkage}}, \code{\link{feasibility_index}}, \code{\link{default_thresholds}}, \code{\link{heterogeneity_ratio}}); Group B realized dependence (\code{\link{realized_rank_corr}}, \code{\link{realized_rank_corr_marginal}}); Group C distributional fidelity (\code{\link{bhattacharyya_coef}}, \code{\link{ks_distance}}); top-of-funnel scenario sweep (\code{\link{scenario_audit}}).}
#'   \item{Presets}{\code{\link{preset_education_small}}, \code{\link{preset_education_modest}}, \code{\link{preset_education_substantial}}, \code{\link{preset_jebs_paper}}, \code{\link{preset_jebs_strict}}, \code{\link{preset_meta_modest}}, \code{\link{preset_small_area_estimation}}, \code{\link{preset_twin_towers}}, \code{\link{preset_walters_2024}}.}
#'   \item{Output adapters}{\code{\link{as_metafor}}, \code{\link{as_baggr}}, \code{\link{as_multisitepower}}.}
#'   \item{Visualization}{\code{\link{plot_effects}}, \code{\link{plot_funnel}}, \code{\link{plot_dependence}}.}
#'   \item{Reproducibility}{\code{\link{canonical_hash}}, \code{\link{provenance_string}}.}
#' }
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
#' @section Error catalog:
#' Errors raised by the package carry a typed S3 class hierarchy so
#' that calling code can branch on the error category. Every error
#' inherits from \code{multisitedgp_error}; six concrete subclasses
#' name the failure category.
#' \describe{
#'   \item{\code{multisitedgp_error}}{Base class. Inherit-test with
#'     \code{inherits(e, "multisitedgp_error")} to catch any
#'     package-typed error.}
#'   \item{\code{multisitedgp_arg_error}}{Argument validation.
#'     Raised when a user-facing argument is missing, the wrong type,
#'     out of range, or exclusive with another argument.}
#'   \item{\code{multisitedgp_coherence_error}}{Design-level
#'     coherence violations across layers. Raised when a combination
#'     of arguments is individually valid but jointly inconsistent
#'     (for example, the residual / marginal target pair specified
#'     incompatibly).}
#'   \item{\code{multisitedgp_engine_dependence_error}}{Engine
#'     compatibility. Raised when the legacy \code{A1} engine is
#'     paired with a non-zero Layer 3 dependence target, the
#'     documented Decision-C constraint.}
#'   \item{\code{multisitedgp_solver_error}}{Solver failure. Raised
#'     when an internal numerical solver (for example, the
#'     direct-precision back-calculation) fails to converge.}
#'   \item{\code{multisitedgp_dependence_solver_error}}{Layer 3
#'     alignment-solver failure. Raised when
#'     \code{\link{align_rank_corr}}, \code{\link{align_copula_corr}},
#'     or \code{\link{align_hybrid_corr}} cannot reach the requested
#'     target within tolerance.}
#'   \item{\code{multisitedgp_marginal_violation_error}}{Multiset
#'     preservation violation. Raised when a Layer 3 alignment
#'     pass would change the empirical marginal distribution of
#'     \eqn{z_j} or \eqn{\widehat{se}_j^2} (the package's marginal-
#'     preservation invariant).}
#' }
#' Every condition object also carries a one-sentence \code{message},
#' a \code{rlang::format_error_bullets()} body explaining what
#' happened, and a \code{fix} field with a concrete next step.
#'
#' @section Funding:
#' This research was supported by the Institute of Education Sciences, U.S.
#' Department of Education, through Grant R305D240078 to the University of
#' Alabama. The opinions expressed are those of the authors and do not
#' represent views of the Institute or the U.S. Department of Education.
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang .data
#' @importFrom tibble as_tibble
## usethis namespace: end
NULL
