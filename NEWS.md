# multisiteDGP 0.1.1

Documentation-only release closing a multi-month overhaul of the
package's public-facing surface. No changes to function bodies,
NAMESPACE exports, or numerical behavior — every documented hash
from v0.1.0 reproduces bit-for-bit.

## Documentation surface rebuilt

- **README.Rmd** rewritten for an applied-researcher audience with a
  reproducible Quick start, a deterministic hash check, and a Where
  to next bridge into the vignette tracks.
- **Pkgdown homepage** (`index.md`) now ships feature cards, a
  paradigm-aware front-door teaser, and IES funding acknowledgment
  in the canonical home block.
- **Hex sticker** + favicon set finalized (palette `#1B4965` /
  `#62B6CB` / `#F6AE2D`).

## Roxygen rewritten across 35 R files

Every function-level help page (53 exports + 28 S3 methods) was
rewritten to follow a single canonical template — applied-first
opener, motivation-style `@param` (range + when-to-move-it), full
`@return` enumeration, pedagogical `@examples`, aggressive
cross-linking via `@seealso`, and `@references` quoted verbatim from
the package's PI-confirmed bibliography. Eleven `@family` tags
group the reference index into navigable cards.

## Two-track vignette set (16 vignettes)

Sixteen new vignettes ship in two tracks, replacing the seven legacy
`v1`–`v7` files (which now redirect via `_pkgdown.yml`):

- **Applied Track (A1–A8)**: Getting started, Choosing a preset,
  Diagnostics in practice, Covariates and precision dependence,
  Calibrating to real data, Case study — multisite trial, Case study
  — meta-analysis, Cookbook.
- **Methodological Track (M1–M8)**: The two-stage DGP, G-distribution
  catalog, Margin and SE models, Precision dependence theory, Custom
  G distributions, Adapters and downstream packages, Reproducibility
  and provenance, Migration from siteBayes2.

Every vignette ships printed output for every primary function call
(no assign-and-hide) and at least the Phase 7 / 8 plot-count
minimum, with captions that explain what to read off each plot.
50 plots total, all with `fig.alt` for screen-reader accessibility.

## External review

The release is backed by **three independent external review
rounds**:

- Round 1 (metadata + pkgdown architecture).
- Round 2 (roxygen surface across 35 R files).
- Round 3 (the 16-vignette layer).

All Critical and Important findings from each round are remediated
in the release; remaining polish-level items are tracked in the
`v0.2.0` roadmap.

## Error catalog

`?multisiteDGP` now enumerates the package's 7-class typed error
hierarchy (`multisitedgp_error` base + 6 concrete subclasses for
argument, coherence, engine-dependence, solver, dependence-solver,
and marginal-violation failures). Calling code can branch on the
typed class with `inherits(e, "multisitedgp_<category>_error")`.

## Funding

This research was supported by the Institute of Education Sciences,
U.S. Department of Education, through Grant R305D240078 to the
University of Alabama. The opinions expressed are those of the
authors and do not represent views of the Institute or the U.S.
Department of Education.

---

# multisiteDGP 0.1.0

First tagged release. Version bumped from `0.0.0.9000` to `0.1.0`.

## New features

- **A two-stage data-generating pipeline** with one front door for
  site-size-driven scenarios and another for direct-precision
  specifications:
  - `sim_multisite()`, `sim_meta()`.

- **A reusable design object** that locks a scenario specification you
  can hand to multiple simulation calls or to a scenario grid:
  - `multisitedgp_design()`, `validate_multisitedgp_design()`,
    `update_multisitedgp_design()`, `is_multisitedgp_design()`,
    `is_multisitedgp_data()`, `design_grid()`.

- **Eight latent-effect distributions** sharing a unit-variance
  convention so a heterogeneity-ratio target means the same thing
  across shapes:
  - `gen_effects()` and the eight shape generators
    `gen_effects_gaussian()`, `gen_effects_studentt()`,
    `gen_effects_skewn()`, `gen_effects_ald()`,
    `gen_effects_mixture()`, `gen_effects_pmslab()`,
    `gen_effects_user()`, `gen_effects_dpm()`.

- **Site-size and standard-error margins** covering the
  site-size-driven path (sample sizes + within-site variance) and the
  direct-precision path:
  - `gen_site_sizes()`, `gen_se_direct()`, `gen_observations()`.

- **Three injection methods for precision dependence** — rank, copula,
  and a hybrid scheme — that hit a target effect-precision correlation
  without distorting either margin:
  - `align_rank_corr()`, `align_copula_corr()`, `align_hybrid_corr()`,
    `realized_rank_corr()`, `realized_rank_corr_marginal()`.

- **Diagnostics built around a four-question rubric** for verifying
  realized effect-size scale, sampling-variance distribution,
  effect-precision dependence, and downstream feasibility before a
  design is committed to a long simulation:
  - `scenario_audit()`, `feasibility_index()`, `informativeness()`,
    `mean_shrinkage()`, `compute_shrinkage()`, `compute_I()`,
    `compute_kappa()`, `heterogeneity_ratio()`, `bhattacharyya_coef()`,
    `ks_distance()`, `default_thresholds()`.

- **Nine bundled scenario presets** as defensible starting designs for
  common multisite trial and meta-analysis questions, each ready to
  override:
  - `preset_education_small()`, `preset_education_modest()`,
    `preset_education_substantial()`, `preset_jebs_paper()`,
    `preset_jebs_strict()`, `preset_walters_2024()`,
    `preset_twin_towers()`, `preset_meta_modest()`,
    `preset_small_area_estimation()`.

- **Adapters into downstream analysis packages** so a simulated
  dataset feeds straight into the estimator you plan to use:
  - `as_metafor()`, `as_baggr()`, `as_multisitepower()`.

- **Diagnostic plots** for the visualizations the rubric depends on:
  - `plot_effects()`, `plot_funnel()`, `plot_dependence()`.

- **Reproducibility helpers** for canonical hashing and human-readable
  provenance strings, so a saved fixture can be checked bit-for-bit
  and a manuscript can record exactly which design produced which
  result:
  - `canonical_hash()`, `provenance_string()`.

- **Cookbook recipes** under `inst/cookbook/` covering the workflows
  the vignettes introduce, with an audit harness at
  `inst/scripts/cookbook_audit.R` for verifying recipe outputs against
  fixtures.

- **Documentation and site structure**: reference pages, README,
  pkgdown navigation, cookbook recipes, and a migration guide for
  users moving from `siteBayes2` simulation workflows. The two-track
  vignette set (Applied A1–A8 + Methodological M1–M8) is planned for
  the public documentation release.

## Migration from siteBayes2

| `siteBayes2`                       | `multisiteDGP`                                |
|------------------------------------|-----------------------------------------------|
| `sim_multisite_data()`             | `sim_multisite()`                             |
| `gen_priorG()`, `gen_priorG2()`    | `gen_effects()` and the shape-specific helpers |
| `sim_sitesize_withinvar()`         | `gen_site_sizes()`                            |
| `sim_observed_effects()`           | `align_rank_corr()` plus `gen_observations()` |
| `get_shrinkage_factor()`           | `mean_shrinkage()`                            |

See `vignette("migration-from-siteBayes2")` for side-by-side examples
and a port / drop / defer mapping table.

## Removed from scope

- Stan, Rcpp, and fitting helpers from `siteBayes2` are intentionally
  not part of `multisiteDGP`. Use `as_metafor()`, `as_baggr()`, or a
  future fitting package for downstream analysis.
- Deprecated `siteBayes2` compatibility wrappers are not exported from
  `multisiteDGP`; the migration shim belongs in a `siteBayes2` patch
  package.

## Documentation

- `vignettes/migration-from-siteBayes2.Rmd` with side-by-side migration
  examples and a port / drop / defer mapping table.
- NEWS is wired into the pkgdown site.
- The two-track Applied / Methodological vignette set is in
  development and ships with the public documentation release.

## Known limitations

- `multisitepower` is a soft `Suggests` dependency for the
  `as_multisitepower()` adapter. It may be unavailable from mainstream
  CRAN mirrors; install it from its development source before calling
  the adapter.
- The shipped golden fixture manifest records macOS / aarch64
  provenance. The Linux x86_64 baseline is pending the first
  continuous-integration run.
- Cross-OS bit-parity for the legacy site-size engine is verified
  locally but not yet through GitHub Actions.
- Manual visual checks for funnel and forest plots are pending.
- Exact reproduction of JEBS Figure 4(b) is deferred until digitized
  targets become available.
