# multisiteDGP 0.2.0

A reliability release. The simulated data is unchanged — every golden
fixture reproduces bit-for-bit — but the reproducibility contract, the
advertised surface, and a number of error paths are not what they were.
Read the breaking changes before upgrading a script that records hashes
or passes `upstream` to `gen_effects()`.

## Breaking changes

### `canonical_hash()` values all move

The hash no longer covers the derived diagnostics (`I_hat`, `R_hat`,
`rho_S_*`, `rho_P_*`, `sigma_tau_*`), and the doubles it does cover are
rounded to nine significant digits before hashing.

**Every hash recorded under v0.1.x will differ under v0.2.0.** The data
did not change; what the hash looks at did. Recompute and update any
hash quoted in a manuscript, a log, or an issue.

What you get for it: **the hash is now portable.** The same design and
the same seed produce the same `canonical_hash()` on any platform. The
old policy named Ubuntu Linux the strict baseline and exempted macOS and
Windows from hash equality; that hierarchy is gone, because it is no
longer needed. A hash mismatch now means a real difference — never
platform noise.

The excluded diagnostics were computed with `cor()`, `sd()` and `mean()`,
whose floating-point accumulation order varies by platform. They are
functions of the hashed data and the hashed design, so they added no
provenance the hash did not already carry. To pin one deliberately:

```r
canonical_hash(dat, diagnostics_to_include = "I_hat")
```

### `gen_effects()` no longer takes `upstream`

Layer 1 starts the pipeline, so it has no upstream. The argument aborted
whenever it was non-`NULL`, so it never did anything; passing it now
raises `unused argument`. Remove it.

The identically named **first positional argument** of `gen_site_sizes()`,
`gen_se_direct()`, `align_rank_corr()`, `align_copula_corr()`,
`align_hybrid_corr()` and `gen_observations()` is unaffected. That one is
the real inter-layer data flow.

### `scenario_audit()` gains a `target_source` column

Code that selects audit columns by position or checks the column count
needs updating. Select by name.

## The catalog is seven shapes, not eight

`DESCRIPTION`, `README`, the pkgdown site, seven vignettes and seventeen
roxygen blocks said eight. The package generates seven: Gaussian,
Student-t, skew-normal, asymmetric Laplace, two-component mixture,
point-mass slab, and a user callback.

`true_dist = "DPM"` is a **reserved slot**. The argument accepts the
string, but built-in Dirichlet-process sampling is deferred to a future
release, so reach it through a `g_fn` bridge. This was already true in
v0.1.x — only the advertising was wrong.

## Fixes you will notice

### `scenario_audit()` works on every shape

Auditing a design that declared `SkewN`, `ALD`, `Mixture` or
`PointMassSlab` aborted the entire run. Four of the seven shapes could
not be audited at all.

Groups A, B and D are now judged normally and only the Group C
distributional gates are skipped, because they need a reference quantile
function and only Gaussian and Student-t have one. `target_source` reports
which happened. The gates are skipped rather than failed: an unmeasured
diagnostic is not a violated one.

**This narrows what `pass` means.** `pass = TRUE` says every gate that
*could* run did run and passed — not that every gate ran. On a grid that
mixes shapes, read `target_source` alongside `pass`, or a cell that
skipped a whole diagnostic group looks identical to one that cleared it.

### A named `beta` may omit the intercept

```r
gen_effects(J = 20L, sigma_tau = 0.2, formula = ~ prior,
            beta = c(prior = 0.3), data = d)
```

This used to fail with "`beta` is missing non-intercept coefficients.
Missing coefficients: (Intercept)." — calling the intercept a
non-intercept coefficient. An unnamed `beta` already allowed the same
omission. Named and unnamed now behave the same: an omitted intercept
defaults to zero.

### Engine A2 no longer accepts a design on one platform and refuses it
on another

The post-solve tolerance was a fixed `1e-6`. Below a certain conditioning
that comparison measures floating-point noise rather than fit quality, and
the verdict became platform dependent — the same design passed on macOS
and aborted on Linux.

The tolerance is now derived from the precision at which the residual can
actually be evaluated. Some designs that were refused near the boundary
now solve. When the achievable precision exceeds a relative error of
`1e-3` on the realized site-size SD, the fit is accepted and a warning
quotes the number, so you know how far you can trust it.

`gen_site_sizes()` now documents the feasible region, which depends only
on the ratio `n_min / nj_mean` — `nj_mean` itself drops out.

### The Student-t warning no longer recommends a value it warns about

Excess kurtosis is `6 / (nu - 4)`, which diverges at `nu = 4` as well as
below it. The warning fired at `nu < 4` and told you to use `nu >= 4`.
It now fires at `nu <= 4` and says `nu > 4`. **Numerical behaviour is
unchanged** — only the boundary of the warning and its wording.

The documentation now also reports the measured realized `var(z_j)` by
`nu`. Standardization is exact in expectation, but the sample variance of
a heavy-tailed draw is right-skewed, so a typical run lands below 1: the
median is 0.68 at `nu = 2.5` and `J = 200`. `sigma_tau` is a target in
expectation, not a guarantee per run.

### Caterpillar plots are readable at realistic `J`

`plot_effects()` drew one y-axis label per site, so at `J = 50` they
collided into an unreadable smear — worse at the `J = 200` designs the
presets encourage. Labels are thinned to at most 25 in effect order.
Every data row still draws.

## Error messages

- Integer arguments beyond the 32-bit range (`J`, `seed`, `max_iter`,
  `n_min`, `M`) used to pass validation and then become `NA`, surfacing
  much later as a bare "missing value where TRUE/FALSE needed". They now
  abort with the same classed error as any other invalid argument, naming
  the range.
- Messages no longer refer to version numbers that do not exist
  ("not supported in v1.0", "deferred to v2"). They say "this release"
  and "a future release".
- Six fix lines that opened with `Call`, `Return` or `Check` were
  rewritten to the documented `Try` / `Use` / `Pass` / `Remove` form.

## Dependencies

- **`multisitepower` is no longer declared in `Suggests`.** It is
  distributed on GitHub rather than CRAN, so declaring it made
  installation fail wherever the resolver could not reach it. The
  `as_multisitepower()` adapter is unchanged and works the moment you
  install the package yourself; the adapter's error tells you how.
- `hedgehog` was declared but never used, and is removed.

## Known limitations resolved since v0.1.0

The v0.1.0 notes listed limitations that no longer hold. Those entries
are left as written — they record what was true then — and the current
status is here.

- **Golden fixture provenance.** v0.1.0 noted that the manifest recorded
  macOS / aarch64 provenance with the Linux x86_64 baseline pending a
  first CI run. That run happened. The fixtures reproduce on
  `linux-release`, `linux-devel`, `linux-oldrel`, `macos-release` and
  `windows-release`, so the platform they were generated on no longer
  carries any meaning.
- **Cross-OS bit parity for the legacy site-size engine.** Verified
  through GitHub Actions, not only locally. The test that checks it runs
  on every platform now; under v0.1.x it was skipped everywhere but
  Linux x86_64, which left the portability claim checked on one of five
  CI cells.
- **Manual visual checks for funnel and forest plots.** Done. Funnel and
  dependence plots were correct; the caterpillar's axis labels were not,
  and are fixed above.

## Known limitations

- Exact reproduction of JEBS Figure 4(b) is still deferred. The lme4
  model behind it needs individual-level input or digitized figure
  targets, neither of which is in the repository. This is missing
  reference data, not a package defect: the appendix's **simulation** is
  reproduced exactly — `preset_jebs_strict()` is `identical()` to an
  independent base-R reimplementation of the appendix on all seven
  columns for all four seeds.

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
