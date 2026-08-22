# multisiteDGP 0.2.0

A reliability release. The reproducibility contract, the advertised surface,
and a number of error paths are now explicit and tested. Canonical numerical
equality and exact binary artifact equality are treated as separate claims.
Read the breaking changes before upgrading a script that records hashes
or passes `upstream` to `gen_effects()`.

## Breaking changes

### `canonical_hash()` values all move

Schema v4 no longer covers the derived diagnostics (`I_hat`, `R_hat`,
`rho_S_*`, `rho_P_*`, `sigma_tau_*`), and the doubles it does cover are
rounded to nine significant digits before hashing. It also preserves factor
labels, level ordering and orderedness, and applies the same normalization to
diagnostics that a caller explicitly includes.

**Every hash recorded under an earlier schema, including v3 development
artifacts, must be recomputed under v4.** A schema change can move the hash
even when raw data does not move.

What you get for it: **the canonical numerical hash is portable.** Seeded
wrappers pin `Mersenne-Twister` / `Inversion` / `Rejection`, so the same
design and seed do not depend on the caller's active RNG kind. The
old policy named Ubuntu Linux the strict baseline and exempted macOS and
Windows from hash equality; that hierarchy is gone, because it is no
longer needed. This does not promise byte-identical R objects or RDS files:
sub-precision values can share a hash, and very close values that straddle a
decimal rounding boundary can differ.

Provenance now records the producing RNG triple and whether it was
package-pinned or caller-controlled. `provenance_string()` prints stored
producer R/platform fields separately from the current verifier and warns on
a runtime mismatch.

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

### `scenario_audit()` gains explicit status metadata

Code that selects audit columns by position or checks the column count
needs updating. Select by name. In addition to `target_source`, the result
now carries `audit_complete`, `groups_evaluated`, `threshold_profile`, and
`n_warnings`. `pass` is literal: it is `TRUE` only for `status == "PASS"`;
a `WARN` row is `FALSE`.

## The catalog is six built-ins plus `User`, not eight built-ins

The package generates seven shapes: six built-ins (Gaussian, Student-t,
skew-normal, asymmetric Laplace, two-component mixture, point-mass slab)
plus a `User` callback.

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
distributional gates are skipped, because they need a package-defined
reference distribution and only Gaussian and Student-t have one. `target_source` reports
which happened. The gates are skipped rather than failed: an unmeasured
diagnostic is not a violated one.

**Status and completeness are separate.** `pass = TRUE` is exactly
`status == "PASS"`; a WARN is not silently promoted to success. On a grid
that mixes shapes, require both `pass` and `audit_complete` when the decision
requires all four diagnostic groups.

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

### Engine A2 verifies mean and SD separately and fails closed

The former adaptive residual floor was shared by the mean and SD checks.
An ill-conditioned SD calculation could therefore authorize an arbitrarily
large mean miss: for example, the old solver accepted a target mean of 100
with a fitted mean near 5.12 at `cv = 1e-5`.

A2 now evaluates truncated-Gamma moments with a stable incomplete-Gamma
recurrence. The requested mean must always satisfy the user's `tol`. Only
the SD check can use its separately estimated roundoff bound, capped at
relative `1e-3`; if the SD cannot be checked within that cap, the solver
aborts. The counterexample now solves to mean 100 and SD 0.001.

### Distribution and dependence diagnostics use their target scales

Gaussian-copula `pearson_corr` is now checked against the achieved latent
Pearson correlation. Raw Pearson correlations on residual and marginal
scales remain available as descriptive, untargeted rows. The Group C KS
statistic and p-value now use a calibrated one-sample test against the
analytic Gaussian or standardized Student-t CDF rather than a deterministic
quantile grid passed to a two-sample test.

The ALD documentation now states the implemented skew direction correctly:
`rho < 0.5` is right-skewed and `rho > 0.5` is left-skewed. Shrinkage
documentation now treats `S = sigma_tau^2 / (sigma_tau^2 + se2_j)` as the
site-estimate retention (reliability) weight; the amount pooled is `1 - S`.

The built-in rank/hybrid hill-climb is documented as implemented: a
deterministic exhaustive pair scan that consumes no RNG. Hybrid
`rank_corr = 0` is an identity/population-null convention, so its realized
sample correlation is not promised to land within `tol`.

`gen_site_sizes()` now documents the feasible region, which depends only
on the ratio `n_min / nj_mean` — `nj_mean` itself drops out.

### Preset and migration claims now match their sources

`preset_jebs_strict()` now uses the appendix's literal lower site-size
bound, `nj_min = 5`. The authority suite includes a floor-active fixture and
checks all seven raw output columns separately from the canonical hash.

`preset_walters_2024()` is now labelled a Walters-anchored proxy. Its
metadata separates direct source anchors (`J = 46`, average `se2 = .010`,
`sigma_tau = .197`, weak precision dependence), the derived `nj_mean = 240`,
and package assumptions such as outcome `R2 = .40`, `cv = .30`, and
`nj_min = 50`. Walters' conditional-prior `R2 = .502` is explicitly excluded
from the outcome-R2 interpretation. The metadata is retained in simulation
provenance.

The siteBayes2 migration guide now gives executable scale conversions for
`gen_priorG2()`: `tau_new = sigma_old * tau_old`,
`beta_new = sigma_old * beta_old`, and
`sigma_tau_new = sigma_old * sqrt(variance_old)`. It also maps the legacy
`T`, `Skew`, and ALD `kappa` parameters and distinguishes statistical parity
from the RNG-consuming `precision_dependence = FALSE` shuffle.

### Error IDs have an installed lookup table

`error_catalog()` exposes the stable E01--E30 condition, affected API, and
remedy table installed with the package. The table is generated from the
traceability ledger, so user-facing help and conformance tests share one
source of truth.

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
  `windows-release`. Their canonical numerical hash is portable; the manifest
  still records the producer platform and exact file SHA for diagnosis.
- **Cross-OS canonical parity for the legacy site-size engine.** Verified
  through GitHub Actions, not only locally. Exact serialized-byte identity is
  not inferred from that numerical check.
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

- **Six built-in latent-effect distributions plus `User`** sharing a unit-variance
  convention so a heterogeneity-ratio target means the same thing
  across shapes:
  - `gen_effects()` and the generated-shape functions
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
