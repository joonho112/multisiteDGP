# metaDGP — Software Architecture & API Design Blueprint (Draft A)

*Author angle: software architecture and API design.*
*Companion drafts cover statistical rigor (Draft B) and user experience (Draft C). This draft does not duplicate those; it commits to a layered architecture, exact function signatures, an output schema, and a migration path from `siteBayes2`.*

---

## Table of contents

1. [Vision and scope](#1-vision-and-scope)
2. [Conceptual model](#2-conceptual-model)
3. [Package architecture](#3-package-architecture)
4. [Public API surface (function signatures)](#4-public-api-surface-function-signatures)
5. [Output schema](#5-output-schema)
6. [Extensibility points](#6-extensibility-points)
7. [Reproducibility & RNG strategy](#7-reproducibility--rng-strategy)
8. [Performance targets and computational notes](#8-performance-targets-and-computational-notes)
9. [Testing strategy](#9-testing-strategy)
10. [Migration plan from siteBayes2](#10-migration-plan-from-sitebayes2)
11. [Open questions](#11-open-questions)

---

## 1. Vision and scope

**metaDGP** is an R package that simulates paired (point-estimate, standard-error) data — the canonical input shape for meta-analysis, multisite-trial summary models, and empirical-Bayes deconvolution. Given a chosen distribution `G` for true site effects, a paradigm for generating sampling variances (either site-size-driven or directly from a target reliability `I` and SE-heterogeneity ratio `R`), and an optional precision–effect dependence specification, metaDGP returns a tidy tibble of `(tau_j, se_j, tau_j_hat)` triples that drops directly into `metafor::rma`, `baggr::baggr`, `multisitepower`, or any custom Stan/JAGS workflow. The package is a clean, general-purpose factoring of the team's working but unpolished code in `dev/code/siteBayes2/` and `dev/Bayes-deconvolution/Northwestern Project/`, plus a copula-based dependence injector that has only existed as research notes so far.

The intended users are methodologists and applied researchers who need realistic, parameterizable summary-level data for power analysis, calibration of estimators, and methods-comparison studies. Non-goals: metaDGP does **not** generate individual-level student-by-site data (that is `blkvar` / `PUMP` territory and is duplicative); it does **not** fit any model (that is `metafor` / `baggr` / `siteBayes2` territory); it does **not** wrap or replace `SimDesign` or `purrr::pmap` — instead it produces a design-grid helper and is engineered to compose cleanly with both. It also does not target multivariate (multi-outcome) meta-analysis in v0.1, though the architecture leaves the door open via a vector-valued `tau_j` extension.

## 2. Conceptual model

metaDGP is organized as a **five-layer pipeline**, each layer with a single responsibility and a well-typed interface to its neighbors. The user can swap any layer without rewriting the others.

```
   +---------------------+   parameters of G (true_dist, sigma_tau, ...)
   |  G-layer            |   produces tau_j (length J) and optionally site covariates
   |  gen_effects_*()    |   responsibility: marginal of true effects
   +----------+----------+
              |  tau_j (numeric[J]), site_index, optional covariates
              v
   +---------------------+   parameters of margin choice:
   |  margin-layer       |     - site-size paradigm (nj_mean, cv, nj_min, p, R2) -> se2_j
   |  gen_site_sizes()   |     - direct paradigm    (I, R, shuffle)              -> se2_j
   |  gen_se_from_I_R()  |   responsibility: marginal of se2_j (independent of tau_j)
   +----------+----------+
              |  se2_j (numeric[J]) and (optionally) n_j
              v
   +---------------------+   dependence target:
   |  dependence-layer   |     - rank   (Spearman rho, hill-climb)
   |  inject_rank_       |     - copula (Pearson  rho, Gaussian copula on (tau_j, se2_j))
   |    dependence()     |   responsibility: align margins so cor(tau_j, se2_j) = target,
   |  inject_copula_     |   preserving both marginals exactly (rank) or up to ECDF-resampling
   |    dependence()     |   accuracy (copula).
   +----------+----------+
              |  aligned (tau_j, se2_j)
              v
   +---------------------+   sampling model:
   |  observation-layer  |     tau_j_hat ~ N(tau_j, se2_j)  (Gaussian sampling, default)
   |  gen_observations() |   responsibility: produce observed estimates
   +----------+----------+
              |
              v
   +---------------------+   output adapters:
   |  output-layer       |     as_tibble()   -> canonical wide format
   |                     |     as_metafor()  -> yi, vi, sei
   |                     |     as_baggr()    -> tau, se
   |                     |     summary()     -> diagnostics (achieved I, R, rho, etc.)
   +---------------------+
```

**What the user can swap at each layer.** At the **G-layer**, choose from a built-in distribution name (`"Gaussian"`, `"T"`, `"Skew"`, `"ALD"`, `"Mixture"`, `"PointMassSlab"`) or pass a user-defined function with signature `function(J, ...) -> numeric(J)`. At the **margin-layer**, choose a built-in paradigm (`paradigm = "site_size"` or `paradigm = "direct"`) or pass a user-defined function with signature `function(J, ...) -> list(se2_j, ...)`. At the **dependence-layer**, choose `method = "rank"`, `method = "copula"`, or `method = "none"`, or supply a function with signature `function(tau_j, se2_j, target, ...) -> numeric(J)` returning the aligned `se2_j`. At the **observation-layer**, the default is Gaussian sampling but a user can pass `obs_fn = function(tau_j, se2_j, ...) -> numeric(J)` (e.g., for `t`-distributed sampling residuals or known-but-non-Gaussian sampling distributions). The **output-layer** is purely formatting and never runs simulation logic.

This decomposition is the key design move: it converts what is currently a monolithic `sim_multisite_data()` (in `siteBayes2`) and a monolithic `simulate_theta_hat()` (in the Northwestern project) into composable layer-functions that share a tibble contract, so that the two paradigms become two configurations of the same machine.

## 3. Package architecture

### 3.1 Folder and file layout

```
metaDGP/
├── DESCRIPTION
├── NAMESPACE                          # generated by roxygen2
├── R/
│   ├── metaDGP-package.R              # package-level docs, rlang/cli imports
│   ├── class-metadgp_design.R         # constructor, validator, print, format
│   ├── class-metadgp_data.R           # constructor, print, summary, as_tibble, [
│   ├── gen_effects.R                  # gen_effects_*() family, dispatcher
│   ├── gen_effects_gaussian.R         # internal generators per distribution
│   ├── gen_effects_t.R
│   ├── gen_effects_skew.R
│   ├── gen_effects_ald.R
│   ├── gen_effects_mixture.R
│   ├── gen_effects_pointslab.R
│   ├── gen_site_sizes.R               # truncated-Gamma site sizes, kappa/n_j -> se2_j
│   ├── gen_se_from_I_R.R              # geometric-mean / log-grid SE generator
│   ├── inject_rank_dependence.R       # hill-climb Spearman injector
│   ├── inject_copula_dependence.R     # Gaussian copula Pearson injector
│   ├── gen_observations.R             # tau_j_hat ~ N(tau_j, se2_j) (default)
│   ├── sim_multisite.R                # end-to-end: site-size paradigm
│   ├── sim_meta.R                     # end-to-end: direct (I, R) paradigm
│   ├── design_grid.R                  # tibble of conditions for SimDesign/pmap
│   ├── adapters.R                     # as_metafor(), as_baggr(), as_multisitepower()
│   ├── feasibility.R                  # compute_I(), compute_shrinkage(), feasibility_index()
│   ├── utils-rng.R                    # local_seed_stream(), with_metadgp_seed()
│   ├── utils-validate.R               # check_J(), check_prob(), check_positive()
│   └── deprecated.R                   # siteBayes2 shims with lifecycle::deprecate_warn()
├── inst/
│   ├── WORDLIST                       # spelling for R CMD check
│   └── defaults/
│       └── weiss2017.rds              # canonical realistic defaults
├── tests/
│   └── testthat/
│       ├── test-gen_effects.R
│       ├── test-gen_site_sizes.R
│       ├── test-gen_se_from_I_R.R
│       ├── test-inject_rank.R
│       ├── test-inject_copula.R
│       ├── test-sim_multisite.R
│       ├── test-sim_meta.R
│       ├── test-feasibility.R
│       ├── test-adapters.R
│       ├── test-classes.R
│       ├── test-rng.R
│       ├── test-schema-snapshot.R     # uses testthat::expect_snapshot
│       └── _snaps/
│           └── schema-snapshot/...
├── vignettes/
│   ├── 01-quickstart.Rmd
│   ├── 02-two-paradigms.Rmd           # site-size vs. (I, R)
│   ├── 03-precision-dependence.Rmd    # rank vs. copula
│   ├── 04-finite-vs-superpop.Rmd      # Miratrix-style framing
│   ├── 05-design-grids.Rmd            # SimDesign / pmap workflows
│   └── 06-extending-metaDGP.Rmd       # custom G, custom SE, custom injector
├── data-raw/
│   └── weiss2017.R                    # script that builds inst/defaults/weiss2017.rds
└── man/                                # generated
```

### 3.2 Module boundaries

The five layers correspond to five roxygen `@family` tags: `family-effects`, `family-margins`, `family-dependence`, `family-observation`, `family-output`. **A function in one module never calls a function in a non-adjacent module.** Concretely:

- `gen_effects_*()` does not know about SEs, dependence, or observations.
- `gen_site_sizes()` and `gen_se_from_I_R()` do not know about `tau_j`.
- `inject_rank_dependence()` and `inject_copula_dependence()` know about `tau_j` and `se2_j` but never about how either was generated.
- `gen_observations()` knows only about (`tau_j`, `se2_j`).
- The end-to-end `sim_multisite()` and `sim_meta()` are the *only* functions that wire layers together, and they do so via a shared `metadgp_design` object (see §3.4).

This is stricter than `siteBayes2` today, where `sim_multisite_data()` accepts a flat list of nineteen arguments forwarded to three sub-functions. The flat-args design is fine for one paradigm with one G family and one dependence method but it does not scale: adding the `(I, R)` paradigm and a copula injector turns the argument list into a kitchen sink with mutually-exclusive options. We replace it with a config object plus thin wrappers (§3.4).

### 3.3 Dependency graph between exported functions

```
                 +---------------------+
                 | metadgp_design()    |  config object (§3.4)
                 +----------+----------+
                            |
        +-------------------+-------------------+
        |                                       |
        v                                       v
 sim_multisite(design)                    sim_meta(design)
        |                                       |
        |  calls in order:                      |  calls in order:
        |    gen_effects_<G>()                  |    gen_effects_<G>()
        |    gen_site_sizes()  --> se2_j        |    gen_se_from_I_R() --> se2_j
        |    inject_*_dependence()              |    inject_*_dependence()
        |    gen_observations()                 |    gen_observations()
        |                                       |
        +----------------+----------------------+
                         |
                         v
                  metadgp_data object
                  /     |      |     \
                 /      |      |      \
        as_tibble()  print()  summary()  as_metafor() / as_baggr() / as_multisitepower()
                                  |
                                  +--> compute_I()
                                  +--> compute_shrinkage()
                                  +--> feasibility_index()

design_grid(...)   --> tibble of metadgp_design rows for SimDesign / purrr::pmap
```

Layer-level functions (`gen_effects_*`, `gen_site_sizes`, `gen_se_from_I_R`, `inject_*`, `gen_observations`) are **all exported** so power users can compose at any granularity; the end-to-end functions are the convenient default path.

### 3.4 Internal-vs-exported policy

**Exported:** every layer-level generator (`gen_effects_*`, `gen_site_sizes`, `gen_se_from_I_R`), every dependence injector, `gen_observations`, the two end-to-end wrappers (`sim_multisite`, `sim_meta`), the config constructor `metadgp_design`, the design-grid helper `design_grid`, all output adapters, all feasibility utilities, and the S3 methods (`print.metadgp_data`, `summary.metadgp_data`, `as_tibble.metadgp_data`, `as_metafor.metadgp_data`, etc.). Predicates `is_metadgp_design()` and `is_metadgp_data()` are exported.

**Internal (not exported):** the truncated-Gamma solver (`solve_trunc_gamma()`, `trunc_gamma_moments()`, `rtrunc_gamma()` from `siteBayes2`), the hill-climb kernel (`reorder_for_spearman()`), validators (`check_J()`, `check_prob()`, `check_positive()`), the RNG-stream helpers (`local_seed_stream()`), and per-distribution unscaled samplers. These are tagged `@keywords internal @noRd` and never given user-facing names.

**Soft-deprecated re-exports:** `gen_priorG2`, `sim_multisite_data`, `sim_observed_effects`, `sim_sitesize_withinvar` are re-exported as thin wrappers in `R/deprecated.R` that call `lifecycle::deprecate_warn()` and forward to the new pipeline (§10).

### 3.5 S3/S4 class strategy

We use **S3** throughout. S4 buys nothing here: there are no multimethod hierarchies, no formal slot checking that `validate_*` cannot do, and S3 plays better with `tibble` and the tidyverse. Two classes:

- **`metadgp_design`** (inherits from `list`) — an immutable configuration record produced by `metadgp_design()`. Stores layer choices and parameters. Methods: `print()`, `format()`, `validate_metadgp_design()` (exported), `update_metadgp_design()` (exported, returns a new design with patched fields). Storing layer functions as character names (e.g., `"Gaussian"`) plus an optional `g_fn` slot for user-supplied callbacks keeps the design serializable to RDS for caching across simulation runs.

- **`metadgp_data`** (inherits from `tbl_df`, `tbl`, `data.frame`) — the simulation output. Carries the canonical columns plus an `attr(., "design")` holding the originating `metadgp_design`, an `attr(., "diagnostics")` list (achieved I, achieved R, achieved rank/Pearson cor, etc.), and an `attr(., "metadgp_version")`. Methods: `print.metadgp_data()` (compact summary plus first rows), `summary.metadgp_data()` (achieved-versus-target diagnostics table), `as_tibble.metadgp_data()` (strips class attributes, returns a plain tibble), `[.metadgp_data` (subsets keep class iff all canonical columns survive; otherwise drops to plain tibble — same pattern `tibble` uses).

### 3.6 Config object vs. flat arguments — recommendation

**Recommendation: a config object (`metadgp_design`), with thin convenience wrappers that accept flat arguments and immediately construct a design internally.** This is the same pattern `parsnip` uses (`linear_reg(...) |> set_engine(...) |> fit(formula, data)`) and that `recipes` uses, and there is a reason it has won in tidymodels.

The flat-args alternative — what `siteBayes2::sim_multisite_data()` does today — has three problems that get worse with metaDGP's larger surface area:

1. **Mutually-exclusive arguments are unenforceable in a flat list.** When `paradigm = "site_size"`, the arguments `nj_mean`, `cv`, `nj_min`, `p`, `R2` are required and `I`, `R`, `shuffle` are forbidden. When `paradigm = "direct"` the relationship inverts. With flat args you fall back to runtime validation that still admits every wrong combination silently if the user does not pass the obvious flag. With a config, the constructor *for that paradigm* takes only the relevant arguments and rejects the wrong ones at construction time.

2. **Reproducibility and design grids both want the same thing.** A `design_grid()` row is a `metadgp_design`. Saving a simulation run means saving its design. Re-running last week's analysis means loading a design from RDS. Flat-args force you to pickle the call's argument list yourself; designs are first-class.

3. **The user-extension story is much cleaner.** A custom G is `metadgp_design(..., g_fn = my_custom_g)`. A custom dependence injector is `metadgp_design(..., dependence_fn = my_injector)`. With flat args, every layer's pluggability becomes another optional argument on the wrapper.

To keep the easy case easy, `sim_multisite()` and `sim_meta()` accept either a `design` argument *or* the union of flat parameters; if flat parameters are passed they are forwarded to `metadgp_design()` internally. So beginners write `sim_meta(J = 100, true_dist = "Gaussian", I = 0.7, R = 5)` and never see the design object; advanced users build designs explicitly and reuse them.

### 3.7 Package dependencies

**Imports** (hard): `stats`, `tibble`, `dplyr`, `withr`, `cli`, `lifecycle`, `rlang`, `nleqslv` (truncated-Gamma solver, inherited from `siteBayes2`).

**Suggests** (soft, gated by `requireNamespace()`): `sn` (skew-normal G), `LaplacesDemon` (asymmetric Laplace G), `copula` (Gaussian copula injector), `metafor`, `baggr`, `multisitepower` (output adapters; the adapters degrade to a clear "package not installed" message rather than failing on package load).

**Removed from siteBayes2's DESCRIPTION:** `bayesplot`, `ggplot2`, `Rcpp`, `RcppParallel`, `RcppEigen`, `BH`, `rstan`, `rstantools`, `StanHeaders`. metaDGP does no plotting, no Stan compilation, and no C++ — it is pure R. The Rcpp/Stan toolchain was only needed for the *fitting* code in `siteBayes2`, which lives downstream of metaDGP and stays in `siteBayes2` (or its successor). This single change drops install time from minutes to seconds and removes the `SystemRequirements: GNU make` line.

## 4. Public API surface (function signatures)

All signatures below are R-syntactically valid. Argument names and defaults match the existing user vocabulary (`tau_j`, `se2_j`, `sigma_tau`, `tau`, `I`, `R`, `rank_corr`, `pearson_corr`, `precision_dependence`, `nj_mean`, `cv`, `nj_min`, `p`, `R2`).

### 4.1 Configuration

```r
metadgp_design <- function(
  J            = 50L,
  paradigm     = c("site_size", "direct"),
  # G-layer
  true_dist    = c("Gaussian", "T", "Skew", "ALD", "Mixture", "PointMassSlab"),
  tau          = 0,
  sigma_tau    = 0.25,
  variance     = 1,
  nu           = NULL,
  slant        = NULL,
  rho          = NULL,
  delta        = NULL,
  eps          = NULL,
  ups          = NULL,
  pi_slab      = NULL,
  mu_slab      = NULL,
  sigma_slab   = NULL,
  formula      = NULL,
  beta         = NULL,
  data         = NULL,
  g_fn         = NULL,
  # margin-layer (site_size paradigm)
  nj_mean      = 50,
  cv           = 0.3,
  nj_min       = 5,
  p            = 0.5,
  R2           = 0,
  # margin-layer (direct paradigm)
  I            = NULL,
  R            = 1,
  shuffle      = TRUE,
  se_fn        = NULL,
  # dependence-layer
  dependence   = c("none", "rank", "copula"),
  rank_corr    = 0,
  pearson_corr = 0,
  dependence_fn = NULL,
  max_iter     = 20000L,
  tol          = 0.01,
  # observation-layer
  obs_fn       = NULL,
  # framing
  framing      = c("superpop", "finite"),
  # rng
  seed         = NULL
) { ... }
#' Construct a metaDGP design (immutable config record). One sentence per layer
#' is validated at construction time; mutually-exclusive arguments raise.

is_metadgp_design <- function(x) { ... }
#' TRUE iff `x` inherits from "metadgp_design".

validate_metadgp_design <- function(design) { ... }
#' Re-runs all consistency checks; returns `design` invisibly on success.

update_metadgp_design <- function(design, ...) { ... }
#' Functional update: returns a new design with the named fields replaced.
```

### 4.2 End-to-end

```r
sim_multisite <- function(
  design = NULL,
  ...,
  seed   = NULL
) { ... }
#' End-to-end site-size paradigm. `design` may be a metadgp_design (paradigm =
#' "site_size") or NULL, in which case `...` is forwarded to metadgp_design().

sim_meta <- function(
  design = NULL,
  ...,
  seed   = NULL
) { ... }
#' End-to-end direct (I, R) paradigm. Same dispatching rule as sim_multisite().
```

### 4.3 Layer-level functions (G-layer)

```r
gen_effects <- function(
  J,
  true_dist = c("Gaussian", "T", "Skew", "ALD", "Mixture", "PointMassSlab"),
  tau       = 0,
  sigma_tau = 0.25,
  variance  = 1,
  ...,
  g_fn      = NULL,
  formula   = NULL,
  beta      = NULL,
  data      = NULL
) { ... }
#' Dispatches to gen_effects_<true_dist>() or to g_fn(J, ...).

gen_effects_gaussian <- function(J, tau = 0, sigma_tau = 0.25, variance = 1,
                                  formula = NULL, beta = NULL, data = NULL) { ... }
#' Site effects ~ N(tau + X*beta, variance), then scaled by sigma_tau.

gen_effects_t <- function(J, tau = 0, sigma_tau = 0.25, variance = 1, nu,
                           formula = NULL, beta = NULL, data = NULL) { ... }
#' Site effects from scaled t with df = nu, then scaled by sigma_tau.

gen_effects_skew <- function(J, tau = 0, sigma_tau = 0.25, variance = 1, slant,
                              formula = NULL, beta = NULL, data = NULL) { ... }
#' Skew-normal via sn::rsn; suggests "sn".

gen_effects_ald <- function(J, tau = 0, sigma_tau = 0.25, variance = 1, rho,
                             formula = NULL, beta = NULL, data = NULL) { ... }
#' Asymmetric Laplace via LaplacesDemon::ralaplace; suggests "LaplacesDemon".

gen_effects_mixture <- function(J, tau = 0, sigma_tau = 0.25, variance = 1,
                                 delta, eps, ups,
                                 formula = NULL, beta = NULL, data = NULL) { ... }
#' Two-component normal mixture: weight (1-eps) at mean - delta, weight eps at + ups.

gen_effects_pointslab <- function(J, tau = 0, sigma_tau = 0.25,
                                   pi_slab, mu_slab = 0, sigma_slab = 1,
                                   formula = NULL, beta = NULL, data = NULL) { ... }
#' Point-mass-at-tau with prob (1 - pi_slab); slab N(mu_slab, sigma_slab^2) with prob pi_slab.
```

### 4.4 Layer-level functions (margin-layer)

```r
gen_site_sizes <- function(
  J,
  nj_mean  = 50,
  cv       = 0.3,
  nj_min   = 5,
  p        = 0.5,
  R2       = 0,
  var_outcome = 1
) { ... }
#' Truncated-Gamma site sizes -> se2_j = kappa / n_j with
#' kappa = (1/p + 1/(1-p)) * (1 - R2) * var_outcome. Returns a tibble with
#' columns: n_j_raw, n_j (integer), se2_j.

gen_se_from_I_R <- function(
  J,
  I,
  R       = 1,
  shuffle = TRUE
) { ... }
#' Direct paradigm: GM(se2) = (1 - I)/I; if R > 1, J values log-evenly spaced
#' between GM/R and GM*R; optionally shuffled. Returns a tibble: se2_j, se_j.
```

### 4.5 Layer-level functions (dependence-layer)

```r
inject_rank_dependence <- function(
  tau_j,
  se2_j,
  rank_corr = 0.5,
  max_iter  = 20000L,
  tol       = 0.01
) { ... }
#' Hill-climbing rank-reorder of se2_j to target Spearman rho with tau_j.
#' Marginals are preserved exactly (permutation only).

inject_copula_dependence <- function(
  tau_j,
  se2_j,
  pearson_corr = 0.5
) { ... }
#' Gaussian-copula reordering. Computes ECDF ranks of (tau_j, se2_j), draws
#' bivariate-normal copula samples with correlation pearson_corr, and reassigns
#' se2_j by matching uniform ranks. Marginals preserved up to ECDF granularity.
#' Suggests "copula" (falls back to a self-contained Gaussian-copula
#' implementation that uses only stats::qnorm / mvtnorm-style draws — see §6).
```

### 4.6 Layer-level functions (observation-layer)

```r
gen_observations <- function(
  tau_j,
  se2_j,
  obs_fn = NULL
) { ... }
#' Default: tau_j_hat ~ N(tau_j, sqrt(se2_j)). If obs_fn is supplied, calls
#' obs_fn(tau_j, se2_j) and expects numeric(length(tau_j)) back.
```

### 4.7 Design-grid helper

```r
design_grid <- function(
  ...,
  reps        = 1L,
  base_design = NULL,
  seed_stream = TRUE
) { ... }
#' Cartesian product of named argument vectors plus per-row metadgp_design
#' objects. Returns a tibble with one row per condition and an embedded
#' `design` list-column. If reps > 1, each condition is replicated and each
#' row gets a distinct seed (see §7). Designed to compose with
#' purrr::pmap(grid$design, sim_multisite) or SimDesign::runSimulation().
```

### 4.8 Output adapters and class methods

```r
print.metadgp_data    <- function(x, n = 6L, ...) { ... }
summary.metadgp_data  <- function(object, ...) { ... }
#' Returns achieved I, achieved R, achieved Spearman rho, achieved Pearson rho,
#' min/median/max se_j, J, paradigm, true_dist; printed as a cli table.

as_tibble.metadgp_data <- function(x, ...) { ... }
#' Strips metadgp_data attributes; returns a plain tibble.

as_metafor <- function(x, ...) { UseMethod("as_metafor") }
as_metafor.metadgp_data <- function(x, ...) { ... }
#' Returns a tibble with metafor's expected columns: yi = tau_j_hat,
#' vi = se2_j, sei = se_j, plus any covariates.

as_baggr <- function(x, ...) { UseMethod("as_baggr") }
as_baggr.metadgp_data <- function(x, ...) { ... }
#' Returns a data.frame ready for baggr::baggr(): tau = tau_j_hat, se = se_j.

as_multisitepower <- function(x, ...) { UseMethod("as_multisitepower") }
as_multisitepower.metadgp_data <- function(x, ...) { ... }
#' Returns a data.frame ready for multisitepower workflows.
```

### 4.9 Feasibility and reliability utilities

```r
compute_I <- function(se2_j, sigma_tau = NULL, tau_j = NULL) { ... }
#' Average reliability I = sigma_tau^2 / (sigma_tau^2 + GM(se2_j))
#' = 1 / (1 + GM(se2_j) / sigma_tau^2).
#' If sigma_tau is NULL and tau_j is supplied, sigma_tau is taken to be sd(tau_j).

compute_shrinkage <- function(se2_j, sigma_tau, monotone = FALSE) { ... }
#' Per-site classical Bayesian shrinkage factor B_j = sigma_tau^2 /
#' (sigma_tau^2 + se2_j). With monotone = TRUE returns a monotone-in-se2_j
#' isotonic version for use in monotonic-shrinkage diagnostics.

feasibility_index <- function(se2_j, sigma_tau, kind = c("efron", "morris")) { ... }
#' Scalar diagnostic of how well the super-population G is recoverable given
#' the observed precision profile. "efron" returns Efron's effective sample
#' size n_eff = sum(B_j); "morris" returns the Morris information index.
```

### 4.10 Canonical workflow example (runnable, ten lines)

```r
library(metaDGP)

# Direct (I, R) paradigm with a skew-normal G and rank-correlated precision
des <- metadgp_design(
  J = 200, paradigm = "direct",
  true_dist = "Skew", sigma_tau = 0.30, slant = 4,
  I = 0.7, R = 5, dependence = "rank", rank_corr = 0.4,
  seed = 20260506
)
dat <- sim_meta(des)
summary(dat)                    # achieved I, achieved rho, etc.
fit <- metafor::rma(yi = tau_j_hat, vi = se2_j, data = as_metafor(dat))
```

## 5. Output schema

### 5.1 Canonical wide format (one row per site)

| column        | type     | description                                                                 |
|---------------|----------|-----------------------------------------------------------------------------|
| `site_index`  | integer  | 1..J unique site identifier (preserved across permutations).                |
| `tau_j`       | double   | True site-specific treatment effect.                                        |
| `tau_j_hat`   | double   | Observed estimate, drawn from `N(tau_j, se2_j)` (or `obs_fn`).              |
| `se_j`        | double   | Standard error: `sqrt(se2_j)`.                                              |
| `se2_j`       | double   | Sampling variance (the canonical heterogeneity input to meta-models).       |
| `n_j`         | integer  | Site size (only present in `paradigm = "site_size"`; `NA_integer_` else).   |
| `<covariates>`| any      | Pass-through columns from the user-supplied `data` argument, if any.        |

`metadgp_data` carries two attributes:

- `attr(x, "design")` — the originating `metadgp_design`.
- `attr(x, "diagnostics")` — a named list: `achieved_I`, `achieved_R`, `achieved_rank_corr`, `achieved_pearson_corr`, `J`, `paradigm`, `true_dist`, `framing`, `seed`.

The columns `corr_est` (currently produced by `sim_observed_effects()` in `siteBayes2`) and `alpha`, `beta`, `n_j_raw` (currently produced by `sim_sitesize_withinvar()`) are **dropped from the canonical output** and moved into the `diagnostics` attribute. Reasoning: a row-replicated scalar like `corr_est` violates relational normalization; the truncated-Gamma fitting parameters are not per-site quantities and have no business in a per-site frame.

### 5.2 Long format (one row per (site, draw)) for replicated simulations

When called with `reps > 1` via `design_grid()`, the output gains a `rep` integer column and the same site-level columns repeat, allowing direct piping into `dplyr::group_by(rep)`.

### 5.3 Mapping to consumers

- **`metafor::rma`:** `yi = tau_j_hat`, `vi = se2_j`, `sei = se_j`. `as_metafor()` returns these names.
- **`baggr::baggr`:** `tau = tau_j_hat`, `se = se_j`. `as_baggr()` returns these names.
- **`multisitepower`:** consumes site-level summaries with `(estimate, se)` plus a `site` identifier; `as_multisitepower()` returns `site = site_index`, `estimate = tau_j_hat`, `se = se_j`.
- **`blkvar` aggregations:** `blkvar` is individual-level, but its block-level summary functions accept `(yi, vi)` — `as_metafor()` is a sufficient adapter.

## 6. Extensibility points

There are exactly three pluggable contracts. All three accept and return ordinary R numeric vectors so users do not need to learn metaDGP-specific types.

### 6.1 Custom G

**Contract.**

```r
g_fn(J, ...) -> numeric(length = J)
```

The function receives `J` and any extra named arguments forwarded from `metadgp_design(..., g_fn = my_g_fn, g_args = list(theta = 0.3))`. It must return a length-`J` numeric vector of true site effects `tau_j` *already on the response scale* (i.e., post-`sigma_tau` scaling — metaDGP does not rescale the output of a user G).

**Example:**

```r
my_horseshoe_g <- function(J, lambda = 1) {
  # Heavy-tailed horseshoe-style draws
  sapply(seq_len(J), function(i) {
    tau <- rcauchy(1, 0, lambda) |> abs()
    rnorm(1, 0, tau)
  })
}
des <- metadgp_design(J = 100, g_fn = my_horseshoe_g, g_args = list(lambda = 0.5),
                      paradigm = "direct", I = 0.7, R = 5)
```

### 6.2 Custom SE generator

**Contract.**

```r
se_fn(J, ...) -> list(se2_j = numeric(J), ...)
```

The function returns a named list with a mandatory `se2_j` element and any optional accompanying vectors (e.g., `n_j`) that get carried into the output frame as columns. metaDGP enforces `length(se2_j) == J` and `all(se2_j > 0)`.

**Example.** A user wants an inverse-Gamma SE distribution mimicking publication-bias-distorted samples:

```r
my_invgamma_se <- function(J, shape = 4, rate = 0.5) {
  list(se2_j = 1 / rgamma(J, shape = shape, rate = rate))
}
des <- metadgp_design(J = 100, true_dist = "Gaussian", sigma_tau = 0.3,
                      paradigm = "direct", se_fn = my_invgamma_se,
                      se_args = list(shape = 4, rate = 0.5))
```

### 6.3 Custom dependence injector

**Contract.**

```r
dependence_fn(tau_j, se2_j, target, ...) -> numeric(length(tau_j))
```

Returns a permutation-or-resample of `se2_j` whose marginals are preserved (this is verified by metaDGP via a marginal-preservation check; see §9). The `target` argument is whatever scalar the user passes via `rank_corr` or `pearson_corr`; metaDGP forwards both and the function picks whichever it implements.

**Example.** A user wants a Frank copula instead of a Gaussian copula:

```r
my_frank_injector <- function(tau_j, se2_j, target, theta = 5) {
  # Use copula::frankCopula etc. then ECDF-rank-match
  ...
}
```

### 6.4 What is *not* pluggable

The site-index column, the canonical schema, and the `metadgp_data` class contract. Users who need a different output shape call `as_tibble()` and reshape downstream. This is deliberate: a stable schema is the precondition for `as_metafor()` / `as_baggr()` / `as_multisitepower()` to keep working.

## 7. Reproducibility & RNG strategy

### 7.1 Principle

Every metaDGP function takes `seed = NULL` as default and *never* mutates the global RNG state when `seed = NULL`. When `seed` is non-NULL, the function uses `withr::with_seed(seed, ...)` so the global state is restored on exit. This is a hard break from `siteBayes2`'s current `set.seed(123)` calls inside `sim_multisite_data()`, `gen_priorG2()`, etc., which mutate global state and (worse) hard-code 123 as a default.

### 7.2 Seed propagation through composed calls

When `sim_multisite(design)` is invoked with a design that has `seed = s`, the wrapper enters a `withr::with_seed(s, ...)` block once, and *all* downstream layer-level calls run inside that block with no further reseeding. This guarantees that the entire pipeline is reproducible from a single seed and that the order of internal RNG draws matches the documented layer order (§2). Each layer-level function, when called *standalone*, also accepts its own `seed` and behaves the same way — so unit tests and ad-hoc calls are independently reproducible.

### 7.3 Per-condition seeds for design grids

`design_grid(..., reps = 100, seed_stream = TRUE)` produces 100 reps × `nrow(conditions)` rows; each row's `design$seed` is generated from a single top-level `master_seed` via `withr::with_seed(master_seed, sample.int(.Machine$integer.max, n_rows))`. This guarantees: (a) re-running the entire grid from `master_seed` reproduces every cell; (b) each cell is independent under the bootstrap-i.i.d. assumption; (c) running just one cell standalone is reproducible without re-running the whole grid.

### 7.4 `withr::with_seed` versus global

We use `withr::with_seed` exclusively for any function that takes a non-NULL seed. The function `local_seed_stream()` is an internal helper that wraps `withr::with_seed` plus an L'Ecuyer-CMRG kind specification when the user passes `parallel = TRUE` to `design_grid()` (sets `RNGkind("L'Ecuyer-CMRG")` only inside the local block, restores on exit). This is the cleanest way to support `future`-based parallelism without polluting the user's RNG settings.

### 7.5 Disagreement with `siteBayes2` here

Three things in `siteBayes2` should not be carried over:

- The `set.seed(123)` hard-coded fallback in `sim_multisite_data()`.
- The `set_seed = TRUE` *default* in `sim_sitesize_withinvar()` (it should default to FALSE, and the user should opt in to seeding).
- The triple-redundant `seed` / `set_seed` / `set.seed(seed)` pattern. metaDGP collapses this to a single `seed = NULL` argument with `withr::with_seed` semantics.

## 8. Performance targets and computational notes

### 8.1 Vectorization policy

`gen_effects_gaussian()`, `gen_effects_t()`, `gen_se_from_I_R()`, and `gen_observations()` are fully vectorized over `J` and consist of one or two `rnorm` / `rt` / `seq` calls. `gen_effects_skew`, `gen_effects_ald`, `gen_effects_mixture`, and `gen_effects_pointslab` currently `vapply` over sites in `siteBayes2`; metaDGP rewrites them to vectorized form where possible — `sn::rsn()` accepts vectorized `xi`, and `LaplacesDemon::ralaplace()` accepts a length-`n` location, so the per-site loop can be replaced with one call. The mixture distribution is rewritten as one `rbinom(J, 1, eps)` plus one `rnorm(J, ...)` with a `where` mask, dropping a ~3x constant.

`gen_site_sizes()` cannot avoid `nleqslv` for solving the truncated-Gamma parameters (a single 2-D root-find), but the result is one solve per call regardless of `J`. The accept-reject sampler is vectorized in batches of `2 * (J - n_acc)` and is `O(J)` expected work for moderate truncation.

### 8.2 The hill-climb is the bottleneck

`inject_rank_dependence()` runs up to `max_iter = 20000` loop iterations of `cor(tau_j, se2_perm, method = "spearman")`, each costing `O(J log J)` for the rank step. For `J = 200`, this is roughly 0.5–1 s per call; for `J = 1000` it is 3–6 s. With a design grid of 100 conditions × 1000 reps × 6 seconds, that is a 6-hour run. Two optimizations:

1. Use `inject_copula_dependence()` whenever Pearson dependence suffices — it is `O(J log J)` total (one Cholesky-of-2x2, two `qnorm` calls, two `rank` calls), about 100× faster.

2. Cache ranks: the inner loop computes `cor(rank(tau_j), rank(se2_perm))`, but `rank(tau_j)` is invariant across iterations. Pre-computing it cuts each iteration's work in half. (This is a deferred optimization — call it out, fix it in v0.2.)

### 8.3 Largest expected workload

The team's planned simulation studies are at most J = 500 sites, 100 conditions, 1000 reps — i.e., 50 million site-rows generated. With the layer-level vectorization above and the copula injector as default, this fits comfortably in under an hour single-threaded, and `future::plan(multisession)` over the design grid brings it to ~10 minutes on an 8-core laptop. We do not target massive (J > 10,000) regimes; users who need that should be steered toward simulating individual-level data with `blkvar` / `PUMP` and aggregating.

### 8.4 Deferred optimizations

(i) Vectorized rank-cache hill-climb (above); (ii) parallel `future`-based dispatch from `design_grid()` — the seed stream supports it but the dispatch glue is v0.2; (iii) C++ kernel for the swap loop (`Rcpp` would yield ~10× but the team has no Rcpp expert and the copula injector usually obviates the need); (iv) data.table backend for very large grids — wait until users ask.

## 9. Testing strategy

### 9.1 Unit tests

One test file per layer module (`test-gen_effects.R`, `test-gen_site_sizes.R`, `test-gen_se_from_I_R.R`, `test-inject_rank.R`, `test-inject_copula.R`, `test-gen_observations.R`). Each tests:

- correct return type and column names;
- correct length (`J`);
- argument-validation errors (`expect_error(..., class = "metadgp_arg_error")`);
- deterministic equality of two calls with the same seed (`expect_identical`);
- RNG state restoration: `set.seed(1); pre <- .Random.seed; gen_effects_gaussian(...); expect_identical(.Random.seed, pre)`.

### 9.2 Statistical regression tests

Tolerance-based tests that the pipeline achieves what the user asked for, on samples large enough that the test is not flaky:

```r
# compute_I() of generated data matches the targeted I
test_that("direct paradigm achieves target I within tolerance", {
  d <- sim_meta(metadgp_design(J = 5000, true_dist = "Gaussian",
                                sigma_tau = 0.3, paradigm = "direct",
                                I = 0.7, R = 5, seed = 1))
  expect_lt(abs(compute_I(d$se2_j, sigma_tau = 0.3) - 0.7), 0.02)
})

# rank injection achieves target Spearman within tol
test_that("inject_rank_dependence achieves target rank corr", {
  set.seed(1)
  tau_j <- rnorm(500); se2_j <- rexp(500) + 0.1
  out <- inject_rank_dependence(tau_j, se2_j, rank_corr = 0.4, tol = 0.02)
  expect_lt(abs(cor(tau_j, out, method = "spearman") - 0.4), 0.02)
})
```

### 9.3 Property-based tests for marginal preservation

Critical for any dependence injector: `sort(injector_output)` must equal `sort(input_se2_j)` exactly (rank injector) or up to ECDF-resampling tolerance (copula injector).

```r
test_that("inject_rank_dependence preserves marginals exactly", {
  set.seed(1); tau_j <- rnorm(200); se2_j <- rexp(200) + 0.1
  out <- inject_rank_dependence(tau_j, se2_j, rank_corr = 0.5)
  expect_identical(sort(out), sort(se2_j))
})
test_that("inject_copula_dependence preserves marginals via ECDF", {
  set.seed(1); tau_j <- rnorm(2000); se2_j <- rexp(2000) + 0.1
  out <- inject_copula_dependence(tau_j, se2_j, pearson_corr = 0.5)
  ks <- suppressWarnings(ks.test(out, se2_j))
  expect_gt(ks$p.value, 0.01)
})
```

We also add a `hedgehog`-style generator test sweeping `rank_corr ∈ {-0.6, -0.3, 0, 0.3, 0.6}` and `J ∈ {50, 200, 1000}` to confirm achievement-within-tolerance across the parameter space.

### 9.4 Snapshot tests for output schema

```r
test_that("metadgp_data canonical schema is stable", {
  d <- sim_meta(metadgp_design(J = 5, paradigm = "direct",
                                true_dist = "Gaussian", sigma_tau = 0.3,
                                I = 0.7, R = 1, seed = 1))
  expect_snapshot(names(d))
  expect_snapshot(sapply(d, class))
})
```

Schema changes will fail this test loudly, forcing intentional updates (and a CHANGELOG entry).

### 9.5 Adapter tests

For each adapter (`as_metafor`, `as_baggr`, `as_multisitepower`), a round-trip test that the column names match the consumer's documented input requirements. We do *not* require the consumer package to be installed for the tests (we use `skip_if_not_installed`), but we do exercise the adapter on a tiny dataset and assert column-name and column-class fidelity.

### 9.6 Reproducibility tests

- `expect_identical(sim_meta(des, seed = 42), sim_meta(des, seed = 42))`.
- `expect_false(identical(sim_meta(des, seed = 1), sim_meta(des, seed = 2)))`.
- "no global state mutation" test: snapshot `.Random.seed` before and after a seeded call.

### 9.7 CI matrix

GitHub Actions matrix on R `oldrel-1`, `release`, `devel`, three OSes. R CMD check at `--as-cran`. `covr` to enforce ≥ 90% line coverage on exported functions.

## 10. Migration plan from siteBayes2

### 10.1 Goal

Move the simulation half of `siteBayes2` into `metaDGP` *without breaking* the team's existing scripts that call `sim_multisite_data()`, `gen_priorG2()`, `sim_sitesize_withinvar()`, and `sim_observed_effects()`. The fitting half stays in `siteBayes2` (or its successor); `siteBayes2` will gain an `Imports: metaDGP` and forward simulation calls to metaDGP.

### 10.2 Renaming map

| old (`siteBayes2`)                         | new (`metaDGP`)                              | status                                  |
|---                                         |---                                           |---                                      |
| `gen_priorG2(true_dist, J, ...)`           | `gen_effects(J, true_dist, ...)`             | renamed; `gen_priorG2` re-exported with `deprecate_warn("0.1.0")` |
| `gen_priorG()`                             | (gone)                                       | superseded by `gen_priorG2`; `gen_priorG` was already deprecated in name |
| `sim_sitesize_withinvar()`                 | `gen_site_sizes()`                           | renamed; old name re-exported with `deprecate_warn` |
| `sim_observed_effects()` (rank only)       | `inject_rank_dependence()` + `gen_observations()` | split into two functions; old combined function re-exported with `deprecate_warn` and forwards |
| `sim_multisite_data()`                     | `sim_multisite()`                            | renamed; old name re-exported with `deprecate_warn` |
| (no equivalent — Northwestern script)      | `sim_meta()` / `gen_se_from_I_R()`           | new                                     |
| (no equivalent — research note)            | `inject_copula_dependence()`                 | new                                     |
| (no equivalent)                            | `metadgp_design()`, `design_grid()`          | new                                     |
| (no equivalent)                            | `compute_I()`, `compute_shrinkage()`, `feasibility_index()` | new (lifted from research notes 04–05) |
| `get_shrinkage_factor()`                   | `compute_shrinkage()`                        | renamed; old name re-exported with `deprecate_warn` |
| Argument: `precision_dependence = FALSE`   | Argument: `dependence = "none"` / `"rank"` / `"copula"` | flag becomes character enum; old flag still accepted with deprecation warning, mapped to `dependence = if (precision_dependence) "rank" else "none"` |
| Output column: `corr_est` (per-row scalar) | `attr(., "diagnostics")$achieved_rank_corr` | column dropped; same value available via `summary(x)` |
| Output columns: `alpha`, `beta`, `n_j_raw` | `attr(., "diagnostics")` (alpha, beta) and dropped (`n_j_raw`) | not per-site, do not belong in the per-site frame |

### 10.3 Deprecation mechanics

Each renamed function gets a four-line shim in `R/deprecated.R`:

```r
#' @export
sim_multisite_data <- function(...) {
  lifecycle::deprecate_warn(
    "0.1.0",
    "siteBayes2::sim_multisite_data()",
    "metaDGP::sim_multisite()"
  )
  metaDGP::sim_multisite(...)
}
```

Deprecation warnings fire once per session (lifecycle handles this), so existing scripts run unchanged with one warning per function. After two minor releases (v0.3.0), the shims become hard errors via `deprecate_stop()`; after v1.0.0, they are removed.

### 10.4 Moving the team's own scripts over

The team has simulation scripts in `dev/Bayes-deconvolution/Northwestern Project/` (using `simulate_theta_hat()`) and in `dev/code/siteBayes2/` (using `sim_multisite_data()`). The migration is a one-time mechanical edit:

- `simulate_theta_hat(theta_true, I, R, shuffle, seed)` becomes `sim_meta(metadgp_design(J = length(theta_true), paradigm = "direct", g_fn = function(J) theta_true, I = I, R = R, shuffle = shuffle, seed = seed))`. (We add `g_fn` accepting a fixed vector as a degenerate case so users with pre-computed `theta_true` can reuse them.)
- `sim_multisite_data(true_dist, J, sigma_tau, ..., precision_dependence, rank_corr, set_seed)` becomes `sim_multisite(metadgp_design(J, true_dist, sigma_tau, ..., dependence = if (precision_dependence) "rank" else "none", rank_corr, seed))`.

A `metaDGP::convert_legacy_call()` helper that takes a `siteBayes2` call and prints the metaDGP equivalent is a small addition that pays for itself within an afternoon for the team.

### 10.5 Disagreements with current code

Worth surfacing explicitly so the design call (§11) can ratify or overrule:

1. **`siteBayes2::gen_priorG2()` returns a `data.frame`, not a `tibble`.** Inconsistent with `sim_sitesize_withinvar` (returns a tibble). metaDGP standardizes on tibble everywhere.

2. **`gen_priorG2()` mixes the G-layer and the covariate layer.** `formula`, `beta`, `data` shift the *mean* of `tau_j`; this conflates "what is G" with "what is the conditional mean of G given X". metaDGP keeps the same surface API but documents the model as `tau_j = X*beta + e_j`, `e_j ~ G_centered`, where the user-named distribution acts on the residual. (No code change; clearer documentation.)

3. **`rank_corr` defaults to 0.5 in `sim_observed_effects()` even when `precision_dependence = FALSE`.** Confusing. metaDGP defaults `rank_corr` and `pearson_corr` to 0 and ties them to `dependence != "none"`.

4. **`sim_observed_effects()` random-shuffles `se2_j` even when no dependence is requested**, which silently destroys the sort order of `n_j` against `se2_j` in the site-size paradigm. metaDGP does *not* shuffle when `dependence = "none"`; the default is the natural order produced by the margin-layer.

5. **`set_seed = TRUE` is the default in `sim_sitesize_withinvar()`.** This silently mutates global RNG state. metaDGP defaults to no seeding (see §7.5).

These are not blockers; they are the kind of items that should be ratified by the team before v0.1 lands.

## 11. Open questions

These need a 30-minute design call before code lands.

1. **Should `g_fn` return centered residuals or post-mean draws?** Two reasonable conventions: (a) `g_fn(J)` returns mean-zero, unit-scale draws and metaDGP applies `tau + X*beta + sigma_tau * out`; (b) `g_fn(J)` returns the final `tau_j`. Convention (a) composes more cleanly with the covariate layer; convention (b) is more intuitive for users porting code. Recommend (a) with a `g_returns = c("residual", "tau_j")` switch defaulting to "residual".

2. **`framing = "finite"` versus `"superpop"` — does it change anything before the model-fitting step?** Miratrix's argument is mostly downstream (in shrinkage and inference), but if metaDGP is the gateway it should at least *record* the framing in `attr(., "diagnostics")` so the consumer (`siteBayes2`, `multisitepower`) can dispatch correctly. Recommend: store framing in design and diagnostics, do not branch generation logic on it (yet).

3. **Should we offer non-Gaussian observation models?** Currently `gen_observations()` defaults to `N(tau_j, se2_j)`. The reviewer feedback hints at scaled-`t` sampling residuals to handle small-`n` per-site quirks. Decision: ship Gaussian only in v0.1, expose `obs_fn` as the extension hook, plan a built-in `obs = "scaled_t"` for v0.2.

4. **Mixture distribution parameterization.** The current `gen_priorG2()` mixture uses `(delta, eps, ups)` with `(1 - eps)` weight at `mean - delta` and `eps` weight at `mean + ups`. This is unusual; the more standard form is `pi_1 N(mu_1, sigma_1)` + `pi_2 N(mu_2, sigma_2)`. Decision: keep `(delta, eps, ups)` for backwards compatibility and add a `gen_effects_mixture2()` with the standard form, or break and rename. Recommend: keep both, document equivalence.

5. **Vector-valued effects (multivariate meta-analysis).** Architectural question: do we want `tau_j` to be a matrix (J × p) for multi-outcome meta-analysis? If yes, the dependence layer needs a major redesign (joint copula on (τ_j, Σ_j)). Recommend: defer to v0.2 with an explicit non-goal in v0.1's documentation.

6. **Defaults from Weiss et al. (2017).** The advisory committee asked for "realistic defaults." Concretely: should `metadgp_design(J = 50)` with no other arguments produce a Weiss-2017-calibrated design, or should defaults be neutral (Gaussian, σ_τ = 0.25, I = 0.7) and the realistic ones live in a `metadgp_design_weiss2017()` preset? Recommend the latter — neutral defaults are predictable, realistic ones are opt-in.

7. **Output adapter for `multisitepower`.** We do not have a copy of `multisitepower`'s expected input schema in front of us; need to confirm with Jonathan Che whether `as_multisitepower()` should produce per-site summaries or per-individual rows (the latter would force metaDGP to optionally generate individual-level data — a non-goal we should not cross).

8. **Lifecycle stage for v0.1.** Mark all functions `lifecycle::badge("experimental")` for v0.1 to give us room to revise after the first internal user (the team itself) puts the package through real workloads. Move to `stable` at v1.0.

---

*End of Draft A.*
