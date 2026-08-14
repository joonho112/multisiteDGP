# metaDGP — Draft C: User Experience, Workflow, Documentation & Adoption

**Author:** UX track
**Status:** Internal design draft, for merge with architecture (A) and statistics (B) tracks
**Audience:** PI, advisory committee, package contributors

This draft argues that metaDGP's adoption hinges on two design moves: **two opinionated front doors** (`sim_multisite()` and `sim_meta()`) keyed to the two reader populations the funded grant must serve, and **a recipe-first documentation layout** in which every concept the user has to learn (`I`, `R`, `G`, precision dependence) is anchored to a worked example before it is anchored to a formula. The architecture and statistics drafts can deliver the most general engine on the market, and it will still go unused if applied researchers cannot get a plot on screen in five minutes.

---

## Table of contents

1. [User personas and primary scenarios](#1-user-personas-and-primary-scenarios)
2. [Hello-world and 5-minute tour](#2-hello-world-and-5-minute-tour)
3. [Two opinionated front doors](#3-two-opinionated-front-doors)
4. [Presets and recipe library](#4-presets-and-recipe-library)
5. [Common workflows (recipes / cookbook)](#5-common-workflows-recipescookbook)
6. [Argument design and defaults](#6-argument-design-and-defaults)
7. [Output design and plotting](#7-output-design-and-plotting)
8. [Help, vignettes, and pkgdown plan](#8-help-vignettes-and-pkgdown-plan)
9. [Error messages and validation UX](#9-error-messages-and-validation-ux)
10. [Onboarding and dissemination plan](#10-onboarding-and-dissemination-plan)
11. [Versioning, contribution, and governance](#11-versioning-contribution-and-governance)
12. [Risks to adoption and mitigations](#12-risks-to-adoption-and-mitigations)

---

## 1. User personas and primary scenarios

Five personas, each with a single sentence on the goal and the one or two functions they actually call. The order is roughly "first to touch the package" to "last." If a feature does not serve at least one of these five, it does not ship in v1.0.

### Persona 1 — Maya, the JEBS-paper replicator (graduate student)

- **Context.** Read Lee et al. (2024 JEBS) for a methods seminar. Wants to reproduce Figure 4 panel (b) on her laptop in under an hour.
- **Constraints.** Knows tidyverse and can read a Stan file but does not want to read one tonight.
- **Goal.** Generate a single condition matching the JEBS paper, fit a Gaussian model, and reproduce the shrinkage plot.
- **Calls.** `preset_jebs_paper()`, then `sim_multisite()`, then hands the tibble to the existing `Appendix_E.R` script.
- **Wins when.** The output of `sim_multisite()` is the same column layout (`tau_j`, `tau_j_hat`, `se2_j`, `n_j`) as the JEBS appendix code expects, with no renaming required.

### Persona 2 — Dr. Chen, the applied multisite analyst (district researcher)

- **Context.** Has 14 schools in a district-wide tutoring trial. Wants to run a power analysis before next year's randomization.
- **Constraints.** Does not write Stan. Uses `lme4` and `metafor`. Plots with `ggplot2` but only because copy-paste worked.
- **Goal.** Generate plausible site-level data under "what if heterogeneity is modest? substantial?" and pipe the result into `multisitepower::run_sim()`.
- **Calls.** `preset_education_modest()`, `sim_multisite()`, then `multisitepower::run_sim(data = ...)`.
- **Wins when.** The default tibble plugs into `multisitepower` without column renaming, and `?sim_multisite` says exactly which preset matches "modest / moderate / substantial heterogeneity per Weiss et al. (2017)."

### Persona 3 — Priya, the methods researcher (postdoc)

- **Context.** Working on small-area estimation and has a manuscript revision asking for additional simulation conditions over the next 72 hours.
- **Constraints.** Comfortable with `furrr`, `targets`, and writes her own Stan models. Wants programmable defaults and reproducible RNG streams.
- **Goal.** Sweep `I` from 0.05 to 0.70 in steps of 0.05, with 200 reps per cell, in a `targets` pipeline. Wants to inject a custom non-Gaussian `G`.
- **Calls.** `dgp_grid()`, `sim_meta()`, `set_G()` (callback), `with_seed()`.
- **Wins when.** A 1,500-condition × 200-rep grid runs in `furrr::future_map()` with a single composable `.options = furrr_options(seed = TRUE)` and the result is a tidy nested tibble.

### Persona 4 — Prof. Rodriguez, the simulation-study lead (PRIMO PI / team)

- **Context.** Running the funded simulation study in the proposal. Needs to defend every default to the advisory committee.
- **Constraints.** Must produce an OSF-deposit-ready scenario file (1,500 cells × 200 reps) and document provenance for every parameter.
- **Goal.** Run the canonical PRIMO simulation, generate the diagnostic feasibility plot per condition, archive the scenario manifest.
- **Calls.** `dgp_manifest()` (writes/reads YAML), `sim_meta()` (or `sim_multisite()`), `feasibility_index()`, `scenario_audit()`.
- **Wins when.** The scenario manifest is a single YAML file the advisory committee can read and the simulation provenance is reproducible from the manifest hash alone.

### Persona 5 — Sam, the workshop attendee (assistant professor in education)

- **Context.** Attending a 90-minute virtual workshop. Has never installed Stan and may never want to.
- **Constraints.** Can install R packages from CRAN. Will not install GitHub-only packages today.
- **Goal.** Run the live demo notebook end to end and walk away with a Quarto file they can adapt for their own grant proposal next month.
- **Calls.** `sim_multisite()` with all defaults; `plot_effects()`; `summary()`.
- **Wins when.** Not a single line in the workshop notebook errors out on a minimal R install, and `summary()` of the simulated dataset is human-readable on the first try.

These five define the design center. Persona 1 (Maya) and Persona 5 (Sam) drive the API simplicity; Persona 3 (Priya) and Persona 4 (Rodriguez) drive the composability; Persona 2 (Dr. Chen) is the median user we are trying to convert. Persona 1 also handles the "JEBS-paper-replication" deliverable explicitly required by the proposal team.

---

## 2. Hello-world and 5-minute tour

This is the only section a brand-new user will read before deciding whether to keep going. Every line below must work as printed against a clean R 4.4 install with `metaDGP` from CRAN.

### Step 0. Install (once)

```r
# CRAN (preferred)
install.packages("metaDGP")

# Or development version
# install.packages("pak")
pak::pak("primo-lab/metaDGP")
```

### Step 1. Load and generate

```r
library(metaDGP)

set.seed(2562)
df <- sim_multisite()   # all defaults; see ?sim_multisite for what they mean

df
```

What the user sees:

```
# A metaDGP_tbl: 50 sites, paradigm = "multisite"
# I (informativeness) = 0.56  |  R (SE heterogeneity) = 1.22  |  rank_corr(tau_j, se2_j) = 0.00
   site_index  n_j   tau_j  tau_j_hat   se_j   se2_j
        <int> <int>   <dbl>      <dbl>  <dbl>   <dbl>
 1          1    20  -0.241     -0.635  0.447  0.2000
 2          2   161  -0.211     -0.036  0.158  0.0248
 3          3    71   0.590      0.345  0.237  0.0563
 4          4    58   0.439      0.545  0.263  0.0690
 5          5    85  -0.181      0.143  0.217  0.0471
 6          6    33  -0.113     -0.449  0.348  0.1212
 7          7   105  -0.169     -0.228  0.195  0.0381
 8          8   128  -0.161     -0.334  0.176  0.0309
 9          9    84  -0.184     -0.061  0.218  0.0476
10         10    31  -0.054     -0.169  0.359  0.1290
# i 40 more rows. Columns: site_index, n_j, tau_j, tau_j_hat, se_j, se2_j.
# Use summary(df) for a one-page report or attr(df, "scenario") for the manifest.
```

Two things to notice. First, the `print()` method already tells the user what `I` and `R` are for *this* simulated dataset — they don't have to compute it. Second, the column order matches the JEBS appendix and the Walters / Rubin convention; nothing has to be renamed.

### Step 2. Summarize

```r
summary(df)
```

```
metaDGP simulated dataset
-------------------------
Paradigm:           multisite (J = 50 sites)
Preset:             education_modest (Weiss et al. 2017)
Within-site:        nbar = 40, CV = 0.50, nj_min = 5
Between-site:       sigma_tau = 0.20, G = "Gaussian"
Dependence:         precision_dependence = FALSE  (rank_corr = 0)
Realised summaries:
  I  (informativeness)        = 0.56   (target 0.50; "moderate")
  R  (SE heterogeneity ratio) = 1.22   ("modest")
  Spearman(tau_j, se2_j)      = 0.00
  Var(tau_j) / sigma_tau^2    = 0.97   (finite-pop / super-pop)
RNG:                .Random.seed snapshot stored; reproducible via with_seed()
```

The `summary()` is opinionated by design: it reports realised `I`, realised `R`, and realised correlation, because those are the quantities the user *thinks* they specified. We learned in the siteBayes2 era that users routinely set `sigma_tau = 0.25` and were surprised that the realised cross-site SD differed by ten percent.

### Step 3. Plot

```r
plot_effects(df)
```

The default plot is a paired density of `tau_j` (true) and `tau_j_hat` (observed) on the same axis, with a rug. It looks like the JEBS Appendix E density plot — deliberately so, so a reader of that paper recognises it instantly. There is no title; the caller can add one.

```r
plot_funnel(df)        # se_j on y-axis, tau_j_hat on x-axis
plot_dependence(df)    # tau_j vs se2_j; reports realised rank_corr in subtitle
```

### Step 4. Hand off

```r
# Plug into multisitepower (Persona 2)
library(multisitepower)
run_sim(data = df, model = "gaussian")

# Or fit metafor (Persona 1)
library(metafor)
metafor::rma(yi = tau_j_hat, sei = se_j, data = df)

# Or hand off to the existing JEBS appendix code
stan_data <- list(J = nrow(df), tau_j_hat = df$tau_j_hat, se_j = df$se_j)
```

Five minutes. Everything works. No environment variables, no RStan toolchain, no MCMC. The point of the simulator is to be the *easy* layer beneath whatever modeling stack the user picks.

---

## 3. Two opinionated front doors

**Design principle.** The user picks the front door that matches the *noun they would use to describe their problem to a colleague*. If they say "multisite trial," they call `sim_multisite()`. If they say "meta-analysis" or "small-area estimation," they call `sim_meta()`. We resist the temptation to ship a "unified" `sim_dgp()` interface; the unified interface is what the architecture draft can build internally, but it should not be what users see.

### 3.1 `sim_multisite()` — for RCT thinkers

```r
sim_multisite(
  J          = 50,           # number of sites
  nbar       = 40,           # average per-site sample size
  cv         = 0.50,         # coefficient of variation in site sizes
  sigma_tau  = 0.20,         # cross-site SD of true effects
  tau        = 0,            # grand mean
  G          = "gaussian",   # shape of G; see ?distributions
  nj_min     = 5,            # minimum site size
  p          = 0.5,          # treatment proportion within a site
  R2         = 0,            # within-site covariate-explained variance
  precision_dependence = FALSE,
  rank_corr  = 0,
  preset     = NULL,         # if non-NULL, overrides above (see ?presets)
  ...
)
```

**Returned object.** A tibble subclassed `metaDGP_tbl`, with attributes `scenario` (a list) and `manifest_hash` (a string). Columns: `site_index`, `n_j`, `tau_j`, `tau_j_hat`, `se_j`, `se2_j`. If covariates were supplied, those go between `site_index` and `n_j`.

**Default justification.** Each default is chosen so a brand-new user gets *something realistic for an education RCT* on the first call.

| Argument | Default | Why this default |
| --- | --- | --- |
| `J = 50` | 50 sites | Median of Lee et al. (2024) simulation grid; below 50, asymptotic feasibility for `G` is poor (Note 04). |
| `nbar = 40` | 40 students | Roughly the median per-school sample in IES tutoring trials (Weiss et al., 2017). |
| `cv = 0.50` | 0.50 | Lee et al. (2024) midpoint; Persona 4 (Rodriguez) defends this in advisory committee. |
| `sigma_tau = 0.20` | 0.20 | "Modest" heterogeneity per Weiss et al. (2017) Table 1; aligns with effect-size SD ~0.05 - 0.30 range observed in education RCTs. |
| `G = "gaussian"` | Gaussian | Most users expect Gaussian; non-Gaussian shapes are an explicit, separate decision. |
| `precision_dependence = FALSE` | FALSE | Default to the textbook Rubin (1981) assumption; users opt in to dependence consciously (Note 14). |

These defaults reproduce a "moderate" condition from the funded grant's simulation grid; running `summary(sim_multisite())` should yield `I ≈ 0.55`. That's the central scenario the package is built around.

### 3.2 `sim_meta()` — for meta-analysis / SAE thinkers

```r
sim_meta(
  K          = 50,           # number of studies
  I          = 0.50,         # informativeness; replaces sigma_tau as the front control
  R          = 1.5,          # SE heterogeneity ratio (max(se_j) / min(se_j))
  tau        = 0,
  G          = "gaussian",
  se_dist    = "lognormal",  # marginal distribution of se_j
  rank_corr  = 0,
  precision_dependence = FALSE,
  preset     = NULL,
  ...
)
```

**Why a different front-end?** A meta-analyst does not have site sizes. They have studies with reported standard errors, and the natural levers are (a) how *informative* the data are — the I metric — and (b) how *heterogeneous* the standard errors are — the R metric. We want their mental model to map directly onto `I` and `R`, not onto `nbar` and `cv`. Internally, of course, the engine reduces to the same generative process; that is the architecture draft's job.

**Returned object.** Same `metaDGP_tbl` subclass, columns: `study_index`, `tau_j`, `tau_j_hat`, `se_j`, `se2_j`. (Note: the user vocabulary stays `tau_j` even though `K` indexes studies; this is deliberate. We keep one set of names across both paradigms so cross-paradigm code samples are portable.)

### 3.3 Mock-up of `?sim_multisite`

The on-screen help page must fit on one screen. Reviewers consistently dock packages whose top-level help is a wall of text.

```
sim_multisite                package:metaDGP                R Documentation

Simulate a multisite trial dataset

Description:
  Generate (tau_j, tau_j_hat, se_j, n_j) tuples for J sites under
  user-specified between-site heterogeneity (sigma_tau), within-site
  sample sizes, and an optional rank correlation between true effects
  and standard errors. The output is a metaDGP_tbl that plugs directly
  into 'metafor', 'multisitepower', 'baggr', and the JEBS appendix code.

Usage:
  sim_multisite(J = 50, nbar = 40, cv = 0.5, sigma_tau = 0.2,
                G = "gaussian", precision_dependence = FALSE,
                rank_corr = 0, preset = NULL, ...)

Quickstart:
  df <- sim_multisite()
  summary(df)
  plot_effects(df)

Arguments:
  J                      Number of sites (>= 5). Default 50.
  nbar                   Average per-site sample size. Default 40.
  cv                     CV of site sizes. Default 0.5.
  sigma_tau              Cross-site SD of true effects (effect-size units). 0.2.
  G                      Shape of G. One of c("gaussian", "skew", "t",
                         "ALD", "mixture", "twin_towers"). Or a function;
                         see ?set_G.
  precision_dependence   Logical; if TRUE, induce a rank correlation
                         between tau_j and se2_j. Default FALSE.
  rank_corr              Target Spearman correlation if dependence is on.
  preset                 Optional preset name; overrides the above.
                         See ?presets for the list.

See also:
  sim_meta, presets, plot_effects, feasibility_index,
  vignette("getting-started"), vignette("replicating-lee-2024").
```

That's the whole help page. Everything beyond the basics — copula vs. hill-climb, finite-pop vs. super-pop, custom G — lives in vignettes that the See Also block names by hand.

---

## 4. Presets and recipe library

Presets are the cheapest possible adoption mechanism: they let a user say "I want a small modest education trial" and get a defensible scenario with one call. They are also the riskiest: a bad preset becomes the *de facto* default in dozens of unread papers. We bias toward conservative, small set, with explicit citations.

### 4.1 Preset table (v1.0)

| Preset name | Paradigm | J / K | nbar | cv | sigma_tau | I (target) | R (target) | G | rank_corr | Citation / rationale |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `preset_education_small()`     | multisite | 25  | 30  | 0.50 | 0.15 | ~0.30 | 1.0  | gaussian      | 0.0 | Weiss et al. (2017) "small / modest" |
| `preset_education_modest()`    | multisite | 50  | 40  | 0.50 | 0.20 | ~0.55 | 1.2  | gaussian      | 0.0 | Weiss et al. (2017) "modest"; the package default |
| `preset_education_substantial()` | multisite | 80 | 60  | 0.50 | 0.30 | ~0.70 | 1.5  | gaussian      | 0.0 | Weiss et al. (2017) "substantial" |
| `preset_jebs_paper()`          | multisite | 100 | 80 | 0.50 | 0.25 | ~0.56 | 1.4  | mixture(0.3,5,2) | 0.0 | Lee et al. (2024 JEBS) Appendix E (replication) |
| `preset_small_area_estimation()` | meta    | 30  | -- | --   | --   | 0.20  | 3.0  | gaussian      | 0.0 | Northwestern small-area analyses; low I, high R |
| `preset_meta_modest()`         | meta      | 20  | -- | --   | --   | 0.50  | 2.0  | gaussian      | 0.0 | Median meta-analysis cell |
| `preset_walters_2024()`        | multisite | 50  | 40 | 0.50 | 0.20 | ~0.55 | 1.2  | gaussian      | 0.3 | Walters (2024) precision-dependence demo |
| `preset_twin_towers()`         | multisite | 100 | 80 | 0.50 | 0.30 | ~0.70 | 1.4  | twin_towers   | 0.0 | Pedagogical bimodal G (Note 02) |

### 4.2 Use

```r
df <- sim_multisite(preset = "education_modest")        # by name
df <- sim_multisite(preset = preset_education_modest()) # or by call

# Inspect a preset without simulating
preset_education_modest()
#> <metaDGP_preset: education_modest>
#> J = 50; nbar = 40; cv = 0.5; sigma_tau = 0.2; G = "gaussian"
#> Source: Weiss et al. (2017), "modest" heterogeneity.

# List all
list_presets()
#>  preset                    paradigm   J  K   I_target  R_target   G
#>  education_small           multisite  25 -   0.30      1.0        gaussian
#>  education_modest          multisite  50 -   0.55      1.2        gaussian
#>  education_substantial     multisite  80 -   0.70      1.5        gaussian
#>  jebs_paper                multisite 100 -   0.56      1.4        mixture
#>  small_area_estimation     meta       -  30  0.20      3.0        gaussian
#>  meta_modest               meta       -  20  0.50      2.0        gaussian
#>  walters_2024              multisite  50 -   0.55      1.2        gaussian (rho=0.3)
#>  twin_towers               multisite 100 -   0.70      1.4        twin_towers
```

### 4.3 Tradeoffs (deliberately stated for the contributor docs)

- **Adoption + reproducibility.** A named preset becomes a citation handle. Three years from now, "we used `preset_education_modest()` from metaDGP v1.2.0" is more reproducible than "we set sigma_tau = 0.2 etc."
- **Risk of monoculture.** If everyone uses the same preset, simulation studies converge. We mitigate by (a) shipping multiple presets at the same "modest" tier and (b) requiring `summary(df)` to print "Preset: X" so reviewers can spot it.
- **Maintenance debt.** Every preset is an API commitment. We freeze presets at v1.0; new ones go in v1.x as additions only; renames trigger a major version.

---

## 5. Common workflows (recipes / cookbook)

Each recipe is a self-contained block: 2 sentences of context, 8 - 15 lines of code, 1 line interpreting the output. They are the spine of `vignette("cookbook")` and the basis of the workshop demos.

### Recipe 5.1 — Replicate one JEBS simulation cell

This is Persona 1 (Maya). Goal: regenerate the dataset that produced Figure 4 of the JEBS paper, then confirm `I ≈ 0.56`.

```r
library(metaDGP)
set.seed(2562)

df <- sim_multisite(preset = "jebs_paper")
summary(df)
# I = 0.56 | sigma_tau (realised) = 0.27 | G = "mixture"

informativeness(df)
# 0.561
```

Interpretation. The realised `I` matches the JEBS paper's Table 1 cell to two decimals; the dataset is plug-compatible with the JEBS appendix Stan code.

### Recipe 5.2 — Sweep `I` from 0.05 to 0.70 with 200 reps

This is Persona 3 (Priya). Goal: build a 14 x 200 grid for a manuscript revision.

```r
library(metaDGP); library(furrr); plan(multisession)

grid <- expand_grid(I = seq(0.05, 0.70, by = 0.05),
                    rep = 1:200) |>
  mutate(seed = 1e6 + row_number())

results <- grid |>
  mutate(df = future_map2(I, seed,
    ~ with_seed(.y, sim_meta(K = 30, I = .x, R = 2.0)),
    .options = furrr_options(seed = TRUE)))

results
# # A tibble: 2,800 x 4
```

Interpretation. The tibble is nested; `results$df[[i]]` is the i-th simulated dataset. Reproducible by `seed`; identical across machines because the per-cell seed is in the grid, not in `set.seed()`.

### Recipe 5.3 — Inject a 0.3 Spearman dependence between `se2_j` and `tau_j`

This addresses Walters (2024) and Note 14. We want to demonstrate that the canonical Rubin (1981) shrinkage formulas are biased when this dependence is non-zero.

```r
df_indep <- sim_multisite(J = 50, sigma_tau = 0.2,
                          precision_dependence = FALSE)
df_dep   <- sim_multisite(J = 50, sigma_tau = 0.2,
                          precision_dependence = TRUE,
                          rank_corr = 0.3)

cor(df_indep$tau_j, df_indep$se2_j, method = "spearman")  # 0.00
cor(df_dep$tau_j,   df_dep$se2_j,   method = "spearman")  # 0.29
plot_dependence(df_dep)
```

Interpretation. `df_dep` exhibits the rank correlation we asked for, within `tol = 0.01`. Using the shrinkage estimator on `df_dep` will systematically misestimate `sigma_tau` (Note 14, Section 3).

### Recipe 5.4 — Generate small-area-estimation data for a single year

This is the Northwestern use case. Small `K`, low `I`, high `R`.

```r
df_sae <- sim_meta(K = 30, I = 0.20, R = 3.0,
                   se_dist = "lognormal", G = "gaussian")
summary(df_sae)
# I = 0.20 | R = 3.04 | range(se_j) = [0.07, 0.21]

plot_funnel(df_sae)
```

Interpretation. The funnel shows the heavy SE heterogeneity characteristic of SAE; downstream estimators that assume homogeneous SEs (e.g., naive REML) will be miscalibrated.

### Recipe 5.5 — Plug into `metafor::rma()` and `multisitepower::run_sim()`

This is Persona 2 (Dr. Chen). The point is "no column renaming."

```r
library(metafor); library(multisitepower)

df <- sim_multisite(preset = "education_modest")

# metafor: yi and sei
fit <- rma(yi = tau_j_hat, sei = se_j, data = df, method = "REML")
fit
# tau^2 = 0.038, I^2 = 71%, ...

# multisitepower: pass the data frame directly
run_sim(data = df, model = "gaussian", n_sims = 100)
```

Interpretation. The `metaDGP_tbl` has the `yi`, `sei` columns under their canonical names; nothing has to be renamed for either downstream package.

### Recipe 5.6 — Hand-supply a custom `G` via callback

This is Persona 3 again. Goal: simulate from an arbitrary user-supplied distribution while keeping the rest of the pipeline intact.

```r
my_G <- function(J, sigma_tau, ...) {
  # 80% Normal(0, 0.5), 20% Normal(2, 1), then rescale to sigma_tau
  z <- ifelse(runif(J) < 0.8,
              rnorm(J, 0, 0.5), rnorm(J, 2, 1))
  sigma_tau * (z - mean(z)) / sd(z)
}

df <- sim_multisite(J = 100, sigma_tau = 0.3, G = set_G(my_G))
summary(df)
plot_effects(df)
```

Interpretation. Custom `G`s plug in without touching the SE generation or the rank-correlation machinery; the package does no validation of `G`'s shape, by design.

### Recipe 5.7 — Simulate a "Twin Towers" bimodal `G`

A pedagogical scenario for the workshop: bimodal site effects with `J = 100`. Useful for showing how a Gaussian model collapses the modes.

```r
df <- sim_multisite(preset = "twin_towers")
plot_effects(df)
# two clear modes around -0.2 and +0.3
```

Interpretation. This is the canonical "Gaussian-misspecified" scenario; the JEBS paper's DPM analysis was motivated by exactly this shape.

### Recipe 5.8 — Compute empirical `I` and feasibility for a dataset

For Persona 4 (Rodriguez): reporting realised informativeness and feasibility per condition.

```r
df <- sim_multisite(preset = "education_modest")

informativeness(df)            # 0.55
heterogeneity_ratio(df)        # R = 1.21
feasibility_index(df,
                  G_truth = attr(df, "scenario")$G,
                  reps = 200)  # FI = 0.87 (high)
```

Interpretation. The feasibility index follows Note 04: 0.87 is "high feasibility" in the four-tier interpretation, meaning the recovered `G` should track the truth closely under a flexible estimator.

---

## 6. Argument design and defaults

### 6.1 Canonical reference table

This table is the spec. It also lives at `?metaDGP_arguments` and in the cheatsheet PDF.

| Argument | Type | Default | Range / values | What it controls | When to change |
| --- | --- | --- | --- | --- | --- |
| `J` | int | 50 | 5 - 5,000 | Number of sites in `sim_multisite()` | Match your trial; below 25, expect feasibility issues for non-Gaussian `G` |
| `K` | int | 50 | 5 - 5,000 | Number of studies in `sim_meta()` | Match your meta-analysis K |
| `nbar` | num | 40 | 5 - 10,000 | Average per-site n | Education 30-100; biomedical 100-1,000 |
| `cv` | num | 0.50 | 0 - 2 | CV of site sizes; 0 = equal sizes | 0.5 is typical; 0 for clean balanced designs |
| `nj_min` | int | 5 | 1 - nbar | Minimum per-site n | Don't change unless you know why |
| `sigma_tau` | num | 0.20 | 0 - 1 | Cross-site SD of true effects (effect-size units) | Per Weiss et al. (2017): 0.05 small, 0.20 modest, 0.30 substantial |
| `tau` | num | 0 | unrestricted | Grand mean | Set to your hypothesized average effect |
| `I` | num | 0.50 | 0 - 1 | Informativeness; only in `sim_meta()` | Lower = more shrinkage; SAE uses ~0.2 |
| `R` | num | 1.5 | 1 - 10 | max(se_j)/min(se_j) ratio | 1 = homoskedastic; SAE uses ~3 |
| `G` | str/fun | "gaussian" | gaussian, t, skew, ALD, mixture, twin_towers, or function | Shape of cross-site distribution | Twin-towers / mixture for non-Gaussian sensitivity analysis |
| `p` | num | 0.5 | 0.05 - 0.95 | Treatment proportion within site | Match your design (often 0.5 in RCTs) |
| `R2` | num | 0 | 0 - 0.95 | Within-site covariate-explained variance | Set if you have within-site covariates |
| `precision_dependence` | logi | FALSE | TRUE/FALSE | Whether to induce dependence between `tau_j` and `se2_j` | TRUE for Walters (2024) -style sensitivity |
| `rank_corr` | num | 0 | -1 to 1 | Target Spearman correlation if dependence on | 0.3 is "moderate" per the literature |
| `pearson_corr` | num | NULL | -1 to 1 | Target Pearson correlation (copula path) | Use copula instead of hill-climb (Note 24) |
| `dependence_method` | str | "hill_climb" | "hill_climb", "copula" | Which dependence algorithm | Copula for tail dependence; hill-climb for marginal preservation |
| `seed` | int | NULL | any int or NULL | Reproducibility | Always set in production runs |
| `preset` | str/list | NULL | name or `dgp_preset()` call | Override-everything escape hatch | Use for replication, teaching |

### 6.2 Renames and deprecations from siteBayes2

The current `sim_multisite_data()` ships ~25 named arguments, most of them mixing layers. Three concrete rename / deprecation calls:

- **`sigma_tau` stays.** Not `between_site_sd`. Reason: `sigma_tau` matches the Stan code in the JEBS appendix and the notation in Lee et al. (2024). Verbosity is not clarity here; `sigma_tau` is the literal Greek that appears in the paper. We add a `?sigma_tau` topic that explains the symbol once.
- **`true_dist = "Gaussian"` -> `G = "gaussian"`.** Reason: the user's vocabulary is "G," matching Note 02 and the methods literature. Lowercase to match `family` conventions in `glm()`. `true_dist` triggers `lifecycle::deprecate_warn()` for one minor cycle.
- **Drop `variance` argument.** Reason: in siteBayes2 it sits at the prior-distribution layer (variance of the within-prior dispersion before scaling by `sigma_tau`), and 100% of users we shadowed conflated it with `sigma_tau^2`. We hard-code `variance = 1` in v1.0 and re-expose it (renamed `prior_scale`) only if Persona 3 (Priya) demands it, in v1.1.
- **Drop `formula`, `beta`, `data` from the top level.** Reason: they belong on a separate `add_covariates()` call. The top-level function keeps four tabular inputs maximum.
- **`set_seed = TRUE` -> remove.** Reason: setting the seed inside the function silently overwrites the user's RNG state — a known footgun. The new convention is `with_seed(123, sim_multisite(...))` or pass `seed = 123`, which restores the RNG state on exit.
- **`max_iter`, `tol` -> hide behind `dependence_method`.** Reason: these are tuning knobs of the hill-climb algorithm; users should not see them. They live in `dgp_control(max_iter, tol)` for power users.
- **Add `paradigm` attribute on the returned object.** Reason: downstream methods (`summary.metaDGP_tbl`, `plot_effects`) dispatch on it.

The bottom line: **the siteBayes2 API has 25 arguments arranged in three layers and exposes them all; metaDGP keeps eight at the top level and demotes the rest to a `dgp_control()` sidecar**. That is the single biggest UX change.

---

## 7. Output design and plotting

### 7.1 The returned object

A `metaDGP_tbl` is a regular tibble plus three attributes:

- `attr(df, "scenario")`: a list with all input arguments, the realised `I`, `R`, and `rank_corr`, the `G` callback or string, and the package version.
- `attr(df, "manifest_hash")`: a SHA-256 of the scenario list, useful for reproducibility manifests on OSF.
- `attr(df, "paradigm")`: "multisite" or "meta".

A user doing `df$tau_j_hat` gets exactly what they expect; everything else is opt-in.

### 7.2 `print.metaDGP_tbl`

Same as Step 1 of the 5-minute tour. The first line says paradigm and J/K; the second line shows realised `I`, `R`, and `rank_corr`; the body is the tibble.

### 7.3 `summary.metaDGP_tbl`

Same as Step 2 of the 5-minute tour. One screen, no scrolling. The structure is `Paradigm / Preset / Within-site / Between-site / Dependence / Realised summaries / RNG`.

### 7.4 Three default plots (function signatures)

```r
plot_effects(df, type = c("density", "caterpillar"),
             show = c("tau_j", "tau_j_hat"),
             ...) -> ggplot

plot_funnel(df, log_se = FALSE, ref_line = TRUE, ...) -> ggplot

plot_dependence(df, method = c("spearman", "pearson"),
                add_smoother = TRUE, ...) -> ggplot
```

**`plot_effects()`.** Default = density of `tau_j` (true) and `tau_j_hat` (observed) in the same panel, with a rug; subtitle reports `sigma_tau` and realised cross-site SD. With `type = "caterpillar"`, sites are sorted by `tau_j_hat` and shown with `+/- 1.96 * se_j` bars. This matches the JEBS appendix density plot.

**`plot_funnel()`.** A meta-analysis funnel plot: `tau_j_hat` on x, `se_j` on y (inverted). With `ref_line = TRUE`, draws the +/- 1.96 reference cones around `tau`. Subtitle reports realised `R`.

**`plot_dependence()`.** Scatterplot of `tau_j` vs `se2_j` with optional GAM smoother and the realised correlation in the subtitle. This is the diagnostic for Recipe 5.3.

All three return ggplot objects; users add titles / themes via `+`. We do not ship `theme_metaDGP()`. We do ship a `+ scale_color_metaDGP()` for two-color contrasts (true vs. observed) so workshop slides are colorblind-safe by default.

---

## 8. Help, vignettes, and pkgdown plan

### 8.1 Top-level help — `?metaDGP`

```
metaDGP                  package:metaDGP                  R Documentation

metaDGP: Simulator for meta-analytic and multisite-trial datasets

Description:
  Generate (tau_j, tau_j_hat, se_j, n_j) tuples under flexible
  cross-site distributions G, controllable informativeness I,
  controllable SE heterogeneity R, and optional precision dependence.
  metaDGP is the data-generating engine behind the PRIMO
  simulation study (IES R305D240078) and is designed to plug
  into 'metafor', 'multisitepower', 'baggr', and 'rstan' workflows.

Two front doors:
  sim_multisite(J, nbar, cv, sigma_tau, ...)     for RCT thinkers
  sim_meta     (K, I, R, ...)                    for meta-analysts

Quickstart:
  df <- sim_multisite()
  summary(df); plot_effects(df)

See vignette("getting-started") for a 5-minute tour.

Index of vignettes:
  getting-started
  multisite-vs-meta
  replicating-lee-2024
  custom-G
  precision-dependence
  plug-in-multisitepower
  calibrating-to-real-data

Citation:
  citation("metaDGP")
```

### 8.2 Vignette plan

| # | Vignette | Audience | Length | Anchor figure |
| --- | --- | --- | --- | --- |
| V1 | Getting started | Sam (workshop attendee) | 1,500 words | `plot_effects()` of default scenario |
| V2 | Site-size DGPs vs. heterogeneity DGPs | Anyone deciding `sim_multisite` vs `sim_meta` | 2,000 words | Side-by-side funnel plots from each paradigm |
| V3 | Replicating Lee et al. (2024) | Maya (replicator) | 3,000 words | Reproduction of JEBS Figure 4(b) |
| V4 | Custom G distributions | Priya (methods) | 2,500 words | Twin-towers density + Gaussian-misspecified posterior |
| V5 | Precision-effect dependence: rank vs. copula | Methods + advisory committee | 2,500 words | Dependence scatter + bias of `sigma_tau` hat |
| V6 | Plugging into multisitepower / metafor / baggr | Dr. Chen (applied) | 2,000 words | Power curve from `multisitepower::run_sim()` |
| V7 | Calibrating to real-world data | Persona 4 (PI) | 2,000 words | Empirical-vs-simulated funnel overlay |

Each vignette opens with the same template: target audience callout, the one figure that anchors it, then 2 - 4 sections of code + prose. We pre-render with `knitr::knit_global()` in a CI job so pkgdown ships them as static HTML even on reviewer machines without Stan.

### 8.3 pkgdown structure

```
metaDGP.org/
  Get started     -> V1
  Articles
    Concepts      -> V2, V5
    Replications  -> V3
    Recipes       -> V6, V7, cookbook
    Advanced      -> V4
  Reference
    Front doors   -> sim_multisite, sim_meta
    Presets       -> list_presets, preset_*
    Diagnostics   -> informativeness, heterogeneity_ratio,
                     feasibility_index, scenario_audit
    Plotting      -> plot_effects, plot_funnel, plot_dependence
    Internal      -> set_G, with_seed, dgp_control
  News
  Citation
```

The `Reference` section is grouped by *function family*, not by alphabet, because alphabetical reference indexes are user-hostile.

---

## 9. Error messages and validation UX

Every error message in metaDGP follows three rules:
1. State what the user passed (`✖`).
2. State what was expected (`ℹ`).
3. Offer a one-line fix (`→`).

We use `cli::cli_abort()` and `cli::cli_inform()` exclusively. Ten worked examples below.

### Example 1 — Bad `J`

```r
sim_multisite(J = 1)
```

```
✖ `J` must be at least 5; you passed 1.
ℹ At least 5 sites are needed for any meaningful between-site variance.
→ Try J = 25 (small trial) or J = 50 (default).
Run `?sim_multisite` for guidance.
```

### Example 2 — `sigma_tau` outside reasonable range

```r
sim_multisite(sigma_tau = 5)
```

```
✖ `sigma_tau` is in effect-size units; you passed 5, which corresponds to
  cross-site SD of 5 standard deviations.
ℹ Per Weiss et al. (2017), education effect-size SDs are typically 0.05 to 0.40.
  Values above 1.0 are almost certainly a unit-of-measurement mistake.
→ If you really mean 5, set `sigma_tau = 5, override_unit_check = TRUE`.
```

### Example 3 — `rank_corr` set but `precision_dependence = FALSE`

```r
sim_multisite(precision_dependence = FALSE, rank_corr = 0.3)
```

```
ℹ You set `rank_corr = 0.3` but `precision_dependence = FALSE`,
  so `rank_corr` will be ignored.
→ Set `precision_dependence = TRUE` to induce dependence,
  or remove `rank_corr` to silence this message.
```

(Note: this is a `cli_inform()`, not an `abort()`. Silent no-ops are worse than messages.)

### Example 4 — Unknown preset

```r
sim_multisite(preset = "modest")
```

```
✖ No preset named "modest".
ℹ Did you mean "education_modest"?
→ Run `list_presets()` to see all 8 v1.0 presets.
```

### Example 5 — Custom `G` returns wrong length

```r
bad_G <- function(J, sigma_tau, ...) rnorm(J + 1)
sim_multisite(G = set_G(bad_G), J = 50)
```

```
✖ Your custom `G` returned 51 values; expected exactly J = 50.
ℹ Custom G callbacks must return a numeric vector of length J.
→ Check the `J` argument inside your function. See ?set_G for the
  callback contract.
```

### Example 6 — Hill-climb did not converge

```r
sim_multisite(precision_dependence = TRUE, rank_corr = 0.95, J = 8)
```

```
! Hill-climb did not reach `tol = 0.01` of `rank_corr = 0.95` after 20,000 iterations.
ℹ Realised correlation: 0.78. With J = 8, |rank_corr| > 0.85 is often
  not achievable due to the combinatorial discreteness of permutations.
→ Try J >= 25, or use `dependence_method = "copula"`.
```

### Example 7 — Trying to use both `pearson_corr` and `rank_corr`

```r
sim_multisite(precision_dependence = TRUE,
              rank_corr = 0.3, pearson_corr = 0.3)
```

```
✖ You specified both `rank_corr` and `pearson_corr`. Pick one.
ℹ `rank_corr` triggers the hill-climb (Spearman); `pearson_corr` triggers
  the Gaussian copula (Pearson). They optimise different objectives.
→ Use `dependence_method = "hill_climb"` + `rank_corr`,
  or `dependence_method = "copula"` + `pearson_corr`.
```

### Example 8 — Mixing meta and multisite arguments

```r
sim_meta(K = 30, sigma_tau = 0.2, nbar = 40)
```

```
✖ `sim_meta()` does not take `nbar` or `sigma_tau`; you passed both.
ℹ `sim_meta()` parameterises by I (informativeness) and R (SE heterogeneity).
  `sigma_tau` and `nbar` are arguments of `sim_multisite()`.
→ Use `sim_multisite(J = 30, sigma_tau = 0.2, nbar = 40)`,
  or translate to `sim_meta(K = 30, I = ..., R = ...)`.
  See vignette("multisite-vs-meta") for the mapping.
```

### Example 9 — `nj_min` >= `nbar`

```r
sim_multisite(nbar = 10, nj_min = 15)
```

```
✖ `nj_min` (15) is greater than `nbar` (10). The Gamma site-size
  generator cannot satisfy both.
ℹ The minimum site size must be less than the mean site size.
→ Lower nj_min (e.g., 5) or raise nbar (e.g., 40).
```

### Example 10 — Stale random seed warning

```r
df <- sim_multisite()  # without set.seed()
```

```
ℹ No `seed` argument supplied and `.Random.seed` snapshot taken at runtime.
  This run is reproducible only if you save attr(df, "manifest_hash").
→ For reproducibility in production, prefer
  `with_seed(2562, sim_multisite())` or `sim_multisite(seed = 2562)`.
```

These messages mean the user never has to run `traceback()` to diagnose a misuse. Every error closes with a `→` line; that is a hard requirement enforced by a CI lint.

---

## 10. Onboarding and dissemination plan

This section maps directly onto the IES Performance Agreement (R305D240078, items A.5, A.6, B.6, B.7) which obligates: 5 - 7 videos, workshops, OSF preregistration, Qualtrics feedback, and ERIC submission. We treat each obligation as a UX deliverable.

### 10.1 30-minute productive-onboarding flow

A first-time user, starting from the project landing page, hits productivity in 30 minutes via this funnel:

1. **0 - 2 min.** Land on `metaDGP.org`. Hero block has the 5-minute tour code right there, copyable. No marketing copy.
2. **2 - 5 min.** `install.packages("metaDGP")`; `library(metaDGP)`; run hello-world. Plot appears.
3. **5 - 15 min.** Follow vignette V1 (Getting started). Try one preset. Try `sim_meta()` once.
4. **15 - 25 min.** Watch Video 1 (5-minute tour) and Video 2 (multisite vs. meta). Each video has a paired Quarto file the viewer can open.
5. **25 - 30 min.** Adapt one cookbook recipe to their own scenario (Recipe 5.5 if applied; Recipe 5.2 if methods).

We instrument the docs with simple page-view analytics (Plausible, no cookies) to confirm this funnel works in practice. The 30-min target is aggressive but achievable because the install graph is shallow.

### 10.2 Video plan (5 - 7 x 15 min)

The Performance Agreement says 5 - 7 videos at 15 minutes each. Proposed lineup:

| # | Title | One-line outline |
| --- | --- | --- |
| 1 | metaDGP in 5 minutes: your first simulated trial | Install, hello-world, summary, plot, hand-off to metafor. |
| 2 | Two front doors: `sim_multisite` vs. `sim_meta` | When to use which; live coding both; mapping `sigma_tau` <-> `I, R`. |
| 3 | Presets: from "small" to "substantial" with one call | Walk through `list_presets()`; show the Weiss et al. (2017) table; live-modify a preset. |
| 4 | Beyond Gaussian: Twin Towers, mixtures, and custom G | Show twin-towers; show what a Gaussian model does to bimodal G; the `set_G()` callback. |
| 5 | Precision dependence: when assumptions break | Walters (2024) demo; rank-corr vs. copula; bias of `sigma_tau` hat. |
| 6 | Hooking metaDGP into your existing pipeline | Plug-ins for multisitepower, metafor, baggr; targets pipeline; furrr parallelism. |
| 7 | The PRIMO simulation study: a behind-the-scenes tour | (Year 2 video.) How we use metaDGP for the funded grant; advisory-committee discussion of defaults. |

Each video ships with: (a) a paired Quarto notebook in `inst/videos/`; (b) closed captions; (c) a chapter list. Videos 1 - 4 ship with v1.0; 5 - 6 with v1.1; 7 with v1.2 / final report.

### 10.3 Workshop session outline (90 min)

Designed for the IES PI meeting and for AERA / SREE submissions.

```
00:00 - 00:10  Why simulators? (Persona vignette: meet Maya, Dr. Chen, Priya.)
00:10 - 00:25  Hands-on: install + 5-minute tour. Everyone runs the hello-world.
00:25 - 00:40  Front doors: choose your paradigm. Pair exercise: pick a preset.
00:40 - 00:55  Recipes: one applied (Dr. Chen's power analysis), one methods (Priya's sweep).
00:55 - 01:10  Diagnostics: I, R, feasibility index. When does a scenario "work"?
01:10 - 01:25  Bring your own data: sketching a calibration to a real trial.
01:25 - 01:30  Wrap. Pointer to OSF preregistration; pointer to Qualtrics feedback link.
```

The exact script is checked in at `inst/workshops/90min/`. We rehearse with three colleagues before any public delivery.

### 10.4 OSF deposit checklist

Per Performance Agreement A.1 ("Preregister simulation study at osf.io"). Checklist for every release:

- [ ] OSF project page links to `metaDGP` GitHub repo.
- [ ] Preregistration document references the *exact* metaDGP version (`packageVersion("metaDGP")`).
- [ ] Scenario manifest YAML uploaded as a Component, hash-matched to the manifest hash printed by the simulation run.
- [ ] Replication script runs end-to-end on a clean R install (CI verifies).
- [ ] CITATION file in repo points to OSF DOI and Zenodo DOI.
- [ ] README badge: "Preregistered on OSF: doi:10.17605/OSF.IO/XXXXX".
- [ ] Performance Agreement IES funding statement appears in README, CITATION, and pkgdown footer.
- [ ] Final results (manuscript + appendix) re-deposited to OSF and ERIC at submission.

### 10.5 Qualtrics feedback questions (end-user survey)

Per Performance Agreement A.6 / B.7. These twelve questions go to anyone who runs `metaDGP::feedback_link()` or follows the pkgdown footer link:

1. Which persona best describes you? (Maya / Dr. Chen / Priya / Rodriguez / Sam / Other)
2. How did you find metaDGP? (CRAN / GitHub / paper / workshop / colleague / other)
3. How long from `install.packages` to your first plot? (<5 min / 5-15 / 15-30 / 30-60 / >60)
4. Which front door did you reach for first? (`sim_multisite` / `sim_meta`)
5. Did you use a preset, and if so, which?
6. Which downstream package(s) did you hand off to? (metafor / multisitepower / baggr / rstan / nimble / custom / none)
7. Any defaults that surprised you? (free text)
8. Any error messages that didn't help you? (free text)
9. Any vignette that you wished existed?
10. Would you recommend metaDGP to a colleague? (1 - 10)
11. Are you planning to cite metaDGP in a paper, talk, or grant?
12. Anything else? (free text)

The survey is short, persona-aware, and explicitly solicits negative feedback (Q7, Q8). We commit to publishing aggregated results in the Year 2 APR and acknowledging recurring complaints in `NEWS.md`.

---

## 11. Versioning, contribution, and governance

### 11.1 SemVer plan

- **0.x.y (pre-1.0).** API may change between any two minor versions. Each `NEWS.md` entry tagged BREAKING gets a `lifecycle::deprecate_warn()` for one cycle.
- **1.0.0.** Frozen API for: function names of the two front doors, eight top-level arguments each, the eight presets, the three plotting helpers, and the column names of the returned tibble. Frozen until v2.0.
- **1.x.y.** Additive only. New presets, new `G`, new diagnostics, new plotting helpers — all OK. Deprecations only by `deprecate_warn` for two minor cycles, then `deprecate_stop`, then removal at v2.0.
- **2.0.0.** Reserved for: a renamed front-door, a different default, or a column-rename. We do not get to v2.0 in the funded period.

### 11.2 Contribution guide (`CONTRIBUTING.md`)

Three sections:
1. **Bug reports.** Reproducible example via `reprex::reprex()`. Required: `sessionInfo()`, the scenario list (`attr(df, "scenario")`), the manifest hash.
2. **New presets.** Must cite a published source for the parameter values; must include a one-line "when to use" docstring; must pass the preset-conformance test (a `testthat` snapshot).
3. **New `G` distributions.** Must implement the `set_G()` callback contract; must include a vignette section under V4; must include a `feasibility_index()` smoke test.

### 11.3 Issue templates

- `bug_report.md` — traceback, reprex, scenario hash.
- `feature_request.md` — which persona benefits, which existing recipe is closest.
- `preset_proposal.md` — citation, parameter values, target `I`/`R`.
- `g_distribution_proposal.md` — closed-form expression or callback, intended use.

### 11.4 Code of conduct

Standard Contributor Covenant 2.1, with a project-specific paragraph that we cite the Performance Agreement and the funding acknowledgment in *every* downstream artifact — papers, talks, posts, slides. Non-negotiable.

### 11.5 Citation file (`CITATION.cff` and `inst/CITATION`)

```
preferred-citation:
  type: software
  title: "metaDGP: A Simulator for Meta-Analytic and Multisite-Trial Datasets"
  authors:
    - family-names: Lee
      given-names: JoonHo
    - family-names: <track A author>
    - family-names: <track B author>
  doi: 10.5281/zenodo.XXXXXXX
  version: "1.0.0"
  year: 2026
  note: "R package version 1.0.0; IES R305D240078"
  url: https://github.com/primo-lab/metaDGP
```

Plus a `references` block with the JEBS paper, Weiss et al. (2017), Walters (2024), and the IES funding acknowledgement string verbatim from the Performance Agreement section IV.A.

### 11.6 Release cadence

- Pre-1.0: roughly monthly minor releases during the development sprint.
- 1.0: one release tied to the OSF preregistration and the Year-1 APR.
- 1.x: every quarter, paired with the advisory committee meetings.

Each release: tag, GitHub release notes, Zenodo DOI mint, `NEWS.md` entry, CRAN submission. Pre-release we run a 7-day preview branch with `pak::pkg_install("primo-lab/metaDGP@preview")` to let the advisory committee kick the tires.

### 11.7 Pre-1.0 vs. post-1.0 stability promise (the README block)

```
> metaDGP is currently 0.x. Function names and defaults may change between
> minor versions. We freeze the API at v1.0.0 (target: <date>). Code that
> runs against v1.0.0 will continue to run against any v1.x.y; we will not
> remove functions or rename arguments inside the v1 series. Track:
> NEWS.md, the OSF preregistration, and the version bumps in CITATION.
```

---

## 12. Risks to adoption and mitigations

These are concrete failure modes, not generic worries. Each has an owner and a mitigation.

### Risk 1 — The install fails because of upstream Stan / NIMBLE deps

**Failure mode.** A workshop attendee runs `install.packages("metaDGP")` and gets a C++ toolchain error. They give up.
**Why it would happen.** Earlier siteBayes2 versions soft-imported Stan, which dragged in `rstan`/Rtools/Xcode dependencies on first use.
**Mitigation.** metaDGP has *zero* hard dependencies on Stan, NIMBLE, or any Bayesian backend. We import `stats`, `tibble`, `cli`, `lifecycle`, `rlang`, `vctrs`, `ggplot2`. The handoff to `multisitepower` / `metafor` / `baggr` is via `Suggests`. The CI matrix tests on a base R install with *only* CRAN packages.

### Risk 2 — Users find `I` and `R` unfamiliar and reach for the multisite door even when they want a meta-analysis

**Failure mode.** A meta-analyst opens `?sim_meta`, sees `I = 0.5` and `R = 1.5`, doesn't recognise the parameterisation, gives up.
**Mitigation.** Vignette V2 ("multisite-vs-meta") is a side-by-side reference. The error message in Example 8 above explicitly translates between the two. The cookbook shows both doors generating the *same* dataset for a worked example.

### Risk 3 — Defaults become a citation monoculture

**Failure mode.** Five years from now, every multisite paper cites `sim_multisite()` with default arguments and the literature converges to `J = 50, sigma_tau = 0.2`.
**Mitigation.** `summary()` always prints "Preset: X" or "Preset: NULL (custom)." Reviewers can see in five seconds whether a paper used defaults. The `scenario_audit()` function emits a one-page report that reviewers can compare across submissions. The cheatsheet recommends sweeping at least three presets in any published simulation study.

### Risk 4 — IES branding requirements break casual re-distribution

**Failure mode.** A teaching colleague embeds a vignette in a tutorial and forgets the funding acknowledgement; the IES program officer raises it at the quarterly call.
**Mitigation.** The funding statement is automatically appended to `citation("metaDGP")`, to the pkgdown footer, and to the README. The `metaDGP_about()` function prints the statement on demand. We do *not* ship the IES logo (per Performance Agreement IV.A.2) and we are explicit about that in the README.

### Risk 5 — Cross-paradigm column names collide with downstream packages

**Failure mode.** A user runs `metafor::rma(yi = tau_j_hat, sei = se_j, data = df)` and gets a column-name clash because `df` has a `yi` column that is something else.
**Mitigation.** The returned tibble has `tau_j_hat` and `se_j` only; we do *not* alias to `yi`/`sei` (that would cause Risk 5 the other direction). The cookbook recipe (5.5) shows the explicit named-argument call. The `as_metafor()` helper generates the renamed version if a user really wants it.

### Risk 6 — The hill-climb dependence algorithm doesn't converge for extreme `rank_corr` and silently returns the closest match

**Failure mode.** Persona 3 (Priya) sets `rank_corr = 0.9` with `J = 30`, gets back a dataset with realised correlation 0.7, doesn't notice, runs 200 reps, writes a paper.
**Mitigation.** Error message Example 6. `summary()` always reports the realised correlation. `scenario_audit()` flags any condition where realised correlation is more than `tol` from target.

### Risk 7 — pkgdown site goes down or moves and the Performance Agreement requires it

**Failure mode.** GitHub Pages outage or org rename breaks the canonical URL during a peer review.
**Mitigation.** Mirror at `metaDGP.org` (CNAME to GitHub Pages) and at OSF (component "Documentation snapshot"). Releases include a static HTML tarball as an artifact. README has all three URLs.

### Risk 8 — Advisory committee disagrees with a key default after the v1.0 freeze

**Failure mode.** At the Year 2 advisory meeting, a committee member objects to `sigma_tau = 0.20` as too low for STEM trials.
**Mitigation.** v1.0 freeze is the *front-door* defaults; presets can be added freely. We add `preset_stem_modest()` in v1.1 with the higher `sigma_tau`, document the rationale in `NEWS.md`, and the front-door default does not move.

### Risk 9 — A user blames metaDGP for a downstream estimator's misbehavior

**Failure mode.** A user runs `multisitepower::run_sim(data = sim_multisite(...))`, gets nonsensical power estimates, and files an issue against metaDGP.
**Mitigation.** The error-routing FAQ in vignette V6 has a paragraph titled "Whose bug is this?" with a short flowchart. The `scenario_audit()` function reports realised values, which usually localises the issue.

### Risk 10 — Internationalization (Korean / Spanish) for the workshop audience

**Failure mode.** A Korean-language workshop participant cannot follow English error messages.
**Mitigation.** Out of scope for v1.0 but tracked. We use `cli` exclusively, which has translation hooks; the translation file is a CSV in `inst/po/` that volunteers can fill in.

---

## Closing

The architecture draft will deliver an engine that can simulate any sensible scenario in the meta-analytic and small-area-estimation literature. The statistics draft will deliver the rigor: copula vs. hill-climb, finite vs. super-population, feasibility indices. This UX draft delivers the part that decides whether anyone uses any of it: two front doors that match how users describe their problems, eight presets that match how reviewers describe scenarios, twelve cookbook recipes that match how working researchers actually write code, and ten error messages that fix themselves. The Performance Agreement obligations — videos, workshops, OSF, Qualtrics, ERIC — fall out as natural consequences of these design decisions, not as a separate stream of work.

If we get this draft right, Persona 5 (Sam) gets a plot in five minutes, Persona 1 (Maya) reproduces the JEBS paper in an evening, and Persona 4 (Rodriguez) has a one-page scenario audit to wave at the advisory committee. That is the package metaDGP needs to be.
