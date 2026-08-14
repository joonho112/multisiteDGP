# metaDGP — Statistical Rigor & Methodological Foundations (Draft B)

A simulator for meta-analytic datasets: $(\hat{\tau}_j, \widehat{se}_j)$ pairs under controlled site-specific
heterogeneity, sampling-error structure, and precision-effect dependence. This draft focuses on
the *statistics* of metaDGP. It pins down the model, the parameterizations, the algebra
connecting them, the diagnostics, and the open methodological questions. Architecture and UX
choices are deferred to the companion drafts.

Notation matches the user vocabulary verbatim: `tau_j`, `tau_j_hat`, `se_j`, `se2_j`,
`sigma_tau`, `tau`, `G`, `I`, `R` (heterogeneity ratio), `R2`, `rank_corr`, `pearson_corr`,
`precision_dependence`, `shrinkage`, `nj_mean`, `cv`, `nj_min`, `p`. Equations from Lee et al.
(2024) JEBS are cited by paper-equation number, e.g. JEBS Eq. (1).

---

## Table of Contents

1. [Statistical model and notation](#1-statistical-model-and-notation)
2. [The G distributions](#2-the-g-distributions)
3. [Site sizes and the κ formula](#3-site-sizes-and-the-κ-formula)
4. [The two heterogeneity dials: σ_τ vs. (I, R)](#4-the-two-heterogeneity-dials-σ_τ-vs-i-r)
5. [Precision–effect dependence: rank vs. copula](#5-precisioneffect-dependence-rank-vs-copula)
6. [Diagnostics and validation](#6-diagnostics-and-validation)
7. [Estimands the simulator supports](#7-estimands-the-simulator-supports)
8. [Calibration to real-world ranges](#8-calibration-to-real-world-ranges)
9. [Statistical regression tests](#9-statistical-regression-tests)
10. [Connection to downstream estimators (PM, CB, GR, DP-mixture)](#10-connection-to-downstream-estimators-pm-cb-gr-dp-mixture)
11. [Statistical pitfalls / known caveats](#11-statistical-pitfalls--known-caveats)
12. [Open methodological questions](#12-open-methodological-questions)

---

## 1. Statistical model and notation

metaDGP generates one realized dataset

$$
\mathcal{D} \;=\; \bigl\{(\hat{\tau}_j,\, \widehat{se}_j^{\,2}, n_j)\bigr\}_{j=1}^{J},
$$

together with the *latent* draws that produced it,

$$
\mathcal{L} \;=\; \bigl\{\tau_j\bigr\}_{j=1}^{J} \,\subset\, \mathbb{R}.
$$

It supports two paradigms for generating $(\tau_j, \widehat{se}_j^{\,2})$. Both share the same
likelihood at the top level.

### 1.1 Stage 1 (likelihood; common to both paradigms)

Given a true site effect $\tau_j \in \mathbb{R}$ and a known sampling variance
$\widehat{se}_j^{\,2} > 0$,

$$
\hat{\tau}_j \,\big|\, \tau_j,\, \widehat{se}_j^{\,2} \;\sim\; \mathcal{N}\!\bigl(\tau_j,\;
\widehat{se}_j^{\,2}\bigr), \qquad j=1,\dots,J. \tag{$\dagger$}
$$

This is JEBS Eq. (1). The CLT inside each site supplies the normal likelihood; metaDGP treats
$\widehat{se}_j^{\,2}$ as the *true* (assumed-known) sampling variance. We discuss in §3 the
consequences of plugging in a stochastic estimate of $\widehat{se}_j^{\,2}$ vs. treating it as
a known design quantity.

### 1.2 Stage 2 (prior $G$; common)

Independently across sites,

$$
\tau_j \,\big|\, \tau, \sigma^{2}_{\tau}, \boldsymbol{\theta}_G \;\sim\; G(\,\cdot\, ;\,\tau,
\sigma^{2}_{\tau}, \boldsymbol{\theta}_G), \qquad j=1,\dots,J, \tag{$\ddagger$}
$$

with $\mathrm{E}[\tau_j] = \tau$ and $\mathrm{Var}(\tau_j) = \sigma^{2}_{\tau}$. The vector
$\boldsymbol{\theta}_G$ collects shape parameters (e.g. degrees of freedom, mixture weights,
skewness). This is JEBS Eq. (2), generalized beyond Gaussian.

The team's code uses the symbol `sigma_tau` for what JEBS calls $\sigma$. We adopt
`sigma_tau` (and the symbol $\sigma_\tau$) throughout, reserving plain $\sigma$ for nothing in
particular — this resolves the long-standing confusion in `gen_priorG2.R` discussed in §2.

### 1.3 Paradigm A: site-size–driven DGP

The user supplies $(J, \bar{n}_j, \mathrm{CV}, n_{j,\min}, p, R^2)$ and metaDGP constructs

$$
n_j \,\sim\, \mathrm{TruncGamma}(\alpha,\beta;\,n_{j,\min}), \qquad
\widehat{se}_j^{\,2} \;=\; \frac{\kappa}{n_j},
$$

with $\kappa = (1/p + 1/(1-p))\,(1-R^2)\,s^2_Y$. See §3 for the derivation and the meaning of
$s^2_Y$. The implied informativeness $I$ is computed *post hoc* via JEBS Eq. (4):

$$
I \;=\; \frac{\sigma_\tau^2}{\sigma_\tau^2 + \mathrm{GM}\!\bigl(\widehat{se}_j^{\,2}\bigr)},
\qquad
\mathrm{GM}(x) := \exp\!\Bigl(\tfrac{1}{J}\textstyle\sum_j \ln x_j\Bigr). \tag{1}
$$

This is the natural paradigm when the user is calibrating to a real multisite design (Weiss et
al., 2017): they know average site size and CV, and $I$ falls out.

### 1.4 Paradigm B: direct-heterogeneity DGP (Northwestern)

The user supplies $(J, I, R)$ where $R = \max_j(\widehat{se}_j^{\,2})/\min_j(\widehat{se}_j^{\,2})
\ge 1$ is the *heterogeneity ratio*. From Eq. (1) the geometric mean is pinned:

$$
\mathrm{GM}\!\bigl(\widehat{se}_j^{\,2}\bigr) \;=\; \frac{1-I}{I}\,\sigma_\tau^2. \tag{2}
$$

If we further set $\sigma_\tau^2 = 1$ (i.e. $\tau_j$ on a standardized scale), Eq. (2) reduces to
the form used in `Part_01_Data Generation.R`: $\mathrm{GM} = (1-I)/I$. The variances are spaced
log-uniformly so that the geometric mean is exactly $\mathrm{GM}$ and the ratio is exactly $R$:

$$
\widehat{se}_j^{\,2} \;=\; \mathrm{GM}\cdot R^{(2j-J-1)/(J-1)}, \qquad j=1,\dots,J,
$$

then optionally permuted to break any deterministic assignment to site index. This paradigm is
attractive when the user wants $I$ as a *first-class control* rather than a derived quantity —
which is exactly the deconvolution-evaluation use case.

### 1.5 What the simulator returns

For a single replication metaDGP returns a tibble with columns

| Column          | Meaning                                                           |
|-----------------|-------------------------------------------------------------------|
| `site_index`    | Integer identifier $1,\dots,J$.                                   |
| `tau_j`         | Latent draw from $G$ (a finite-population realization).           |
| `se2_j`         | Sampling variance $\widehat{se}_j^{\,2}$ used to draw $\hat{\tau}_j$. |
| `n_j`           | Site size (Paradigm A) or `NA` (Paradigm B).                      |
| `tau_j_hat`     | Observed estimate $\hat{\tau}_j$.                                  |
| `shrinkage_j`   | Diagnostic $S_j = \sigma_\tau^2 / (\sigma_\tau^2 + \widehat{se}_j^{\,2})$ (JEBS Eq. 3). |

Both $\tau_j$ (latent) and $\hat{\tau}_j$ (observed) are returned, so users can score either
the finite-population estimand $\{\tau_j\}_{j=1}^J$ or, in conjunction with metadata about the
generative $G$, the super-population estimand $G$ itself (see §7).

---

## 2. The G distributions

### 2.1 Critique of the existing `gen_priorG2()` parameterization

The current code applies *two* multiplicative scale steps that conflate "shape variance" with
"target variance":

```r
# step 5: draw devs with variance = `variance`
unscaled_effects <- rnorm(J, mean = site_means, sd = sqrt(variance))   # sd = sqrt(variance)
# step 6: rescale by sigma_tau
final_effects <- unscaled_effects * sigma_tau
```

So the realized variance of `final_effects` is `sigma_tau^2 * variance`, *not*
`sigma_tau^2`. For Gaussian draws this is harmless if the user understands the convention, but
for non-Gaussian shapes it is statistically wrong: the documented relationship `Var(tau_j) =
sigma_tau^2` only holds when `variance = 1`, and only when the underlying base draw has
variance exactly one (which the team's `T`, `Skew`, and `Mixture` branches do *not* guarantee).

Concretely:

- **Student-t.** `rt(J, df=nu)` has variance $\nu/(\nu-2)$, *not* 1. Multiplying by
  `sqrt(variance)` and then by `sigma_tau` gives realized variance
  $\nu/(\nu-2)\cdot \mathrm{variance}\cdot \sigma_\tau^2$, which is larger than the user
  expects.
- **Skew normal.** `sn::rsn(xi=mu, omega=sqrt(variance), alpha=slant)` has
  $\mathrm{Var} = \omega^2\bigl(1 - 2\delta^2/\pi\bigr)$ where $\delta = \alpha/\sqrt{1+\alpha^2}$.
  Realized variance is $\omega^2(1 - 2\delta^2/\pi)\cdot \sigma_\tau^2$, again not what the
  docstring promises.
- **Asymmetric Laplace (`LaplacesDemon::ralaplace`).** Variance depends on the asymmetry
  `kappa` in a nontrivial way; a separate scaling step is missing.
- **2-component Gaussian mixture.** With weights $1-\varepsilon$ and $\varepsilon$ on means
  $\mu - \delta$ and $\mu + \upsilon$ and common variance `variance`, the variance of the
  mixture is $\mathrm{variance} + \varepsilon(1-\varepsilon)(\delta+\upsilon)^2$. This is then
  multiplied by $\sigma_\tau^2$. The user has no clean way to set "mixture variance =
  $\sigma_\tau^2$" because the within-component variance and the location parameters
  contribute on different scales.

**Recommended fix.** metaDGP should expose a *single* variance axis, `sigma_tau`, and
internally normalize each shape to unit variance before scaling. Concretely:

1. Draw $z_j$ from a *standardized* version of the chosen shape with $\mathrm{E}[z_j]=0$ and
   $\mathrm{Var}(z_j)=1$.
2. Set $\tau_j = \tau + \sigma_\tau\,z_j + \mathbf{x}_j^\top \boldsymbol{\beta}$, where the
   covariate term is optional.

This makes `Var(tau_j) = sigma_tau^2 + Var(x^T beta)` (and equals `sigma_tau^2` exactly when
covariates are absent or centered with zero coefficient variance). The standardization is
analytic for some shapes and numeric for others — see below.

### 2.2 Supported shapes

For each shape we list (a) the parameter names exposed by the API, (b) the constraints,
(c) how the target variance is enforced, (d) the moment relationship used for standardization,
(e) how to draw.

#### 2.2.1 Gaussian

- **Parameters:** none beyond $(\tau, \sigma_\tau)$.
- **Standardization:** $z_j \sim \mathcal{N}(0,1)$.
- **Closed form:** $\tau_j = \tau + \sigma_\tau z_j \sim \mathcal{N}(\tau, \sigma_\tau^2)$.

#### 2.2.2 Student-$t$

- **Parameters:** `nu` (degrees of freedom), with constraint $\nu > 2$ to ensure finite
  variance. The current `gen_priorG2.R` does enforce `nu > 2` (good); the variance-
  standardization step is what is missing.
- **Moment fact:** if $T \sim t_\nu$ then $\mathrm{Var}(T) = \nu/(\nu-2)$.
- **Standardization:** $z_j = T_j \sqrt{(\nu-2)/\nu}$.
- **Caveat about $\nu$ near 2.** As $\nu \downarrow 2$, the standardizing factor diverges; the
  user should be warned that $\nu \in (2, 4]$ produces extremely heavy tails (kurtosis
  infinite for $\nu \le 4$). metaDGP should refuse $\nu \le 2$ outright.

#### 2.2.3 Skew normal

- **Parameters:** `slant` $= \alpha \in \mathbb{R}$.
- **Moment fact (Azzalini):** for $\xi=0$, $\omega = 1$,
  $\mathrm{E}[Z] = \delta\sqrt{2/\pi}$, $\mathrm{Var}(Z) = 1 - 2\delta^2/\pi$, with
  $\delta = \alpha/\sqrt{1+\alpha^2}$.
- **Standardization:** draw $Y \sim \mathrm{SN}(0,1,\alpha)$, set
  $z = (Y - \delta\sqrt{2/\pi}) / \sqrt{1 - 2\delta^2/\pi}$. Then $\tau_j = \tau + \sigma_\tau z$.
- **Constraint:** $|\alpha| < \infty$. As $|\alpha| \to \infty$ the distribution tends to a
  half-normal; not pathological but worth documenting.

#### 2.2.4 Asymmetric Laplace (ALD)

- **Parameters:** `rho` $\in (0,1)$ controls skewness; we use the Yu–Zhang parameterization
  where $\rho = 0.5$ recovers the symmetric Laplace.
- **Moment fact:** with location $0$ and scale $s$,
  $\mathrm{E}[X] = s\,(1-2\rho)/[\rho(1-\rho)]$ and
  $\mathrm{Var}(X) = s^2\,[(1-2\rho)^2 + 1]/[\rho^2(1-\rho)^2]$ — the exact form depends on the
  parameterization; what matters is that there is a closed-form standardization factor
  $c(\rho)$ with $\mathrm{Var}(X/c(\rho)) = 1$.
- **Standardization:** divide by $\sqrt{\mathrm{Var}(X)}$ and re-center.
- The current `gen_priorG2.R` invocation `LaplacesDemon::ralaplace(scale = sqrt(variance),
  kappa = rho)` does not standardize.

#### 2.2.5 Two-component Gaussian mixture

- **Parameters:** `eps` $= \varepsilon \in (0,1)$ (mixing weight), `delta` $= \delta$ and
  `ups` $= \upsilon$ (mode separations), `mix_var_ratio` $= \upsilon^{(\sigma)}$ (variance
  ratio between components).
- **Generative form:** with probability $1-\varepsilon$ draw from
  $\mathcal{N}(-\varepsilon\,\Delta,\sigma_1^2)$, with probability $\varepsilon$ draw from
  $\mathcal{N}((1-\varepsilon)\Delta, \sigma_2^2)$, where
  $\Delta = \delta + \upsilon$ is the total mode separation. Centering at $-\varepsilon\Delta$
  vs. $(1-\varepsilon)\Delta$ guarantees mean zero by construction.
- **Variance of mixture:**
  $\mathrm{Var}(z) = (1-\varepsilon)\sigma_1^2 + \varepsilon\sigma_2^2 +
  \varepsilon(1-\varepsilon)\Delta^2.$
- **Standardization:** rescale so this equals 1, then apply `sigma_tau`. metaDGP exposes
  `(eps, sep = Delta, var_ratio)`; the API computes $\sigma_1^2, \sigma_2^2$ given a chosen
  variance ratio and a constraint that the mixture variance equals 1.
- **Distinguishing role of $\delta$ vs. $\upsilon$.** The team's `gen_priorG2` treats
  `delta` and `ups` asymmetrically (one is subtracted, one added) but never documents that
  this is *the difference between the negative and positive component centers, given a
  weight-balanced mean*. Recommend renaming to `sep` and `weight_ratio` or similar.

#### 2.2.6 Point-mass-plus-slab

- **Parameters:** `pi0` $= \pi_0$ (mass at zero), `slab_shape` (Gaussian / Laplace), and slab
  parameters.
- **Generative form:** $\tau_j = 0$ with probability $\pi_0$, else draw from a mean-zero slab.
- **Variance:** $\mathrm{Var}(\tau_j) = (1-\pi_0)\,\sigma_{\rm slab}^2$. Solve
  $\sigma_{\rm slab}^2 = \sigma_\tau^2/(1-\pi_0)$.
- **Use case:** "many sites have zero treatment effect, a few have a large effect" — common
  in policy-relevance testing. Not in `gen_priorG2.R`; recommend adding.

#### 2.2.7 User-supplied $G$

- **Interface:** the user passes either (a) a function `r_g(n, ...)` that draws $n$ i.i.d.
  values, plus optional `e_g(...)` and `var_g(...)` for analytic moments; or (b) a vector of
  pre-drawn values that metaDGP rescales to mean $\tau$ and variance $\sigma_\tau^2$.
- **Standardization:** if moments are not analytically supplied, metaDGP estimates them from
  $10^5$ pre-draws and rescales (with a warning that the empirical moments introduce a small
  bias).
- **Disjoint Theta example.** The Northwestern code uses `deconvolveR::disjointTheta` (a
  bimodal "twin towers" distribution). This becomes a one-line user-supplied $G$ in metaDGP.

#### 2.2.8 Dirichlet process mixture (DP-diffuse, DP-inform)

The team has a $\chi^2$-based informative prior on the number of clusters $K$ (DP-inform with
`Chi^2(df=u)`, e.g. $u=5$ for $J=50$; cf. the proposal §1.4 and JEBS pp. 12–13). For metaDGP
the question is whether DPM generators ship with the package or in a sibling. The conservative
recommendation: implement (a) and (b) here, defer DP-mixture *generation* to a sibling that
depends on `dirichletprocess` or NIMBLE, and document the interface so users can plug a DPM
draw into metaDGP via the user-supplied $G$ slot. See §12 for discussion.

### 2.3 Site-level covariates

The current `gen_priorG2.R` allows $\tau_j = \tau + \mathbf{x}_j^\top\boldsymbol{\beta} +
\sigma_\tau z_j$ with `model.matrix(formula, data)`. This is a sound design and we keep it.
The key statistical caveat: with covariates, $\sigma_\tau^2$ is the *residual* variance after
controlling for $\mathbf{x}$, not the marginal variance of $\tau_j$. metaDGP must report both
quantities in its diagnostic output to avoid confusion.

---

## 3. Site sizes and the κ formula

### 3.1 Derivation

For a within-site difference-in-means estimator under a Neyman repeated-sampling design with
constant variance $s_Y^2$ across treatment arms:

$$
\widehat{\tau}_j \;=\; \bar{Y}_j^{(T)} - \bar{Y}_j^{(C)},
\qquad
\mathrm{Var}\bigl(\widehat{\tau}_j\bigr) \;=\; \frac{s_Y^2}{n_j p_j} + \frac{s_Y^2}{n_j(1-p_j)}
\;=\; \frac{s_Y^2}{n_j}\Bigl(\frac{1}{p_j} + \frac{1}{1-p_j}\Bigr).
$$

With pre-treatment covariates explaining a fraction $R^2$ of within-site outcome variance,
the residual variance after adjustment is $s_Y^2(1-R^2)$, and

$$
\widehat{se}_j^{\,2} \;=\; \frac{s_Y^2(1-R^2)}{n_j}\Bigl(\frac{1}{p_j} + \frac{1}{1-p_j}\Bigr). \tag{3}
$$

This matches JEBS Eq. (1) and the team's `sim_sitesize_withinvar.R`. With $p_j \equiv p$
constant across sites, define

$$
\kappa \;\equiv\; s_Y^2(1-R^2)\Bigl(\frac{1}{p} + \frac{1}{1-p}\Bigr) \quad\Longrightarrow\quad
\widehat{se}_j^{\,2} \;=\; \frac{\kappa}{n_j}. \tag{4}
$$

### 3.2 Defaults and when to change them

The default is the JEBS simulation default: $s_Y^2 = 1$ (effect-size units), $p = 0.5$,
$R^2 = 0$, giving $\kappa = 4$ and $\widehat{se}_j^{\,2} = 4/n_j$. **The `s_Y^2 = 1`
convention deserves explicit documentation**: it means $\tau_j$ is in effect-size units of the
*control-side* SD, and `sigma_tau` is in the same units. If a user wants to simulate raw-scale
data ($s_Y^2 \ne 1$), they must set both `var_outcome` and `sigma_tau` consistently — see §11
on "the var_outcome silent convention".

When to change defaults:

- $p \ne 0.5$: any unbalanced design (e.g. 1:2 treatment-to-control). $\kappa$ is minimized at
  $p=0.5$ ($\kappa=4s_Y^2(1-R^2)$) and grows asymmetrically as $p \to 0$ or $1$; for $p=0.25$,
  $\kappa = 5.33$ at $R^2=0$, a 33% inflation.
- $R^2 > 0$: covariate-adjusted within-site analyses. A pretest with $R^2 = 0.5$ halves
  $\kappa$, doubling the effective $n_j$. This is *the* lever for design-stage power
  improvement that the JEBS paper highlights (p. 6) and that metaDGP should make easy to
  explore.
- Cluster-robust or MLM-based SEs: see §3.4.

### 3.3 The truncated-Gamma site-size distribution

Following JEBS p. 14, $n_j \sim \mathrm{Gamma}(\alpha,\beta)$ truncated to $[n_{j,\min},\infty)$
with target conditional mean $\bar{n}_j$ and target conditional CV equal to user-supplied `cv`.
The team's `solve_trunc_gamma()` solves the 2×2 nonlinear system

$$
\mathrm{E}[X \mid X \ge n_{j,\min}] = \bar{n}_j, \qquad
\mathrm{Var}(X \mid X \ge n_{j,\min}) = (\mathrm{cv}\cdot \bar{n}_j)^2,
$$

via `nleqslv`. **Solver-failure behavior is the weakest link in the current code.** When
`termcd != 1`, the function only emits a warning and returns whatever final iterate `nleqslv`
produced, which can be wildly off the target. metaDGP must:

1. Verify post-solve that the realized truncated moments are within tolerance of the target,
   and `stop()` (not `warning()`) if not.
2. Try multiple starting points before giving up. Sensible alternative starts: the
   *un-truncated* method-of-moments solution $\alpha_0 = \bar{n}_j^2 / (\mathrm{cv}\bar{n}_j)^2 =
   1/\mathrm{cv}^2$, $\beta_0 = \alpha_0 / \bar{n}_j$.
3. Detect infeasibility: if $\bar{n}_j \le n_{j,\min}$ or $\mathrm{cv}$ is small enough that
   the unconstrained Gamma puts negligible mass below $n_{j,\min}$, fall back to the
   un-truncated Gamma and emit a one-line note. If $\mathrm{cv} > $ some threshold and
   $n_{j,\min}$ is large, the targets may be infeasible — refuse with an informative error.
4. Special-case $\mathrm{cv} = 0$: deterministic $n_j = \bar{n}_j$ for all sites (the JEBS
   simulation includes this case; the current solver would presumably fail).

### 3.4 Alternative SE models

Equation (3) is a stylized model. Real multisite trials often have:

- **Cluster-robust SEs.** $\widehat{se}_j^{\,2}$ is heteroskedastic-and-cluster-corrected; the
  $1/n_j$ scaling is replaced by a sandwich estimator. metaDGP should accept a user-supplied
  function `se2_from_design(n_j, p_j, ...)` that overrides Eq. (4).
- **Cluster-aggregated.** When sites comprise sub-clusters (classrooms within schools), the
  effective $n_j$ is reduced by a design effect $1 + (m-1)\rho_{ICC}$, where $m$ is sub-cluster
  size and $\rho_{ICC}$ the intraclass correlation. metaDGP should expose this as
  `design_effect_fn(n_j, m_j, icc)`.
- **MLM-based SEs.** When the within-site model is itself multilevel, the nominal SE depends
  on the random-effects structure. metaDGP can accept a user-supplied function and pass
  through.

The internal default remains JEBS-standard Eq. (4); the SE pluggability lives on the same axis
that `gen_priorG2.R`'s `formula` argument already lives on for $\tau_j$.

---

## 4. The two heterogeneity dials: σ_τ vs. (I, R)

### 4.1 What each controls

- **`sigma_tau`** is the *between-site standard deviation of $\tau_j$* on the effect-size
  scale. It is a property of the *prior* $G$.
- **$(I, R)$** parameterize the *signal-to-noise environment*. $I$ is the average reliability
  of $\hat{\tau}_j$ (JEBS Eq. 4); $R$ is the heterogeneity ratio of $\widehat{se}_j^{\,2}$
  across sites.

These describe *different* objects. `sigma_tau` is a population parameter of $G$;
$I$ is a *function* of both `sigma_tau` and the site-size design. They become
interchangeable only when the SE design is fixed.

### 4.2 Algebraic relationship

Combining Eqs. (1) and (4), with constant $p$ and $R^2$:

$$
I \;=\; \frac{\sigma_\tau^2}{\sigma_\tau^2 + \kappa\cdot\mathrm{GM}(1/n_j)}. \tag{5}
$$

For $\mathrm{cv} = 0$ (constant $n_j = \bar{n}_j$):

$$
I \;=\; \frac{\sigma_\tau^2}{\sigma_\tau^2 + \kappa/\bar{n}_j}, \qquad
\sigma_\tau^2 \;=\; \frac{I}{1-I}\cdot \frac{\kappa}{\bar{n}_j}. \tag{6}
$$

For $\mathrm{cv} > 0$, $\mathrm{GM}(1/n_j) \ne 1/\mathrm{GM}(n_j)$ in general, but Jensen's
inequality gives $\mathrm{GM}(1/n_j) \ge 1/\mathrm{AM}(n_j) = 1/\bar{n}_j$, so the implied $I$
when CV varies is *smaller* than the deterministic-$\bar{n}$ formula suggests. The package
must therefore **not** estimate `GM` analytically from $(\bar{n}_j, \mathrm{cv})$; it must
compute it empirically from the realized $\{n_j\}$.

The other direction (Paradigm B) follows from Eq. (2):

$$
\mathrm{GM}\bigl(\widehat{se}_j^{\,2}\bigr) \;=\; \sigma_\tau^2 \cdot \frac{1-I}{I}. \tag{7}
$$

If $\sigma_\tau^2 = 1$, the $\widehat{se}_j^{\,2}$ have geometric mean $(1-I)/I$ and span
$[\mathrm{GM}/R, \mathrm{GM}\cdot R]$ in log-uniform spacing.

### 4.3 The translator function

metaDGP exposes a translator with two directions:

```r
# Direction 1: design -> implied I
implied_I(J, nbar, cv, sigma_tau, p = 0.5, R2 = 0,
          n_replications = 200)
# Returns a list with: realized_GM_se2, implied_I, MC_SE_implied_I.

# Direction 2: target (I, R, sigma_tau = 1) -> SE^2 grid
se2_grid_from_IR(J, I, R, sigma_tau = 1)
# Returns a vector of length J spaced log-uniformly with the right GM and ratio.
```

The first form is *Monte Carlo* because of the dependence on the realized $n_j$ draws under
$\mathrm{cv} > 0$; the package warns that $I$ has Monte Carlo variation across replications and
reports both the mean $I$ and its $\pm 2\,\mathrm{SE}$ band over the requested replications.

### 4.4 When to use which parameterization

- **Use `sigma_tau` (Paradigm A) when** the user is calibrating to a real multisite design and
  wants the simulation to match Weiss-style design tables. The "natural" inputs here are
  $(J, \bar{n}_j, \mathrm{cv}, \sigma_\tau)$; $I$ is a derived consequence.
- **Use $(I, R)$ (Paradigm B) when** the user is *evaluating an estimator* (PM, CB, GR,
  DP-mixture) on a grid of signal-to-noise levels. Here $I$ is the natural axis: the user
  wants to know "how does estimator $X$ behave at $I=0.1, 0.3, 0.5, \dots$?", and the actual
  site sizes are not of substantive interest. This is the use case in the Northwestern
  replication codes.

Both parameterizations should ship; `metaDGP::sim_dataset()` accepts either via a
`paradigm = c("site_size", "I_R")` argument.

---

## 5. Precision–effect dependence: rank vs. copula

The Rubin model assumes $\tau_j \perp \widehat{se}_j^{\,2}$. Real data routinely violate this:
larger schools may have larger or smaller true effects than smaller schools (Walters 2024;
Chen 2023; the precision-dependence research note). metaDGP must therefore allow the user to
inject a controlled dependence between $\tau_j$ and $\widehat{se}_j^{\,2}$.

### 5.1 Hill-climbing rank reorder (Spearman target)

Given a vector $\boldsymbol{\tau} = (\tau_1, \dots, \tau_J)$ and a multiset
$\mathcal{S} = \{s^2_1, \dots, s^2_J\}$ of sampling variances, the rank reorder seeks a
permutation $\pi$ of $\{1,\dots,J\}$ minimizing

$$
\bigl|\,\rho_S\bigl(\boldsymbol{\tau},\, \mathbf{s}^2_\pi\bigr) - \rho_{\rm target}\bigr|, \qquad
\rho_S = \mathrm{cor}\bigl(\mathrm{rank}(\boldsymbol{\tau}),\, \mathrm{rank}(\mathbf{s}^2_\pi)
\bigr),
$$

via random pairwise swaps (`reorder_for_spearman()` in `sim_observed_effects.R`). Properties:

1. **Marginals exactly preserved.** Both $\boldsymbol{\tau}$ and $\mathcal{S}$ keep their
   exact empirical distributions; only the *assignment* of which $s^2$ goes with which $\tau$
   changes.
2. **Targets Spearman, not Pearson.** The objective is invariant to monotone transformations
   of either margin. This is the right thing if the user thinks of "precision dependence" as
   a rank phenomenon ("sites in the top quartile of $\tau$ are also in the top quartile of
   $s^2$"), and the wrong thing if they think of it as a linear correlation.
3. **No global-optimum guarantee.** Hill-climbing in permutation space; converges to a local
   optimum within `tol` for moderate $|\rho_{\rm target}| \le 0.7$ at $J \le 200$. For
   extreme targets ($|\rho| \approx 0.9$) and large $J$, the swap procedure may stall.
4. **Site identities permute.** This is the load-bearing API caveat. After
   `precision_dependence = TRUE`, `se2_j[k]` no longer corresponds to "the $k$-th site as
   originally generated" — it corresponds to whichever original site happened to land at
   position $k$ after the permutation. **The output tibble must record the inverse permutation
   $\pi^{-1}$ so that downstream covariate matching, site labeling, and joins to other
   site-level metadata stay consistent.** The current `sim_observed_effects()` does *not* do
   this; it returns `corr_est` but throws away the permutation. Users who need to align with
   external site metadata are silently broken.

### 5.2 Gaussian copula (Pearson target)

Given the same inputs and a target Pearson correlation $\rho \in (-1, 1)$:

1. Map each margin to standard normals via the rank-PIT:
   $z_{\tau,j} = \Phi^{-1}\bigl((\mathrm{rank}(\tau_j) - 0.5)/J\bigr)$ and likewise $z_{s,j}$.
2. Construct a new normal vector with target correlation:
   $z^\star_{s,j} = \rho\, z_{\tau,j} + \sqrt{1-\rho^2}\,\varepsilon_j$,
   $\varepsilon_j \stackrel{\rm iid}{\sim} \mathcal{N}(0,1)$.
3. Map back to the empirical marginal of $\mathcal{S}$ via
   $s^{2\star}_j = F_S^{-1}\bigl(\Phi(z^\star_{s,j})\bigr)$ where $F_S^{-1}$ is the empirical
   inverse CDF.

Properties:

1. **Marginals approximately preserved.** Because step 3 maps to *order statistics* of
   $\mathcal{S}$, the realized values are exactly drawn from $\mathcal{S}$ but with possible
   ties from the discrete inverse CDF. For continuous $\mathcal{S}$ the marginal is exact.
2. **Targets Pearson by construction in $z$-space.** The realized Pearson correlation of
   $(\tau_j, s^2_j)$ on the original scale is *not* exactly $\rho$ — it depends on the marginal
   shapes. For symmetric, light-tailed margins it is very close; for heavy-tailed or skewed
   margins it can deviate by 0.05–0.10.
3. **No tail dependence.** The Gaussian copula has zero asymptotic tail dependence: extreme
   $\tau_j$ is not *especially* coupled to extreme $s^2_j$. If the user wants concordance in
   the tails (e.g. "the most extreme effects are also the most uncertain"), the Gaussian
   copula understates this.
4. **Smooth and differentiable.** Unlike the rank reorder, the copula maps continuously in
   $\rho$, which makes derivative-of-loss-with-respect-to-$\rho$ analyses feasible.

### 5.3 Why both ship

These methods solve *different* problems:

- The rank reorder targets a rank-based dependence and preserves marginals exactly. It is the
  right tool when the user has *fixed* $\boldsymbol{\tau}$ and $\mathcal{S}$ values
  (e.g. drawn from a particular $G$ and a particular $n_j$ design) and only wants to
  rearrange the pairing.
- The copula targets a linear dependence in a transformed (z) space. It is the right tool
  when the user wants smooth control over $\rho$ for sensitivity analysis and is willing to
  accept marginal noise from the discrete back-mapping.

For *most* downstream estimator comparisons (PM/CB/GR/DPM under precision dependence), the
rank reorder is the safer default, because it preserves the exact empirical distribution of
$\widehat{se}_j^{\,2}$ — which the estimators may use in their normalization.

### 5.4 Future extension: Student-$t$ copula

For tail-coupled scenarios, the Student-$t$ copula

$$
\mathbf{u} \,=\, \bigl(t_\nu(z_1),\, t_\nu(z_2)\bigr) \quad \text{with } \mathbf{z} \sim
\mathcal{N}_2(\mathbf{0},\,\boldsymbol{\Sigma}_{\rho}),
$$

has positive lower and upper tail dependence $\lambda = 2 t_{\nu+1}(-\sqrt{(\nu+1)(1-\rho)/(1+\rho)})$,
which approaches $1$ as $\nu \to 0$. metaDGP should mark this as a v2 feature with an issue
template referencing this draft.

### 5.5 API surface

```r
sim_observed_effects(
  tau_j, se2_j,
  precision_dependence = c("none", "rank", "copula", "tcopula"),
  rank_corr     = 0.5,    # used if "rank"
  pearson_corr  = 0.5,    # used if "copula"
  copula_df     = 4,      # used if "tcopula"
  max_iter      = 20000,
  tol           = 0.01,
  return_perm   = TRUE    # NEW: return the permutation as an attribute
)
```

The `return_perm` flag fixes the silent identity-loss bug noted in §5.1.

---

## 6. Diagnostics and validation

For any simulated dataset metaDGP computes the following, returned as a `diagnostics` slot on
the output object.

### 6.1 Quantitative diagnostics

| Quantity | Formula | What it checks |
|----------|---------|----------------|
| Realized informativeness $\hat{I}$ | $\frac{\hat{\sigma}_\tau^2}{\hat{\sigma}_\tau^2 + \mathrm{GM}(\widehat{se}_j^{\,2})}$ where $\hat{\sigma}_\tau^2 = \mathrm{Var}_J(\tau_j)$ | Did the simulator hit the target $I$? |
| Realized $\hat{\sigma}_\tau$ | $\sqrt{(J-1)^{-1}\sum_j(\tau_j - \bar{\tau})^2}$ | Did the simulator hit the target $\sigma_\tau$? |
| Realized $\hat{R}$ | $\max(\widehat{se}_j^{\,2})/\min(\widehat{se}_j^{\,2})$ | Did the simulator hit the target $R$? |
| GM of $\widehat{se}_j^{\,2}$ | $\exp(J^{-1}\sum_j \ln \widehat{se}_j^{\,2})$ | The denominator of $\hat{I}$. |
| Realized rank correlation | $\hat{\rho}_S = \mathrm{cor}(\mathrm{rank}(\tau_j), \mathrm{rank}(\widehat{se}_j^{\,2}))$ | Did the rank reorder converge to `rank_corr`? |
| Realized Pearson correlation | $\hat{\rho}_P = \mathrm{cor}(\tau_j, \widehat{se}_j^{\,2})$ | Did the copula hit `pearson_corr`? |
| Empirical KS distance | $D_J = \sup_t \bigl| \hat{F}_J(t) - F_G(t) \bigr|$ | Does the empirical EDF of $\tau_j$ resemble target $G$? |
| Bhattacharyya coefficient | $\mathrm{BC} = \int \sqrt{\hat{f}_J(\tau)\,f_G(\tau)}\,d\tau$ | Density-overlap with target $G$ (research note 04). |
| Mean shrinkage $\bar{S}$ | $J^{-1}\sum_j \frac{\sigma_\tau^2}{\sigma_\tau^2 + \widehat{se}_j^{\,2}}$ | Diagnostic of expected PM shrinkage (JEBS Eq. 3). |

### 6.2 KS and Bhattacharyya in detail

**Kolmogorov–Smirnov** tests the realized EDF of $\{\tau_j\}$ against the analytic CDF $F_G$
of the target $G$. Asymptotically $\sqrt{J}\,D_J$ has a Kolmogorov distribution; for the
Gaussian, Skew-normal, ALD, Student-$t$, and 2-component mixture, $F_G$ is available in
closed form or via efficient quadrature. This is a *one-shot* check per realization;
combining $K$ replications gives $\sqrt{KJ}\,D_{KJ}$ for the pooled EDF, with much higher
power.

**Bhattacharyya coefficient** measures density overlap and lies in $[0,1]$ with $1$ for
identical densities (research note 04). For continuous distributions,

$$
\mathrm{BC} \;=\; \int_{\mathbb{R}} \sqrt{\hat{f}_J(\tau)\,f_G(\tau)}\,d\tau,
$$

estimated by binning to a histogram with the Freedman–Diaconis rule and computing
$\sum_k \sqrt{\hat{w}_{J,k}\,w_{G,k}}$. The four-tier interpretation from research note 04 —
$\mathrm{BC} \ge 0.90$ Very High, $\ge 0.80$ High, $\ge 0.50$ Medium, $< 0.50$ Low — applies.
The KS statistic and BC are *complementary*: KS is sup-norm on CDFs, BC is $L^1/2$-style on
densities. For diagnosing tail mismatch, KS dominates; for diagnosing mode/multimodality
mismatch, BC dominates.

### 6.3 Shrinkage diagnostic

The PM shrinkage factor for site $j$ is $S_j = \sigma_\tau^2 / (\sigma_\tau^2 + \widehat{se}_j^{\,2})$
(JEBS Eq. 3). metaDGP returns $S_j$ for each site and the geometric/arithmetic mean over
sites. The arithmetic mean is the *site-average* expected shrinkage; the geometric mean is
algebraically tied to $I$ via $I = \mathrm{AM}(\sigma_\tau^2)/[\mathrm{AM}(\sigma_\tau^2) +
\mathrm{GM}(\widehat{se}_j^{\,2})]$ but only equals $\mathrm{GM}(S_j)$ in the homoskedastic
limit. We document both.

### 6.4 Multisitepower / margin-of-error connection (per Jonathan)

The average margin of error of $\hat{\tau}_j$ at the $1-\alpha$ level is

$$
\overline{\mathrm{MOE}} \;=\; z_{1-\alpha/2}\cdot \mathrm{AM}\bigl(\widehat{se}_j\bigr).
$$

This connects directly to the `multisitepower` framing: a designer choosing $J$, $\bar{n}_j$,
$\sigma_\tau$ effectively chooses a distribution of $\widehat{se}_j$ and hence of MOEs. metaDGP
returns the realized $\overline{\mathrm{MOE}}$ alongside $I$ in the diagnostics, so users can
swap between "informativeness-of-shrinkage" and "average-precision-of-confidence-intervals"
languages without re-simulating.

---

## 7. Estimands the simulator supports

JEBS pp. 8–9 distinguishes two estimands. metaDGP must support both, and *return enough
information to score either one.*

### 7.1 Finite-population estimand

The vector $\{\tau_1, \dots, \tau_J\}$ is the entire population of interest. Its EDF is

$$
F_J(t) \;=\; \frac{1}{J}\sum_{j=1}^{J} \mathbf{1}\{\tau_j \le t\}.
$$

Loss functions:

- **MSEL** (JEBS Eq. 5): $J^{-1}\sum (\hat{a}_j - \tau_j)^2$, minimized by the PM
  $\hat{a}_j = \mathrm{E}[\tau_j \mid \mathcal{D}]$.
- **MSELP** (JEBS Eq. 7): $J^{-1}\sum (\hat{A}_j/J - R_j/J)^2$, minimized by expected
  posterior percentiles.

To score either, the user needs the *realized $\tau_j$ vector*, which metaDGP returns by
construction.

### 7.2 Super-population estimand

The whole distribution $G$ (with hyperparameters $\tau, \sigma_\tau, \boldsymbol{\theta}_G$).
Loss function:

- **ISEL** (JEBS Eq. 8): $\int \{A(t) - G_J(t)\}^2 dt$ for an estimated EDF $A(\cdot)$,
  minimized in the JEBS finite-pop framing by $A(t) = J^{-1}\sum_j \Pr(\tau_j \le t \mid
  \mathcal{D})$. For super-pop scoring, replace $G_J$ with $G$ itself.

To score the super-pop ISEL, the user needs the *analytic CDF $F_G$* (or a way to draw
arbitrarily many extra $\tau$'s from $G$). metaDGP records the parameter vector
$(\tau, \sigma_\tau, \boldsymbol{\theta}_G, \text{shape\_id})$ on the output object, so the
analytic CDF can be reconstructed at any time.

### 7.3 Why metaDGP returns *both* the realized $\tau_j$ and the latent $G$

Some downstream estimators (e.g. PM under correctly-specified Gaussian $G$) condition on the
known $G$ to produce a "best-case" benchmark. Some scoring rules (super-pop ISEL) need both
the realized empirical EDF and the true $G$. By returning both, metaDGP supports:

- finite-pop scoring (uses realized $\tau_j$);
- super-pop scoring (uses true $G$);
- benchmark calibration (e.g. compute the "oracle PM" under the *known* $G$, against which
  any estimator's PM is compared).

The team's current `sim_multisite_data.R` returns the realized vector but throws away the
parametric description of $G$; metaDGP must keep both.

---

## 8. Calibration to real-world ranges

### 8.1 Anchor: Weiss et al. (2017)

Weiss et al. catalog 16 multisite trials in education with the following ranges (JEBS p. 14):

- $J$: 20 to 356 (median 78).
- $\bar{n}_j$: 11 (Head Start Impact Study) to 1,176 (Welfare-to-Work).
- 25th/75th percentile of $\bar{n}_j$: 75 / 163.
- $\sigma_\tau$ (effect-size units): 0.00 to 0.35; bulk 0.10–0.25.
- Weiss benchmarks: 0.05 modest, 0.15 moderate, 0.25 substantial impact variation.
- $I$ across the JEBS simulation grid: 0.01 to 0.71, mean 0.25, median 0.18.

### 8.2 Default grid

metaDGP ships with a default `weiss_grid()` and `primo_grid()` returning factor combinations
calibrated to the JEBS / IES proposal study grid:

- $J \in \{25, 50, 75, 100, 300\}$;
- $\bar{n}_j \in \{10, 20, 40, 80, 160\}$;
- $\mathrm{cv} \in \{0.00, 0.25, 0.50, 0.75\}$;
- $\sigma_\tau \in \{.05, .10, .15, .20, .25\}$;
- $G$ shapes: Gaussian, 2-component mixture, ALD, plus the proposal's extensions (Student-$t$,
  point-mass-plus-slab).

### 8.3 (J, n̄, σ_τ) → expected I

Assuming $\mathrm{cv} = 0$ (so $n_j = \bar{n}_j$), Eq. (6) gives

$$
I = \frac{\sigma_\tau^2}{\sigma_\tau^2 + 4/\bar{n}_j}.
$$

| $\sigma_\tau \backslash \bar{n}_j$ | 10 | 20 | 40 | 80 | 160 |
|------------------------------------|-----|-----|-----|-----|------|
| 0.05 | 0.006 | 0.012 | 0.024 | 0.048 | 0.091 |
| 0.10 | 0.024 | 0.048 | 0.091 | 0.167 | 0.286 |
| 0.15 | 0.053 | 0.101 | 0.184 | 0.310 | 0.474 |
| 0.20 | 0.091 | 0.167 | 0.286 | 0.444 | 0.615 |
| 0.25 | 0.135 | 0.238 | 0.385 | 0.556 | 0.714 |

This reproduces JEBS's "$I$ ranges from 0.01 to 0.71" claim for the corner cells. The
package's `expected_I()` helper produces this table for any user grid.

### 8.4 Tipton-style and small-area-estimation context

For multisite trials in education, Tipton's generalizability work (Tipton 2014; Tipton &
Olsen 2022) identifies (a) compositional similarity between sample and target population
(measured by Bhattacharyya overlap of propensity scores) and (b) feasibility of recovering
the super-pop $G$ as separate axes. metaDGP supports the latter via the §6 BC diagnostic,
calibrated against the four-tier guideline from research note 04 (Very High/High/Medium/Low).

For small-area estimation contexts (the Northwestern bridge), the natural variant uses
$(J, I, R)$ directly with $\sigma_\tau = 1$ on a *standardized* scale (Paradigm B). The
package's `nw_grid()` ships with $I \in \{0.5, 0.6, 0.7, 0.8, 0.9\}$ and $R \in \{1, 5, 9\}$,
the levels in `Part_01_Data Generation.R`.

---

## 9. Statistical regression tests

These tests should ship in `tests/testthat/test-stats-regression.R`. Each lists the math and a
target Monte Carlo tolerance.

**T1 — Empirical $I$ at large $J$.** Simulate at $(J=300, \sigma_\tau=0.25, \bar{n}=80,
\mathrm{cv}=0)$. Then $\sigma_\tau^2 = 0.0625$, $\kappa/\bar{n} = 4/80 = 0.05$, so the
target is $I = 0.0625/(0.0625+0.05) = 0.556$. Tolerance $\pm 0.01$ across $K=200$
replications. Math: realized $\hat{I} \to I$ at rate $\sqrt{KJ}$ by LLN on the geometric
mean.

**T2 — Empirical $R$ at $\mathrm{cv}=0$.** Simulate at $\mathrm{cv}=0$. Expected $\hat R = 1$
exactly (homoskedastic). Tolerance $0$ (exact).

**T3 — Empirical $R$ at user-supplied target.** Simulate Paradigm B with
$(J=100, I=0.5, R=9)$. The log-uniform grid pins $\hat R = 9$ exactly before any optional
permutation. Tolerance $0$ on $\hat R$, tolerance $\pm 0.01$ on $\hat I$.

**T4 — KS test against Gaussian $G$.** Simulate $G = \mathcal{N}(0, 0.04)$ at $J=200$, $K=100$
replications. Pooled KS statistic on $20{,}000$ realized $\tau_j$ values vs. $\Phi(\cdot/0.2)$
should not reject at $\alpha=0.01$. Math: under the null, $\sqrt{KJ}\, D_{KJ}$ has a
Kolmogorov distribution; reject only if $> 2.576$.

**T5 — KS test against 2-component mixture $G$.** Same $J,K$ but with a 50/50 mixture of
$\mathcal{N}(\pm 0.15, 0.0025)$. Realized variance should be $0.15^2 + 0.0025 = 0.0250$;
realized KS against the analytic CDF should not reject.

**T6 — Variance recovery for Student-$t$.** With $\nu = 5, \sigma_\tau = 0.2$ at $J=200, K=200$,
realized $\hat{\sigma}_\tau^2$ should equal $0.04$ within Monte Carlo error. Math: under
proper standardization (§2.2.2), $\mathrm{Var}(\tau_j) = \sigma_\tau^2$ exactly, so
$\mathrm{E}[\hat{\sigma}_\tau^2] = 0.04$ and $\mathrm{Var}(\hat{\sigma}_\tau^2) =
2\sigma_\tau^4(1+\mathrm{kurt}/J)/J$. With $\nu=5$, kurtosis is $6/(5-4)=6$, giving an SE of
$\approx \sigma_\tau^2 \sqrt{2\cdot 7/J} = 0.04\sqrt{14/200} \approx 0.011$. Tolerance
$\pm 0.025$ (about $2$ SE).

**T7 — Spearman $\rho$ recovery (rank reorder).** Simulate at $(J=100, \sigma_\tau=0.2, \bar n
= 40, \mathrm{cv}=0.5)$ with `rank_corr = 0.3`. Realized
$\hat{\rho}_S(\tau_j, \widehat{se}_j^{\,2})$ should be $0.3 \pm 0.02$ across $K=100$
replications. Math: hill-climb converges to within `tol=0.01` of target by construction; the
$\pm 0.02$ accounts for swap-search stochasticity.

**T8 — Pearson $\rho$ recovery (copula).** Same design with `pearson_corr = 0.3`. Realized
Pearson $\hat{\rho}_P$ should be $0.30 \pm 0.05$ (looser tolerance because copula -> empirical
margin mapping is exact only asymptotically).

**T9 — Marginal preservation under reorder.** After `precision_dependence = "rank"`, the
multiset of $\widehat{se}_j^{\,2}$ values should equal the original multiset *exactly*. Test
via `setequal(sort(se2_old), sort(se2_new))`.

**T10 — Permutation invariance of $I$.** $\hat I$ depends only on $\sigma_\tau^2$ and on
$\mathrm{GM}(\widehat{se}_j^{\,2})$, neither of which changes under the rank reorder.
Therefore $\hat I$ before and after `precision_dependence = "rank"` should agree exactly.

**T11 — Truncated-Gamma moment recovery.** For
$(\bar{n}_j=40, \mathrm{cv}=0.5, n_{j,\min}=5)$, draw $J=10^4$ values, compute conditional
mean and CV on the realized $\{n_j\}$; both should match targets to within $\pm 1\%$.

**T12 — Bhattacharyya monotonicity in $J$.** For fixed $G$, $\mathrm{BC}(\hat F_J, F_G)$
should converge to $1$ as $J$ grows. Test at $J \in \{50, 200, 1000\}$, expect
$\mathrm{BC}(50) < \mathrm{BC}(200) < \mathrm{BC}(1000)$ on average across $K=20$ reps. Math:
empirical density converges to $f_G$ in $L^1$ at rate $J^{-2/5}$ for kernel methods.

---

## 10. Connection to downstream estimators (PM, CB, GR, DP-mixture)

### 10.1 What metaDGP does *not* do

metaDGP is a DGP, not an estimator. It does not:

- fit Bayesian models (`baggr`, `siteBayes2::fit_*`, NIMBLE);
- compute posterior summaries (PM/CB/GR);
- perform deconvolution (`deconvolveR`, NPMLE).

These belong to a sibling estimator package (or to `siteBayes2` itself).

### 10.2 What metaDGP does provide

To make estimator comparisons easy, metaDGP returns:

1. **Pre-computed shrinkage** $S_j = \sigma_\tau^2 / (\sigma_\tau^2 + \widehat{se}_j^{\,2})$
   for every site, the JEBS Eq. (3) quantity. Under correctly-specified Gaussian $G$, this is
   the *closed-form* PM shrinkage; estimator comparisons can use $S_j$ as an oracle baseline.
2. **Ground-truth EDF $\hat F_J$** for finite-population scoring (MSEL, MSELP, finite-pop
   ISEL).
3. **Ground-truth $G$ parameters** $(\tau, \sigma_\tau, \boldsymbol{\theta}_G, \text{shape\_id})$
   so super-pop scoring (super-pop ISEL, deconvolution recovery) can reconstruct $F_G$
   analytically.
4. **Oracle posterior moments under known $G$** (optional helper, not core). For Gaussian
   $G$, JEBS Eq. (3) gives the exact conditional posterior $\tau_j \mid \tau, \sigma_\tau^2,
   \hat{\tau}_j \sim \mathcal{N}(\tau_j^*, V_j)$; for non-Gaussian $G$ a numeric
   marginal-posterior helper is provided.

### 10.3 Pointers to JEBS for estimator definitions

- **PM** ("posterior mean"): Goal 1, MSEL (JEBS Eq. 5). The conditional PM under Gaussian $G$
  is JEBS Eq. (3): $\tau_j^* = \tau + (\hat{\tau}_j - \tau)\cdot S_j$. The marginal PM
  integrates over the posterior of $(\tau, \sigma_\tau^2)$ in a fully Bayesian fit.
- **CB** ("constrained Bayes"; Ghosh 1992): JEBS Eqs. (9)–(10). Rescales PM estimates so
  their finite-pop variance matches $\hat\sigma^2$. Specifically:
  $$\tau_j^{\rm CB} = \bar\tau + (\tau_j^* - \bar\tau)\cdot \frac{\hat\sigma}{\sqrt{v}},
  \quad \hat\sigma = \sqrt{J^{-1}\sum V_j + v},\ v = (J-1)^{-1}\sum (\tau_j^* - \bar\tau)^2.$$
- **GR** ("triple-goal"; Shen & Louis 1998): JEBS Eqs. (11)–(12). Three-step estimator
  using evenly spaced quantiles of $\bar{G}_J(t) = J^{-1}\sum \Pr(\tau_j \le t \mid \mathcal{D})$.
- **DP-diffuse / DP-inform**: JEBS Eq. (13), with hyperprior on $\alpha$ tuned to
  $\mathrm{Gamma}(2.5, 0.1)$ (diffuse) or $\mathrm{Gamma}(1.6, 1.22)$ (informative,
  $\chi^2(5)$-derived).

### 10.4 Why under-dispersion is unavoidable

Per Luke's reviewer guidance (and JEBS p. 7): the PM estimates have variance $v = (J-1)^{-1}
\sum (\tau_j^* - \bar\tau)^2 < \sigma_\tau^2$ because shrinkage pulls each $\tau_j^*$ toward
$\bar\tau$. metaDGP's role is to make this *visible*: by returning the realized variance of
$\tau_j$ alongside the realized variance of $\hat{\tau}_j$ and the design-implied $S_j$, users
can compare the spread of PM estimates against ground truth without re-fitting. This
under-dispersion is *the* statistical motivation for CB/GR.

---

## 11. Statistical pitfalls / known caveats

### 11.1 The `sigma_tau` × `variance` confusion (current code)

As detailed in §2.1, `gen_priorG2()` exposes both `sigma_tau` and `variance` and applies them
multiplicatively without ensuring that the underlying shape has unit variance. This is
silently wrong for Student-$t$, skew-normal, ALD, and the 2-component mixture. **Fix**:
expose only `sigma_tau` and standardize internally.

### 11.2 The `var_outcome` silent convention

Both JEBS Eq. (1) and `sim_sitesize_withinvar.R` set $s_Y^2 = 1$ implicitly (effect-size
units). A user who interprets `tau_j_hat` as a raw mean difference will silently get
incorrect SEs by a factor of $s_Y^2$. **Recommendation**: expose `var_outcome` (default 1)
and *all* outputs include explicit unit metadata: `units = "effect_size"` or
`units = "raw"`. Refuse mixed conventions: `tau` and `sigma_tau` must be specified in the
same units as `var_outcome`.

### 11.3 Mixture identifiability (label switching for the *DGP*)

When $G$ is a 2-component mixture, the assignment of "component 1" vs. "component 2" is
arbitrary in *estimation* — but the *DGP* must commit to a labeling for reproducibility.
metaDGP fixes the labeling by (a) ordering components by mean, then (b) by variance ties.
The output includes a per-site `latent_component` integer for users who want to score how
well a downstream model recovers the latent assignment.

### 11.4 Edge cases of $I$ and $R$

- $I \to 0$: $\sigma_\tau^2 \to 0$ or GM(SE²) → ∞. metaDGP refuses $I = 0$ (degenerate
  prior) and warns when the user requests $I < 0.01$ (rare in real designs; $I \approx 0.01$
  is the JEBS extreme).
- $I \to 1$: GM(SE²) → 0 (huge sites) or $\sigma_\tau^2 \to \infty$. metaDGP refuses $I=1$
  and warns at $I > 0.95$.
- $R = 1$: homoskedastic, fine.
- $R \to \infty$: log-uniform grid spans an arbitrary range. metaDGP warns at $R > 100$:
  the smallest sites become numerically negligible.

### 11.5 Truncated-Gamma infeasibility

If $\bar{n}_j$ is close to $n_{j,\min}$ and `cv` is small, the targets may be
infeasible — the truncation point already captures most of the mass. metaDGP performs a
feasibility check before solving (§3.3) and refuses with a clear error message rather than
returning a silent `nleqslv` non-convergence.

### 11.6 Site-identity loss under rank reorder

As noted in §5.1, the current `sim_observed_effects()` permutes site identities but does not
return the permutation. **Fix**: always return the permutation map as
`attr(result, "se2_perm")`, so downstream joins to site-level metadata stay consistent.

### 11.7 Precision dependence and EB hyperparameter bias

When $\rho_{\rm Spearman}(\tau_j, \widehat{se}_j^{\,2}) \ne 0$, *standard* EB estimators of
$(\tau, \sigma_\tau^2)$ are biased — see research note 14 and Walters (2024). metaDGP's role
is to *generate* these scenarios cleanly so downstream estimators can be benchmarked; it
should also document that the bias is real and that users evaluating PM/CB/GR with non-zero
`rank_corr` are evaluating estimators that *implicitly assume independence*.

### 11.8 Floating-point in BC and KS

For very tight target $G$ (e.g. point mass), naive density estimation on a histogram grid can
produce $\mathrm{BC} \to 1$ artifactually (both densities concentrated in one bin). metaDGP
uses a kernel density estimate with a Silverman bandwidth as the default, with an option for
adaptive bandwidth.

---

## 12. Open methodological questions

These should be discussed with Luke, Jonathan, Sophia, Avi, and the IES advisory before
metaDGP locks its API.

**Q1. Should DPM generators ship in metaDGP itself, or in a sibling?** Argument for *here*:
the team's current code already includes DP generation, and end-users want a one-stop shop.
Argument against: DPM requires `dirichletprocess` or NIMBLE as a heavyweight dependency, and
the DP-inform / DP-diffuse choices are estimation-side concerns better located in the
estimator package. *Lean*: ship DPM generation in a sibling, expose the user-supplied $G$
slot in metaDGP that takes any function or vector.

**Q2. Should Bhattacharyya feasibility indices be computed natively?** They are useful
diagnostics (research note 04), but they require the user to specify a target $G$ density,
which couples metaDGP to a density-estimation library. *Lean*: ship a thin wrapper
`feasibility_index()` that takes a fitted-vs-true comparison and returns $\mathrm{BC}$, and
use it internally for §6 diagnostics. Keep the four-tier interpretation as a *guideline* not
an enforced threshold.

**Q3. Finite-pop vs. super-pop default reporting.** Should the default `summary()` method
prioritize finite-pop diagnostics (realized $\hat\sigma_\tau$, $\hat I$, $\hat F_J$) or
super-pop diagnostics (KS, BC, parametric form of $G$)? *Lean*: report both, with a one-line
flag indicating which estimand each is closest to.

**Q4. Student-$t$ copula for tail-dependent precision dependence.** Should this ship in v1
or wait for evidence that practitioners want it? The Gaussian copula has zero tail dependence,
which may understate real-world precision-dependence in the tails (large effects also have
large SEs *especially* in the tails). *Lean*: defer to v2; document the gap and provide a
`copula = "gaussian"` API with a clear extension point.

**Q5. Cluster-randomized vs. individual-randomized sites.** The default $\kappa$ formula
assumes individual randomization within site. For cluster-randomized designs (classrooms
within schools as the unit of randomization), the design effect $1+(m-1)\rho_{ICC}$ enters.
Should this be a first-class parameter or a user-supplied SE function? *Lean*: expose
`design_effect = "individual"` (default) | `"cluster"` | function, with the cluster variant
taking $(m_j, \rho_{ICC})$ as additional inputs.

**Q6. Should metaDGP support multiple outcomes / multivariate $G$?** Real multisite trials
often have several outcomes (test scores, attendance, graduation), inducing a $K$-dimensional
$G$ on the joint $\boldsymbol{\tau}_j \in \mathbb{R}^K$. *Lean*: out of scope for v1; the
current 1-D framework already covers JEBS 2024 / IES R305D240078; multivariate is a
v2 feature.

**Q7. Should metaDGP handle non-normal Stage-1 likelihoods?** JEBS Eq. (1) uses normal
because of CLT; for sparse outcomes (rare events, very small $n_j$) the normal approximation
fails. Binomial / Poisson / negative-binomial Stage-1 likelihoods are useful for
sensitivity-to-CLT analyses. *Lean*: out of scope for v1, document as a future direction.

**Q8. Connection to `multisitepower` framing (per Jonathan).** Should metaDGP expose
$\overline{\mathrm{MOE}}$ and minimum-detectable-effect (MDE) summaries directly? *Lean*: yes,
ship as part of §6.4 diagnostics, since the math is one line. Defer power-curve computation
(which requires inverting MOE for different effect sizes) to a sibling.

---

## Appendix: Notation reference

| Symbol | Meaning | Vocab |
|--------|---------|-------|
| $J$ | Number of sites. | `J` |
| $\tau_j$ | True site-specific effect at site $j$. | `tau_j` |
| $\hat{\tau}_j$ | ML estimate of $\tau_j$ (Stage-1 likelihood draw). | `tau_j_hat` |
| $\widehat{se}_j$ | Standard error of $\hat{\tau}_j$. | `se_j` |
| $\widehat{se}_j^{\,2}$ | Sampling variance. | `se2_j` |
| $\tau$ | Mean of $G$. | `tau` |
| $\sigma_\tau$ | SD of $G$. | `sigma_tau` |
| $\sigma_\tau^2$ | Variance of $G$. | `sigma_tau^2` |
| $G$ | Prior / super-pop distribution of $\tau_j$. | `G` |
| $\boldsymbol{\theta}_G$ | Shape parameters of $G$ (e.g. df, mixture weights). | shape parameters |
| $I$ | Average informativeness (JEBS Eq. 4). | `I` |
| $R$ | Heterogeneity ratio of $\widehat{se}_j^{\,2}$. | `R` |
| $R^2$ | Within-site $R^2$ from covariates (distinct from $R$). | `R2` |
| $\kappa$ | $(1/p + 1/(1-p))(1-R^2)\,s_Y^2$. | `kappa` |
| $n_j$ | Site $j$ sample size. | `nj` |
| $\bar{n}_j$ | Average site size. | `nj_mean` |
| $\mathrm{CV}$ | $\mathrm{sd}(n_j)/\bar{n}_j$. | `cv` |
| $n_{j,\min}$ | Truncation lower bound for $n_j$. | `nj_min` |
| $p$ | Treatment proportion. | `p` |
| $S_j$ | PM shrinkage factor (JEBS Eq. 3). | `shrinkage_j` |
| $F_J$ | Empirical CDF of $\{\tau_j\}_{j=1}^J$. | finite-pop EDF |
| $F_G$ | CDF of $G$. | super-pop CDF |
| $\rho_S, \rho_P$ | Spearman / Pearson correlation. | `rank_corr`, `pearson_corr` |
| $\mathrm{BC}$ | Bhattacharyya coefficient. | feasibility index |
| $D_J$ | Kolmogorov–Smirnov statistic. | KS |
