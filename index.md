# multisiteDGP <img src="man/figures/logo.png" align="right" height="139" alt="" />

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

**Data-generating processes for multisite trial simulations**

You have a multisite trial to design, a meta-analysis to plan, or an estimator to stress-test. You need a defensible scenario you can take to a manuscript reviewer — citable provenance, realistic site-effect heterogeneity, plausible per-site sampling errors, and the dependence between effects and precisions that real trials exhibit. Rather than requiring you to assemble latent effects, sampling-error margins, and dependence structures from raw variance components, `multisiteDGP` allows specification through intuitive quantities — site counts, per-site sample sizes, a heterogeneity ratio, and a defensible preset.

<div class="row">
  <div class="col-md-4">
    <div class="card mb-4 h-100">
      <div class="card-body">
        <h5 class="card-title">Layered DGP</h5>
        <p class="card-text">Four generative layers — latent effects, site-size margins, precision dependence, observation draws — with eight built-in distribution shapes and a single-call front door.</p>
        <a href="articles/a1-getting-started.html" class="btn btn-primary">Get started</a>
      </div>
    </div>
  </div>
  <div class="col-md-4">
    <div class="card mb-4 h-100">
      <div class="card-body">
        <h5 class="card-title">Defensible presets</h5>
        <p class="card-text">Nine bundled scenarios — JEBS-paper, Walters-2024, Weiss-style education trials — each with a citation and a locked parameter set you can defend to a reviewer.</p>
        <a href="articles/a2-choosing-a-preset.html" class="btn btn-primary">Choose a preset</a>
      </div>
    </div>
  </div>
  <div class="col-md-4">
    <div class="card mb-4 h-100">
      <div class="card-body">
        <h5 class="card-title">Diagnostics</h5>
        <p class="card-text">Read realized heterogeneity, effect-precision correlation, and distributional fit off a diagnostics attribute — verify the design behaves as intended before committing to a long simulation run.</p>
        <a href="articles/a3-diagnostics-in-practice.html" class="btn btn-primary">Read diagnostics</a>
      </div>
    </div>
  </div>
</div>

<div class="alert alert-info">
The documentation runs on two tracks. Start with the
<a href="articles/a1-getting-started.html" class="alert-link">Applied Track</a>
if you have a trial to design, a power calculation to deliver, or a case study to write.
Start with the
<a href="articles/m1-statistical-dgp.html" class="alert-link">Methodological Track</a>
if you want the formal four-layer specification, the standardized-residual convention, and the contracts that govern every preset.
</div>

## Installation

```r
# Install from GitHub
# install.packages("devtools")
devtools::install_github("joonho112/multisiteDGP")

# Install from CRAN, when available
# install.packages("multisiteDGP")
```

## Quick start

```r
library(multisiteDGP)

# Verify a defensible scenario reproduces canonically.
# Scenario: 50-site education trial calibrated to the JEBS paper preset.
dat <- sim_multisite(preset_jebs_paper(), seed = 1L)

# Inspect the simulated dataset.
print(dat)
#> # A multisitedgp_data: 50 sites, paradigm = "site_size"
#> # Realized vs intended:
#> #   I: realized=0.250 (no target)
#> #   R: realized=7.583 (no target)
#> #   sigma_tau: target=0.200, realized=0.207, PASS
#> #   rho_S: target=0.000, realized=-0.193, PASS
#> #   Feasibility: WARN (n_eff=13.098)
#> # A tibble: 50 × 7
#>   site_index    z_j   tau_j tau_j_hat  se_j  se2_j   n_j
#>        <int>  <dbl>   <dbl>     <dbl> <dbl>  <dbl> <int>
#> 1          1 -0.582 -0.116    0.652   0.426 0.182     22
#> 2          2 -0.619 -0.124   -0.315   0.577 0.333     12
#> 3          3 -1.11  -0.222   -0.633   0.256 0.0656    61
#> # ℹ 47 more rows

# Read the realized informativeness off the diagnostics attribute.
attr(dat, "diagnostics")$I_hat
#> [1] 0.2500832

# Visualize the latent and observed site effects.
plot_effects(dat)

# Stamp the scenario for reproducibility — the hash is identical
# every time you rerun the same call.
canonical_hash(dat)
#> [1] "c52e75f276d82836"
```

In a single call you have a citable scenario, a printed dataset, a realized-target diagnostic, a publication-ready plot, and a stable provenance hash.

## Vignettes

### Applied Track

| Vignette | What you get |
|----------|--------------|
| [A1 · Getting started](articles/a1-getting-started.html) | First simulation in 5 minutes |
| [A2 · Choosing a preset](articles/a2-choosing-a-preset.html) | Walk through the nine bundled presets |
| [A3 · Diagnostics in practice](articles/a3-diagnostics-in-practice.html) | Verify a design before you simulate |
| [A4 · Covariates and dependence](articles/a4-covariates-precision-dependence.html) | Inject effect-precision dependence |
| [A5 · Calibrating to real data](articles/a5-calibrating-real-data.html) | Match a simulation to a target study |
| [A6 · Case study — multisite trial](articles/a6-case-study-multisite.html) | End-to-end education-trial example |
| [A7 · Case study — meta-analysis](articles/a7-case-study-meta-analysis.html) | End-to-end meta-analysis example |
| [A8 · Cookbook](articles/a8-cookbook.html) | Nine end-to-end recipes |

### Methodological Track

| Vignette | What you get |
|----------|--------------|
| [M1 · The two-stage DGP](articles/m1-statistical-dgp.html) | Mathematical framing of the four layers |
| [M2 · G-distribution catalog](articles/m2-g-distribution-catalog.html) | The eight built-in shapes, side by side |
| [M3 · Margin and SE models](articles/m3-margin-se-models.html) | Site-size-driven and direct-precision paths |
| [M4 · Precision dependence theory](articles/m4-precision-dependence-theory.html) | Rank, copula, and hybrid methods compared |
| [M5 · Custom G distributions](articles/m5-custom-g-distributions.html) | Bring your own latent distribution |
| [M6 · Adapters and downstream](articles/m6-adapters-downstream.html) | Adapter contract and round-trip invariants |
| [M7 · Reproducibility and provenance](articles/m7-reproducibility-provenance.html) | Seeds, hashes, and provenance strings |
| [M8 · Migration from siteBayes2](articles/m8-migration-from-siteBayes2.html) | Translate legacy scenario specs |

## Citation

Run `citation("multisiteDGP")` after install for the canonical software and JEBS-paper entries.

## Support

This research was supported by the Institute of Education Sciences,
U.S. Department of Education, through Grant R305D240078 to the
University of Alabama. The opinions expressed are those of the
authors and do not represent views of the Institute or the U.S.
Department of Education.

## License

MIT &copy; [JoonHo Lee](https://github.com/joonho112). See `LICENSE`.
