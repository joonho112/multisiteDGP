# multisiteDGP 0.2.0 — review packet

You are reviewing an R package before its 0.2.0 release. This packet is
self-contained: everything you need to judge the release is either here or in
the package itself. Please do not ask for the development history — the point of
this review is to see the package the way a new user or a journal referee would.

---

## 1. What the package does

`multisiteDGP` generates synthetic multisite-trial data from summary-level
design quantities, so that a researcher can study estimator behaviour before any
real data exist. The typical user is designing a multisite trial power analysis,
choosing between meta-analytic estimators, or sweeping a scenario grid.

The generative process has four layers:

| Layer | What it produces | Entry point |
|---|---|---|
| 1 | standardized latent site effects `z_j`, rescaled to `tau_j` | `gen_effects()` |
| 2 | per-site precision — site sizes `n_j` and sampling variances `se2_j` | `gen_site_sizes()`, `gen_se_direct()` |
| 3 | dependence between effects and precisions | `align_rank_corr()`, `align_copula_corr()`, `align_hybrid_corr()` |
| 4 | observed estimates `tau_j_hat` | `gen_observations()` |

`sim_multisite()` and `sim_meta()` are single-call front doors over all four.

Two paradigms exist. **Site-size** designs derive precision from generated site
sizes; **direct** designs set precision through an informativeness parameter `I`
and a max/min variance ratio `R`.

The methods follow Lee, Che, Rabe-Hesketh, Feller and Miratrix (2025),
*Journal of Educational and Behavioral Statistics* 50(5), 731–764.

## 2. What 0.2.0 changes

0.2.0 is a reliability release. **The simulated data is unchanged** — every
golden fixture reproduces bit for bit. What changed is the reproducibility
contract, the advertised surface, and a number of error paths.

The three breaking changes:

1. **`canonical_hash()` values all move.** The hash no longer covers the derived
   diagnostics, and the package version is no longer part of the payload. In
   exchange the hash became portable: the same design and seed give the same
   hash on any platform, where previously Linux was a strict baseline and
   macOS/Windows were exempt from hash equality.
2. **`gen_effects()` no longer takes `upstream`.** It only ever aborted.
3. **`scenario_audit()` gained a `target_source` column.**

`NEWS.md` has the full entry. Read it before the code.

## 3. Four review focuses

### Focus 1 — the reproducibility contract

The package makes a strong claim: *the same design and the same seed produce the
same data, on any platform, bit for bit.*

- Is the claim stated clearly enough that a user knows what to expect?
- Do the documentation, the code, the tests and the shipped fixtures agree about
  what the contract is?
- Is there anything the contract implies that the package does not actually
  check?

Start at `system.file("REPRODUCIBILITY.md", package = "multisiteDGP")` and
`vignette("m7-reproducibility-provenance")`.

### Focus 2 — honesty

- Does the package advertise anything it does not do? `DESCRIPTION`, `NEWS.md`,
  `README.md`, the pkgdown site and the vignettes are all in scope.
- Are unimplemented features clearly marked as such at the point a user would
  reach for them, not only in a limitations section?
- Do the error messages send a user somewhere useful? Every error follows a
  three-part shape: what went wrong, why the rule exists, and a fix line.

### Focus 3 — do the tests actually verify anything

- When the suite passes, what has been established?
- The suite has no environment-gated tests and no skips. Does anything look like
  it is testing its own implementation rather than a contract?
- What defect could pass through this suite unnoticed?

### Focus 4 — statistical validity

- Do the mathematical claims match the implementation? Specifically the
  unit-variance standardization convention, the dependence-injection methods,
  and the diagnostic formulas (`I_hat`, `R_hat`, shrinkage, feasibility).
- `vignette("m1-statistical-dgp")` states the formal DGP;
  `vignette("m2-g-distribution-catalog")` derives the standardization for each
  shape. Do the derivations hold, and does the code do what they say?

## 4. Where to start

In this order:

| # | File | Why |
|---|---|---|
| 1 | `README.md` | what the package claims in one page |
| 2 | `NEWS.md` | what 0.2.0 changed and what breaks |
| 3 | `vignette("m1-statistical-dgp")` | the formal DGP |
| 4 | `vignette("m7-reproducibility-provenance")` | the contract, in use |
| 5 | `R/utils-reproducibility.R` | the contract, in code |
| 6 | `R/layer1-gen_effects.R` and `R/layer1-effects-common.R` | Layer 1 and the covariate path |
| 7 | `R/layer2-engine-a2.R` | the site-size solver, the most numerically delicate part |
| 8 | `tests/testthat/` | what is actually checked |

Run `devtools::test()` and `devtools::check()` if you want; both should be clean.

## 5. Questions we want answered

Please answer these directly, even if the answer is "yes, fine".

1. **Read the reproducibility contract. Can you say what you would have to do to
   get identical results on a different computer?** If any step is ambiguous,
   name it.
2. **Read `DESCRIPTION`, `NEWS.md` and the vignettes. Can you find anywhere the
   package advertises something that does not work?**
3. **Look at the test suite. Can you imagine a defect that would pass?** We are
   more interested in a plausible one than an exhaustive list.
4. **Which mathematical claim is not demonstrated by the implementation?**
5. Is there anything in the package that would embarrass a referee reading the
   JEBS paper alongside it?

## 6. Out of scope

Please do not spend time on:

- **Documentation prose and style.** The wording went through a separate review;
  we are not reopening it.
- **Feature proposals for a future version.** Useful, but not this review.
  `as_lme4()`, `as_brms()` and a built-in Dirichlet-process sampler are already
  known gaps and deliberately deferred.
- **Architecture redesign.** The four-layer structure is fixed for this release.
- **Test style or coverage percentage as a number.** We care whether the tests
  catch things, not how many lines they touch.

## 7. Known limitations, stated up front

So you can spend your time elsewhere:

- Exact reproduction of JEBS Figure 4(b) is deferred. The lme4 model behind it
  needs individual-level input or digitized figure targets, neither of which is
  in the repository. The appendix's *simulation* is reproduced exactly.
- `true_dist = "DPM"` is a reserved value that aborts; reach a Dirichlet-process
  mixture through the `g_fn` bridge.
- `scenario_audit()`'s Group C distributional gates need a reference quantile
  function, and only Gaussian and Student-t have one. Other shapes report
  `target_source = "not_available"` and are audited on the other three groups.
- The installed package is 14.2 MB, above CRAN's 5 MB guideline, because the
  vignettes embed a lot of figures. 0.2.0 is not a CRAN submission.
- `multisitepower` is not a declared `Suggests` dependency because it is not on
  CRAN; `as_multisitepower()` works once you install it yourself.
