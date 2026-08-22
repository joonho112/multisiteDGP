# GPT Codex External Review — multisiteDGP v0.2.0

## Scope: the reliability release — reproducibility contract, advertised surface, test suite, statistical validity

You are the primary external reviewer for the `multisiteDGP` R package immediately
before its v0.2.0 release. You have direct filesystem access to every path below;
no archive is attached and none is needed.

**This is a different kind of review from the three you did in 2026-05.** Those
rounds reviewed *documentation* — metadata, roxygen, vignettes. This round reviews
**behaviour and contracts**: what the package promises, what it does, and whether
the tests can tell the difference.

The package root for this review is:

```
/Users/joonholee/Documents/00_IES Multisite Trial Project/multisiteDGP-R-package/
```

> **Path note.** The project folder was renamed since Rounds 1–3. The prefix is now
> `00_IES Multisite Trial Project`, not `IES Multisite Trial Project`. Every path in
> this document has been verified to exist as written.

---

## 1. What you are reviewing

### 1.1 The package as it will ship

`multisiteDGP` generates synthetic multisite-trial data from summary-level design
quantities, so a researcher can study estimator behaviour before real data exist.
Four generative layers:

| Layer | Produces | Entry point |
|---|---|---|
| 1 | standardized latent site effects `z_j`, rescaled to `tau_j` | `gen_effects()` |
| 2 | per-site precision — site sizes `n_j`, sampling variances `se2_j` | `gen_site_sizes()`, `gen_se_direct()` |
| 3 | dependence between effects and precisions | `align_rank_corr()`, `align_copula_corr()`, `align_hybrid_corr()` |
| 4 | observed estimates `tau_j_hat` | `gen_observations()` |

`sim_multisite()` and `sim_meta()` are single-call front doors. Two paradigms:
**site-size** derives precision from generated `n_j`; **direct** sets it through
informativeness `I` and max/min variance ratio `R`.

The methods follow Lee, Che, Rabe-Hesketh, Feller & Miratrix (2025), *JEBS* 50(5),
731–764.

### 1.2 In scope

Everything below the package root, with weight in this order:

1. **The reproducibility contract** — `R/utils-reproducibility.R`,
   `inst/REPRODUCIBILITY.md`, `tools/cross-os-reproducibility-policy.md`,
   `vignette("m7-reproducibility-provenance")`, and the fixtures that are supposed
   to enforce it.
2. **The numerical kernel** — `R/layer1-*.R`, `R/layer2-engine-a1.R`,
   `R/layer2-engine-a2.R`, `R/layer3-*.R`, `R/layer4-*.R`,
   `R/layer2-diagnostics.R`.
3. **The test suite** — all of `tests/testthat/`, plus `tests/data-raw/`.
4. **The advertised surface** — `DESCRIPTION`, `NEWS.md`, `README.md`,
   `_pkgdown.yml`, `index.md`, all 16 vignettes.
5. **The validation harness** — `tools/validation/`.

### 1.3 Out of scope

- **Documentation prose and voice.** Rounds 1–3 covered that surface and it is not
  reopened here. Flag prose only when it makes a *factual* claim that is wrong.
- **Feature proposals for v0.3+.** `as_lme4()`, `as_brms()`, a built-in
  Dirichlet-process sampler and a marginal-correlation target are all deliberately
  deferred and documented as such.
- **Architecture redesign.** The four-layer structure is fixed for this release.
- **Coverage percentage as a number.** Coverage is 96.42 %; that figure is not the
  point. Whether the tests catch things is.
- **The `main` branch-protection setting** and the **installed size** (14.2 MB,
  above CRAN's 5 MB guideline). Both are known, measured, and decided.

---

## 2. Context — what v0.2.0 changed and why

### 2.1 The starting condition

The package shipped v0.1.1 in 2026-05 after a documentation overhaul (your three
review rounds). It was then rushed into an IES annual report. By 2026-08 its CI had
been red for **three months** and the working copy was outside version control.

The upgrade found the red CI was not one bug but **four independent faults stacked**:

1. `.lintr` was never committed, so CI linted with defaults — 440 violations
   instead of 65.
2. `multisitepower` was declared in `Suggests` but resolvable from no repository,
   so macOS and Windows died in dependency installation *before running any code*.
3. The reproducibility contract was **unachievable as written** (see §2.2).
4. `covr::to_cobertura()` needs `xml2`, absent from the coverage workflow.

### 2.2 The reproducibility contract — the deepest change

**v0.1.x claimed:** Ubuntu Linux x86_64 is the strict hash baseline; macOS and
Windows are *demoted* to same-machine reproducibility plus distributional parity,
because cross-platform hash equality is unachievable.

**Why it was unachievable:** `canonical_hash()` hashed raw IEEE-754 doubles — one
ULP flips the hash — while the payload carried **derived diagnostics** (`I_hat`,
`R_hat`, `rho_S_*`, `rho_P_*`, `sigma_tau_*`) computed with `cor()`, `sd()` and
`mean()`, whose floating-point accumulation order is platform dependent. The drift
was visible *on a single machine*: for a covariate-free design `rho_P_marginal` and
`rho_P_residual` are mathematically identical yet differed in their last digits.

**v0.2.0 claims:** the same design and the same seed produce the same data on any
platform, bit for bit, and therefore the same `canonical_hash()`. No platform
hierarchy, no per-OS exemption.

**What made that achievable:** hash schema v3 drops the derived diagnostics from
the payload and rounds the remaining doubles to nine significant digits. The
diagnostics are functions of the hashed data and the hashed design, so they added
no provenance the hash did not already carry — only noise.

**Two further changes to the payload:**

- The **package version was removed from the manifest** (very late, during the
  version bump). v0.1.x had a hardcoded lineage bucket collapsing `0.0.0.9000` and
  `0.1.*` into one string, with no rule for `0.2`, so the bump moved every hash. The
  reasoning for removal: a hash is quoted in a manuscript as an anchor for *this
  data, this design*, and a version in the payload invalidated every recorded hash
  on every release. What now moves the hash when the package changes what the hash
  *means* is `hash_schema_version`.
- Engine A2's post-solve tolerance is **derived** rather than fixed at `1e-6`.
  Below a certain conditioning the residual comparison measures floating-point noise
  rather than fit quality, and the verdict was platform dependent — the same design
  passed on macOS and aborted on Linux.

### 2.3 What did *not* change

**The simulated data.** Every golden fixture reproduces bit for bit. The four JEBS
appendix fixtures are `identical()` on all seven columns for all four seeds, and
their `.rds` SHA-256 are unchanged throughout the upgrade.

### 2.4 Where the release stands

| Gate | State |
|---|---|
| Test suite | 4882 pass, 0 fail, **0 skip** (was 30 skips) |
| lint | 0 violations (was 440 under CI conditions) |
| `R CMD check --as-cran` | 0 error, 0 warning, 0 note |
| CI | green on `linux-release`, `linux-devel`, `linux-oldrel`, `macos-release`, `windows-release` |
| Validation | V0–V12, all thirteen pass in `full` mode |
| Defect ledger | 37 rows: 36 fixed, 1 documented limitation, 0 open |

**Twenty of the thirty-seven ledger entries were not in the original plan.** They
were found during execution. That is the relevant fact for calibrating your review:
the plan was not a complete inventory of what was wrong, and there is no reason to
believe the execution was either.

---

## 3. Reference materials

Read what you need. The paths are absolute and verified.

### 3.1 The package itself

```
/Users/joonholee/Documents/00_IES Multisite Trial Project/multisiteDGP-R-package/
```

Highest-value files for this review:

| Path (relative to package root) | Why |
|---|---|
| `R/utils-reproducibility.R` | the hash contract in code — canonicalization, payload, manifest |
| `R/layer2-engine-a2.R` | truncated-Gamma solver; the most numerically delicate code in the package |
| `R/layer1-effects-common.R` | the covariate path — lowest-covered file before this upgrade |
| `R/layer1-gen_effects.R` | Layer 1 dispatcher and the `true_dist` catalog |
| `R/layer3-align_copula_corr.R`, `R/layer3-align_rank_corr.R`, `R/layer3-align_hybrid_corr.R` | three dependence-injection methods |
| `R/layer2-diagnostics.R` | `I_hat`, `R_hat`, shrinkage, feasibility formulas |
| `R/scenario_audit.R` | the four diagnostic gate groups |
| `R/00-errors-validation.R` | the typed error hierarchy and shared validators |
| `inst/REPRODUCIBILITY.md` | the policy that ships to users |
| `tools/cross-os-reproducibility-policy.md` | the maintainer-side policy |
| `NEWS.md` | the v0.2.0 entry, including breaking changes |
| `tests/testthat/` | 505 test files |
| `tests/data-raw/` | fixture and print-example generators |
| `tools/validation/` | thirteen validation experiments and their harness |
| `tools/jebs-golden-fixtures/` | the JEBS appendix reimplementation and its manifest |

### 3.2 The upgrade's own paperwork

The log folder for this upgrade:

```
/Users/joonholee/Documents/00_IES Multisite Trial Project/multisiteDGP-R-package/log/log-version-up-2026-08-14/
```

| Path | What it is |
|---|---|
| `STATUS.md` | current state, read this first for orientation |
| `defect-ledger.csv` | all 37 defects: symptom, root cause, repro, fix, regression test |
| `001_plan-master-version-upgrade/` | the master plan (Quarto Book, 19 chapters) |
| `002_phase01-foundation-recovery.qmd` … `012_phase10-external-review.qmd` | eleven phase logs |
| `decisions/` | PI decision records D1–D4, D6–D9, branch-protection request |
| `evidence/phase01/` … `evidence/phase09/` | measurement scripts and their outputs |
| `review-packet/README.md` | the reviewer-facing packet written for this round |

**On using this material.** The plan and the logs are the *claim*, not the evidence.
Where a log says a thing was verified, the verification script is under
`evidence/`. If you want to know whether something holds, prefer running the check
to reading the claim. Places where the logs record my own errors during execution
are listed in §6.1 — those are calibration, not decoration.

### 3.3 The design blueprint — original intent

```
/Users/joonholee/Documents/00_IES Multisite Trial Project/multisiteDGP-design-archive/blueprint-multisiteDGP-book/
```

29 chapters. The ones that bind this review:

| Chapter | Topic |
|---|---|
| `04-statistical-dgp.qmd` | the formal two-stage DGP |
| `05-effect-distributions.qmd` | the `G` catalog and the unit-variance convention |
| `06-margin-se-models.qmd` | site-size and direct-precision margins; engines A1 / A2 |
| `07-precision-dependence.qmd` | rank / copula / hybrid injection |
| `08-covariates-and-identifiability.qmd` | the residual-scale interpretation |
| `14-diagnostics.qmd` | diagnostic groups A–D and their formulas |
| `16-error-ux.qmd` | the typed error hierarchy and the three-part message shape |
| `18-test-strategy.qmd` | what the test suite was designed to prove |
| `19-validation-experiments.qmd` | V0–V12 definitions |
| `25-open-questions.qmd` | Q1–Q20, including Q14 on OS-specific reproducibility |
| `appendix-A-jebs-mapping.qmd` | JEBS paper → package mapping |

**Q14 matters for this review.** The blueprint deferred cross-OS bit-identical RNG
as research. v0.2.0 claims the question is closed. Is the blueprint's framing of the
problem still right, and does the v0.2.0 answer actually address it?

### 3.4 The JEBS paper — the scientific source

| Path | What it is |
|---|---|
| `/Users/joonholee/Documents/00_IES Multisite Trial Project/dev/references/lee-et-al-2024-improving-the-estimation-of-site-specific-effects-and-their-distribution-in-multisite.md` | the published paper, full text markdown |
| `/Users/joonholee/Documents/00_IES Multisite Trial Project/dev/references/lee-et-al-2024-improving-the-estimation-of-site-specific-effects-and-their-distribution-in-multisite-trials.pdf` | the same, PDF |
| `/Users/joonholee/Documents/00_IES Multisite Trial Project/2023-11-30_JEBS Multisite Paper Revision/Lee et al. (2023). JEBS_arXiv preprint.pdf` | arXiv preprint |
| `/Users/joonholee/Documents/00_IES Multisite Trial Project/2023-11-30_JEBS Multisite Paper Revision/2024-03-01_arXiv updates/Version_02_TeX Source/qmhwkvdrfgxygphzbpvzwvwzsdbysjqf/main.tex` | LaTeX source of the preprint |
| `/Users/joonholee/Documents/00_IES Multisite Trial Project/dev/Bayes-deconvolution/posts/2024-01-21_JEBS_Software_Appendix/2024-01-21_JEBS_Software_Appendix_E.qmd` | **the software appendix — the source the golden fixtures reproduce** |

The appendix QMD is load-bearing. `tools/jebs-golden-fixtures/generate-jebs-golden-fixtures.R`
reimplements its `gen_priorG_mixture`, `gen_nj_se2j_vec_gamma` and `gen_tau_j_hat`
in base R, and `tests/testthat/test-T1a-jebs-bit-identical.R` asserts the package
matches that reimplementation exactly. **Check that the reimplementation is faithful
to the appendix.** If it drifted, the whole reproduction claim rests on comparing
the package against a wrong reference.

### 3.5 Other reference papers

Read selectively when a specific claim looks suspicious. All under
`/Users/joonholee/Documents/00_IES Multisite Trial Project/dev/references/`:

| File | Relevance |
|---|---|
| `Walters - 2024 - Empirical Bayes Methods in Labor Economics copy.md` | `preset_walters_2024`; EB methods |
| `Walters - 2024 - Empirical Bayes Methods in Labor Economics_Short version.md` | short version |
| `Raudenbush and Bloom - 2015 - Learning About and From a Distribution of Program _Unannotated.md` | cross-site effect distribution |
| `Miratrix et al. - 2021 - An Applied Researcher's Guide to Estimating Effect.md` | Miratrix is a JEBS coauthor |
| `Jonathan Che_dissertation_chapter 3.md` | Che is a JEBS coauthor |
| `Chen - 2024 - Empirical Bayes When Estimation Precision Predicts.md` | **effect–precision dependence — directly relevant to Layer 3** |
| `Using copulas for making calibrated data generating processes (DGPs) for simulation _ Cares Blog.md` | **copula DGP construction — relevant to `align_copula_corr()`** |
| `Meager - 2019 - Understanding the Average Impact of Microcredit Ex copy.md` | heterogeneity benchmarks |
| `Andrews et al. - 2024 - Inference on Winners.md` | winner's curse; Group D diagnostics |
| `BDA3_Finite mixture models.md` | mixture-shape standardization |
| `Stephens - 2017 - False discovery rates a new deal.md` | EB foundations |
| `Gu and Koenker - 2023 - Invidious Comparisons Ranking and Selection as Co.md` | ranking and selection |
| `Tipton - 2014 - How Generalizable Is Your Experiment An Index for Comparing Experimental Samples and.md` | generalizability index |
| `Deke et al. - The BASIE (BAyeSian Interpretation of Estimates) framework for interpreting findings f.md` | IES-funded framework; `preset_education_*` context |

### 3.6 Reference R package sources

| Path | Why |
|---|---|
| `/Users/joonholee/Documents/00_IES Multisite Trial Project/dev/code/siteBayes2/` | **predecessor package; `vignette("m8-migration-from-siteBayes2")` documents migration from it — verify the rename map against this source** |
| `/Users/joonholee/Documents/00_IES Multisite Trial Project/dev/code/siteBayes/` | earlier predecessor |
| `/Users/joonholee/Documents/00_IES Multisite Trial Project/dev/code/ebnm-master/` | Stephens' EB normal means package — diagnostic terminology benchmark |
| `/Users/joonholee/Documents/00_IES Multisite Trial Project/dev/code/saeSim-main/` | small-area-estimation simulation package — `preset_small_area_estimation` context |
| `/Users/joonholee/Documents/00_IES Multisite Trial Project/dev/code/Walters - 2024 - Empirical Bayes Methods in Labor Economics_Replication/` | **Walters replication code — verify `preset_walters_2024` parameter values** |
| `/Users/joonholee/Documents/00_IES Multisite Trial Project/dev/code/MultisiteMediation-master/` | multisite mediation reference |
| `/Users/joonholee/Documents/00_IES Multisite Trial Project/DPprior/` | sister package, same PI and grant — R-code convention benchmark |
| `/Users/joonholee/Documents/00_IES Multisite Trial Project/bayesEfron-R-package/` | sister package, same PI — reproducibility-apparatus benchmark |

### 3.7 The previous external review cycle (Rounds 1–3, 2026-05)

Same reviewer, same package, documentation surface. Directory:

```
/Users/joonholee/Documents/00_IES Multisite Trial Project/multisiteDGP-design-archive/documentation-updates/log/
```

| Round | Prompt | Your review | The PI's synthesis |
|---|---|---|---|
| 1 — metadata + pkgdown | `019_codex-review-prompt-round-1-metadata-pkgdown.md` | `019_codex_review/` | `020_review-synthesis-round-1/` |
| 2 — roxygen | `055_codex-review-prompt-round-2-roxygen.md` | `055_codex_review/` | `056_review-synthesis-round-2/` |
| 3 — vignettes | `082_codex-review-prompt-round-3-vignettes.md` | `082_codex_review/` | `083_review-synthesis-round-3/` |

Also `032_step4.1-codex-round-1-closeout.qmd` (Round 1 remediation closeout) and
`CLOSEOUT.md` (the whole overhaul's closeout).

**Read at minimum the three synthesis books' `03-validity-assessment.qmd`.** They
record which of your findings the PI judged valid and which were rejected, with
reasons. That is the most direct calibration available for what this PI considers a
useful finding.

**One carry-over is live.** Round 2's **F-7** — the typed error catalog (E01–E30)
absent from `?multisiteDGP` — was deferred to "Phase 10" of that overhaul and, as
far as this upgrade's ledger records, was never closed. `tools/traceability/error-index.csv`
tracks E01–E30. If it is still absent, say so.

### 3.8 Traceability ledgers

```
/Users/joonholee/Documents/00_IES Multisite Trial Project/multisiteDGP-R-package/tools/traceability/
```

Nine CSVs: `api-index.csv`, `decision-index.csv`, `docs-index.csv`, `error-index.csv`,
`fixture-index.csv`, `invariant-index.csv`, `preset-index.csv`, `validation-index.csv`,
`conflict-checklist.csv`. `tests/testthat/test-traceability.R` enforces some
consistency between them and the code. **Where it does not, the ledgers can drift
silently — check a sample against reality.**

---

## 4. Review criteria

### 4.1 The reproducibility contract

The package makes an unusually strong claim. Test it.

- **Is the contract stated the same way everywhere?** `inst/REPRODUCIBILITY.md`,
  `tools/cross-os-reproducibility-policy.md`, `vignette("m7-...")`, and the
  `canonical_hash()` roxygen were all edited during this upgrade, some of them late.
  Do they agree?
- **Does the code do what the policy says?** The policy claims the hash covers data
  columns, column names, and a manifest of paradigm / `design_hash` /
  `hash_schema_version` / callback names — and deliberately excludes the derived
  diagnostics, function bodies, and the package version. Verify against
  `.hash_manifest()` and `.canonicalize_for_hash()`.
- **Is the portability claim actually established, or only asserted?** The evidence
  offered is that CI passes on five cells. What exactly does `test-T1a-jebs-bit-identical.R`
  compare, and does passing it establish the general claim or only the four-seed
  JEBS case?
- **What breaks the contract?** The policy says adding a libm-dependent quantity to
  a data column would break it. Is anything already close? Engine A2's solver output
  is quantised — is the quantisation sufficient, and is the argument for it sound?
- **Nine significant digits.** Is that the right threshold? Rounding to 9 digits
  means two genuinely different simulations agreeing to 9 digits hash the same. Is
  that reachable in practice for any realistic design?
- **Removing the version from the payload.** Is this defensible? The stated
  reasoning is in `inst/REPRODUCIBILITY.md`. What does a user lose?

### 4.2 Honesty — advertised versus implemented

The upgrade's stated goal was "advertised surface == implemented surface". Test it.

- **The catalog is now advertised as seven shapes** (Gaussian, Student-t,
  skew-normal, asymmetric Laplace, two-component mixture, point-mass slab, user
  callback), with `"DPM"` a reserved `true_dist` value that aborts. Is that count
  right *everywhere*, and is DPM's status clear at the point a user reaches for it?
- **Reserved arguments.** `gen_effects()`'s `upstream` was removed;
  `target_marginal_rho` was kept and aborts. Is that pair of decisions coherent?
- **Do the error messages send a user somewhere useful?** Every error follows a
  three-part shape: what went wrong, why the rule exists, a fix line beginning
  `Try` / `Use` / `Pass` / `Remove`. `tests/testthat/test-error-message-conformance.R`
  parses `R/` and checks all 223 fix lines statically. Are the messages *right*, not
  just well-shaped?
- **`scenario_audit()`'s narrowed `pass`.** For shapes without a reference quantile
  function the Group C gates are skipped, not failed, and `pass = TRUE` therefore
  means "every gate that could run passed". A `target_source` column reports which
  happened. **Is that adequate, or does `pass` now mislead?** The PI considered
  promoting such cells to `WARN` and chose not to; the reasoning is in
  `008_phase07-defect-remediation.qmd`. Argue the other side if you think it is
  stronger.

### 4.3 Do the tests verify anything

This is the focus where an outside reader is most valuable, because the person who
wrote the tests cannot see what they assume.

- **When the suite passes, what has been established?** 4882 assertions, 0 skips.
- **What defect could pass through?** One plausible, concrete example is worth more
  than a list.
- **Are any tests testing their own implementation?** Several were written by
  reading the code they test. Look especially at
  `test-error-message-conformance.R` (parses the source it validates),
  `test-scenario-audit-validation.R` (derives its thresholds from the cell's own
  measured quantiles), and `test-utils-reproducibility-canonicalization.R`.
- **Are the invariants still what the blueprint intended?** `18-test-strategy.qmd`
  defines T1–T20 and the property tests. Thirty tests that were previously gated
  behind environment variables now always run. Did anything get weakened while being
  ungated?
- **The golden fixtures.** They are the authority for "did the data move". Are they
  a strong enough authority? Four JEBS seeds and five preset outputs, nine files.
- **The validation harness.** V0–V12 in `tools/validation/`. `RESUME` defaults to
  true, so a re-run without `OVERWRITE=true` reuses previous results and reports a
  pass. Is that a footgun that has already fired somewhere in this repository?

### 4.4 Statistical validity

- **The unit-variance convention.** Every `G` shape is standardized so `var(z_j) = 1`
  in expectation. Verify the derivations in `05-effect-distributions.qmd` and
  `vignette("m2-g-distribution-catalog")` against the implementations in
  `R/layer1-gen_effects_*.R`. The Student-t documentation now reports measured
  realized `var(z_j)` by `nu` (median 0.68 at `nu = 2.5`, `J = 200`) and explains it
  as right-skew of the sample variance. **Is that explanation correct?**
- **Dependence injection.** Three methods — rank hill-climb, Gaussian copula,
  hybrid. `align_copula_corr()`'s `pearson_corr` is the *latent* Gaussian-copula
  correlation, with realized Spearman ≈ (6/π)·arcsin(ρ_P/2). Is the parameterization
  documented in a way that would stop a user from misreading it? (During this
  upgrade I misread it myself and briefly recorded a false defect.)
- **The diagnostics.** `I_hat`, `R_hat`, `mean_shrinkage()`, `feasibility_index()`,
  `bhattacharyya_coef()`, `ks_distance()`. Do the formulas in `R/layer2-diagnostics.R`
  and `R/diagnostics-core.R` match `14-diagnostics.qmd` and the JEBS paper?
- **Engine A2's feasible region.** `gen_site_sizes()` now documents that the
  boundary depends only on the ratio `n_min / nj_mean`, with a measured table. Is
  that scale-invariance claim right, and is the derived tolerance
  (`tol_effective = max(tol, noise_floor(alpha, cv))`) sound? The noise floor comes
  from cancellation in `lgamma(alpha + k) − lgamma(alpha)` and in
  `sd² = E[X²] − E[X]²`.
- **The covariate path.** Effects under a covariate are interpreted on the
  *residual* scale. `08-covariates-and-identifiability.qmd` states the identification
  argument. Does `R/layer1-effects-common.R` implement it, and is `sigma_tau`'s
  meaning under a covariate unambiguous to a user?

### 4.5 Improvement suggestions

Separate from defects. What would make this package more trustworthy to a referee
reading the JEBS paper alongside it? Concrete and scoped, please — "add more tests"
is not actionable; "the copula parameterization needs a worked numerical example
showing ρ_P versus realized ρ_S" is.

---

## 5. Output

### 5.1 Where to write

Create this directory and write your report into it:

```
/Users/joonholee/Documents/00_IES Multisite Trial Project/multisiteDGP-R-package/log/log-version-up-2026-08-14/013_codex_review/
```

Deliver **both**:

- `013_codex_review.qmd` — Quarto source
- `013_codex_review.html` — rendered, self-contained

Use this frontmatter so the render is self-contained and matches the project's logs:

```yaml
---
title: "GPT Codex External Review — multisiteDGP v0.2.0"
subtitle: "Reliability release: reproducibility contract, advertised surface, test suite, statistical validity"
author: "GPT Codex"
date: "2026-08-15"
format:
  html:
    theme: cosmo
    toc: true
    toc-depth: 3
    number-sections: true
    code-overflow: wrap
    embed-resources: true
execute:
  echo: true
  eval: false
---
```

Render with `quarto render 013_codex_review.qmd`. `embed-resources: true` is
required — the HTML must open standalone.

### 5.2 Report structure

1. **Executive summary** — what you reviewed, how long you spent, your overall
   verdict, and the three findings that matter most.
2. **Findings by severity.** For each finding give: an ID (`C-1`, `I-1`, `P-1`),
   the file and line, what is wrong, why it matters, how to reproduce or verify it,
   and a suggested fix. Group as:
   - **Critical** — a factual error in a contract or a claim; a test that cannot
     catch what it says it catches; a statistical formula that is wrong; a
     reproducibility claim that does not hold; a documented behaviour the code does
     not exhibit.
   - **Important** — an ambiguity a user could act on wrongly; a missing check where
     the contract implies one; an inconsistency between two documents that both
     ship; a test that is weaker than it appears.
   - **Polish** — anything that would improve trust without changing behaviour.
3. **Focus 1 — the reproducibility contract.** Your assessment as a section, not
   only as findings.
4. **Focus 2 — honesty.**
5. **Focus 3 — do the tests verify anything.**
6. **Focus 4 — statistical validity.**
7. **Answers to the five direct questions** (§7 below), one subsection each.
8. **Improvement suggestions**, ranked by value-to-effort.
9. **Verdict** — one of:
   - **RELEASE**: ship v0.2.0 as is.
   - **RELEASE WITH FIXES**: ship after the listed Critical items; name them.
   - **DEFER**: something is wrong enough that the release should wait; say what.
10. **Appendix: methodology.** What you read in full versus spot-checked, what you
    executed, what you flagged but could not verify, and how you sampled. **Say
    plainly where you ran out of time or confidence** — a stated gap is more useful
    than an unstated one.

### 5.3 Tone, depth, calibration

- **Tone:** direct, professional, fair. Match Rounds 1–3.
- **Depth:** weight your time toward `R/utils-reproducibility.R`,
  `R/layer2-engine-a2.R`, `tests/testthat/`, and the three reproducibility policy
  documents. Spend the least on vignette prose — Rounds 1–3 covered it.
- **Execution is welcome.** You have filesystem access. Running
  `devtools::test()`, `covr`, or a targeted script is more valuable than inferring
  from source. If you verify a claim by execution, say so; if you infer, say that
  too.
- **Severity calibration.** A finding is Critical if a competent user could be
  misled into a wrong scientific conclusion, or if a stated contract does not hold.
  A finding is Important if it would cost a user time or trust. Everything else is
  Polish. **Do not inflate.** Rounds 1–3 were valued because the Critical findings
  were genuinely critical.

---

## 6. Things to be alert about

### 6.1 Errors I made during this upgrade

These were caught, but they show where this codebase misleads a careful reader. If
you find the same *pattern* still present, that is a finding.

- **I compared `pearson_corr` against realized Pearson correlation** and reported a
  0.20 error as a copula defect. `pearson_corr` is the *latent* Gaussian-copula
  correlation; realized Spearman is (6/π)·arcsin(ρ_P/2). The measurement was wrong,
  not the package. **If the parameterization misled me after reading the roxygen, it
  will mislead a user.**
- **I asserted "the data columns are bit-identical, so rounding is unnecessary"**
  and shipped a hash schema without rounding. It held for the nine golden fixtures
  but not for print examples and snapshot presets; Linux CI disproved it. The
  inference was from too small a sample.
- **I counted eight presets** using a regex that excluded `preset_walters_2024`
  (digits), and nearly "corrected" a vignette sentence that was already right.
- **I read `!identical(shuffle, TRUE)` as an inverted guard.** It is correct —
  `shuffle` defaults to `TRUE`, so the guard fires on departure from the default,
  exactly as `R` (default 1) does. **A guard that reads as inverted to a careful
  reader is worth flagging even when correct.**
- **I edited the wrong fixture manifest** — `tools/jebs-golden-fixtures/` has a
  4-fixture manifest, `inst/extdata/golden/` has the 9-fixture one the tests read.
  Their `rds_sha256` columns do not agree, which briefly looked like a data
  regression. **Two manifests describing overlapping fixtures with disagreeing
  provenance fields is itself worth a look.**

### 6.2 Late changes carry the most risk

The version bump to 0.2.0 happened at the very end and broke twelve tests, leading
to the removal of the package version from the hash payload — a contract change made
under time pressure, after most of the review-worthy work was done. Everything
downstream of that (every hash literal in the repository) was regenerated in one
pass. **Look there first.**

### 6.3 Places the upgrade deliberately left alone

Not defects; do not report them as such:

- v0.1.0's "Known limitations" section in `NEWS.md` is preserved as written, with
  corrections recorded in the v0.2.0 entry instead. That is a deliberate decision
  about what release notes are for.
- The installed size (14.2 MB) and the `main` branch-protection setting.
- Exact reproduction of JEBS Figure 4(b), which needs reference data the repository
  does not have.

### 6.4 What a "documented limitation" means here

`defect-ledger.csv` has one row with status `documented-limitation` (D-013, package
size). Everything else is `fixed`. **If you think something marked `fixed` is not,
say so** — that is exactly the kind of finding the ledger cannot produce about
itself.

---

## 7. The five questions

Answer each directly in §5.2 item 7, even if the answer is "yes, this is fine".

1. **Read the reproducibility contract. Can you state what a user must do to get
   identical results on a different computer?** Name any step that is ambiguous.
2. **Read `DESCRIPTION`, `NEWS.md` and the vignettes. Can you find anywhere the
   package advertises something that does not work?**
3. **Look at the test suite. Can you describe a defect that would pass?** One
   plausible example beats an exhaustive list.
4. **Which mathematical claim is not demonstrated by the implementation?**
5. **Is there anything here that would embarrass a referee reading the JEBS paper
   alongside the package?**

---

## 8. Logistics

- **Time budget:** roughly 4–6 hours. Weight it as §5.3 says.
- **Execution:** encouraged. The package installs with `pkgload::load_all()`; the
  suite runs with `devtools::test()`; `R CMD check --as-cran` is clean as of this
  writing. Environment for the check:
  `_R_CHECK_FORCE_SUGGESTS_=false`, `MULTISITEDGP_REPRODUCIBILITY_POLICY=portable-hash-v3`.
- **Quoting:** quote source lines exactly, with file and line number. For blueprint
  chapters prefer paraphrase plus section number.
- **Self-check:** before finalizing, re-read your Critical findings and confirm each
  is reproducible by another reader from what you wrote.
- **What happens next:** the PI translates your report into Korean and walks every
  Critical finding through with the Claude session, then builds a synthesis Quarto
  Book (your review verbatim, a Korean summary, a validity assessment, a remediation
  plan, and a phased implementation plan). **Your review is the base document for
  that plan** — findings you state precisely become work items; findings you state
  vaguely become arguments.

---

## 9. Acknowledgments

You reviewed this package three times in 2026-05, on its documentation surface.
Round 1 caught two citation fabrications; Round 2 caught seven roxygen findings;
Round 3 closed that cycle. The package then shipped, was rushed into a report, and
its CI went red for three months.

This round is the other half. The documentation was right and the behaviour was not
— an unachievable reproducibility contract, a catalog advertising a shape that
aborts, error branches no test had ever executed, and a test suite that reported
green while skipping thirty statistical invariants.

Twenty of the thirty-seven defects in the ledger were found during execution rather
than planned for. **Your job is the twenty-first onward.**

— PI: JoonHo Lee (University of Alabama, IES Grant R305D240078)
— Claude session: Opus 5, autonomous execution recorded in logs 002–012
