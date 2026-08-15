# Reproducibility Policy

multisiteDGP 0.2.0. This supersedes the v0.1 policy in full; see
[What changed in 0.2.0](#what-changed-in-020).

## The contract

**The same design and the same seed produce the same data, on any platform,
bit for bit — and therefore the same `canonical_hash()`.**

That is the whole promise. There is no baseline operating system, no
authoritative machine, and no per-OS exemption. A hash you record on your
laptop is a hash a reviewer reproduces on theirs.

Two things follow.

- **`canonical_hash()` is a portable identity.** Quote it in a manuscript, a
  log, or an issue and it means the same thing everywhere.
- **A hash mismatch is a real difference.** The design, the seed, or the
  package's hash schema genuinely differs. It is never platform noise, so do
  not go looking for a BLAS explanation.

## What the hash covers

The simulated data columns, the column names, and a manifest carrying the
paradigm, `design_hash`, `hash_schema_version`, and the names of any user
callbacks.

Data columns are hashed after rounding to nine significant digits. Each column
is platform stable for a specific reason:

| Column | Why it is stable |
|---|---|
| `n_j` | rounded to an integer before it leaves Layer 2 |
| `z_j`, `tau_j` | drawn from R's own RNG, which is identical across platforms |
| `se_j`, `se2_j`, `tau_j_hat` | IEEE arithmetic on the above |
| `site_index` | integer |

## What the hash deliberately excludes

**The package version.** A hash is quoted as an anchor for "this data, this
design", and putting the version in the payload made every release invalidate
every recorded hash even when nothing about the data moved. Upgrading
multisiteDGP does not change a hash you already published.

What *does* move the hash when the package changes what the hash means is
`hash_schema_version`, which is in the manifest. That is the field to check
when a hash you recorded no longer reproduces. The producing package version is
still recorded on the object and printed in `provenance_string()`; it is simply
not hashed.

**The derived diagnostics** — `I_hat`, `R_hat`, `rho_S_*`, `rho_P_*`,
`sigma_tau_*`. They are computed with `cor()`, `sd()` and `mean()`, whose
floating-point accumulation order varies by platform. The drift is visible even
on one machine: for a covariate-free design `rho_P_marginal` and
`rho_P_residual` are mathematically identical yet differ in their last digits.

Excluding them costs nothing. They are functions of the hashed data and the
hashed design, so they add no provenance the hash does not already carry — only
noise. To pin one deliberately:

```r
canonical_hash(dat, diagnostics_to_include = "I_hat")
```

**Function bodies and environments.** Callbacks (`g_fn`, `se_fn`,
`dependence_fn`, `obs_fn`) are recorded by presence and hook name, not by body.
Two runs passing different closures in the same role hash the same; the
manifest records that a hook was present.

## Same-machine reproducibility

Given the same package version, R runtime, machine, design and seed, repeated
calls return identical `canonical_hash()` values, and a seed-supplied wrapper
call leaves your `.Random.seed` untouched. The package never manufactures seeds
from your global RNG state.

## What this rests on

The contract holds because every hashed column is platform stable. **Adding a
libm-dependent quantity to a data column would break it**, and the symptom
would be a cross-platform hash mismatch that looks like the v0.1 problem all
over again.

Engine A2's truncated-Gamma solver is the case that made this concrete. It
calls `nleqslv` with `ftol = 1e-12`, so its solution is pinned only to about
1e-12 relative — five starting points land roughly 5,000 ULP apart and all
satisfy the tolerance. Two guards keep that out of the data: the solution is
quantised before anything downstream sees it, and start selection compares
residual norms at a coarser precision so libm differences cannot reorder
near-ties. `n_j` being an integer absorbs what is left.

Engine A2's post-solve check has a related subtlety. Below a certain
conditioning, comparing a scaled residual against a fixed tolerance measures
floating-point noise rather than fit quality, and the verdict becomes platform
dependent. The tolerance is therefore derived from the precision at which the
residual can actually be evaluated, not fixed. When that precision is worse
than a relative error of `1e-3` on the realized site-size SD, the fit is
accepted and a warning quotes the number, so you know how far to trust it.

## Regenerating fixtures

Any platform. Golden fixtures, print examples and the JEBS manifest produce the
same bytes everywhere, so there is no authoritative machine.

```sh
Rscript tests/data-raw/generate_golden_fixtures.R
Rscript tests/data-raw/generate_print_examples.R
Rscript tools/jebs-golden-fixtures/generate-jebs-golden-fixtures.R
```

Off Linux the first two ask for an `ALLOW_NON_LINUX_*` environment variable.
That is a speed bump against regenerating by accident, not a platform claim.
Regenerate only when a change is *meant* to move the data, and say why in the
commit — an unintended fixture diff is a regression, not a refresh.

## What is checked, and where

`R CMD check` runs on `linux-release`, `linux-devel`, `linux-oldrel`,
`macos-release` and `windows-release`, all treated identically. The JEBS
appendix regression runs on every one of them: `preset_jebs_strict()` is
compared against fixtures generated by an independent base-R reimplementation
of the appendix, and the two are `identical()` on all seven columns for four
seeds.

A weekly scheduled run covers the slower statistical invariants and opens a
GitHub issue when it fails, so a red scheduled run cannot pass unnoticed.

## Release-blocking failures

- `canonical_hash()` differs across the CI matrix for the same design and seed.
- Same-machine reproducibility fails on any OS.
- A seed-supplied wrapper call mutates the caller's RNG state.
- Golden fixtures change without a stated reason.
- The hash schema changes without a documented decision and a `NEWS.md` entry.

## What changed in 0.2.0 {#what-changed-in-020}

The v0.1 policy named Ubuntu Linux the strict baseline and demoted macOS and
Windows to *distributional parity* — their point estimates and SEs had to match
Linux to numerical tolerance, but their `canonical_hash()` was allowed to
differ.

That structure existed because the hash could not be made portable, and it did
not work: the shipped fixtures were generated on macOS while the policy named
Linux authoritative, the test comparing them applied no platform gate at all,
and the weekly scheduled run failed for ten consecutive weeks.

The v0.1 diagnosis was that the drift could not be removed. It was half right.
The data was already portable; only the derived diagnostics were not. Dropping
them from the hash made the contract achievable, and the platform hierarchy
became unnecessary rather than merely inconvenient.

**Consequence for you:** every hash recorded under v0.1.x differs under 0.2.0.
The data did not change — what the hash looks at did. Recompute and update any
hash you have quoted.
