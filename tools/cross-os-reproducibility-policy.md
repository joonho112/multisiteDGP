# Reproducibility Policy

Status: multisiteDGP 0.2.0. Supersedes the v0.1 policy in full — see [What
changed in v0.2.0](#what-changed-in-v020) for why the old structure is gone.

## The contract

**The same design and the same seed produce the same data, on any platform,
bit for bit — and therefore the same `canonical_hash()`.**

That is the whole promise. There is no platform hierarchy, no authoritative
operating system, and no per-OS exemption. A hash you record on your laptop is
a hash a reviewer can reproduce on theirs.

Two things follow from it.

- **`canonical_hash()` is a portable identity.** Quote it in a manuscript, a
  log, or an issue and it means the same thing everywhere.
- **A hash mismatch is a real difference.** It means the design, the seed, the
  package version or the data genuinely differ. It is never platform noise.

## What is covered

`canonical_hash()` covers the simulated data columns, the column names, and a
manifest that carries the paradigm, `design_hash`, the schema version and the
names of any user callbacks.

**The package version is deliberately not hashed.** A hash gets quoted in a
manuscript or an issue as an anchor for "this data, this design", and a version
in the payload made every release invalidate every recorded hash even when
nothing about the data moved. v0.1.x worked around that with a hardcoded lineage
bucket collapsing `0.0.0.9000` and `0.1.*` into one string; it had no rule for
`0.2` and the bump moved every hash. What moves the hash when the package
changes what the hash *means* is `hash_schema_version`, and that is the thing
worth pinning. The producing version is still recorded on the object's
provenance attribute and printed.

**Data columns are hashed exactly, with no rounding.** Each is platform stable
for a specific reason:

| Column | Why it is stable |
|---|---|
| `n_j` | rounded to an integer before it leaves Layer 2 |
| `z_j`, `tau_j` | drawn from R's own RNG, which is identical across platforms |
| `se_j`, `se2_j`, `tau_j_hat` | IEEE arithmetic on the above |
| `site_index` | integer |

## What is deliberately excluded

**Derived diagnostics** — `I_hat`, `R_hat`, `rho_S_*`, `rho_P_*`,
`sigma_tau_*`. They are computed with `cor()`, `sd()` and `mean()`, whose
floating-point accumulation order varies by platform. The drift is visible even
on a single machine: for a covariate-free design `rho_P_marginal` and
`rho_P_residual` are mathematically identical yet differ in their last digits.

Excluding them costs nothing. They are functions of the hashed data and the
hashed design, so they add no provenance the hash does not already carry — only
noise.

A caller who wants one pinned deliberately can still ask:

```r
canonical_hash(dat, diagnostics_to_include = "I_hat")
```

**Function bodies and environments.** Callbacks (`g_fn`, `se_fn`,
`dependence_fn`) are recorded by presence and hook name, not by body. Two runs
that pass different closures with the same role hash the same; the manifest
records that a hook was present.

## What this rests on

The contract holds because every hashed column is platform stable. **Adding a
libm-dependent quantity to a data column would break it**, and the symptom
would be a cross-platform hash mismatch that looks like the v0.1 problem all
over again. Weigh that before introducing one.

Engine A2's truncated-Gamma solver is the case that made this concrete. It
calls `nleqslv` with `ftol = 1e-12`, so its solution is pinned only to about
1e-12 relative — five starting points land roughly 5,000 ULP apart and all
satisfy the tolerance. Two guards keep that out of the data:

- the solution is quantised to `.TRUNC_GAMMA_SOLUTION_DIGITS` significant
  digits before anything downstream sees it, and start selection compares
  residual norms at a coarser precision so libm differences cannot reorder
  near-ties;
- `n_j` is an integer, which absorbs what is left.

## Verification precision, not just verification

Engine A2's post-solve check compares a scaled residual against a tolerance.
Below a certain conditioning that comparison measures floating-point noise
rather than fit quality, and the verdict becomes platform dependent — the same
design passed on macOS and aborted on Linux.

The tolerance is therefore derived, not fixed:

```
tol_effective = max(tol, noise_floor(alpha, cv))
```

where the floor comes from the two cancellations in the moment evaluation —
`lgamma(alpha + k) - lgamma(alpha)` and `sd^2 = E[X^2] - E[X]^2` — and was
calibrated against a measured `(n_bar, cv, n_min)` grid. `solve_trunc_gamma()`
returns `tol_effective` so the decision is inspectable.

When the floor exceeds a relative error of `1e-3` on the realized site-size SD,
the fit is accepted but a warning says so and quotes the number. The design
still runs; the user is told its SD is only checkable to that level.

## Same-machine reproducibility

Unchanged and still required. Given the same package version, R runtime,
machine, design and seed, repeated calls return identical
`canonical_hash()` values, and a seed-supplied wrapper call leaves the
caller's `.Random.seed` untouched. Invariant T20 covers this on every OS.

## CI

Required `R CMD check` matrix, all treated identically:

- `linux-release`, `linux-devel`, `linux-oldrel`
- `macos-release`
- `windows-release`

Required environment:

```
_R_CHECK_FORCE_SUGGESTS_=false
MULTISITEDGP_REPRODUCIBILITY_POLICY=portable-hash-v3
```

`extended-tests` runs weekly on a schedule and opens a GitHub issue when it
fails, so a red scheduled run cannot pass unnoticed. GitHub disables scheduled
workflows after 60 days of repository inactivity; the same job reports that it
ran, so a silent stop is visible too.

## Regenerating fixtures

Any platform. Golden fixtures, print examples and the JEBS manifest produce the
same bytes everywhere, so there is no authoritative machine and no
`ALLOW_NON_LINUX_*` gate on correctness. The environment variables remain as a
speed bump against accidental regeneration, not as a platform claim.

```sh
Rscript tests/data-raw/generate_golden_fixtures.R
Rscript tests/data-raw/generate_print_examples.R
Rscript tools/jebs-golden-fixtures/generate-jebs-golden-fixtures.R
```

Regenerate only when a change is *meant* to move the data, and say why in the
commit. A fixture diff that nobody intended is a regression, not a refresh.

## Release-blocking failures

- `canonical_hash()` differs across the CI matrix for the same design and seed.
- Same-machine reproducibility fails on any OS.
- A seed-supplied wrapper call mutates the caller's RNG state.
- Golden fixtures change without a stated reason.
- The hash schema changes without a documented decision and a `NEWS.md` entry.

## What changed in v0.2.0 {#what-changed-in-v020}

The v0.1 policy named Ubuntu Linux the strict baseline and demoted macOS and
Windows from hash equality. That structure existed because the hash could not
be made portable — and it did not work. The checked-in fixtures were generated
on macOS while the policy named Linux authoritative, so the two contradicted
each other, `test-golden.R` compared `.rds` files byte-for-byte with no
platform gate at all, and the weekly `extended-tests` run failed for ten
consecutive weeks.

The v0.1 diagnosis was that the drift could not be removed. It was half right:
the data was already portable, and only the derived diagnostics were not.
Dropping them from the hash (schema v3) made the contract achievable, and the
platform hierarchy became unnecessary. See defect ledger rows D-002, D-007,
D-022 and D-024 in `log/log-version-up-2026-08-14/`.
