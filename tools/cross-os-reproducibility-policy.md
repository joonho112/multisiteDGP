# Cross-OS Reproducibility Verification Policy

Status: release policy for multisiteDGP 0.2.0. Public-facing language is
defined in `inst/REPRODUCIBILITY.md`; this file specifies maintainer checks.

## Contract under test

For the same validated design, non-`NULL` seed, package code, and hash schema,
supported platforms must agree on canonical numerical content and on
`canonical_hash()`. A seeded wrapper pins `Mersenne-Twister` / `Inversion` /
`Rejection` and restores the caller's prior RNG kind and `.Random.seed`.

The following claims are intentionally out of scope:

- byte identity of in-memory R objects or `.rds` serialization;
- equality below the nine-significant-digit canonical precision;
- replay from design alone when `seed = NULL`;
- identity of custom callback implementations that share a hook name.

## Hash payload: schema v4

The v4 payload contains canonicalized data, column names, factor semantics,
paradigm, design hash, schema version, and callback hook names. Numeric values
are rounded to nine significant digits. Derived diagnostics are excluded by
default; named opt-in diagnostics are numerically canonicalized before
hashing. Package version, R runtime, and platform remain provenance fields,
not hash fields.

This definition admits two edge cases that tests must document:

1. sub-precision raw differences can have the same canonical hash;
2. values straddling a rounding boundary can have different hashes despite a
   very small absolute difference.

Neither outcome is a contradiction. The hash identifies the defined
canonical equivalence class, not the raw byte stream.

## Required tests

### RNG isolation

Run every locked seeded case after at least two caller RNG configurations,
including `Mersenne-Twister` and `L'Ecuyer-CMRG`. Assert:

- identical generated columns and canonical hashes;
- exact restoration of the caller's RNG triple;
- exact restoration of an existing `.Random.seed`;
- continued absence of `.Random.seed` when none existed before the call;
- provenance records the pinned triple and `rng_policy = "package-pinned"`.

### Hash semantics

Assert that factor labels, level ordering, orderedness, column order, and
schema changes affect the hash. Assert that data-frame and tibble
representations with the same supported semantics agree. Lock examples for
the two rounding-boundary edge cases above.

### Provenance

For every full-object golden fixture, assert:

```r
identical(
  attr(fixture, "provenance")$canonical_hash,
  canonical_hash(fixture)
)
```

`provenance_string()` must display the stored producing R version, platform,
RNG triple, and schema separately from the current verifier runtime. Mutated
test provenance must trigger an R/platform mismatch warning.

### Cross-platform matrix

Required `R CMD check` targets:

- Linux release, devel, and oldrel;
- macOS release;
- Windows release.

The matrix compares v4 canonical hashes for a small locked corpus covering
both wrappers, every built-in effect family, both site-size engines, direct
precision, and dependence methods. Platform reports must include package
version, R version, platform, RNG triple, schema, design hash, and canonical
hash.

## Fixture manifests

Each manifest row must record:

- fixture path and file SHA-256;
- canonical hash and hash schema;
- package version and source revision;
- R version and platform;
- RNG triple and resolved seed;
- generator script path and SHA-256.

Binary SHA equality is only an integrity check for the checked-in artifact.
Canonical-hash equality is the cross-platform numerical criterion.

## Regeneration and release gates

Regeneration requires a stated reason and a clean comparison of old versus new
canonical hashes. A release is blocked when:

- caller RNG configuration changes a seeded result;
- the wrapper leaks or manufactures caller RNG state;
- supported platforms disagree on a locked v4 canonical hash;
- a fixture's stored provenance, manifest hash, and recomputed hash disagree;
- source, generator, artifact, or validation evidence hashes are stale;
- the schema changes without updating policy, NEWS, fixtures, literals, and
  validation evidence together.

The environment marker for CI is:

```text
MULTISITEDGP_REPRODUCIBILITY_POLICY=portable-numerical-hash-v4
```

## Migration record

v3 excluded package version and default diagnostics, but its published policy
mixed exact-data and rounded-data claims and did not pin the RNG kind. v4
defines a single numerical-equivalence contract, pins seeded RNG behavior,
retains factor semantics, normalizes opt-in diagnostics, and separates
producer from verifier provenance. Consequently all v3 hash literals and
full-object fixture provenance must be regenerated before release.
