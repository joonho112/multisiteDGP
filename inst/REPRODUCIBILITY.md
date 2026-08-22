# Reproducibility Policy

Status: multisiteDGP 0.2.0. The active canonical-hash schema is
`multisiteDGP-canonical-hash-v4`.

## The public contract

For a wrapper call with the same validated design and the same non-`NULL`
seed, multisiteDGP pins the RNG triple to `Mersenne-Twister` / `Inversion` /
`Rejection`. The caller's RNG kind and `.Random.seed` are restored exactly on
exit. The result should therefore reproduce the same **canonical numerical
content** across supported platforms and produce the same `canonical_hash()`
under the same hash schema.

This is not a promise that two R objects or serialized `.rds` files are
byte-for-byte identical. R runtime, serialization, attributes outside the
hash payload, and floating-point values below the canonical precision may
differ without changing the canonical hash.

If `seed = NULL`, multisiteDGP deliberately uses the caller's current RNG kind
and state. Such calls are useful for an externally managed simulation stream,
but the design alone is not a complete replay key.

## What `canonical_hash()` means

The default hash covers:

- the simulated data columns and their names;
- values rounded to nine significant digits before serialization;
- factor levels, orderedness, and semantic labels;
- the paradigm, design hash, hash schema, and callback hook names.

Derived diagnostics are excluded by default because they are functions of the
hashed data and may accumulate floating-point error differently. A caller can
pin selected diagnostics explicitly:

```r
canonical_hash(dat, diagnostics_to_include = c("I_hat", "R_hat"))
```

The package version is also excluded from the hash payload. It is retained in
provenance so an upgrade can be diagnosed without forcing every release to
change hashes whose canonical payload did not change.

### Important precision boundary

Nine-significant-digit canonicalization is a tolerance policy, not proof of
raw-data identity. Values that differ only below that precision can collide.
Conversely, two very close values on opposite sides of a rounding boundary can
hash differently. Store the original data when exact raw values matter; use
the hash as an identifier for this explicitly defined numerical equivalence
class.

## Provenance and verification

Each simulated object records:

- package version and hash schema;
- producing R version and platform;
- resolved seed, RNG triple, and whether the stream was package-pinned or
  caller-controlled;
- canonical data hash and design hash;
- callback hook names and the recorded call.

`provenance_string()` prints the stored producer information separately from
the current verifier runtime. It warns when the current R version or platform
differs from the stored producer. A warning is diagnostic context, not by
itself a verification failure; recompute `canonical_hash()` and compare it to
the recorded value.

Callback bodies and closure environments are not hashed. For custom
callbacks, archive the source or package version that defines the callback in
addition to the simulated object.

## Fixture policy

Golden fixtures are evidence tied to their manifest, not universal binary
artifacts. A fixture manifest must record at least the package version, R
runtime, platform, RNG triple, hash schema, canonical hash, and file SHA-256.
The canonical hash is the portable numerical check; the file SHA-256 verifies
the exact checked-in artifact.

Regenerate fixtures only when a reviewed change is intended to move either the
data or the hash schema:

```sh
Rscript tests/data-raw/generate_golden_fixtures.R
Rscript tests/data-raw/generate_print_examples.R
Rscript tools/jebs-golden-fixtures/generate-jebs-golden-fixtures.R
```

After regeneration, test both directions: live generation against the
manifest, and every fixture's stored provenance hash against a fresh
`canonical_hash()` recomputation.

## Release-blocking failures

- A seeded wrapper call depends on the caller's prior RNG kind or state.
- A seeded wrapper call fails to restore the caller's RNG kind or state.
- Supported CI platforms disagree on the v4 canonical hash for a locked test
  case.
- A fixture's stored provenance hash differs from a fresh recomputation.
- A hash schema change lacks a documented migration decision, updated
  literals, and regenerated validation evidence.

## Schema migration: v3 to v4

Schema v4 makes the seeded RNG policy explicit and corrects two
canonicalization gaps: factor semantics are now retained, and explicitly
included diagnostics receive the same numeric normalization as data columns.
All v3 hash literals must therefore be recomputed. A v3 and a v4 hash are not
comparable even when they arose from the same raw object.
