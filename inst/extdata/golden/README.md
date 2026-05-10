# Golden Fixture Policy

This directory stores the shipped manifest for the Step 8.1 golden fixture
inventory.

The binary RDS fixtures live in `tests/testthat/_snaps/golden/` because they
are test references, not user-facing example data. This `inst/extdata/golden/`
directory ships only lightweight provenance and policy metadata.

This is an intentional small-fixture release tradeoff: the test tarball keeps
self-contained binary regression references, while runtime package data stays
limited to the manifest and README.

Regenerate the inventory with:

```sh
Rscript tests/data-raw/generate_golden_fixtures.R
```

Authoritative regeneration is Linux x86_64/amd64 only; see
`tools/cross-os-reproducibility-policy.md`. The current v0.1 checked-in
manifest records local macOS/aarch64 provenance for the bootstrap fixtures.
Linux x86_64/amd64 remains the intended strict `canonical_hash()` baseline. The
first GitHub Actions artifact pass should confirm equality with these hashes or
regenerate the fixtures and manifest as the Linux-baseline authority. Non-Linux
regeneration is exploratory only and must not be committed as authoritative.

The authoritative v1 inventory is nine files:

- four JEBS appendix normalized seed fixtures;
- five package preset output fixtures.

The manifest rows must match `tools/traceability/fixture-index.csv`.
