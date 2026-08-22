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

Regenerate on a supported platform; see
`tools/cross-os-reproducibility-policy.md`. The schema-v4
`canonical_hash()` verifies canonical numerical content across platforms.
The `rds_sha256` field verifies only the exact checked-in binary artifact;
raw RDS byte identity is not part of the public contract. The manifest records
the producing machine as provenance, not as a claim of authority.

Off Linux the generator asks for `MULTISITEDGP_ALLOW_NON_LINUX_GOLDEN_REGEN=true`.
That is a speed bump against regenerating by accident, not a platform claim: an
unintended fixture diff is a regression, so say in the commit why the values
moved.

The inventory is ten files:

- four JEBS appendix normalized seed fixtures;
- five package preset output fixtures;
- one floor-active JEBS appendix authority fixture.

The manifest rows must match `tools/traceability/fixture-index.csv`.
