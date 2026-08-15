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

Regenerate on any platform; see `tools/cross-os-reproducibility-policy.md`.
These fixtures are byte-identical wherever they are built, and their
`canonical_hash()` agrees across the whole CI matrix — linux-release,
linux-devel, linux-oldrel, macos-release and windows-release. The manifest
still records which machine generated them, as provenance; it is not a claim
of authority.

Off Linux the generator asks for `MULTISITEDGP_ALLOW_NON_LINUX_GOLDEN_REGEN=true`.
That is a speed bump against regenerating by accident, not a platform claim: an
unintended fixture diff is a regression, so say in the commit why the values
moved.

The inventory is nine files:

- four JEBS appendix normalized seed fixtures;
- five package preset output fixtures.

The manifest rows must match `tools/traceability/fixture-index.csv`.
