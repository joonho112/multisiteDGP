# Golden Fixture Policy

This directory stores binary RDS test references for the Step 8.1 golden
fixture inventory.

Regenerate this directory with:

```sh
Rscript tests/data-raw/generate_golden_fixtures.R
```

Authoritative regeneration is Linux x86_64/amd64 only. Non-Linux
regeneration requires MULTISITEDGP_ALLOW_NON_LINUX_GOLDEN_REGEN=true
and is exploratory only. Non-Linux fixture, manifest, or hash-only
diffs must not be committed as authoritative.

For the v0.1 bootstrap manifest, local macOS/aarch64 provenance remains
documented in `inst/extdata/golden/golden-fixture-manifest.csv` until a
GitHub Actions Linux x86_64/amd64 artifact pass confirms equality or
regenerates the fixtures and manifest as the Linux-baseline authority.

The authoritative v1 inventory is nine files:

- four JEBS appendix normalized seed fixtures;
- five package preset output fixtures.

The shipped metadata manifest lives in `inst/extdata/golden/`.
