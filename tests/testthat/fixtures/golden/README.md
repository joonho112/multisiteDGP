# Golden Fixture Policy

This directory stores binary RDS test references for the Step 8.1 golden
fixture inventory.

Regenerate this directory with:

```sh
Rscript tests/data-raw/generate_golden_fixtures.R
```

Regenerate on any supported platform. The canonical_hash verifies the
schema-v4 numerical payload across platforms; rds_sha256 verifies only
the exact checked-in binary artifact. Raw RDS byte identity is not part
of the public contract.

Off Linux the generator asks for
MULTISITEDGP_ALLOW_NON_LINUX_GOLDEN_REGEN=true. That is a speed bump
against regenerating by accident, not a platform claim: an unintended
fixture diff is a regression, so say in the commit why the values moved.

The inventory is ten files:

- four JEBS appendix normalized seed fixtures;
- five package preset output fixtures.
- one floor-active JEBS appendix authority fixture.

The shipped metadata manifest lives in `inst/extdata/golden/`.
