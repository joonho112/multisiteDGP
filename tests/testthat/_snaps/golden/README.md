# Golden Fixture Policy

This directory stores binary RDS test references for the Step 8.1 golden
fixture inventory.

Regenerate this directory with:

```sh
Rscript tests/data-raw/generate_golden_fixtures.R
```

Regenerate on any platform. These fixtures are byte-identical wherever
they are built, and their canonical_hash agrees across the whole CI
matrix -- linux-release, linux-devel, linux-oldrel, macos-release and
windows-release. There is no authoritative machine.

Off Linux the generator asks for
MULTISITEDGP_ALLOW_NON_LINUX_GOLDEN_REGEN=true. That is a speed bump
against regenerating by accident, not a platform claim: an unintended
fixture diff is a regression, so say in the commit why the values moved.

The inventory is nine files:

- four JEBS appendix normalized seed fixtures;
- five package preset output fixtures.

The shipped metadata manifest lives in `inst/extdata/golden/`.
