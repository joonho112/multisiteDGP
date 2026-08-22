# JEBS Golden Fixture Manifest

This directory contains Step 4.1 reference artifacts for the JEBS Appendix
Engine A1 mixture parity fixtures, including a floor-active authority case.

`generate-jebs-golden-fixtures.R` reproduces the three relevant JEBS Appendix
generators in a package-normalized schema and writes:

- `jebs-golden-fixture-manifest.csv`: the authoritative Step 4.1 manifest with
  seed, pinned RNG triple, hash schema, parameter, source, platform, generator
  SHA-256, component hash, and full-output hash fields;
- optional `generated/jebs_appendix_mixture_seed*.rds` files when run with
  `--write-rds`. Four ordinary seeds use the strict anchor; the additional
  `J = 300`, `nj_mean = 10`, `cv = 0.75`, `nj_min = 5`, seed-42 fixture
  activates the lower bound stated in the paper and appendix.

The matching checked-in authority files live in
`tests/testthat/_snaps/golden/`; `tests/data-raw/generate_golden_fixtures.R`
owns that final inventory and shipped manifest. This directory remains the
independent appendix-code provenance source. By default the script records
in-memory canonical hashes and temporary-RDS SHA-256 hashes without leaving
RDS artifacts here. Canonical hashes represent schema-v4 numerical content;
temporary RDS SHA-256 values identify exact serialized artifacts and are not a
cross-platform byte-identity claim.
