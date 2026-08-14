# JEBS Golden Fixture Manifest

This directory contains Step 4.1 reference artifacts for the JEBS Appendix
Engine A1 mixture parity fixtures.

`generate-jebs-golden-fixtures.R` reproduces the three relevant JEBS Appendix
generators in a package-normalized schema and writes:

- `jebs-golden-fixture-manifest.csv`: the authoritative Step 4.1 manifest with
  seed, parameter, source, platform, component hash, and full-output hash fields;
- optional `generated/jebs_appendix_mixture_seed*.rds` files when run with
  `--write-rds`.

The generated RDS files are not promoted into `tests/testthat/_snaps/golden/`
in Step 4.1. Step 8.1 owns the final golden fixture inventory for all nine
golden files. Until then, this directory is the provenance and hash source for
Step 4.3 Engine A1 development and Step 4.7 Claude Gate B review. By default
the script records in-memory canonical hashes and temporary-RDS SHA-256 hashes
without leaving RDS artifacts in the repository.
