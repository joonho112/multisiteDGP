# Validation Artifacts

This directory holds development-time validation scripts and generated summaries
for the multisiteDGP build.

Full release validation experiments are owned by Phase 9. Earlier phase scripts
here are intentionally small, reproducible subsets used to catch integration
mistakes while the package is still being assembled.

## Re-running after a code change

The v0.2.0 reliability work re-ran all thirteen experiments on 2026-08-15 and
all thirteen pass in `full` mode. See
`log/log-version-up-2026-08-14/evidence/phase08/validation-summary-2026-08-15.md`.

```sh
export MULTISITEDGP_VALIDATION_MODE=full
export MULTISITEDGP_VALIDATION_OVERWRITE=true
export MULTISITEDGP_VALIDATION_RESUME=false
for v in v0 v01 v02 v03 v04 v05 v06 v07 v08 v09 v10 v11 v12; do
  Rscript "tools/validation/jobs/run-${v}-validation.R"
done
```

**`OVERWRITE=true` is not optional after a code change.** `RESUME` defaults to
true, so an existing complete run is reused and reported as a pass without the
new code ever running.

Two things to know before reading a failure:

- `smoke` mode is for catching interface breakage cheaply, not for judging
  acceptance. V06 and V12 declare `default_mode: full` and their acceptance
  rules assume full-mode sample sizes; both fail in `smoke` by design.
- V12 pins a literal `expected_hash`. `canonical_hash()` carries the schema
  version in its payload, so a schema change moves that value even when the
  data is identical. The summary now records `schema_matches_expected` so the
  two cases are distinguishable. The authority on whether the data moved is the
  golden `.rds` set in `tests/testthat/_snaps/golden`, which compares exactly
  and does not depend on the schema — V02 hashes those at run time instead of
  pinning, which is why it survives schema changes.

## Phase 9 Harness

Phase 9 validation scripts share `R/validation-harness.R`.

Core conventions:

- generated outputs live under `tools/validation/generated`;
- `validation-plan-manifest.csv` lists V0 plus V01-V12 and their planned
  evidence contract;
- every run writes or replaces one run row in
  `generated/validation-run-manifest.csv`;
- result files are keyed by `run_id`, `experiment_id`, and `mode`;
- seed streams are derived from `MULTISITEDGP_VALIDATION_SEED_ROOT` and restore
  the caller RNG state after allocation;
- existing outputs are reused when `MULTISITEDGP_VALIDATION_RESUME=true` and
  `MULTISITEDGP_VALIDATION_OVERWRITE=false`;
- full release evidence should be summarized into
  `tools/validation/reports/validation-report-template.qmd`.

Smoke check:

```sh
Rscript tools/validation/jobs/run-v0-smoke-validation.R
```

Useful environment variables:

```sh
MULTISITEDGP_VALIDATION_MODE=smoke
MULTISITEDGP_VALIDATION_REPS=3
MULTISITEDGP_VALIDATION_SEED_ROOT=910001
MULTISITEDGP_VALIDATION_RUN_ID=v0-smoke-manual
MULTISITEDGP_VALIDATION_RESUME=true
MULTISITEDGP_VALIDATION_OVERWRITE=false
```
