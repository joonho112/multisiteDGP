# Validation Artifacts

This directory holds development-time validation scripts and generated summaries
for the multisiteDGP build.

Full release validation experiments are owned by Phase 9. Earlier phase scripts
here are intentionally small, reproducible subsets used to catch integration
mistakes while the package is still being assembled.

## Re-running after a code change

The 2026-08-15 full runs predate the final v0.2.0 source and contract schema.
They remain historical artifacts but are not current release evidence. The
traceability and plan manifests therefore mark V0--V12
`pending-current-source` until the Phase 6 full rerun completes.

```sh
export MULTISITEDGP_VALIDATION_MODE=full
export MULTISITEDGP_VALIDATION_OVERWRITE=true
export MULTISITEDGP_VALIDATION_RESUME=false
for v in v0 v01 v02 v03 v04 v05 v06 v07 v08 v09 v10 v11 v12; do
  Rscript "tools/validation/jobs/run-${v}-validation.R"
done
```

`RESUME` defaults to false. A new timestamped run ID normally needs neither
resume nor overwrite. If an explicit run ID collides with existing files, the
harness stops unless `OVERWRITE=true`. Explicit `RESUME=true` succeeds only
when the result, summary, and `-contract.csv` sidecar all match the current
source digest, package version, job SHA, parameter digest, seed/mode,
schema/RNG policy, and artifact SHA values.

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
- a fresh run writes a `produced` event and a compatible reuse writes a
  distinct `reuse` event in `generated/validation-run-manifest.csv`;
- result files are keyed by `run_id`, `experiment_id`, and `mode`;
- each result/summary pair has a `-contract.csv` producer sidecar;
- seed streams are derived from `MULTISITEDGP_VALIDATION_SEED_ROOT` and restore
  the caller RNG state after allocation;
- producer fields are retained on reuse; current runtime metadata is not
  stamped onto an old artifact;
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
MULTISITEDGP_VALIDATION_RESUME=false
MULTISITEDGP_VALIDATION_OVERWRITE=false
```

To request reuse of that exact run ID, set `RESUME=true` without changing any
other contract input. A missing legacy sidecar or any mismatch is refused; use
a new run ID for a fresh run, or set `OVERWRITE=true` deliberately.
