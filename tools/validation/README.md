# Validation Artifacts

This directory holds development-time validation scripts and generated summaries
for the multisiteDGP build.

Full release validation experiments are owned by Phase 9. Earlier phase scripts
here are intentionally small, reproducible subsets used to catch integration
mistakes while the package is still being assembled.

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
