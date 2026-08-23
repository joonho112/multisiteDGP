# Workflow Overview

Phase 10 Step 10.6 installs the active GitHub Actions workflows:

- `R-CMD-check.yaml`: 5-cell R CMD check matrix.
- `lint.yaml`: package lint plus pure-R/no-Stan boundary checks.
- `test-coverage.yaml`: exported-function coverage gate and optional Codecov upload.
- `pkgdown-deploy.yaml`: pkgdown build and GitHub Pages deployment.
- `extended-tests.yaml`: scheduled/manual slow, property, and validation-smoke tests.
- `pr-commands.yaml`: collaborator-only `/document` and `/style` checks without auto-commit.

Main CI uses `_R_CHECK_FORCE_SUGGESTS_=false` and installs only hard
dependencies plus targeted test/docs tooling. This keeps the package build
pure-R and avoids pulling Stan or other heavy optional Suggests into required
PR checks.

Cross-OS reproducibility follows `tools/cross-os-reproducibility-policy.md`.
All five matrix cells must agree on locked schema-v4 canonical numerical
hashes. No operating system is privileged; exact RDS byte identity is a
separate artifact-integrity check rather than the cross-platform contract.
