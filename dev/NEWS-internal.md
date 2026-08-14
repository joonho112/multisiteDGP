# Internal NEWS for multisiteDGP — internal use only. Do not ship.

# multisiteDGP 0.1.0

## Release status

- Prepared the internal v0.1 release candidate after Gate E external review
  remediation. This release is approved for internal v0.1 use, not for CRAN or
  public release submission.
- Version bumped from `0.0.0.9000` to `0.1.0`.

## Internal validation readiness (v0.1)

- Local Phase 9 validation blocker readiness is unblocked for internal v0.1:
  all 9 working-register blockers are resolved, including the 5 original
  release-blocking P0 blockers, with no open P0/P1/P2 validation blocker and no
  `fail-blocker` manifest status.
- The closed P0 blockers are V0, V01, V02, V03, and V04; V05 was already P0
  `full-pass`. For V0-V03, the documented resolutions reframed over-specified
  historical gates into aggregate or contract-aligned validation evidence:
  aggregate default calibration (V0), residual-only dependence reporting (V01),
  grid-level JEBS seed stability (V02), and Walters shrinkage-implied
  consistency (V03). V04 closed through feasible A2 fixture validation.
- V09 was resolved by a targeted code fix in `R/layer3-align_rank_corr.R` for
  the `rho = 0` rank-alignment path, with broader boundary hardening deferred.
- Remaining caveats are local/internal: V08 cross-OS evidence still requires
  GitHub Actions artifacts, V10 manual visual review remains pending, and V12
  exact Lee Figure 4(b) / `lme4` numeric reproduction remains deferred until
  controlled digitized targets or source data exist.
- The v0.1 golden fixture manifest currently records macOS/aarch64 provenance;
  Linux x86_64/amd64 remains the intended strict `canonical_hash()` baseline,
  pending the first GitHub Actions artifact pass to confirm or rebaseline the
  fixture hashes.
- `multisitepower` remains a guarded soft `Suggests` dependency for the
  downstream adapter and may be unavailable from mainstream CRAN/check
  repositories; install it from its development source before using
  `as_multisitepower()`.
