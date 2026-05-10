# Cross-OS Reproducibility Policy

Status: Phase 11 Step 11.4 internal v0.1 release policy for multisiteDGP 0.1.0.

## Scope

This policy defines what multisiteDGP promises across operating systems for
generated data, canonical hashes, golden fixtures, print snapshots, and CI.

The policy is intentionally narrower than "all operating systems produce the
same bytes." multisiteDGP promises stable simulation contracts, not byte-level
identity across every BLAS, compiler, CPU, and R runtime combination.

## Definitions

Same-machine reproducibility means that the same package version, same R
runtime, same machine, same design, and same seed produce the same
`canonical_hash()` across repeated runs.

Linux strict hash means that Ubuntu Linux is the canonical strict baseline for
golden fixture hashes and print-example hash regeneration.

Distributional parity means that macOS and Windows must pass statistical,
shape, diagnostic, and same-machine reproducibility tests, but do not need to
match the Linux byte-level `canonical_hash()` for JEBS strict golden fixtures.

## Policy

1. Linux is the strict cross-run hash baseline.

   T1a JEBS golden fixture hash equality is enforced only on Linux. In CI this
   applies to the Ubuntu matrix cells. Locally, `skip_if_not_linux_strict_hash()`
   prevents macOS and Windows from treating Linux golden hashes as byte-level
   expectations.

2. macOS and Windows are demoted from strict cross-OS hash equality.

   macOS and Windows still run R CMD check and the normal test suite. They must
   pass same-machine reproducibility, distributional checks, examples, vignettes,
   and adapter tests. They are not expected to reproduce Linux `canonical_hash()`
   strings for T1a golden fixture data.

3. Same-machine reproducibility is required on every OS.

   T20 remains active across OSes. Given the same local runtime and seed,
   repeated `sim_multisite()` calls must return identical `canonical_hash()`
   values and preserve caller RNG state when a seed is supplied.

4. T1b-style distributional parity is the cross-OS substitute.

   For macOS and Windows, acceptance rests on distributional tests such as T1b,
   shape diagnostics, and validation summaries rather than byte-identical Linux
   fixture hashes.

5. Authoritative golden and print-example regeneration is Linux-only.

   Generated print examples contain `canonical_hash` and `design_hash` values.
   Authoritative regeneration belongs on Linux x86-64. A non-Linux user may set
   `MULTISITEDGP_ALLOW_NON_LINUX_PRINT_REGEN=true` for exploratory local
   regeneration, but non-Linux hash-only diffs should not be committed.

   Current v0.1 golden fixture note: the checked-in Step 8.1 manifest records
   local macOS/aarch64 provenance (`generated_platform =
   aarch64-apple-darwin20`) for the nine bootstrap fixture files. The intended
   strict `canonical_hash()` baseline remains Ubuntu Linux x86_64/amd64. The
   first GitHub Actions artifact pass must either confirm that the checked-in
   hashes are identical on Linux or regenerate and commit Linux-baseline
   fixtures and manifest metadata from Linux x86_64/amd64. Non-Linux local
   regeneration is exploratory only and must not be committed as authoritative.

6. `canonical_hash()` is stable by schema, not by hiding numeric drift.

   The hash schema removes known avoidable drift from column order, row names,
   attribute order, callback function bodies, callback environments, and
   non-allowlisted diagnostics. It does not mask true numeric differences.

## CI Rules

Required R CMD check matrix:

- `linux-release`: strict hash baseline plus full check suite.
- `linux-devel`: forward-compatibility with Linux strict hash tests active.
- `linux-oldrel`: backward-compatibility with Linux strict hash tests active.
- `macos-release`: full package checks with T1a strict hash skipped.
- `windows-release`: full package checks with T1a strict hash skipped.

Required environment posture:

- `_R_CHECK_FORCE_SUGGESTS_=false`
- `MULTISITEDGP_REPRODUCIBILITY_POLICY=linux-strict-hash-cross-os-demoted`
- `MULTISITEDGP_ALLOW_NON_LINUX_PRINT_REGEN=false`

Extended validation:

- V08 records local repeated-run canonical hash evidence.
- A true Linux/macOS/Windows cross-machine validation matrix is external to the
  local validation jobs until release infrastructure runs on GitHub Actions.

## Release-Blocking Failures

These are release-blocking:

- T1a strict golden hash fails on Linux.
- T20 same-machine reproducibility fails on any OS.
- Caller RNG state is mutated by seed-supplied wrapper calls.
- T1b distributional parity fails on required CI cells.
- A non-Linux print/golden hash regeneration is committed as authoritative.
- `canonical_hash()` schema changes without a documented major-version
  reproducibility decision.

These are not release-blocking by themselves:

- macOS or Windows `canonical_hash()` differs from Linux for T1a golden fixture
  data.
- A local Darwin V08 validation run reports `cross_os_matrix_status =
  "not_run_in_local_phase9_job"`.

## Developer Commands

Local same-machine reproducibility smoke:

```r
dat1 <- sim_multisite(preset_education_modest(), seed = 12345L)
dat2 <- sim_multisite(preset_education_modest(), seed = 12345L)
stopifnot(identical(canonical_hash(dat1), canonical_hash(dat2)))
```

Linux strict T1a check:

```sh
MULTISITEDGP_RUN_SLOW=true Rscript -e 'devtools::test(filter = "T1a|T20|T1b")'
```

Non-Linux policy check:

```sh
Rscript -e 'devtools::test(filter = "cross-os-reproducibility-policy")'
```
