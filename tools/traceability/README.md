# multisiteDGP Traceability Index

Created in Step 1.3.

This directory maps blueprint commitments to implementation ownership. It is a
development artifact and is excluded from the package tarball by `.Rbuildignore`
through the top-level `tools/` ignore rule.

Files:

- `api-index.csv`: exported functions, public S3 methods, and dual-role API surface rows from blueprint ch.10; currently 60 rows including public `compute_kappa()` as API060.
- `invariant-index.csv`: T-invariant regression suite items from ch.04 and ch.18.
- `error-index.csv`: E01-E30 error catalog from ch.16.
- `validation-index.csv`: V0 plus V01-V12 validation experiments from ch.19.
- `preset-index.csv`: 9 preset functions from ch.13.
- `docs-index.csv`: reference docs, vignettes, cookbook, NEWS, pkgdown items from ch.22 and ch.21.
- `fixture-index.csv`: 9 golden RDS fixture targets from ch.18.
- `decision-index.csv`: Decision A-F, Q1-Q16, and PA-1-PA-5 owner ledger.
- `conflict-checklist.csv`: unresolved or deferred conflicts that must remain visible during implementation.

Counting policy:

- API surface rows currently total 60. `compute_kappa()` is public in v0.1 as API060; Q16/C14 are resolved by the amended inventory. `as_tibble.multisitedgp_data()` appears twice by policy because API025 records the adapter escape-hatch contract and API058 records the S3 method registration.
- Gate C decisions DE/DF are locked; Q15 is resolved as an internal helper for v0.1, and Q16 is resolved by keeping `compute_kappa()` public and amending the API inventory to 60 surface rows.
- Error catalog entries must total 30 with E24 marked deferred.
- Validation experiments must total 13 when V0 is counted with V01-V12.
- T-invariant rows include T14a and T14b separately because ch.18 gives separate test claims.
- Golden fixtures must total 9 files as 4 JEBS appendix seeds plus 5 preset outputs, not 9 preset outputs.
