# Contributing to `multisiteDGP`

This package is academic research software. Contributions are welcome, particularly bug reports, new G-distribution shapes, new diagnostics, and new downstream adapters.

## Where to start

- **A bug?** [Open an issue](https://github.com/joonho112/multisiteDGP/issues/new?template=bug_report.md) with a minimal reproducible example.
- **A feature idea?** [Open an issue](https://github.com/joonho112/multisiteDGP/issues/new?template=feature_request.md) sketching the user-facing API and the design problem it solves.
- **Code change?** Fork the repository, branch from `main`, and open a pull request using the [PR template](.github/PULL_REQUEST_TEMPLATE.md).

## Code conventions

- R code follows the [tidyverse style guide](https://style.tidyverse.org/).
- Roxygen headers follow the package's established style: applied-researcher first, motivation-style `@param` (range + when-to-move-it), pedagogical `@examples` (commented; ends with interpretation), aggressive `@seealso` cross-linking, `@references` quoted verbatim from `vignettes/references.bib`.
- Vignettes follow the standardized frontmatter, setup chunk, and footer chunk used across the existing 16 `aN`/`mN` vignettes (see any of them for a template).
- Voice and tone: every primary function call shows its rendered output; plot captions explain *what to read off* the plot, not what the plot is; cross-link aggressively.

## Tests

`devtools::test()` runs everything. There are **no environment-gated tests** —
if a test exists, it runs. Under v0.1.x the slow and property-based invariants
sat behind environment variables, so a default run reported green while skipping
thirty statistical invariants. Turning them all on cost 95 seconds, which is not
a price worth paying to be misled.

The only remaining gate is `skip_if_not_installed()` for soft dependencies.

Two things to know when you add a test:

- **Error messages have a three-part shape** — what went wrong, why the rule
  exists, and a fix line that starts with `Try`, `Use`, `Pass` or `Remove`.
  `expect_multisitedgp_error()` enforces it at run time, and
  `test-error-message-conformance.R` parses `R/` and checks all 223 fix lines
  statically, so a branch no test reaches is still covered.
- **An error branch nobody executes is an error message nobody has checked.**
  Four of this release's defects were found by writing the first test that
  reached a given `.abort_*()` call.

## Fixtures and hashes

`canonical_hash()` is portable: the same design and seed give the same hash on
any platform, and the package version is not part of it, so upgrading does not
invalidate a recorded hash. What moves it is `hash_schema_version` — changing
that is a documented decision plus a `NEWS.md` entry, never a side effect.

Regenerate fixtures on any platform:

```sh
Rscript tests/data-raw/generate_golden_fixtures.R
Rscript tests/data-raw/generate_print_examples.R
Rscript tools/jebs-golden-fixtures/generate-jebs-golden-fixtures.R
```

Off Linux the first two ask for an `ALLOW_NON_LINUX_*` environment variable.
That is a speed bump against regenerating by accident, not a platform claim.

**Regenerate only when a change is meant to move the data, and say why in the
commit.** An unintended fixture diff is a regression, not a refresh. If a hash
moved and you are not sure which happened, the golden `.rds` files in
`tests/testthat/_snaps/golden/` settle it — they compare exactly and do not
depend on the hash schema.

## Validation experiments

`tools/validation/` holds thirteen experiments (V0–V12) covering calibration,
the JEBS reproduction, solver convergence and the diagnostic gates. They are not
part of `devtools::test()`; run them when you change generation or the solver:

```sh
export MULTISITEDGP_VALIDATION_MODE=full
export MULTISITEDGP_VALIDATION_OVERWRITE=true
Rscript tools/validation/jobs/run-v0-validation.R
```

`OVERWRITE=true` is not optional after a code change — `RESUME` defaults to
true, so an existing run is reused and reported as a pass without your code ever
running. `smoke` mode is for catching interface breakage cheaply; V06 and V12
declare `default_mode: full` and fail in `smoke` by design.

## Pre-merge checklist

- `devtools::document()` runs cleanly.
- `devtools::check()` passes locally; CI must pass on Linux + macOS + Windows.
- `devtools::test()` reports zero skips. A skip that is not
  `skip_if_not_installed()` needs a reason in the PR.
- `lintr::lint_package()` is clean. If a rule is wrong for this package, change
  `.lintr` and record why in `.lintr.md` — a rule loosened without a reason
  reads as "nobody looks here".
- `NEWS.md` updated for any user-visible change.
- Vignettes that reference the changed function still render.
- No unintended fixture, snapshot, or hash diff.

## Funding context

This package is supported by the Institute of Education Sciences, U.S. Department of Education, through Grant R305D240078 to the University of Alabama. Contributions should not commit the project to dependencies that conflict with this funding context (e.g., licenses incompatible with MIT, dependencies with restrictive use clauses).

## Code of conduct

This project adheres to the [Contributor Covenant](https://www.contributor-covenant.org/version/2/1/code_of_conduct/). By participating, you agree to abide by its terms.
