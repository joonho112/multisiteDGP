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

## Pre-merge checklist

- `devtools::document()` runs cleanly.
- `devtools::check()` passes locally; CI must pass on Linux + macOS + Windows.
- `NEWS.md` updated for any user-visible change.
- Vignettes that reference the changed function still render.

## Funding context

This package is supported by the Institute of Education Sciences, U.S. Department of Education, through Grant R305D240078 to the University of Alabama. Contributions should not commit the project to dependencies that conflict with this funding context (e.g., licenses incompatible with MIT, dependencies with restrictive use clauses).

## Code of conduct

This project adheres to the [Contributor Covenant](https://www.contributor-covenant.org/version/2/1/code_of_conduct/). By participating, you agree to abide by its terms.
