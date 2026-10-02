# Contributing to multisiteDGP

Bug reports, questions, and pull requests are welcome.

## Reporting a problem

Open an issue at <https://github.com/joonho112/multisiteDGP/issues>. A short
example that reproduces the problem in a fresh R session helps most. Include
the output of `packageVersion("multisiteDGP")` and `R.version.string`.

## Proposing a change

For anything larger than a typo, open an issue first so the change can be
discussed before you write it.

1. Fork the repository and create a branch.
2. Install the development dependencies with
   `devtools::install_dev_deps()`.
3. Make the change. Follow the [tidyverse style guide](https://style.tidyverse.org).
4. Add or update tests in `tests/testthat/`. Run them with `devtools::test()`.
5. If you changed a roxygen comment, run `devtools::document()`.
6. Add a line to `NEWS.md` under the development version for any change a
   user would notice.
7. Run `devtools::check()`. It should finish without errors or warnings.
8. Open a pull request that says what changed and why.

## Tests

Tests are seeded and run in under a minute. A test of a simulated quantity
compares a statistic with its expected value using a tolerance that follows
from the sample size. `test-regression-presets.R` pins summary values of the
presets at a fixed seed. If a change is meant to alter simulated numbers, say
so in the pull request and update those values.

## Documentation

Help pages are written with roxygen2 and Markdown. Keep them short: a
one-line title, one to three sentences of description, the arguments with
their type and default, the returned value, and an example that runs in
under a second.

## Code of conduct

Please be respectful and constructive in issues and pull requests.

## Funding

This research was supported by the Institute of Education Sciences, U.S.
Department of Education, through Grant R305D240078 to the University of
Alabama. The opinions expressed are those of the authors and do not represent
views of the Institute or the U.S. Department of Education.
