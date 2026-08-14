# No Compiled-Code Policy

multisiteDGP v1 is a pure-R data-generating-process package.

## Forbidden Package Surface

The package must not add any of the following DESCRIPTION fields:

- `LinkingTo`
- `SystemRequirements`
- `NeedsCompilation: yes`

The package must not add any of the following hard dependencies:

- `Rcpp`
- `RcppParallel`
- `RcppEigen`
- `BH`
- `rstan`
- `rstantools`
- `StanHeaders`
- `bayesplot`

## Forbidden Directories

The package root must not contain:

- `src/`
- `inst/stan/`

## Verification

Run these checks before any external review gate:

```sh
test ! -d src
test ! -d inst/stan
Rscript -e 'd <- read.dcf("DESCRIPTION"); stopifnot(!"LinkingTo" %in% colnames(d), !"SystemRequirements" %in% colnames(d))'
```

The policy follows the blueprint commitment that multisiteDGP v1 is DGP-only.
Bayesian fitting and Stan compilation belong to downstream packages or a future
separate package.
