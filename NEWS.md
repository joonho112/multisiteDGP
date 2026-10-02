# multisiteDGP 0.3.0

This release removes functions, arguments, and attributes that were not needed
to simulate data. It also renames the two values of `engine`, cuts the
dependencies, and rewrites the documentation. For the same seed, 0.3.0
simulates the same numbers as 0.2.0 for every design that both versions
accept, including `true_dist = "SkewN"` and `"ALD"`, whose draws no longer
come from other packages. The statuses of `summary()` and `scenario_audit()`
follow new rules that take the number of sites into account.

## Removed

- `canonical_hash()` is removed. `all.equal()` on two results already checks
  more than it promised.
- `provenance_string()` is removed. The seed is `attr(x, "design")$seed` and
  the version is `packageVersion("multisiteDGP")`.
- `error_catalog()` is removed. The error message says what is wrong.
- `feasibility_index()` is removed, with its row in `summary()`, the threshold
  `feasibility_min`, and the columns `med_feasibility_efron`,
  `q05_feasibility_efron`, and `med_feasibility_morris` of `scenario_audit()`.
  Its value was `sum(compute_shrinkage(se2_j, sigma_tau))`.
- `as_multisitepower()` is removed. multisitepower is a repository of scripts,
  not a package that can be installed.
- `gen_effects_dpm()` and `true_dist = "DPM"` are removed. Use
  `gen_effects_user()` and `true_dist = "User"`, which gave the same result.
- `target_marginal_rho` of `multisitedgp_design()` is removed. It only raised
  an error when set.
- `variance` of `multisitedgp_design()`, `gen_effects()`, and
  `gen_effects_gaussian()`, which accepted only the value 1, is removed.
  Positional arguments after it move up by one.
- `framing` of `multisitedgp_design()` is removed. It was a label that did not
  affect the simulated data.
- `parallel` of `scenario_audit()` is removed.
- `caption` of `plot_effects()`, `plot_funnel()`, and `plot_dependence()` is
  removed. It printed the string from `provenance_string()`.
- `reps` of `design_grid()`, which was deprecated, is removed.
- `margin_engine` in the diagnostics of `sim_meta()` is removed.
  `attr(x, "diagnostics")$direct_se_method` says how the sampling variances
  were set.

## Renamed

- `engine` takes `"truncated_gamma"` and `"censored"` in place of
  `"A2_modern"` and `"A1_legacy"`. The old values are not accepted.
- `group` in the diagnostics table (`attr(x, "diagnostics")$target_vs_realized`)
  holds `"scale"`, `"dependence"`, `"shape"`, and `"precision"` in place of
  `"A"`, `"B"`, `"C"`, and `"D"`.
- `status` of a diagnostic and of `scenario_audit()` is `"pass"`, `"warn"`, or
  `"fail"` in place of `"PASS"`, `"WARN"`, or `"FAIL"`.
- `groups_evaluated` of `scenario_audit()` is `"scale, precision, shape"` or
  `"scale, precision"` in place of `"A,B,C,D"` or `"A,B,D"`.
- `legacy_a1_shuffle` in the attribute `observation_diagnostics` of
  `gen_observations()` is `precision_shuffled`. In the output of
  `sim_multisite()` and `sim_meta()` it is under
  `attr(x, "diagnostics")$observation_diagnostics`.

## Changed

- `summary()` gives the `sigma_tau` row and the copula correlation row a
  `pass` within 2 standard errors of the target and a `fail` beyond 3. The
  earlier limits did not depend on the number of sites: with 50 sites, a data
  set simulated from a normal design passed the `sigma_tau` row 38 times in
  100 data sets. The row has no status for `true_dist = "User"` and for a
  Student t distribution with `nu` of 4 or less.
- `summary()` reports the Bhattacharyya coefficient without a status. Its
  size depends on the number of sites more than on the fit.
- `default_thresholds()` returns three thresholds. `ks_scaled_max = 1.63`
  applies to the KS distance times `sqrt(J)` and replaces `ks_max = 0.10`,
  which a normal design with 50 sites did not meet. `bhattacharyya_min` is
  removed.
- Simulated data saved with an earlier version keep the statuses that were
  computed with them. Simulate the data again to get the new ones.
- `sim_multisite()` and `sim_meta()` attach two attributes to the data,
  `design` and `diagnostics`. The attributes `provenance`,
  `multisitedgp_version`, and `paradigm` are gone.
- `scenario_audit()` no longer returns the columns `design_hash`,
  `audit_complete`, `target_source`, `threshold_profile`, `n_violations`, and
  `n_warnings`.
- `api_id`, `helper`, and `threshold_profile` are no longer columns of the
  diagnostics table.
- Every error has class `multisitedgp_error`. Errors from a numerical procedure
  that could not reach its target also have class `multisitedgp_solver_error`.
- The classes `multisitedgp_arg_error`, `multisitedgp_engine_dependence_error`,
  `multisitedgp_coherence_error`, `multisitedgp_dependence_solver_error`, and
  `multisitedgp_marginal_violation_error` are gone.
- A design object saved with 0.2.0 does not validate in 0.3.0, because it has
  the removed fields. Build it again.
- `print()` of designs and of simulated data and `summary()` of simulated data
  have a new, shorter layout.
- `plot_effects()`, `plot_funnel()`, and `plot_dependence()` use the words of
  `print()` and `summary()` in titles, subtitles, and axis labels.
  `plot_effects()` has a legend for the estimates and the true effects.
- `as_metafor()` and `as_baggr()` work without the packages they are named
  after. They only rename columns.
- `true_dist = "SkewN"` and `"ALD"` no longer need `sn` and `LaplacesDemon`.
  For a seed the draws equal those of `sn::rsn()` and
  `LaplacesDemon::ralaplace()`, as the two help pages say.
- `x[j]` keeps the diagnostics when it selects or reorders columns without
  removing any of the seven columns of simulated data, as `x[, j]` did. It
  used to drop them with a warning about rows.
- `dplyr::select()`, `dplyr::relocate()`, and `dplyr::mutate()` keep the
  diagnostics when the seven columns stay as they are. `dplyr::mutate()` drops
  them without a warning when it changes a value in one of the seven. The
  three used to drop them in every case, with a warning about rows.
- `x$se_j <- ` and the other forms of assignment drop the stored diagnostics
  when they change a value in one of the seven columns. `summary()` then
  describes the data as they are, where it used to show the values from before
  the change.
- `names(x) <- ` and `dplyr::rename()` return a plain tibble when they rename
  one of the seven columns, and drop the stored diagnostics when they exchange
  the names of two of them. They used to keep the class and the diagnostics.
- `print()` and `summary()` compute the diagnostics again when the number of
  rows no longer matches the stored diagnostics, as after `rbind()`, and the
  plots then leave out their subtitle. All of them used to show the stored
  values.
- `gen_observations()` without `obs_fn` stops with an error when its input
  comes from `engine = "censored"` and has been through a dependence function.
  Its shuffle used to undo the order that the function had set, without a
  message.
- `gen_effects_skewn()`, `gen_effects_mixture()`, and `gen_effects_pmslab()`
  stop with an error for finite parameters that overflow or cancel in the
  standardization. Some of them, such as `slant = 1e160`, used to give draws
  from another distribution.
- `gen_se_direct()` stops with an error when `I`, `R`, and `sigma_tau` give
  sampling variances that are not positive finite numbers, and when `se_fn`
  returns an `n_j` that is not positive or is beyond the integer range.
- `update_multisitedgp_design()` stops with an error for an argument without
  a name. A call with only such arguments used to return the design unchanged.
- `scenario_audit()` writes the thresholds in `fail_reasons` and
  `warn_reasons` with three significant digits, as in
  `"mean_shrinkage:median<0.3"`. Three decimals showed a small threshold as 0.
- Error and warning messages are shorter, name the arguments of the function
  that was called, and no longer start with the name of an internal function.

## Dependencies

- Imports went from 12 packages to 7: `stats`, `tibble`, `dplyr`, `cli`,
  `rlang`, `nleqslv`, and `ggplot2`. `digest`, `tidyr`, `withr`,
  `RColorBrewer`, and `viridisLite` are no longer imported.
- Suggests went from 18 packages to 7: `metafor`, `baggr`, `testthat`, `knitr`,
  `rmarkdown`, `patchwork`, and `withr`.

## Documentation

- The help pages are rewritten and shorter. Functions that describe the same
  quantity share a page: `informativeness()` with `compute_I()`,
  `compute_shrinkage()` with `mean_shrinkage()`, `realized_rank_corr()` with
  `realized_rank_corr_marginal()`, `update_multisitedgp_design()` with
  `validate_multisitedgp_design()`, and the methods of simulated data on the
  page `multisitedgp_data`.
- Eight vignettes replace the previous sixteen: "Get started with
  multisiteDGP", "Site-effect distributions", "Site sizes and standard errors",
  "Dependence between effects and precision", "Diagnostics and presets",
  "Calibrating a design to observed data", "The data-generating model", and
  "Handing data to metafor and baggr".
- The cookbook scripts in `inst/cookbook` are removed.

# multisiteDGP 0.2.0

## Breaking changes

- `gen_effects()` no longer takes `upstream`, which only raised an error.
- `scenario_audit()` returns the new columns `audit_complete`,
  `groups_evaluated`, `threshold_profile`, and `n_warnings`.
- `scenario_audit()` sets `pass = TRUE` only for status `"PASS"` (now `"pass"`).
- `canonical_hash()` and `provenance_string()` were reworked, so values
  recorded with an earlier version no longer match. Both were removed in 0.3.0.

## New features

- `sim_multisite()` and `sim_meta()` with a `seed` use Mersenne-Twister with
  inversion and rejection sampling, whatever the caller's `RNGkind()`.
- `preset_walters_2024()` records in `attr(design, "preset_metadata")` which
  values come from Walters (2024), which are derived, and which are assumed.
- `error_catalog()` was added. It was removed in 0.3.0.

## Bug fixes

- `scenario_audit()` no longer stops with an error for `true_dist` `"SkewN"`,
  `"ALD"`, `"Mixture"`, or `"PointMassSlab"`. It skips their shape diagnostics.
- `gen_effects()` accepts a named `beta` without the intercept, which is then 0.
- `engine = "A2_modern"` (now `"truncated_gamma"`) checks the fitted mean and
  standard deviation of the site sizes separately, so a wrong mean cannot pass.
- `pearson_corr` is checked against the correlation achieved on the latent
  normal scale of the copula.
- The KS diagnostic is a one-sample test against the distribution of the design.
- `gen_effects_studentt()` warns at `nu = 4` too and recommends `nu > 4`.
- `plot_effects()` labels at most 25 sites on the axis of the caterpillar plot.
- `J`, `seed`, `max_iter`, `nj_min`, and `M` beyond the 32-bit integer range
  raise an error that names the range.
- `preset_jebs_strict()` uses `nj_min = 5`, as in Lee et al. (2025).

## Dependencies

- `multisitepower` (not on CRAN) and `hedgehog` are no longer in Suggests.
- `future` and `pkgload` are added to Suggests.

# multisiteDGP 0.1.1

A documentation release. Functions and simulated numbers are as in 0.1.0.

## Documentation

- The README and all help pages are rewritten.
- Sixteen vignettes replace the seven earlier ones.
- `?multisiteDGP` lists the error classes: `multisitedgp_error` and six
  subclasses, of which only `multisitedgp_solver_error` remains in 0.3.0.

# multisiteDGP 0.1.0

First release.

- `sim_multisite()` simulates one estimated effect and one standard error per
  site, with standard errors derived from site sizes. `sim_meta()` sets the
  standard errors directly.
- `multisitedgp_design()` builds a design that several calls can share, and
  `design_grid()` builds a grid of designs.
- `gen_effects()` draws site effects from six built-in distributions, each
  scaled to variance 1, or from a user-supplied function.
- `align_rank_corr()`, `align_copula_corr()`, and `align_hybrid_corr()` reorder
  the standard errors to reach a target correlation with the site effects.
- `scenario_audit()` and the diagnostic functions, such as `informativeness()`
  and `mean_shrinkage()`, check a design before a long simulation.
- Nine `preset_*()` functions return ready-made designs.
- `as_metafor()`, `as_baggr()`, and `as_multisitepower()` convert the data for
  other packages. `as_multisitepower()` was removed in 0.3.0.
- `plot_effects()`, `plot_funnel()`, and `plot_dependence()` plot the data.
- `canonical_hash()`, `provenance_string()`, and the scripts in `inst/cookbook`
  are included. All were removed in 0.3.0.
- Functions taken over from the earlier package `siteBayes2` have new names:
  `sim_multisite_data()` is `sim_multisite()`, `gen_priorG()` and
  `gen_priorG2()` are `gen_effects()`, `sim_sitesize_withinvar()` is
  `gen_site_sizes()`, `sim_observed_effects()` is `align_rank_corr()` with
  `gen_observations()`, and `get_shrinkage_factor()` is `mean_shrinkage()`.
- The model-fitting functions of `siteBayes2` are not part of the package.
