# E01 snapshot records J lower-bound abort

    Code
      multisitedgp_design(J = 5L)
    Condition
      Error in `.abort_multisitedgp()`:
      x `J` must be at least 10; you passed 5.
      i J >= 10 is required for stable (I, R) reporting.
      > Try `J = 25L` for a small design or `J = 50L` for the default design.

# E02 snapshot records sigma_tau positivity abort

    Code
      sim_multisite(sigma_tau = 0)
    Condition
      Error in `.abort_multisitedgp()`:
      x `sigma_tau` must be > 0; you passed 0.
      i `sigma_tau == 0` is a degenerate point-mass G distribution.
      > Try `sigma_tau = 0.20` for modest heterogeneity or `sigma_tau = 0.05` for very low heterogeneity.

# E03 snapshot records nj_mean and nj_min relation abort

    Code
      sim_multisite(nj_mean = 10, nj_min = 15L)
    Condition
      Error in `.abort_multisitedgp()`:
      x `nj_mean` (10) must exceed `nj_min` (15).
      i The truncated-Gamma site-size generator requires `nj_mean > nj_min`.
      > Try `nj_mean = 40, nj_min = 5L` or `nj_mean = 60, nj_min = 10L`.

# E04 snapshot records Engine A1 dependence abort

    Code
      multisitedgp_design(engine = "A1_legacy", dependence = "rank", rank_corr = 0.3)
    Condition
      Error in `.abort_multisitedgp()`:
      x Engine `A1_legacy` is not compatible with `dependence = "rank"`.
      i Engine A1 is reserved for JEBS legacy bit-identical reproduction.
      > Use `engine = "A2_modern", dependence = "rank", rank_corr = 0.3`.

# E05 snapshot records direct-design site-size argument abort

    Code
      snapshot_internal(".validate_direct_args")(nj_mean = 40, cv = 0.5)
    Condition
      Error in `.abort_multisitedgp()`:
      x `sim_meta()` is for direct (I, R) designs and does not accept: nj_mean, cv.
      i Direct meta-analysis designs specify precision through `I` and `R`, not site-size arguments.
      > Use `sim_multisite()` with site-size arguments or call `sim_meta(I = ..., R = ...)`.

# E06 snapshot records g_fn and true_dist coherence abort

    Code
      multisitedgp_design(true_dist = "Gaussian", g_fn = function(J) rep(0, J))
    Condition
      Error in `.abort_multisitedgp()`:
      x `g_fn` and `true_dist = "Gaussian"` were both supplied.
      i `g_fn` defines a user distribution, so `true_dist` must be `User`, `DPM`, or omitted.
      > Remove `true_dist`, set `true_dist = "User"`, or use `true_dist = "DPM"` as an explicit bridge.

# E07 snapshot records missing StudentT theta_G nu abort

    Code
      multisitedgp_design(true_dist = "StudentT", theta_G = list(other = 1))
    Condition
      Error in `.abort_multisitedgp()`:
      x `true_dist = "StudentT"` requires `theta_G$nu`.
      i Student-t standardization requires `nu > 2` for finite variance.
      > Use `theta_G = list(nu = 5)`.

# E08 snapshot records mutually exclusive correlation target abort

    Code
      multisitedgp_design(rank_corr = 0.3, pearson_corr = 0.3)
    Condition
      Error in `.abort_multisitedgp()`:
      x `rank_corr` and `pearson_corr` were both specified; choose one.
      i `rank_corr` targets Spearman rank alignment; `pearson_corr` targets Gaussian copula alignment.
      > Use `dependence = "rank", rank_corr = 0.3` or `dependence = "copula", pearson_corr = 0.3`.

# E09 snapshot records sim_meta wrong-door argument abort

    Code
      sim_meta(nj_mean = 40, cv = 0.5)
    Condition
      Error in `.abort_multisitedgp()`:
      x `sim_meta()` received site-size argument(s).
      i Wrong-door argument(s): nj_mean, cv.
      > Use `sim_multisite()` for site-size designs or remove those arguments.

# E10 snapshot records sim_multisite direct-argument abort

    Code
      sim_multisite(I = 0.7)
    Condition
      Error in `.abort_multisitedgp()`:
      x `sim_multisite()` received direct meta-analysis argument(s).
      i Wrong-door argument(s): I.
      > Use `sim_meta()` for `(I, R)` or direct-SE designs.

# E11 snapshot records sim_meta missing-I abort

    Code
      sim_meta()
    Condition
      Error in `.abort_multisitedgp()`:
      x `I` is required when `paradigm = "direct"`.
      i Direct designs specify standard errors through exact `(I, R)` targets.
      > Pass `I = 0.30` or use `paradigm = "site_size"`.

# E12 snapshot records ALD rho boundary abort

    Code
      snapshot_internal(".validate_ald_rho")(0.99)
    Condition
      Error in `.abort_multisitedgp()`:
      x `rho` must be strictly between 0.05 and 0.95.
      i ALD moments become unstable near the degenerate boundary.
      > Use `rho = 0.25`, `rho = 0.5`, or another interior value.

# E13 snapshot records Student-t nu boundary abort

    Code
      gen_effects_studentt(J = 10L, nu = 1.5)
    Condition
      Error in `.abort_multisitedgp()`:
      x `theta_G$nu` must be > 2; you passed 1.5.
      i Student-t standardization requires finite variance.
      > Use `theta_G = list(nu = 5)`.

# E14 snapshot records user g_fn wrong-length abort

    Code
      gen_effects_user(J = 10L, g_fn = function(J) stats::rnorm(J - 1L))
    Condition
      Error in `.abort_multisitedgp()`:
      x `g_fn` must return a finite numeric vector of length `J`.
      i Expected length 10; got length 9.
      > Use one finite numeric value per site.

# E15 snapshot records A2 truncated-Gamma solver abort

    Code
      suppressWarnings(gen_site_sizes(snapshot_l1_upstream(), J = 25L, nj_mean = 6,
      cv = 1.8, nj_min = 5L, engine = "A2_modern"))
    Condition
      Error in `.abort_multisitedgp()`:
      x Engine A2 truncated-Gamma post-solve verification failed.
      i Maximum scaled residual was <residual> with tolerance 1e-06.
      > Try a less extreme site-size design or increase `tol` slightly.

# E16 snapshot records direct-I boundary abort

    Code
      gen_se_direct(snapshot_l1_upstream(), J = 25L, I = 1.5)
    Condition
      Error in `.abort_multisitedgp()`:
      x `I` must be strictly between 0 and 1; you passed 1.5.
      i Direct designs use `I` as an informativeness proportion; endpoints are degenerate.
      > Try `I = 0.30`, `I = 0.50`, or another value in `(0, 1)`.

# E17 snapshot records deterministic hill-climb sign-gate abort

    Code
      snapshot_internal(".check_rank_fit")(list(achieved = 0.1, converged = FALSE),
      target = -0.3, tol = 0.02, J = 25L)
    Condition
      Error in `.abort_multisitedgp()`:
      x Hill-climb sign gate failed: target -0.3, achieved 0.1.
      i Combinatorial discreteness blocks this target at J = 25.
      > Try a larger `J`, a less extreme target, or a copula-based aligner.

# E18 snapshot records obs_fn wrong-length abort

    Code
      gen_observations(snapshot_l2_upstream(), obs_fn = function(tau_j, se2_j) tau_j[
        -1L])
    Condition
      Error in `.abort_multisitedgp()`:
      x `obs_fn` must return finite numeric values of length J.
      i Got type `double` and length 24; expected numeric length 25.
      > Use `obs_fn = function(tau_j, se2_j, ...) stats::rnorm(length(tau_j), tau_j, sqrt(se2_j))`.

# E19 snapshot records missing sn package abort

    Code
      gen_effects_skewn(J = 10L, slant = 2)
    Condition
      Error in `.abort_multisitedgp()`:
      x Package `sn` is required for `gen_effects_skewn()`.
      i `sn` is a multisiteDGP Suggests dependency for this distribution.
      > Use `install.packages("sn")` and try again.

# E20 snapshot records missing LaplacesDemon package abort

    Code
      gen_effects_ald(J = 10L, rho = 0.3)
    Condition
      Error in `.abort_multisitedgp()`:
      x Package `LaplacesDemon` is required for `gen_effects_ald()`.
      i `LaplacesDemon` is a multisiteDGP Suggests dependency for this distribution.
      > Use `install.packages("LaplacesDemon")` and try again.

# E21 snapshot records PointMassSlab pi0 boundary abort

    Code
      gen_effects_pmslab(J = 10L, pi0 = 0)
    Condition
      Error in `.abort_multisitedgp()`:
      x `pi0` must be strictly between 0 and 1.
      i Both endpoints are refused: `pi0 = 0` is all slab; `pi0 = 1` is degenerate.
      > Use `pi0 = 0.20`, `pi0 = 0.50`, or another interior value.

# E22 snapshot records DPM v1 stub abort

    Code
      gen_effects_dpm(J = 10L)
    Condition
      Error in `.abort_multisitedgp()`:
      x `true_dist = "DPM"` is not implemented in multisiteDGP v1.
      i Built-in Dirichlet Process Mixture sampling is deferred to v2 or a sibling package.
      > Use `true_dist = "User"` with a custom `g_fn`, or pass `g_fn` as an explicit DPM bridge.

# E23 snapshot records target_marginal_rho v1 abort

    Code
      multisitedgp_design(target_marginal_rho = 0.3)
    Condition
      Error in `.abort_multisitedgp()`:
      x `target_marginal_rho` is not supported in v1.0.
      i multisiteDGP v1 uses residual interpretation for covariate-dependent effects.
      > Remove `target_marginal_rho`; the marginal API is deferred to v2.

# E24 snapshot records deferred PSD placeholder

    Code
      cat(
        "E24 PSD compatibility violation is deferred in v1; no active abort path.\n")
    Output
      E24 PSD compatibility violation is deferred in v1; no active abort path.

# E25 snapshot records custom dependence marginal violation abort

    Code
      align_rank_corr(snapshot_custom_dependence_upstream(), rank_corr = 0.3,
      dependence_fn = drift_hook)
    Condition
      Error in `.abort_multisitedgp()`:
      x Custom `dependence_fn` violated marginal preservation.
      i `sort(out$se2_j)` must be bit-identical to `sort(se2_j)`.
      > Use `se2_j[perm]`; do not mutate precision values.

# E26 snapshot records missing metafor package abort

    Code
      as_metafor(dat)
    Condition
      Error in `.abort_multisitedgp()`:
      x Package `metafor` is required for `as_metafor()`.
      i `metafor` is a multisiteDGP Suggests dependency for this distribution.
      > Use `install.packages("metafor")` and try again.

# E27 snapshot records missing baggr package abort

    Code
      as_baggr(dat)
    Condition
      Error in `.abort_multisitedgp()`:
      x Package `baggr` is required for `as_baggr()`.
      i `baggr` is a multisiteDGP Suggests dependency for this distribution.
      > Use `install.packages("baggr")` and try again.

# E28 snapshot records feasibility warning-not-abort path

    Code
      feasibility_index(rep(100, 10L), sigma_tau = 0.01, warn = TRUE)
    Condition
      Warning:
      ! Feasibility index = FAIL -- partial-pooling estimator may be degenerate.
      i Efron effective information is 0.000; FAIL threshold is < 5.
      > Try larger `J`, larger `nj_mean`, or larger `sigma_tau`.
    Output
      [1] 9.99999e-06

# E29 snapshot records site-size paradigm se_fn coherence abort

    Code
      multisitedgp_design(se_fn = function(J) rep(0.1, J))
    Condition
      Error in `.abort_multisitedgp()`:
      x `se_fn` is for `paradigm = "direct"`, not `paradigm = "site_size"`.
      i Site-size designs use the Layer 2 margin engine to create standard errors.
      > Use `paradigm = "direct", se_fn = my_se_fn` or remove `se_fn`.

# E30 snapshot records adapter reserved-name collision abort

    Code
      as_metafor(dat)
    Condition
      Error in `.abort_multisitedgp()`:
      x Covariate column "yi" collides with metafor reserved name.
      i metafor uses reserved adapter column name(s): yi, vi, sei.
      > Use a renamed covariate column or `tibble::as_tibble()` for a plain tibble.

