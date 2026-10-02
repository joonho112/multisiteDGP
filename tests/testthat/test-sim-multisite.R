test_that("`sim_multisite()` is exported and returns multisitedgp_data with its design and diagnostics", {
  expect_true("sim_multisite" %in% getNamespaceExports("multisiteDGP"))

  out <- sim_multisite(seed = 6101L)
  design <- attr(out, "design", exact = TRUE)
  diagnostics <- attr(out, "diagnostics", exact = TRUE)

  expect_s3_class(out, "multisitedgp_data")
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("site_index", "z_j", "tau_j", "tau_j_hat", "se_j", "se2_j", "n_j"))
  expect_identical(nrow(out), 50L)
  expect_true(is_multisitedgp_design(design))
  expect_identical(design$paradigm, "site_size")
  expect_identical(design$seed, 6101L)
  expect_identical(diagnostics$J, 50L)
  expect_identical(diagnostics$dependence_method, "none")
  expect_true(is.finite(diagnostics$I_hat))
  expect_true(is.finite(diagnostics$R_hat))
  expect_true(all(is.finite(out$tau_j_hat)))
})

test_that("`sim_multisite()` output carries no intermediate attributes", {
  out <- sim_multisite(seed = 6111L)
  final_attrs <- names(attributes(out))
  plain <- tibble::as_tibble(out)

  expect_false(any(c(
    "engine", "kappa", "dependence_diagnostics", "observation_diagnostics",
    "permutation_perm", "observation_permutation_perm"
  ) %in% final_attrs))
  expect_false(any(c("design", "diagnostics") %in% names(attributes(plain))))
  expect_s3_class(plain, "tbl_df")
  expect_false(is_multisitedgp_data(plain))
})

test_that("`sim_multisite()` flat args and design path agree under the same seed", {
  design <- multisitedgp_design(
    J = 25L,
    sigma_tau = 0.25,
    nj_mean = 60,
    cv = 0.35,
    nj_min = 5L,
    dependence = "rank",
    rank_corr = 0.25
  )

  from_design <- sim_multisite(design, seed = 6102L)
  from_flat <- sim_multisite(
    J = 25L,
    sigma_tau = 0.25,
    nj_mean = 60,
    cv = 0.35,
    nj_min = 5L,
    dependence = "rank",
    rank_corr = 0.25,
    seed = 6102L
  )

  expect_identical(as.data.frame(from_design), as.data.frame(from_flat))
  expect_identical(attr(from_design, "diagnostics", exact = TRUE)$dependence_method, "rank")
  expect_identical(
    attr(from_design, "diagnostics", exact = TRUE)$dependence_diagnostics$target_type,
    "residual_spearman"
  )
})

test_that("`sim_multisite()` forwards hybrid controls from design and flat args", {
  design <- multisitedgp_design(
    J = 40L,
    dependence = "hybrid",
    rank_corr = 0.25,
    hybrid_init = "rank",
    hybrid_polish = "none"
  )

  from_design <- sim_multisite(design, seed = 6106L)
  from_flat <- sim_multisite(
    J = 40L,
    dependence = "hybrid",
    rank_corr = 0.25,
    hybrid_init = "rank",
    hybrid_polish = "none",
    seed = 6106L
  )
  design_diag <- attr(from_design, "diagnostics", exact = TRUE)$dependence_diagnostics
  flat_diag <- attr(from_flat, "diagnostics", exact = TRUE)$dependence_diagnostics

  expect_identical(as.data.frame(from_design), as.data.frame(from_flat))
  expect_identical(design_diag$method, "hybrid")
  expect_identical(design_diag$init, "rank")
  expect_identical(design_diag$polish, "none")
  expect_identical(design_diag$rng_draws, 0L)
  expect_identical(flat_diag$init, "rank")
  expect_identical(flat_diag$polish, "none")
})

test_that("`sim_multisite()` seed override wins and integer seeds preserve global RNG state", {
  design <- multisitedgp_design(J = 25L, seed = 1L)

  set.seed(6103L)
  before <- .Random.seed
  override <- sim_multisite(design, seed = 2L)
  after <- .Random.seed
  expected <- sim_multisite(update_multisitedgp_design(design, seed = 2L))

  expect_identical(after, before)
  expect_identical(as.data.frame(override), as.data.frame(expected))
  expect_identical(attr(override, "design", exact = TRUE)$seed, 2L)
})

test_that("`sim_multisite()` seed NULL uses the active RNG stream without manufacturing a seed", {
  set.seed(6104L)
  before <- .Random.seed
  out <- sim_multisite(J = 25L, seed = NULL)
  after <- .Random.seed

  expect_s3_class(out, "multisitedgp_data")
  expect_false(identical(after, before))
  expect_null(attr(out, "design", exact = TRUE)$seed)
})

test_that("`sim_multisite()` rejects invalid designs and the arguments of `sim_meta()`", {
  direct_design <- multisitedgp_design(paradigm = "direct", I = 0.3)

  expect_multisitedgp_error(sim_multisite(design = list()), regexp = "`design` must be a design from")
  expect_multisitedgp_error(sim_multisite(direct_design), regexp = "requires `paradigm = \"site_size\"`")
  expect_multisitedgp_error(sim_multisite(direct_design, J = 25L), regexp = "must be empty when `design` is supplied")
  expect_multisitedgp_error(sim_multisite(I = 0.3), regexp = "does not take `I`, `R`, `shuffle`")
  expect_multisitedgp_error(sim_multisite(R = 2), regexp = "does not take `I`, `R`, `shuffle`")
  expect_multisitedgp_error(sim_multisite(shuffle = TRUE), regexp = "does not take `I`, `R`, `shuffle`")
  expect_multisitedgp_error(sim_multisite(se_args = list()), regexp = "does not take `I`, `R`, `shuffle`")
  expect_multisitedgp_error(sim_multisite(se_fn = function(...) 1), regexp = "does not take `I`, `R`, `shuffle`")
  expect_multisitedgp_error(sim_multisite(paradigm = "direct"), regexp = "does not accept `paradigm`")
  expect_multisitedgp_error(sim_multisite(set_seed = TRUE), regexp = "`set_seed` is not supported")
  expect_multisitedgp_error(
    sim_multisite(engine = "censored", dependence = "rank", rank_corr = 0.3),
    regexp = "is not compatible with"
  )
})

test_that("`sim_multisite()` preserves covariates and targets residual dependence", {
  data_x <- tibble::tibble(x_site = seq(-1, 1, length.out = 60L))
  out <- sim_multisite(
    J = 60L,
    formula = ~ x_site,
    beta = 1.5,
    data = data_x,
    dependence = "rank",
    rank_corr = -0.3,
    seed = 6105L
  )
  diagnostics <- attr(out, "diagnostics", exact = TRUE)

  expect_named(out, c("site_index", "z_j", "tau_j", "tau_j_hat", "se_j", "se2_j", "n_j", "x_site"))
  expect_equal(out$x_site, data_x$x_site, tolerance = tol_deterministic)
  expect_lt(abs(diagnostics$rho_S_residual - -0.3), tol_spearman_continuous)
  expect_gt(abs(diagnostics$rho_S_marginal - diagnostics$rho_S_residual), tol_spearman_ties)
})

test_that("`sim_multisite()` with the censored site-size generator reproduces the JEBS appendix draws", {
  out <- sim_multisite(
    J = 100L,
    tau = 0,
    sigma_tau = 0.15,
    true_dist = "Mixture",
    theta_G = list(delta = 5, eps = 0.3, ups = 2),
    nj_mean = 80,
    cv = 0.5,
    nj_min = 4L,
    p = 0.5,
    R2 = 0,
    engine = "censored",
    seed = 42L
  )

  expect_false("latent_component" %in% names(out))
  expect_identical(sum(out$n_j), 7263L)
  expect_equal(out$tau_j_hat[1:3], c(0.249583801676, -0.0452138825603, 0.0386598851721), tolerance = 1e-8)
  expect_equal(
    c(mean(out$tau_j), stats::sd(out$tau_j), mean(out$tau_j_hat), stats::sd(out$tau_j_hat), mean(out$se2_j)),
    c(0.01119308716689, 0.16999140787187, 0.00836756180938, 0.31923956892019, 0.07178314353686),
    tolerance = 1e-8
  )
  expect_equal(out$z_j, out$tau_j / 0.15, tolerance = tol_deterministic)
})
