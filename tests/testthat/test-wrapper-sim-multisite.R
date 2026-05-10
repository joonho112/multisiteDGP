# nolint start: object_usage_linter
test_that("sim_multisite is exported and returns canonical multisitedgp_data", {
  expect_true("sim_multisite" %in% getNamespaceExports("multisiteDGP"))

  out <- sim_multisite(seed = 6101L)
  design <- attr(out, "design", exact = TRUE)
  diagnostics <- attr(out, "diagnostics", exact = TRUE)
  provenance <- attr(out, "provenance", exact = TRUE)

  expect_s3_class(out, "multisitedgp_data")
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("site_index", "z_j", "tau_j", "tau_j_hat", "se_j", "se2_j", "n_j"))
  expect_identical(nrow(out), 50L)
  expect_true(is_multisitedgp_design(design))
  expect_identical(design$paradigm, "site_size")
  expect_identical(design$seed, 6101L)
  expect_identical(attr(out, "paradigm", exact = TRUE), "site_size")
  expect_identical(diagnostics$J, 50L)
  expect_identical(diagnostics$dependence_method, "none")
  expect_true(is.finite(diagnostics$I_hat))
  expect_true(is.finite(diagnostics$R_hat))
  expect_identical(provenance$canonical_hash, canonical_hash(out))
  expect_true(all(is.finite(out$tau_j_hat)))
})

test_that("sim_multisite final object strips layer-private attributes", {
  out <- sim_multisite(seed = 6111L)
  final_attrs <- names(attributes(out))
  plain <- tibble::as_tibble(out)

  expect_false(any(c(
    "engine", "kappa", "dependence_diagnostics", "observation_diagnostics",
    "permutation_perm", "observation_permutation_perm"
  ) %in% final_attrs))
  expect_false(any(c("design", "diagnostics", "provenance", "paradigm") %in% names(attributes(plain))))
  expect_s3_class(plain, "tbl_df")
  expect_false(is_multisitedgp_data(plain))
})

test_that("sim_multisite flat args and design path agree under the same seed", {
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

  expect_identical(canonical_hash(from_design), canonical_hash(from_flat))
  expect_identical(attr(from_design, "diagnostics", exact = TRUE)$dependence_method, "rank")
  expect_identical(attr(from_design, "diagnostics", exact = TRUE)$dependence_diagnostics$target_type, "residual_spearman")
})

test_that("sim_multisite forwards hybrid controls from design and flat args", {
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

  expect_identical(canonical_hash(from_design), canonical_hash(from_flat))
  expect_identical(design_diag$method, "hybrid")
  expect_identical(design_diag$init, "rank")
  expect_identical(design_diag$polish, "none")
  expect_identical(design_diag$rng_draws, 0L)
  expect_identical(flat_diag$init, "rank")
  expect_identical(flat_diag$polish, "none")
})

test_that("sim_multisite seed override wins and integer seeds preserve global RNG state", {
  design <- multisitedgp_design(J = 25L, seed = 1L)

  set.seed(6103L)
  before <- .Random.seed
  override <- sim_multisite(design, seed = 2L)
  after <- .Random.seed
  expected <- sim_multisite(update_multisitedgp_design(design, seed = 2L))

  expect_identical(after, before)
  expect_identical(canonical_hash(override), canonical_hash(expected))
  expect_identical(attr(override, "design", exact = TRUE)$seed, 2L)
})

test_that("sim_multisite seed NULL uses the active RNG stream without manufacturing a seed", {
  set.seed(6104L)
  before <- .Random.seed
  out <- sim_multisite(J = 25L, seed = NULL)
  after <- .Random.seed

  expect_s3_class(out, "multisitedgp_data")
  expect_false(identical(after, before))
  expect_null(attr(out, "design", exact = TRUE)$seed)
})

test_that("sim_multisite validates design and wrong-door calls", {
  direct_design <- multisitedgp_design(paradigm = "direct", I = 0.3)

  expect_multisitedgp_error(sim_multisite(design = list()), "multisitedgp_arg_error")
  expect_multisitedgp_error(sim_multisite(direct_design), "multisitedgp_coherence_error")
  expect_multisitedgp_error(sim_multisite(direct_design, J = 25L), "multisitedgp_arg_error")
  expect_multisitedgp_error(sim_multisite(I = 0.3), "multisitedgp_coherence_error")
  expect_multisitedgp_error(sim_multisite(R = 2), "multisitedgp_coherence_error")
  expect_multisitedgp_error(sim_multisite(shuffle = TRUE), "multisitedgp_coherence_error")
  expect_multisitedgp_error(sim_multisite(se_args = list()), "multisitedgp_coherence_error")
  expect_multisitedgp_error(sim_multisite(se_fn = function(...) 1), "multisitedgp_coherence_error")
  expect_multisitedgp_error(sim_multisite(paradigm = "direct"), "multisitedgp_coherence_error")
  expect_multisitedgp_error(sim_multisite(set_seed = TRUE), "multisitedgp_arg_error")
  expect_multisitedgp_error(
    sim_multisite(engine = "A1_legacy", dependence = "rank", rank_corr = 0.3),
    "multisitedgp_engine_dependence_error"
  )
})

test_that("sim_multisite preserves covariates and targets residual dependence", {
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

test_that("sim_multisite A1 legacy path matches JEBS normalized fixture hash", {
  manifest_file <- test_path("../../tools/jebs-golden-fixtures/jebs-golden-fixture-manifest.csv")
  skip_if_not(file.exists(manifest_file), "Step 4.1 JEBS manifest is not shipped in the package tarball.")

  manifest <- read.csv(manifest_file, stringsAsFactors = FALSE)
  manifest_row <- manifest[manifest$seed == 42L, , drop = FALSE]
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
    engine = "A1_legacy",
    seed = 42L
  )

  expect_false("latent_component" %in% names(out))
  raw_observed <- data.frame(
    n_j = as.numeric(out$n_j),
    se2_j = out$se2_j,
    tau_j = out$tau_j,
    tau_j_hat = out$tau_j_hat
  )
  expect_identical(canonical_hash(raw_observed), manifest_row$component_observed_raw_hash)
  expect_equal(out$z_j, out$tau_j / 0.15, tolerance = tol_deterministic)
})
# nolint end
