test_that("`sim_meta()` is exported and returns multisitedgp_meta with its design and diagnostics", {
  expect_true("sim_meta" %in% getNamespaceExports("multisiteDGP"))

  out <- sim_meta(I = 0.4, R = 3, seed = 6201L)
  design <- attr(out, "design", exact = TRUE)
  diagnostics <- attr(out, "diagnostics", exact = TRUE)

  expect_s3_class(out, "multisitedgp_meta")
  expect_s3_class(out, "multisitedgp_data")
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("site_index", "z_j", "tau_j", "tau_j_hat", "se_j", "se2_j", "n_j"))
  expect_identical(nrow(out), 50L)
  expect_identical(out$n_j, rep(NA_integer_, 50L))
  expect_true(is_multisitedgp_design(design))
  expect_identical(design$paradigm, "direct")
  expect_identical(design$seed, 6201L)
  expect_equal(compute_I(out$se2_j, sigma_tau = design$sigma_tau), 0.4, tolerance = tol_deterministic)
  expect_equal(max(out$se2_j) / min(out$se2_j), 3, tolerance = tol_deterministic)
  expect_identical(diagnostics$target_I, 0.4)
  expect_identical(diagnostics$target_R, 3)
  expect_identical(diagnostics$engine, "direct")
  expect_false("margin_engine" %in% names(diagnostics))
  expect_identical(diagnostics$direct_se_method, "grid")
  expect_equal(diagnostics$I_error, 0, tolerance = tol_deterministic)
  expect_equal(diagnostics$R_error, 0, tolerance = tol_deterministic)
  expect_true(diagnostics$I_exact)
  expect_true(diagnostics$R_exact)
})

test_that("`sim_meta()` output carries no intermediate attributes", {
  out <- sim_meta(I = 0.5, R = 3, seed = 6207L)
  final_attrs <- names(attributes(out))
  plain <- tibble::as_tibble(out)

  expect_false(any(c(
    "engine", "I", "R", "direct_se_diagnostics", "dependence_diagnostics",
    "observation_diagnostics", "permutation_perm"
  ) %in% final_attrs))
  expect_false(any(c("design", "diagnostics") %in% names(attributes(plain))))
  expect_s3_class(plain, "tbl_df")
  expect_false(is_multisitedgp_data(plain))
})

test_that("`sim_meta()` flat args and design path agree under the same seed", {
  design <- multisitedgp_design(
    paradigm = "direct",
    J = 30L,
    I = 0.35,
    R = 5,
    sigma_tau = 0.25,
    shuffle = TRUE,
    dependence = "rank",
    rank_corr = 0.25
  )

  from_design <- sim_meta(design, seed = 6202L)
  from_flat <- sim_meta(
    J = 30L,
    I = 0.35,
    R = 5,
    sigma_tau = 0.25,
    shuffle = TRUE,
    dependence = "rank",
    rank_corr = 0.25,
    seed = 6202L
  )

  expect_identical(as.data.frame(from_design), as.data.frame(from_flat))
  expect_identical(attr(from_design, "diagnostics", exact = TRUE)$dependence_method, "rank")
  expect_identical(
    attr(from_design, "diagnostics", exact = TRUE)$dependence_diagnostics$target_type,
    "residual_spearman"
  )
})

test_that("`sim_meta()` forwards hybrid controls from design and flat args", {
  design <- multisitedgp_design(
    paradigm = "direct",
    J = 36L,
    I = 0.45,
    R = 3,
    shuffle = TRUE,
    dependence = "hybrid",
    rank_corr = -0.25,
    hybrid_init = "copula",
    hybrid_polish = "none"
  )

  from_design <- sim_meta(design, seed = 6206L)
  from_flat <- sim_meta(
    J = 36L,
    I = 0.45,
    R = 3,
    shuffle = TRUE,
    dependence = "hybrid",
    rank_corr = -0.25,
    hybrid_init = "copula",
    hybrid_polish = "none",
    seed = 6206L
  )
  design_diag <- attr(from_design, "diagnostics", exact = TRUE)$dependence_diagnostics
  flat_diag <- attr(from_flat, "diagnostics", exact = TRUE)$dependence_diagnostics

  expect_identical(as.data.frame(from_design), as.data.frame(from_flat))
  expect_identical(design_diag$method, "hybrid")
  expect_identical(design_diag$init, "copula")
  expect_identical(design_diag$polish, "none")
  expect_identical(flat_diag$init, "copula")
  expect_identical(flat_diag$polish, "none")
})

test_that("`sim_meta()` seed override wins and integer seeds preserve global RNG state", {
  design <- multisitedgp_design(paradigm = "direct", I = 0.5, R = 4, seed = 1L)

  set.seed(6203L)
  before <- .Random.seed
  override <- sim_meta(design, seed = 2L)
  after <- .Random.seed
  expected <- sim_meta(update_multisitedgp_design(design, seed = 2L))

  expect_identical(after, before)
  expect_identical(as.data.frame(override), as.data.frame(expected))
  expect_identical(attr(override, "design", exact = TRUE)$seed, 2L)
})

test_that("`sim_meta()` seed NULL uses active RNG stream without manufacturing a seed", {
  set.seed(6204L)
  before <- .Random.seed
  out <- sim_meta(J = 25L, I = 0.45, R = 2, seed = NULL)
  after <- .Random.seed

  expect_s3_class(out, "multisitedgp_meta")
  expect_false(identical(after, before))
  expect_null(attr(out, "design", exact = TRUE)$seed)
})

test_that("`sim_meta()` rejects invalid designs and the arguments of `sim_multisite()`", {
  site_design <- multisitedgp_design(paradigm = "site_size")

  expect_multisitedgp_error(sim_meta(design = list()), regexp = "`design` must be a design from")
  expect_multisitedgp_error(sim_meta(site_design), regexp = "requires `paradigm = \"direct\"`")
  expect_multisitedgp_error(sim_meta(site_design, I = 0.4), regexp = "must be empty when `design` is supplied")
  expect_multisitedgp_error(sim_meta(), regexp = "`I` is required")
  expect_multisitedgp_error(sim_meta(nj_mean = 40), regexp = "does not take site-size arguments")
  expect_multisitedgp_error(sim_meta(I = 0.5, cv = 0.5), regexp = "does not take site-size arguments")
  expect_multisitedgp_error(sim_meta(I = 0.5, nj_min = 5L), regexp = "does not take site-size arguments")
  expect_multisitedgp_error(sim_meta(I = 0.5, p = 0.5), regexp = "does not take site-size arguments")
  expect_multisitedgp_error(sim_meta(I = 0.5, R2 = 0), regexp = "does not take site-size arguments")
  expect_multisitedgp_error(sim_meta(I = 0.5, var_outcome = 1), regexp = "does not take site-size arguments")
  expect_multisitedgp_error(sim_meta(I = 0.5, engine = "truncated_gamma"), regexp = "does not take site-size arguments")
  expect_multisitedgp_error(sim_meta(I = 0.5, n_per_site = 50), regexp = "does not take site-size arguments")
  expect_multisitedgp_error(sim_meta(I = 0.5, paradigm = "direct"), regexp = "does not accept `paradigm`")
  expect_multisitedgp_error(sim_meta(I = 0.5, set_seed = TRUE), regexp = "`set_seed` is not supported")
})

test_that("`sim_meta()` preserves covariates and targets residual dependence", {
  data_x <- tibble::tibble(x_site = seq(-1, 1, length.out = 60L))
  out <- sim_meta(
    J = 60L,
    I = 0.5,
    R = 4,
    formula = ~ x_site,
    beta = 1.5,
    data = data_x,
    dependence = "rank",
    rank_corr = -0.3,
    seed = 6206L
  )
  diagnostics <- attr(out, "diagnostics", exact = TRUE)

  expect_named(out, c("site_index", "z_j", "tau_j", "tau_j_hat", "se_j", "se2_j", "n_j", "x_site"))
  expect_equal(out$x_site, data_x$x_site, tolerance = tol_deterministic)
  expect_equal(compute_I(out$se2_j, sigma_tau = 0.20), 0.5, tolerance = tol_deterministic)
  expect_equal(max(out$se2_j) / min(out$se2_j), 4, tolerance = tol_deterministic)
  expect_true(diagnostics$I_exact)
  expect_true(diagnostics$R_exact)
  expect_true(all(is.na(out$n_j)))
  expect_lt(abs(diagnostics$rho_S_residual - -0.3), tol_spearman_continuous)
  expect_gt(abs(diagnostics$rho_S_marginal - diagnostics$rho_S_residual), tol_spearman_ties)
})

test_that("`sim_meta()` accepts a user-supplied se_fn", {
  se_fn <- function(J, scale = 0.3) {
    list(
      se2_j = rep(scale^2, J),
      n_j = seq_len(J) + 100L
    )
  }
  out <- sim_meta(
    J = 20L,
    I = 0.5,
    R = 1,
    se_fn = se_fn,
    se_args = list(scale = 0.3),
    seed = 6205L
  )
  diagnostics <- attr(out, "diagnostics", exact = TRUE)

  expect_s3_class(out, "multisitedgp_meta")
  expect_identical(out$n_j, as.integer(seq_len(20L) + 100L))
  expect_equal(out$se2_j, rep(0.09, 20L), tolerance = tol_deterministic)
  expect_identical(diagnostics$direct_se_method, "custom")
  expect_identical(diagnostics$direct_se_diagnostics$method, "custom")

  bad_fn <- function(J) list(se2_j = rep(0.1, J - 1L))
  expect_multisitedgp_error(
    sim_meta(J = 20L, I = 0.5, se_fn = bad_fn, seed = 6205L)
  )
})

test_that("`sim_meta()` with a user-supplied se_fn keeps NA n_j through rank dependence", {
  se_fn <- function(J) {
    list(se2_j = seq(0.01, 0.05, length.out = J), n_j = NULL)
  }
  out <- sim_meta(
    J = 25L,
    I = 0.5,
    R = 1,
    se_fn = se_fn,
    dependence = "rank",
    rank_corr = 0,
    seed = 6208L
  )
  diagnostics <- attr(out, "diagnostics", exact = TRUE)

  expect_identical(out$n_j, rep(NA_integer_, 25L))
  expect_identical(diagnostics$direct_se_method, "custom")
  expect_true(is.na(diagnostics$I_exact))
  expect_true(is.na(diagnostics$R_exact))
  expect_identical(diagnostics$dependence_method, "rank")
})
