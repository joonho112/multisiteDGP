# nolint start: object_usage_linter
property_shape_specs <- function() {
  shapes <- list(
    Gaussian = list(true_dist = "Gaussian", theta_G = list()),
    StudentT = list(true_dist = "StudentT", theta_G = list(nu = 5)),
    Mixture = list(true_dist = "Mixture", theta_G = list(delta = 5, eps = 0.3, ups = 2)),
    PointMassSlab = list(true_dist = "PointMassSlab", theta_G = list(pi0 = 0.3, slab_shape = "Gaussian"))
  )
  if (requireNamespace("sn", quietly = TRUE)) {
    shapes$SkewN <- list(true_dist = "SkewN", theta_G = list(slant = 2))
  }
  if (requireNamespace("LaplacesDemon", quietly = TRUE)) {
    shapes$ALD <- list(true_dist = "ALD", theta_G = list(rho = 0.3))
  }
  shapes
}

property_site_cases <- function(n = 24L) {
  withr::with_seed(840003L, {
    data.frame(
      J = sample(c(25L, 40L, 60L, 80L, 100L), n, replace = TRUE),
      nj_mean = sample(c(40, 80, 160), n, replace = TRUE),
      cv = sample(c(0, 0.25, 0.50, 0.75), n, replace = TRUE),
      sigma_tau = sample(c(0.05, 0.10, 0.20, 0.30), n, replace = TRUE),
      I = sample(c(0.15, 0.25, 0.40, 0.65, 0.80), n, replace = TRUE),
      R = sample(c(1, 1.5, 3, 5), n, replace = TRUE),
      rank_corr = stats::runif(n, -0.35, 0.35),
      seed = sample.int(.Machine$integer.max, n),
      stringsAsFactors = FALSE
    )
  })
}

property_gen_effects <- function(spec, J, seed) {
  withr::with_seed(seed, gen_effects(J = J, true_dist = spec$true_dist, theta_G = spec$theta_G))
}

property_rank_upstream <- function(case) {
  effects <- withr::with_seed(
    case$seed,
    gen_effects_gaussian(J = case$J, sigma_tau = case$sigma_tau)
  )
  withr::with_seed(
    case$seed + 1L,
    gen_site_sizes(
      effects,
      J = case$J,
      nj_mean = case$nj_mean,
      cv = case$cv,
      nj_min = 5L,
      engine = "A2_modern"
    )
  )
}

test_that("P01 Var(z_j) approaches one for finite-variance G shapes", {
  skip_if_not_property()

  shapes <- property_shape_specs()
  for (idx in seq_along(shapes)) {
    out <- property_gen_effects(shapes[[idx]], J = 100000L, seed = 840100L + idx)
    expect_lt(abs(stats::var(out$z_j) - 1), tol_mc_moment_n1e5)
  }
})

test_that("P02 Mean(z_j) approaches zero for finite-variance G shapes", {
  skip_if_not_property()

  shapes <- property_shape_specs()
  for (idx in seq_along(shapes)) {
    out <- property_gen_effects(shapes[[idx]], J = 100000L, seed = 840200L + idx)
    expect_lt(abs(mean(out$z_j)), tol_mc_moment_n1e5)
  }
})

test_that("P03 se2_j is positive for random Paradigm A and B fixtures", {
  skip_if_not_property()

  cases <- property_site_cases()
  for (idx in seq_len(nrow(cases))) {
    case <- cases[idx, ]
    site <- sim_multisite(
      J = case$J,
      sigma_tau = case$sigma_tau,
      nj_mean = case$nj_mean,
      cv = case$cv,
      nj_min = 5L,
      engine = "A2_modern",
      seed = case$seed
    )
    direct <- sim_meta(J = case$J, I = case$I, R = case$R, seed = case$seed)

    expect_true(all(site$se2_j > 0))
    expect_true(all(direct$se2_j > 0))
  }
})

test_that("P04 Layer 3 rank dependence preserves the se2_j multiset", {
  skip_if_not_property()

  cases <- subset(property_site_cases(24L), cv > 0)
  cases <- cases[seq_len(12L), , drop = FALSE]
  for (idx in seq_len(nrow(cases))) {
    case <- cases[idx, ]
    upstream <- property_rank_upstream(case)
    out <- align_rank_corr(
      upstream,
      rank_corr = case$rank_corr,
      max_iter = 20000L,
      tol = tol_spearman_continuous
    )

    expect_equal(sort(out$se2_j), sort(upstream$se2_j), tolerance = tol_deterministic)
  }
})

test_that("P05 rank dependence achieves random feasible Spearman targets", {
  skip_if_not_property()

  cases <- subset(property_site_cases(24L), cv > 0)
  cases <- cases[seq_len(12L), , drop = FALSE]
  for (idx in seq_len(nrow(cases))) {
    case <- cases[idx, ]
    upstream <- property_rank_upstream(case)
    out <- align_rank_corr(
      upstream,
      rank_corr = case$rank_corr,
      max_iter = 20000L,
      tol = tol_spearman_continuous
    )
    diag <- attr(out, "dependence_diagnostics", exact = TRUE)
    achieved <- multisitedgp_internal(".realized_spearman")(out$z_j, out$se2_j)

    expect_identical(diag$converged, TRUE)
    expect_lt(abs(achieved - case$rank_corr), tol_spearman_continuous)
  }
})

test_that("P06 layer functions return one row per requested site", {
  skip_if_not_property()

  cases <- subset(property_site_cases(24L), cv > 0)
  cases <- cases[seq_len(10L), , drop = FALSE]
  for (idx in seq_len(nrow(cases))) {
    case <- cases[idx, ]
    effects <- withr::with_seed(case$seed, gen_effects_gaussian(J = case$J, sigma_tau = case$sigma_tau))
    sizes <- withr::with_seed(
      case$seed + 1L,
      gen_site_sizes(effects, J = case$J, nj_mean = case$nj_mean, cv = case$cv, nj_min = 5L)
    )
    aligned <- align_rank_corr(sizes, rank_corr = 0)
    observed <- withr::with_seed(case$seed + 2L, gen_observations(sizes))
    wrapped <- sim_multisite(J = case$J, sigma_tau = case$sigma_tau, seed = case$seed)

    expect_identical(nrow(effects), as.integer(case$J))
    expect_identical(nrow(sizes), as.integer(case$J))
    expect_identical(nrow(aligned), as.integer(case$J))
    expect_identical(nrow(observed), as.integer(case$J))
    expect_identical(nrow(wrapped), as.integer(case$J))
  }
})

test_that("P07 final wrappers preserve canonical column types", {
  skip_if_not_property()

  cases <- property_site_cases(20L)
  for (idx in seq_len(nrow(cases))) {
    case <- cases[idx, ]
    site <- sim_multisite(J = case$J, sigma_tau = case$sigma_tau, seed = case$seed)
    direct <- sim_meta(J = case$J, I = case$I, R = case$R, seed = case$seed)

    expect_type(site$n_j, "integer")
    expect_type(site$se2_j, "double")
    expect_type(site$tau_j, "double")
    expect_type(direct$n_j, "integer")
    expect_type(direct$se2_j, "double")
    expect_type(direct$tau_j, "double")
  }
})

test_that("P08 realized informativeness remains inside the open unit interval", {
  skip_if_not_property()

  cases <- property_site_cases(20L)
  for (idx in seq_len(nrow(cases))) {
    case <- cases[idx, ]
    site <- sim_multisite(J = case$J, sigma_tau = case$sigma_tau, seed = case$seed)
    direct <- sim_meta(J = case$J, I = case$I, R = case$R, seed = case$seed)

    expect_gt(informativeness(site), 0)
    expect_lt(informativeness(site), 1)
    expect_gt(informativeness(direct), 0)
    expect_lt(informativeness(direct), 1)
  }
})

test_that("P09 covariate-off path is identical to beta-zero formula path", {
  skip_if_not_property()

  cases <- property_site_cases(12L)
  for (idx in seq_len(nrow(cases))) {
    case <- cases[idx, ]
    data_x <- tibble::tibble(x = seq(-1, 1, length.out = case$J))
    no_cov <- sim_multisite(J = case$J, sigma_tau = case$sigma_tau, seed = case$seed)
    beta_zero <- sim_multisite(
      J = case$J,
      sigma_tau = case$sigma_tau,
      formula = ~ x,
      beta = 0,
      data = data_x,
      seed = case$seed
    )

    for (column in .canonical_data_columns()) {
      expect_equal(no_cov[[column]], beta_zero[[column]], tolerance = tol_deterministic)
    }
  }
})

test_that("P10 random A2 fixtures are bit-identical at the same seed", {
  skip_if_not_property()

  cases <- property_site_cases(12L)
  for (idx in seq_len(nrow(cases))) {
    case <- cases[idx, ]
    hashes <- vapply(seq_len(5), function(i) {
      canonical_hash(sim_multisite(
        J = case$J,
        sigma_tau = case$sigma_tau,
        nj_mean = case$nj_mean,
        cv = case$cv,
        nj_min = 5L,
        engine = "A2_modern",
        seed = case$seed
      ))
    }, character(1))

    expect_identical(length(unique(hashes)), 1L)
  }
})

test_that("P11 dependence none records identity permutation for random shapes", {
  skip_if_not_property()

  shapes <- property_shape_specs()
  shapes <- shapes[intersect(names(shapes), c("Gaussian", "Mixture", "PointMassSlab"))]
  for (idx in seq_along(shapes)) {
    spec <- shapes[[idx]]
    out <- sim_multisite(
      J = 60L,
      true_dist = spec$true_dist,
      theta_G = spec$theta_G,
      dependence = "none",
      engine = "A2_modern",
      seed = 841100L + idx
    )
    diagnostics <- attr(out, "diagnostics", exact = TRUE)

    expect_identical(diagnostics$permutation, "identity")
    expect_null(attr(out, "permutation_perm", exact = TRUE))
  }
})

test_that("P12 every public preset returns a valid multisitedgp_design", {
  skip_if_not_property()

  preset_names <- c(
    "preset_education_small",
    "preset_education_modest",
    "preset_education_substantial",
    "preset_jebs_paper",
    "preset_jebs_strict",
    "preset_walters_2024",
    "preset_twin_towers",
    "preset_meta_modest",
    "preset_small_area_estimation"
  )

  for (name in preset_names) {
    preset <- get(name, envir = asNamespace("multisiteDGP"))
    design <- preset()

    expect_s3_class(design, "multisitedgp_design")
    expect_error(validate_multisitedgp_design(design), NA)
  }
})
# nolint end
