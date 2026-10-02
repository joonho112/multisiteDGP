# `kurtosis` is the fourth moment of z_j for these parameter values. It gives
# the standard error of the sample variance, sqrt((kurtosis - 1) / n).
property_shape_specs <- function() {
  shapes <- list(
    Gaussian = list(true_dist = "Gaussian", theta_G = list(), kurtosis = 3),
    StudentT = list(true_dist = "StudentT", theta_G = list(nu = 5), kurtosis = 9),
    Mixture = list(true_dist = "Mixture", theta_G = list(delta = 5, eps = 0.3, ups = 2), kurtosis = 3.18),
    PointMassSlab = list(
      true_dist = "PointMassSlab", theta_G = list(pi0 = 0.3, slab_shape = "Gaussian"), kurtosis = 3 / 0.7
    ),
    SkewN = list(true_dist = "SkewN", theta_G = list(slant = 2), kurtosis = 3.31),
    ALD = list(true_dist = "ALD", theta_G = list(rho = 0.3), kurtosis = 7.43)
  )
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
      engine = "truncated_gamma"
    )
  )
}

test_that("Var(z_j) approaches one for finite-variance effect distributions", {
  # The tolerance is 4 standard errors of the sample variance at n = 100000.
  n <- n_moment_check
  shapes <- property_shape_specs()
  for (idx in seq_along(shapes)) {
    out <- property_gen_effects(shapes[[idx]], J = n, seed = 840100L + idx)
    expect_lt(abs(stats::var(out$z_j) - 1), 4 * se_variance(n, shapes[[idx]]$kurtosis))
  }
})

test_that("Mean(z_j) approaches zero for finite-variance effect distributions", {
  # The tolerance is 4 standard errors of the sample mean at n = 100000.
  n <- n_moment_check
  shapes <- property_shape_specs()
  for (idx in seq_along(shapes)) {
    out <- property_gen_effects(shapes[[idx]], J = n, seed = 840200L + idx)
    expect_lt(abs(mean(out$z_j)), 4 * se_mean(n))
  }
})

test_that("se2_j is positive for random site-size and direct designs", {
  # se2_j > 0 holds for every draw, so 8 random designs are enough. They
  # include cv = 0 and cv > 0, and R = 1 and R > 1.
  cases <- property_site_cases(8L)
  for (idx in seq_len(nrow(cases))) {
    case <- cases[idx, ]
    site <- sim_multisite(
      J = case$J,
      sigma_tau = case$sigma_tau,
      nj_mean = case$nj_mean,
      cv = case$cv,
      nj_min = 5L,
      engine = "truncated_gamma",
      seed = case$seed
    )
    direct <- sim_meta(J = case$J, I = case$I, R = case$R, seed = case$seed)

    expect_true(all(site$se2_j > 0))
    expect_true(all(direct$se2_j > 0))
  }
})

test_that("rank dependence preserves the set of se2_j values", {
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

test_that("rank dependence achieves random feasible Spearman targets", {
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

test_that("each generating step returns one row per requested site", {
  # The row count is the same for every draw, so 5 random designs are enough.
  # Their J runs from 25 to 100.
  cases <- subset(property_site_cases(24L), cv > 0)
  cases <- cases[seq_len(5L), , drop = FALSE]
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

test_that("`sim_multisite()` and `sim_meta()` return n_j as integer and se2_j and tau_j as double", {
  # The column types are the same for every draw, so 8 random designs are
  # enough. They include R = 1 and R > 1.
  cases <- property_site_cases(8L)
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

test_that("realized informativeness remains inside the open unit interval", {
  # 0 < I < 1 holds for every draw, so 8 random designs are enough. They
  # include R = 1 and R > 1.
  cases <- property_site_cases(8L)
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

test_that("no formula and a zero beta give identical data for random designs", {
  # The identity is exact for every draw, so 8 random designs are enough.
  cases <- property_site_cases(8L)
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

    for (column in .data_columns()) {
      expect_equal(no_cov[[column]], beta_zero[[column]], tolerance = tol_deterministic)
    }
  }
})

test_that("random truncated-gamma designs are bit-identical at the same seed", {
  # A seeded run is reproduced exactly, so 8 random designs run 3 times each
  # are enough. They include cv = 0 and cv > 0.
  cases <- property_site_cases(8L)
  for (idx in seq_len(nrow(cases))) {
    case <- cases[idx, ]
    runs <- lapply(seq_len(3), function(i) {
      as.data.frame(sim_multisite(
        J = case$J,
        sigma_tau = case$sigma_tau,
        nj_mean = case$nj_mean,
        cv = case$cv,
        nj_min = 5L,
        engine = "truncated_gamma",
        seed = case$seed
      ))
    })

    for (run in runs[-1L]) {
      expect_identical(run, runs[[1L]])
    }
  }
})

test_that("without dependence no permutation is recorded for Gaussian, Mixture and PointMassSlab effects", {
  shapes <- property_shape_specs()
  shapes <- shapes[intersect(names(shapes), c("Gaussian", "Mixture", "PointMassSlab"))]
  for (idx in seq_along(shapes)) {
    spec <- shapes[[idx]]
    out <- sim_multisite(
      J = 60L,
      true_dist = spec$true_dist,
      theta_G = spec$theta_G,
      dependence = "none",
      engine = "truncated_gamma",
      seed = 841100L + idx
    )
    diagnostics <- attr(out, "diagnostics", exact = TRUE)

    expect_identical(diagnostics$permutation, "identity")
    expect_null(attr(out, "permutation_perm", exact = TRUE))
  }
})

test_that("every preset returns a valid multisitedgp_design", {
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
