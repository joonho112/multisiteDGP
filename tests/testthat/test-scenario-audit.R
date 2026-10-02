scenario_relaxed_thresholds <- function(...) {
  modifyList(
    modifyList(
      default_thresholds(),
      list(
        mean_shrinkage_min = 0,
        R_max = 1e6,
        ks_scaled_max = 1e6
      )
    ),
    list(...)
  )
}

scenario_metric_frame <- function(
  mean_shrinkage,
  R_hat,
  bhattacharyya,
  ks
) {
  n <- length(mean_shrinkage)
  tibble::tibble(
    cell_id = 1L,
    rep = seq_len(n),
    seed = seq_len(n),
    I_hat = rep(0.30, n),
    R_hat = R_hat,
    mean_shrinkage = mean_shrinkage,
    bhattacharyya = bhattacharyya,
    ks = ks
  )
}

grid_ok <- function() {
  design_grid(J = 25L, sigma_tau = 0.15, nj_mean = 60, seed_root = 777L)
}

# What the tests check on these audits (no abort, the groups evaluated, which
# columns are NA) holds for every replication, so two per cell are enough.
audit_shape <- function(true_dist, theta = list(), n_reps = 2L) {
  grid <- design_grid(
    J = 40L, sigma_tau = 0.2, nj_mean = 100, seed_root = 12345L,
    true_dist = true_dist, theta_G = list(theta)
  )
  suppressWarnings(scenario_audit(grid, M = n_reps))
}

shapes_without_reference <- list(
  SkewN = list(slant = 3),
  ALD = list(rho = 0.3),
  Mixture = list(delta = 5, eps = 0.3, ups = 2),
  PointMassSlab = list(pi0 = 0.3)
)

test_that("scenario_audit is exported and summarizes cells", {
  exports <- getNamespaceExports("multisiteDGP")
  expect_true("scenario_audit" %in% exports)

  # With R = 1 every site has the same se2_j, so the mean shrinkage equals I:
  # 0.2 in the first row and 0.6 in the second.
  grid <- design_grid(
    paradigm = "direct",
    J = 20L,
    I = c(0.2, 0.6),
    R = 1,
    shuffle = FALSE,
    seed_root = 7601L
  )
  audit <- scenario_audit(
    grid,
    M = 2L,
    thresholds = scenario_relaxed_thresholds(mean_shrinkage_min = 0.3)
  )

  expect_s3_class(audit, "tbl_df")
  expect_equal(nrow(audit), nrow(grid))
  expect_named(audit, c(
    "cell_id", "paradigm", "J", "I", "R", "shuffle", "M", "seed_root",
    "status", "pass", "groups_evaluated", "fail_reasons", "warn_reasons",
    "med_I_hat", "q05_I_hat", "q95_I_hat", "med_R_hat", "q95_R_hat",
    "med_mean_shrinkage", "q05_mean_shrinkage",
    "med_bhattacharyya", "q05_bhattacharyya", "med_ks", "q95_ks"
  ))
  expect_identical(audit$cell_id, seq_len(nrow(grid)))
  expect_identical(audit$M, c(2L, 2L))
  expect_equal(audit$med_mean_shrinkage, c(0.2, 0.6), tolerance = tol_deterministic)
  expect_identical(audit$status, c("fail", "pass"))
  expect_identical(audit$pass, c(FALSE, TRUE))
  expect_identical(audit$fail_reasons[[1L]], "mean_shrinkage:median<0.3")
  expect_identical(audit$fail_reasons[[2L]], "")
  expect_true(all(is.finite(audit$med_I_hat)))
})

test_that("scenario_audit is deterministic and RNG-neutral", {
  grid <- design_grid(
    paradigm = "direct",
    J = c(20L, 25L),
    I = 0.4,
    R = 1,
    shuffle = FALSE,
    seed_root = 7602L
  )

  set.seed(7602L)
  before <- get(".Random.seed", envir = .GlobalEnv)
  audit_one <- scenario_audit(grid, M = 2L, thresholds = scenario_relaxed_thresholds())
  after <- get(".Random.seed", envir = .GlobalEnv)
  audit_two <- scenario_audit(grid, M = 2L, thresholds = scenario_relaxed_thresholds())

  expect_identical(after, before)
  expect_identical(audit_one, audit_two)
})

test_that("scenario_audit runs site-size and direct designs", {
  site_grid <- design_grid(
    J = 12L,
    sigma_tau = 0.2,
    nj_mean = 50,
    seed_root = 7603L
  )
  direct_grid <- design_grid(
    paradigm = "direct",
    J = 12L,
    I = 0.5,
    R = 1,
    shuffle = FALSE,
    seed_root = 7604L
  )

  site_audit <- scenario_audit(site_grid, M = 1L, thresholds = scenario_relaxed_thresholds())
  direct_audit <- scenario_audit(direct_grid, M = 1L, thresholds = scenario_relaxed_thresholds())

  expect_equal(nrow(site_audit), 1L)
  expect_equal(nrow(direct_audit), 1L)
  expect_identical(site_audit$cell_id, 1L)
  expect_identical(direct_audit$cell_id, 1L)
  expect_true(is.logical(site_audit$pass))
  expect_true(is.logical(direct_audit$pass))
})

test_that("scenario_audit can use fixed design seeds when grid seed stream is off", {
  base <- multisitedgp_design(seed = 7605L)
  grid <- design_grid(
    J = c(12L, 14L),
    base_design = base,
    seed_stream = FALSE
  )

  audit <- scenario_audit(grid, M = 1L, thresholds = scenario_relaxed_thresholds())

  expect_equal(nrow(audit), 2L)
  expect_identical(audit$seed_root, c(7605L, 7605L))
})

test_that("scenario_audit validates inputs with classed errors", {
  grid <- design_grid(J = 12L, seed_root = 7606L)
  seedless <- design_grid(J = 12L, seed_stream = FALSE)
  malformed <- grid
  malformed$design[[1L]] <- list()

  expect_multisitedgp_error(
    scenario_audit(data.frame(x = 1), M = 1L)
  )
  expect_multisitedgp_error(
    scenario_audit(seedless, M = 1L)
  )
  expect_multisitedgp_error(
    scenario_audit(malformed, M = 1L)
  )
  expect_multisitedgp_error(
    scenario_audit(grid, M = 0L)
  )
  expect_multisitedgp_error(
    scenario_audit(grid, M = 1.5)
  )
  expect_multisitedgp_error(
    scenario_audit(grid, M = 1L, thresholds = list(not_a_gate = 1))
  )
  expect_multisitedgp_error(
    scenario_audit(grid, M = 1L, thresholds = list(R_max = c(1, 2)))
  )
  expect_multisitedgp_error(
    scenario_audit(grid, M = 1L, thresholds = list(ks_scaled_max = 0))
  )
})

test_that("the thresholds removed in 0.3.0 are refused by name", {
  grid <- design_grid(J = 12L, seed_root = 7606L)

  for (removed in list(list(feasibility_min = 5), list(bhattacharyya_min = 0.85), list(ks_max = 0.1))) {
    expect_multisitedgp_error(
      scenario_audit(grid, M = 1L, thresholds = removed),
      regexp = "mean_shrinkage_min, R_max, ks_scaled_max"
    )
  }
})

test_that("scenario_audit rejects a grid that is not a design_grid", {
  expect_multisitedgp_error(
    scenario_audit(data.frame(J = 10L), M = 2L)
  )
})

test_that("scenario_audit rejects a grid without a design list-column", {
  g <- grid_ok()
  g$design <- NULL
  expect_multisitedgp_error(scenario_audit(g, M = 2L))
})

test_that("scenario_audit rejects an empty grid", {
  g <- grid_ok()
  empty <- g[0L, , drop = FALSE]
  class(empty) <- class(g)
  expect_multisitedgp_error(scenario_audit(empty, M = 2L))
})

test_that("scenario_audit rejects a grid whose design column is not designs", {
  g <- grid_ok()
  g$design <- list("not a design")
  expect_multisitedgp_error(scenario_audit(g, M = 2L))
})

test_that("scenario_audit requires a deterministic cell seed", {
  # The audit must not consume the caller's RNG, so a grid with neither a cell
  # seed nor a base-design seed is refused rather than silently seeded.
  g <- grid_ok()
  g$seed <- NA_integer_
  g$design <- list(multisitedgp_design(J = 25L, sigma_tau = 0.15, nj_mean = 60))
  expect_multisitedgp_error(scenario_audit(g, M = 2L))
})

test_that("thresholds must be a named list", {
  g <- grid_ok()
  expect_multisitedgp_error(scenario_audit(g, M = 2L, thresholds = 1))
  expect_multisitedgp_error(
    scenario_audit(g, M = 2L, thresholds = list(0.5))
  )
})

test_that("thresholds reject unknown gate names", {
  expect_multisitedgp_error(
    scenario_audit(grid_ok(), M = 2L, thresholds = list(no_such_gate = 1))
  )
})

test_that("each threshold enforces its own admissible range", {
  g <- grid_ok()
  bad <- list(
    R_max = 0,                    # positive
    mean_shrinkage_min = 2,       # unit interval
    ks_scaled_max = -1            # positive
  )
  for (nm in names(bad)) {
    expect_multisitedgp_error(
      scenario_audit(g, M = 2L, thresholds = bad[nm])
    )
  }
})

test_that("M is validated as a positive integer within integer range", {
  g <- grid_ok()
  expect_multisitedgp_error(scenario_audit(g, M = 0L))
  expect_multisitedgp_error(scenario_audit(g, M = 2.5))
  expect_multisitedgp_error(scenario_audit(g, M = 2^40))
})

test_that("scenario metric summarizer classifies pass, warn, and fail gates", {
  summarize <- multisitedgp_internal(".summarize_scenario_metrics")
  # At 100 sites ks_scaled_max = 2 is a KS distance of 2 / sqrt(100) = 0.2.
  thresholds <- scenario_relaxed_thresholds(
    mean_shrinkage_min = 0.50,
    R_max = 2,
    ks_scaled_max = 2
  )

  pass <- summarize(
    scenario_metric_frame(
      mean_shrinkage = rep(0.70, 5L),
      R_hat = rep(1.2, 5L),
      bhattacharyya = rep(0.90, 5L),
      ks = rep(0.10, 5L)
    ),
    thresholds = thresholds,
    J = 100L
  )
  warn <- summarize(
    scenario_metric_frame(
      mean_shrinkage = c(0.20, 0.70, 0.70, 0.70, 0.70),
      R_hat = rep(1.2, 5L),
      bhattacharyya = rep(0.90, 5L),
      ks = rep(0.10, 5L)
    ),
    thresholds = thresholds,
    J = 100L
  )
  fail <- summarize(
    scenario_metric_frame(
      mean_shrinkage = rep(0.20, 5L),
      R_hat = rep(Inf, 5L),
      bhattacharyya = rep(0.50, 5L),
      ks = rep(0.50, 5L)
    ),
    thresholds = thresholds,
    J = 100L
  )

  expect_identical(pass$status, "pass")
  expect_true(pass$pass)
  expect_identical(pass$fail_reasons, "")
  expect_identical(pass$warn_reasons, "")

  expect_identical(warn$status, "warn")
  expect_false(warn$pass)
  expect_match(warn$warn_reasons, "mean_shrinkage", fixed = TRUE)
  expect_identical(warn$fail_reasons, "")

  expect_identical(fail$status, "fail")
  expect_false(fail$pass)
  expect_identical(
    fail$fail_reasons,
    "mean_shrinkage:median<0.5; R:median>2; ks:median>0.2"
  )
  expect_identical(fail$warn_reasons, "")
})

test_that("the KS threshold is a distance times sqrt(J)", {
  summarize <- multisitedgp_internal(".summarize_scenario_metrics")
  thresholds <- scenario_relaxed_thresholds(ks_scaled_max = 1.63)
  metrics <- scenario_metric_frame(
    mean_shrinkage = rep(0.70, 5L),
    R_hat = rep(1.2, 5L),
    bhattacharyya = rep(0.90, 5L),
    ks = rep(0.15, 5L)
  )

  # A KS distance of 0.15 is below 1.63 / sqrt(50) = 0.231 and above
  # 1.63 / sqrt(200) = 0.115.
  at_50 <- summarize(metrics, thresholds = thresholds, J = 50L)
  at_200 <- summarize(metrics, thresholds = thresholds, J = 200L)

  expect_identical(at_50$status, "pass")
  expect_identical(at_200$status, "fail")
  expect_identical(at_200$fail_reasons, "ks:median>0.115")
})

test_that("the Bhattacharyya coefficient is summarized but has no threshold", {
  summarize <- multisitedgp_internal(".summarize_scenario_metrics")
  low <- summarize(
    scenario_metric_frame(
      mean_shrinkage = rep(0.70, 5L),
      R_hat = rep(1.2, 5L),
      bhattacharyya = rep(0.10, 5L),
      ks = rep(0.10, 5L)
    ),
    thresholds = default_thresholds(),
    J = 100L
  )

  expect_identical(low$status, "pass")
  expect_equal(low$med_bhattacharyya, 0.10, tolerance = tol_deterministic)
  expect_false("bhattacharyya_min" %in% names(default_thresholds()))
})

test_that("a correctly simulated Gaussian design passes the default KS threshold at every J", {
  # sqrt(J) times the KS distance has nearly the same distribution at every
  # J: median near 0.8, above 1.63 in about 1 simulation in 100. The row is
  # "warn" when the 95th percentile of 40 simulations is above the threshold,
  # which needs three of them above it and has probability below 0.01.
  grid <- design_grid(
    paradigm = "direct",
    J = c(25L, 50L, 200L),
    I = 0.5,
    R = 3,
    seed_root = 7611L
  )
  audit <- scenario_audit(grid, M = 40L)

  expect_identical(audit$status, rep("pass", 3L))
  expect_identical(audit$groups_evaluated, rep("scale, precision, shape", 3L))
  # The median of 40 values has standard error near 0.05, so 4 standard errors
  # around 0.8 are 0.6 to 1.0.
  scaled_median <- audit$med_ks * sqrt(audit$J)
  expect_true(all(scaled_median > 0.6 & scaled_median < 1.0))
  # The unscaled distance is far from constant. Its median with 25 sites is
  # sqrt(8) = 2.8 times the median with 200, and the ratio of two medians of
  # 40 values has a relative standard error near 0.09, so 1.5 is 5 standard
  # errors below 2.8.
  expect_gt(audit$med_ks[[1L]], 1.5 * audit$med_ks[[3L]])
})

test_that("the tail gates fire on their own, without the median gates", {
  # A gate gives "fail" on the median and "warn" on the tail quantile. Thresholds
  # are derived from the cell's own measured quantiles rather than hard-coded, so
  # this stays true if the RNG stream moves. Each threshold lies between the
  # median and the tail quantile of the same replications, so the outcome does
  # not depend on their number: 10 are enough to keep the two quantiles apart.
  g <- grid_ok()
  baseline <- suppressWarnings(scenario_audit(g, M = 10L))

  mid <- function(med, tail) (med + tail) / 2
  thresholds <- list(
    mean_shrinkage_min = mid(baseline$med_mean_shrinkage, baseline$q05_mean_shrinkage),
    R_max              = mid(baseline$med_R_hat, baseline$q95_R_hat),
    ks_scaled_max      = mid(baseline$med_ks, baseline$q95_ks) * sqrt(g$J)
  )

  out <- suppressWarnings(scenario_audit(g, M = 10L, thresholds = thresholds))

  expect_identical(out$status, "warn")
  expect_false(out$pass)
  expect_identical(out$fail_reasons, "")
  for (gate in c("mean_shrinkage", "R", "ks")) {
    expect_match(out$warn_reasons, gate, fixed = TRUE)
  }
})

test_that("a cell with no violations reports pass and empty reason strings", {
  wide <- list(mean_shrinkage_min = 0, R_max = 1e6, ks_scaled_max = 1e6)
  out <- suppressWarnings(scenario_audit(grid_ok(), M = 5L, thresholds = wide))

  expect_identical(out$status, "pass")
  expect_true(out$pass)
  expect_identical(out$groups_evaluated, "scale, precision, shape")
  expect_identical(out$fail_reasons, "")
  expect_identical(out$warn_reasons, "")
})

test_that("a fail suppresses the warn reasons rather than reporting both", {
  # Both the median and the 5th percentile of mean shrinkage are below 1.
  strict <- list(mean_shrinkage_min = 1)
  out <- suppressWarnings(scenario_audit(grid_ok(), M = 5L, thresholds = strict))

  expect_identical(out$status, "fail")
  expect_false(out$pass)
  expect_match(out$fail_reasons, "mean_shrinkage", fixed = TRUE)
  expect_identical(out$warn_reasons, "")
})

test_that("a small threshold is written with three significant digits in the reasons", {
  grid <- design_grid(J = 25L, base_design = preset_education_modest(), seed_root = 42L)
  out <- scenario_audit(grid, M = 3L, thresholds = list(ks_scaled_max = 0.002))
  reasons <- strsplit(out$fail_reasons[[1L]], "; ", fixed = TRUE)[[1L]]

  expect_identical(out$status[[1L]], "fail")
  # The KS distance is compared with 0.002 / sqrt(25), which three decimals
  # would show as 0.000.
  expect_identical(reasons[startsWith(reasons, "ks:")], "ks:median>0.0004")
  expect_gt(out$med_ks[[1L]], 0.0004)
})

# bhattacharyya_coef() and ks_distance() compare the simulated effects with a
# reference distribution, and only Gaussian and StudentT have one. For the other
# distributions the audit leaves the shape diagnostics out instead of stopping.
# Only the KS distance has a threshold.

test_that("every catalog shape can be audited without aborting", {
  for (nm in names(shapes_without_reference)) {
    expect_no_error(audit_shape(nm, shapes_without_reference[[nm]]))
  }
  expect_no_error(audit_shape("Gaussian"))
  expect_no_error(audit_shape("StudentT", list(nu = 5)))
})

test_that("shapes with a reference distribution are audited on scale, precision and shape", {
  for (a in list(audit_shape("Gaussian"), audit_shape("StudentT", list(nu = 5)))) {
    expect_identical(a$groups_evaluated, "scale, precision, shape")
    expect_false(is.na(a$med_bhattacharyya))
    expect_false(is.na(a$med_ks))
  }
})

test_that("shapes without one are audited on scale and precision, with NA shape columns", {
  for (nm in names(shapes_without_reference)) {
    a <- audit_shape(nm, shapes_without_reference[[nm]])
    expect_identical(a$groups_evaluated, "scale, precision", info = nm)
    for (col in c("med_bhattacharyya", "q05_bhattacharyya", "med_ks", "q95_ks")) {
      expect_true(is.na(a[[col]]), info = sprintf("%s / %s", nm, col))
    }
  }
})

test_that("shape diagnostics that cannot be measured are skipped, not counted as a violation", {
  # .gate_high_fail() treats a non-finite value as a failure, which is right for
  # a metric that was measured and came back NaN. Letting NA reach it would turn
  # "no reference distribution" into a failure.
  for (nm in names(shapes_without_reference)) {
    a <- audit_shape(nm, shapes_without_reference[[nm]])
    expect_false(grepl("ks", a$fail_reasons))
    expect_false(grepl("ks", a$warn_reasons))
  }
})

test_that("the audit has the same columns whether or not the shape diagnostics ran", {
  gaussian <- audit_shape("Gaussian")
  mixture <- audit_shape("Mixture", shapes_without_reference$Mixture)

  expect_identical(names(gaussian), names(mixture))
  expect_identical(
    names(gaussian),
    c(
      "cell_id", "J", "sigma_tau", "nj_mean", "true_dist", "theta_G", "M", "seed_root",
      "status", "pass", "groups_evaluated", "fail_reasons", "warn_reasons",
      "med_I_hat", "q05_I_hat", "q95_I_hat", "med_R_hat", "q95_R_hat",
      "med_mean_shrinkage", "q05_mean_shrinkage",
      "med_bhattacharyya", "q05_bhattacharyya", "med_ks", "q95_ks"
    )
  )
})

test_that("the auto-reference shape list matches the resolver", {
  # Two places encode "which shapes have a reference": the predicate that
  # scenario_audit() asks up front, and the switch() inside
  # .reference_z_quantiles(). If they drift apart the audit aborts.
  for (nm in .auto_reference_shapes()) {
    dat <- sim_multisite(
      multisitedgp_design(
        J = 20L, sigma_tau = 0.2, nj_mean = 100, seed = 1L, true_dist = nm,
        theta_G = if (identical(nm, "StudentT")) list(nu = 5) else list()
      )
    )
    expect_no_error(bhattacharyya_coef(dat))
  }
})
