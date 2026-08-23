# Group C coverage — scenario_audit() across the shape catalog.
#
# v0.1.x called bhattacharyya_coef() / ks_distance() unconditionally inside
# .audit_scenario_replicate(). Those resolve a reference quantile function from
# the design, and only Gaussian and StudentT have one, so auditing a SkewN,
# ALD, Mixture or PointMassSlab design aborted the whole run — four of the seven
# catalog shapes could not be audited at all, while vignette("m2-...") described
# a graceful fallback that did not exist. Defect ledger D-031.

audit_shape <- function(true_dist, theta = list(), n_reps = 5L) {
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

test_that("every catalog shape can be audited without aborting", {
  for (nm in names(shapes_without_reference)) {
    expect_no_error(audit_shape(nm, shapes_without_reference[[nm]]))
  }
  expect_no_error(audit_shape("Gaussian"))
  expect_no_error(audit_shape("StudentT", list(nu = 5)))
})

test_that("shapes with an automatic reference report target_source auto", {
  for (a in list(audit_shape("Gaussian"), audit_shape("StudentT", list(nu = 5)))) {
    expect_identical(a$target_source, "auto")
    expect_true(a$audit_complete)
    expect_identical(a$groups_evaluated, "A,B,C,D")
    expect_false(is.na(a$med_bhattacharyya))
    expect_false(is.na(a$med_ks))
  }
})

test_that("shapes without one report not_available and NA Group C columns", {
  for (nm in names(shapes_without_reference)) {
    a <- audit_shape(nm, shapes_without_reference[[nm]])
    expect_identical(a$target_source, "not_available")
    expect_false(a$audit_complete)
    expect_identical(a$groups_evaluated, "A,B,D")
    for (col in c("med_bhattacharyya", "q05_bhattacharyya", "med_ks", "q95_ks")) {
      expect_true(is.na(a[[col]]), info = sprintf("%s / %s", nm, col))
    }
  }
})

test_that("an unmeasurable Group C is skipped, not counted as a violation", {
  # .gate_low_fail() treats a non-finite value as a failure, which is right for a
  # metric that was measured and came back NaN. Letting NA reach it would turn
  # "no reference distribution" into FAIL.
  for (nm in names(shapes_without_reference)) {
    a <- audit_shape(nm, shapes_without_reference[[nm]])
    expect_false(grepl("bhattacharyya|ks", a$fail_reasons))
    expect_false(grepl("bhattacharyya|ks", a$warn_reasons))
  }
})

test_that("the auto-reference shape list matches the resolver", {
  # Two places encode "which shapes have a reference": the predicate that
  # scenario_audit() asks up front, and the switch() inside
  # .reference_z_quantiles(). If they drift apart the audit aborts again.
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
