# scenario_audit() 의 검증 분기와 WARN 게이트.
#
# Step 2.2 가 이 파일을 커버리지 하위로 지목했고, 미커버 행 39 개 중 대부분이
# .abort_arg() 분기였다. 오류 메시지는 실행되지 않으면 틀렸는지 알 수 없다 —
# Phase 7 이 찾은 결함 두 건(.OFF_CRAN_SOURCES 조회 실패, fix-line 규약 위반)이
# 모두 한 번도 실행된 적 없는 오류 분기에 있었다.
#
# WARN 게이트 5 개는 스위트 전체에서 한 번도 발화한 적이 없었다. FAIL 게이트만
# 검증되고 있었으므로 "중앙값은 통과하지만 꼬리가 걸린다" 는 경로 전체가
# 미검증이었다.

grid_ok <- function() {
  design_grid(J = 25L, sigma_tau = 0.15, nj_mean = 60, seed_root = 777L)
}

# ── grid 검증 ─────────────────────────────────────────────────────────

test_that("scenario_audit rejects a grid that is not a design_grid", {
  expect_multisitedgp_error(
    scenario_audit(data.frame(J = 10L), M = 2L),
    "multisitedgp_arg_error"
  )
})

test_that("scenario_audit rejects a grid without a design list-column", {
  g <- grid_ok()
  g$design <- NULL
  expect_multisitedgp_error(scenario_audit(g, M = 2L), "multisitedgp_arg_error")
})

test_that("scenario_audit rejects an empty grid", {
  g <- grid_ok()
  empty <- g[0L, , drop = FALSE]
  class(empty) <- class(g)
  expect_multisitedgp_error(scenario_audit(empty, M = 2L), "multisitedgp_arg_error")
})

test_that("scenario_audit rejects a grid whose design column is not designs", {
  g <- grid_ok()
  g$design <- list("not a design")
  expect_multisitedgp_error(scenario_audit(g, M = 2L), "multisitedgp_arg_error")
})

test_that("scenario_audit requires a deterministic cell seed", {
  # The audit must not consume the caller's RNG, so a grid with neither a cell
  # seed nor a base-design seed is refused rather than silently seeded.
  g <- grid_ok()
  g$seed <- NA_integer_
  g$design <- list(multisitedgp_design(J = 25L, sigma_tau = 0.15, nj_mean = 60))
  expect_multisitedgp_error(scenario_audit(g, M = 2L), "multisitedgp_arg_error")
})

# ── thresholds 검증 ───────────────────────────────────────────────────

test_that("thresholds must be a named list", {
  g <- grid_ok()
  expect_multisitedgp_error(scenario_audit(g, M = 2L, thresholds = 1), "multisitedgp_arg_error")
  expect_multisitedgp_error(
    scenario_audit(g, M = 2L, thresholds = list(0.5)),
    "multisitedgp_arg_error"
  )
})

test_that("thresholds reject unknown gate names", {
  expect_multisitedgp_error(
    scenario_audit(grid_ok(), M = 2L, thresholds = list(no_such_gate = 1)),
    "multisitedgp_arg_error"
  )
})

test_that("each threshold enforces its own admissible range", {
  g <- grid_ok()
  bad <- list(
    R_max = 0,                    # positive
    feasibility_min = -1,         # non-negative
    mean_shrinkage_min = 2,       # unit interval
    bhattacharyya_min = -0.5,     # unit interval
    ks_max = 1.5                  # unit interval
  )
  for (nm in names(bad)) {
    expect_multisitedgp_error(
      scenario_audit(g, M = 2L, thresholds = bad[nm]),
      "multisitedgp_arg_error"
    )
  }
})

test_that("M is validated as a positive integer within integer range", {
  g <- grid_ok()
  expect_multisitedgp_error(scenario_audit(g, M = 0L), "multisitedgp_arg_error")
  expect_multisitedgp_error(scenario_audit(g, M = 2.5), "multisitedgp_arg_error")
  expect_multisitedgp_error(scenario_audit(g, M = 2^40), "multisitedgp_arg_error")
})

# ── WARN 게이트 ───────────────────────────────────────────────────────

test_that("the tail gates fire on their own, without the median gates", {
  # A gate fires at FAIL on the median and at WARN on the tail quantile. Only the
  # FAIL side had ever run. Thresholds are derived from the cell's own measured
  # quantiles rather than hard-coded, so this stays true if the RNG stream moves.
  g <- grid_ok()
  baseline <- suppressWarnings(scenario_audit(g, M = 30L))

  mid <- function(med, tail) (med + tail) / 2
  thresholds <- list(
    mean_shrinkage_min = mid(baseline$med_mean_shrinkage, baseline$q05_mean_shrinkage),
    feasibility_min    = mid(baseline$med_feasibility_efron, baseline$q05_feasibility_efron),
    R_max              = mid(baseline$med_R_hat, baseline$q95_R_hat),
    bhattacharyya_min  = mid(baseline$med_bhattacharyya, baseline$q05_bhattacharyya),
    ks_max             = mid(baseline$med_ks, baseline$q95_ks)
  )

  out <- suppressWarnings(scenario_audit(g, M = 30L, thresholds = thresholds))

  expect_identical(out$status, "WARN")
  expect_false(out$pass)
  expect_gt(out$n_warnings, 0L)
  expect_identical(out$fail_reasons, "")
  for (gate in c("mean_shrinkage", "feasibility", "R", "bhattacharyya", "ks")) {
    expect_match(out$warn_reasons, gate, fixed = TRUE)
  }
})

test_that("a cell with no violations reports PASS and empty reason strings", {
  wide <- list(
    mean_shrinkage_min = 0, feasibility_min = 0, R_max = 1e6,
    bhattacharyya_min = 0, ks_max = 1
  )
  out <- suppressWarnings(scenario_audit(grid_ok(), M = 5L, thresholds = wide))

  expect_identical(out$status, "PASS")
  expect_true(out$pass)
  expect_true(out$audit_complete)
  expect_identical(out$groups_evaluated, "A,B,C,D")
  expect_identical(out$threshold_profile, "replicate-grid-audit-v1")
  expect_identical(out$n_violations, 0L)
  expect_identical(out$fail_reasons, "")
  expect_identical(out$warn_reasons, "")
})

test_that("a FAIL suppresses the warn reasons rather than reporting both", {
  strict <- list(mean_shrinkage_min = 1, feasibility_min = 1e6)
  out <- suppressWarnings(scenario_audit(grid_ok(), M = 5L, thresholds = strict))

  expect_identical(out$status, "FAIL")
  expect_false(out$pass)
  expect_gt(out$n_violations, 0L)
  expect_identical(out$warn_reasons, "")
})

# ── 병렬 경로 ─────────────────────────────────────────────────────────

test_that("the parallel path returns what the sequential path returns", {
  skip_if_not_installed("furrr")
  g <- design_grid(J = 25L, sigma_tau = c(0.15, 0.20), nj_mean = 60,
                   seed_root = 777L)

  sequential <- suppressWarnings(scenario_audit(g, M = 5L, parallel = FALSE))
  concurrent <- suppressWarnings(scenario_audit(g, M = 5L, parallel = TRUE))

  # Same cell seeds, so the two must agree exactly — parallelism must not change
  # which replicates a cell draws.
  expect_equal(concurrent, sequential)
})

# ── 정수 범위 (D-032) ─────────────────────────────────────────────────

test_that("integer arguments past 2^31 abort instead of coercing to NA", {
  # as.integer() returns NA beyond the 32-bit range, and the NA used to surface
  # much later as a bare "missing value where TRUE/FALSE needed" — a simpleError,
  # outside the typed hierarchy every other invalid argument produces.
  expect_multisitedgp_error(
    multisitedgp_design(J = 2^35, sigma_tau = 0.2, nj_mean = 100, seed = 1L),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    multisitedgp_design(J = 10L, sigma_tau = 0.2, nj_mean = 100, seed = 2^40),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    solve_trunc_gamma(n_bar = 100, cv = 0.5, n_min = 2^40),
    "multisitedgp_arg_error"
  )

  # The boundary itself is still admissible.
  expect_no_error(
    multisitedgp_design(J = 10L, sigma_tau = 0.2, nj_mean = 100,
                        seed = .Machine$integer.max)
  )
})
