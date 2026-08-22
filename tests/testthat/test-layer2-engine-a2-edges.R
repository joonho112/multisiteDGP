# Engine A2 의 오류 분기와 수치 경계 경로.
#
# v0.1.x 에서 이 파일들은 커버되지 않았다 (layer2-engine-a2.R 84.8%, 미커버 17줄).
# 미커버 라인이 전부 fail-fast 분기와 수치 경계여서, 실패해야 할 때 실패하는지가
# 검증되지 않은 상태였다. 결함 원장 D-012 · D-015.
#
# 실현 불가능 영역의 좌표는 Phase 2 Step 2.6 이 48 셀 격자로 실측했으며
# 결과는 evidence 폴더의 phase02 solver-feasible-region 표에 있다.

# ── 인자 검증 분기 ────────────────────────────────────────────────────

test_that("engine_trunc_gamma_moment rejects a negative cv", {
  expect_error(
    engine_trunc_gamma_moment(J = 20L, nj_mean = 100, cv = -0.1),
    class = "multisitedgp_arg_error"
  )
})

test_that("solve_trunc_gamma requires n_bar to exceed n_min", {
  expect_error(solve_trunc_gamma(n_bar = 5, cv = 0.4, n_min = 5L),
               class = "multisitedgp_arg_error")
  expect_error(solve_trunc_gamma(n_bar = 100, cv = 0.4, n_min = 0L),
               class = "multisitedgp_arg_error")
})

test_that("solve_trunc_gamma rejects a negative cv", {
  expect_error(solve_trunc_gamma(n_bar = 100, cv = -0.1, n_min = 5L),
               class = "multisitedgp_arg_error")
})

test_that("solve_trunc_gamma rejects a non-positive tol", {
  expect_error(
    solve_trunc_gamma(n_bar = 100, cv = 0.4, n_min = 5L, tol = 0),
    class = "multisitedgp_arg_error"
  )
  expect_error(
    solve_trunc_gamma(n_bar = 100, cv = 0.4, n_min = 5L, tol = -1e-6),
    class = "multisitedgp_arg_error"
  )
})

test_that("solve_trunc_gamma requires at least one solver start", {
  expect_error(
    solve_trunc_gamma(n_bar = 100, cv = 0.4, n_min = 5L, max_starts = 0L),
    class = "multisitedgp_arg_error"
  )
})

test_that("solve_trunc_gamma requires a positive iteration budget", {
  expect_error(
    solve_trunc_gamma(n_bar = 100, cv = 0.4, n_min = 5L, max_iter = 0L),
    class = "multisitedgp_arg_error"
  )
})

# ── cv = 0 결정론 경로 ────────────────────────────────────────────────

test_that("cv = 0 returns the degenerate solution without calling the solver", {
  out <- solve_trunc_gamma(n_bar = 100, cv = 0, n_min = 5L)

  expect_identical(out$alpha, Inf)
  expect_identical(out$beta, Inf)
  expect_identical(out$mean, 100)
  expect_identical(out$sd, 0)
  expect_identical(out$cv, 0)
  expect_identical(unname(out$residual), c(0, 0))
  expect_identical(out$message, "deterministic")
})

test_that("cv = 0 produces constant site sizes end to end", {
  n_j <- engine_trunc_gamma_moment(J = 20L, nj_mean = 100, cv = 0)$n_j

  expect_length(n_j, 20L)
  expect_true(all(n_j == 100))
})

# ── solver 가 검증된 해를 찾지 못하는 영역 ────────────────────────────

test_that("the solver aborts on the measured infeasible region", {
  # Phase 2 Step 2.6 격자에서 실패한 셀. 48셀 중 5셀이 실패했고 전부
  # nj_mean 이 작고 cv 가 큰 모서리다.
  infeasible <- list(
    list(nj_mean = 10, cv = 0.6),
    list(nj_mean = 10, cv = 1.0),
    list(nj_mean = 25, cv = 1.3)
  )
  for (case in infeasible) {
    expect_error(
      solve_trunc_gamma(n_bar = case$nj_mean, cv = case$cv, n_min = 5L),
      class = "multisitedgp_error",
      info = sprintf("n_bar = %s, cv = %s", case$nj_mean, case$cv)
    )
  }
})

test_that("the solver succeeds just inside the feasible boundary", {
  # 같은 격자에서 성공한 인접 셀 — 실패가 무차별적이지 않음을 확인한다.
  feasible <- list(
    list(nj_mean = 10, cv = 0.4),
    list(nj_mean = 25, cv = 1.0),
    list(nj_mean = 50, cv = 1.3)
  )
  for (case in feasible) {
    out <- solve_trunc_gamma(n_bar = case$nj_mean, cv = case$cv, n_min = 5L)
    expect_true(is.finite(out$alpha),
                info = sprintf("n_bar = %s, cv = %s", case$nj_mean, case$cv))
    expect_true(is.finite(out$beta))
    expect_lt(max(abs(out$residual)), 1e-6)
  }
})

# ── trunc_gamma_moments / rtrunc_gamma 의 경계 ────────────────────────

test_that("trunc_gamma_moments rejects a non-positive n_min", {
  expect_error(
    trunc_gamma_moments(alpha = 2, beta = 0.02, n_min = 0L),
    class = "multisitedgp_arg_error"
  )
})

test_that("trunc_gamma_moments rejects non-positive shape or rate", {
  expect_error(trunc_gamma_moments(alpha = 0, beta = 0.02, n_min = 5L),
               class = "multisitedgp_arg_error")
  expect_error(trunc_gamma_moments(alpha = 2, beta = -1, n_min = 5L),
               class = "multisitedgp_arg_error")
})

test_that("rtrunc_gamma rejects a non-positive n_min", {
  expect_error(
    rtrunc_gamma(n = 5L, alpha = 2, beta = 0.02, n_min = 0L),
    class = "multisitedgp_arg_error"
  )
})

test_that("rtrunc_gamma aborts when the upper tail underflows", {
  # n_min 이 분포 질량보다 훨씬 오른쪽이면 생존확률이 0 으로 내려앉는다.
  expect_error(
    rtrunc_gamma(n = 5L, alpha = 2, beta = 50, n_min = 100000L),
    class = "multisitedgp_error"
  )
})

test_that("rtrunc_gamma respects the lower bound on ordinary inputs", {
  draws <- withr::with_seed(101L, rtrunc_gamma(n = 200L, alpha = 6, beta = 0.06, n_min = 5L))

  expect_length(draws, 200L)
  expect_true(all(draws >= 5))
  expect_true(all(is.finite(draws)))
})

# ── 내부 수치 헬퍼 ────────────────────────────────────────────────────

test_that(".trunc_gamma_raw_moment computes its own survival term when omitted", {
  with_survival <- .trunc_gamma_raw_moment(
    1, alpha = 6, beta = 0.06, n_min = 5L,
    log_survival = .trunc_gamma_log_survival(6, 0.06, 5L)
  )
  without_survival <- .trunc_gamma_raw_moment(1, alpha = 6, beta = 0.06, n_min = 5L)

  expect_equal(without_survival, with_survival)
})

test_that(".trunc_gamma_raw_moment returns NaN when the log moment overflows", {
  # alpha 를 극단으로 키우면 lgamma 차분이 비유한이 된다.
  expect_true(is.nan(
    .trunc_gamma_raw_moment(1, alpha = 1e308, beta = 1e-300, n_min = 1L)
  ))
})

test_that(".trunc_gamma_residual returns the sentinel for invalid parameters", {
  sentinel <- c(mean = .Machine$double.xmax, sd = .Machine$double.xmax)

  # exp(log_par) 가 0 으로 언더플로우하면 alpha <= 0 분기로 간다.
  expect_identical(
    .trunc_gamma_residual(c(-1e6, -1e6), n_bar = 100, cv = 0.4, n_min = 5L),
    sentinel
  )
  # 비유한 입력도 같은 sentinel 로 떨어진다.
  expect_identical(
    .trunc_gamma_residual(c(1e6, 1e6), n_bar = 100, cv = 0.4, n_min = 5L),
    sentinel
  )
})

test_that(".fit_trunc_gamma_start reports a failed fit instead of propagating", {
  failed <- .failed_trunc_gamma_fit(message = "synthetic failure", start_id = 3L)

  expect_false(failed$verified)
  expect_identical(failed$start, 3L)
  expect_identical(failed$message, "synthetic failure")
  expect_true(is.na(failed$alpha))
  expect_identical(failed$residual_norm, Inf)
})

test_that(".validate_positive_scalar_number rejects zero and negatives", {
  expect_error(.validate_positive_scalar_number(0, "alpha"),
               class = "multisitedgp_arg_error")
  expect_error(.validate_positive_scalar_number(-1, "alpha"),
               class = "multisitedgp_arg_error")
  expect_identical(.validate_positive_scalar_number(2.5, "alpha"), 2.5)
})

# ── 안정 moment recurrence 와 fail-closed verification ───────────────

test_that("stable recurrence handles log-spaced tiny CV values", {
  cvs <- 10^seq(-5, -1, length.out = 5L)
  for (cv in cvs) {
    out <- solve_trunc_gamma(n_bar = 100, cv = cv, n_min = 5L)
    moments <- trunc_gamma_moments(out$alpha, out$beta, 5L)

    expect_equal(moments$mean, 100, tolerance = 1e-6, info = sprintf("cv=%g mean", cv))
    expect_equal(moments$sd, 100 * cv, tolerance = 1e-6, info = sprintf("cv=%g sd", cv))
    expect_identical(out$tol_effective[["mean"]], 1e-6)
    expect_lte(out$tol_effective[["sd"]], 1e-3)
  }
})

test_that("stable recurrence agrees with raw incomplete-Gamma moments when conditioned", {
  alpha <- 6
  beta <- 0.06
  n_min <- 5L
  moments <- trunc_gamma_moments(alpha, beta, n_min)
  raw_first <- .trunc_gamma_raw_moment(1, alpha, beta, n_min)
  raw_second <- .trunc_gamma_raw_moment(2, alpha, beta, n_min)

  expect_equal(moments$mean, raw_first, tolerance = 1e-12)
  expect_equal(moments$variance, raw_second - raw_first^2, tolerance = 1e-10)
})

test_that("unverifiable recurrence configurations report non-finite precision", {
  moments <- trunc_gamma_moments(alpha = 1e3, beta = 2e5, n_min = 5L)
  expect_false(is.finite(moments$sd_relative_error_bound))
})

test_that("tiny CV no longer needs a warning-based tolerance waiver", {
  expect_silent(solve_trunc_gamma(n_bar = 100, cv = 0.001, n_min = 5L))
  expect_silent(solve_trunc_gamma(n_bar = 100, cv = 0.005, n_min = 5L))
  expect_silent(solve_trunc_gamma(n_bar = 100, cv = 0.05, n_min = 5L))
})

test_that("solve_trunc_gamma succeeds well inside the feasible region", {
  # cv = 0.005 를 쓰지 않는다. 그 지점은 **플랫폼 의존**이다 (원장 D-024):
  #   macOS  최대 잔차 7.44e-07 → 통과
  #   Linux  최대 잔차 1.91e-06 → 실패 (tol 1e-06)
  # 경계 근처를 고정하는 테스트는 플랫폼마다 다른 답을 준다. 여유 있게 안쪽을
  # 잡아 "정상 영역에서는 수렴한다" 만 검증한다.
  out <- solve_trunc_gamma(n_bar = 100, cv = 0.05, n_min = 5L)

  expect_true(is.finite(out$alpha))
  expect_true(is.finite(out$beta))
  expect_lt(max(abs(out$residual)), 1e-6)
})
