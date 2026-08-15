# 진단 함수와 Layer 2 direct-SE 콜백의 오류 경로.
#
# Step 2.2 가 layer2-diagnostics.R (25 행) 과 layer2-gen_se_direct.R (21 행) 을
# 커버리지 하위로 지목했고, 미커버 행이 전부 .abort_arg() 분기였다. 이 분기들은
# 사용자가 실제로 마주치는 계약 위반 — 잘못된 S3 dispatch, 콜백이 계약을 어긴
# 반환값 — 을 설명하는 자리인데, 한 번도 실행되지 않았으므로 메시지가 옳은지
# 확인된 적이 없다.

dat_ok <- function(...) {
  sim_multisite(multisitedgp_design(J = 20L, sigma_tau = 0.2, nj_mean = 100,
                                    seed = 1L, ...))
}

# ── compute_I 의 선택 인자 계약 ───────────────────────────────────────

test_that("compute_I refuses a tau_j that does not match se2_j", {
  # min_length 검증은 짧은 쪽만 잡는다. 긴 쪽은 이 분기가 잡는다.
  expect_multisitedgp_error(
    compute_I(se2_j = c(0.01, 0.02), sigma_tau = 0.2, tau_j = c(1, 2, 3)),
    "multisitedgp_arg_error"
  )
  expect_no_error(
    compute_I(se2_j = c(0.01, 0.02), sigma_tau = 0.2, tau_j = c(1, 2))
  )
})

# ── S3 dispatch 의 default 분기 ───────────────────────────────────────

test_that("informativeness rejects an object it has no method for", {
  for (x in list(list(a = 1), TRUE, as.Date("2026-08-15"))) {
    expect_multisitedgp_error(informativeness(x), "multisitedgp_arg_error")
  }
})

test_that("mean_shrinkage rejects an object it has no method for", {
  expect_multisitedgp_error(mean_shrinkage(list(a = 1)), "multisitedgp_arg_error")
  expect_multisitedgp_error(mean_shrinkage(as.Date("2026-08-15")), "multisitedgp_arg_error")
})

test_that("mean_shrinkage on data needs the design's sigma_tau", {
  dat <- dat_ok()
  attr(dat, "design") <- NULL
  expect_multisitedgp_error(mean_shrinkage(dat), "multisitedgp_arg_error")

  stripped <- dat_ok()
  design <- attr(stripped, "design")
  design$sigma_tau <- NULL
  attr(stripped, "design") <- design
  expect_multisitedgp_error(mean_shrinkage(stripped), "multisitedgp_arg_error")
})

test_that("mean_shrinkage's numeric method needs sigma_tau", {
  expect_multisitedgp_error(mean_shrinkage(c(0.01, 0.02)), "multisitedgp_arg_error")
  expect_no_error(mean_shrinkage(c(0.01, 0.02), sigma_tau = 0.2))
})

test_that("the closed-form path needs both nj_mean and sigma_tau", {
  expect_multisitedgp_error(mean_shrinkage(nj_mean = 50), "multisitedgp_arg_error")
  expect_multisitedgp_error(mean_shrinkage(sigma_tau = 0.2), "multisitedgp_arg_error")
  expect_no_error(mean_shrinkage(nj_mean = 50, sigma_tau = 0.2))
})

# ── se_fn 콜백 계약 ───────────────────────────────────────────────────

se_direct <- function(se_fn, se_args = list(), J = 10L) {
  upstream <- gen_effects(J = J, sigma_tau = 0.2)
  gen_se_direct(upstream, J = J, I = 0.5, R = 2, se_fn = se_fn, se_args = se_args)
}

test_that("se_fn must return something carrying se2_j", {
  bad_returns <- list(
    function(J) numeric(J),                    # bare vector, not a named list
    function(J) list(numeric(J)),              # list without names
    function(J) list(variance = numeric(J))    # named, but not se2_j
  )
  for (fn in bad_returns) {
    expect_multisitedgp_error(se_direct(fn), "multisitedgp_arg_error")
  }
})

test_that("a data frame return is accepted and read column-wise", {
  # The adapter converts a data.frame to a list before the contract check, so a
  # tibble with an se2_j column is a legitimate se_fn return.
  out <- se_direct(function(J) data.frame(se2_j = rep(0.04, J)))
  expect_true(is_multisitedgp_data(out) || is.data.frame(out))
  expect_equal(unique(out$se2_j), 0.04)
})

test_that("se_fn must return exactly one se2_j per site", {
  expect_multisitedgp_error(
    se_direct(function(J) list(se2_j = rep(0.04, J + 3L))),
    "multisitedgp_arg_error"
  )
})

test_that("se_fn's n_j must be integer-like and one per site, or NULL", {
  ok <- se_direct(function(J) list(se2_j = rep(0.04, J), n_j = NULL))
  expect_true(all(is.na(ok$n_j)))

  bad <- list(
    function(J) list(se2_j = rep(0.04, J), n_j = rep(10.5, J)),   # not integer-like
    function(J) list(se2_j = rep(0.04, J), n_j = rep(10L, J - 1L)),
    function(J) list(se2_j = rep(0.04, J), n_j = rep(NA_real_, J))
  )
  for (fn in bad) {
    expect_multisitedgp_error(se_direct(fn), "multisitedgp_arg_error")
  }
})

test_that("se_args must be named and must not smuggle in J", {
  fn <- function(J, scale = 1) list(se2_j = rep(0.04 * scale, J))

  expect_multisitedgp_error(se_direct(fn, se_args = list(0.5)), "multisitedgp_arg_error")
  expect_multisitedgp_error(se_direct(fn, se_args = list(J = 99L)), "multisitedgp_arg_error")
  expect_no_error(se_direct(fn, se_args = list(scale = 0.5)))
})
