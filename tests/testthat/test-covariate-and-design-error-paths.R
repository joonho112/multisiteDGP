# Layer 1 공변량 경로, 설계 클래스, 콜백 계약의 오류 분기.
#
# Step 5.6 이 다룬 나머지 절반이다. 여기도 미커버 행이 거의 전부 .abort_*() 이고,
# 특히 layer1-effects-common.R 의 공변량 경로는 커버리지 79.7 % 로 패키지에서
# 가장 낮았다. 공변량 설계는 사용자가 formula / beta / data 세 가지를 서로 맞춰
# 넘겨야 하므로 어긋날 방법이 많고, 그만큼 오류 메시지가 실제로 읽힌다.

school <- function(n = 20L) {
  data.frame(prior = as.numeric(scale(seq_len(n))),
             urban = rep(c(0, 1), length.out = n))
}

# ── formula / beta / data 삼자 정합성 ─────────────────────────────────

test_that("beta or data without a formula is refused", {
  expect_multisitedgp_error(
    gen_effects(J = 20L, sigma_tau = 0.2, beta = 0.3),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    gen_effects(J = 20L, sigma_tau = 0.2, data = school()),
    "multisitedgp_arg_error"
  )
})

test_that("formula must actually be a formula", {
  expect_multisitedgp_error(
    gen_effects(J = 20L, sigma_tau = 0.2, formula = "~ prior",
                beta = 0.3, data = school()),
    "multisitedgp_arg_error"
  )
})

test_that("a formula naming absent variables is refused with the modelling error", {
  expect_multisitedgp_error(
    gen_effects(J = 20L, sigma_tau = 0.2, formula = ~ no_such_column,
                beta = 0.3, data = school()),
    "multisitedgp_arg_error"
  )
})

test_that("beta must be finite numeric", {
  for (bad in list("0.3", c(0.3, NA), c(0.3, Inf), list(0.3))) {
    expect_multisitedgp_error(
      gen_effects(J = 20L, sigma_tau = 0.2, formula = ~ prior,
                  beta = bad, data = school()),
      "multisitedgp_arg_error"
    )
  }
})

test_that("a non-finite linear predictor is caught before it reaches the draw", {
  d <- school()
  d$prior[3L] <- Inf
  expect_multisitedgp_error(
    gen_effects(J = 20L, sigma_tau = 0.2, formula = ~ prior, beta = 0.3, data = d),
    "multisitedgp_arg_error"
  )
})

# ── beta 이름 해석 ────────────────────────────────────────────────────

test_that("named beta must name model-matrix columns", {
  expect_multisitedgp_error(
    gen_effects(J = 20L, sigma_tau = 0.2, formula = ~ prior,
                beta = c(nope = 0.3), data = school()),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    gen_effects(J = 20L, sigma_tau = 0.2, formula = ~ prior,
                beta = c(prior = 0.3, prior = 0.4), data = school()),
    "multisitedgp_arg_error"
  )
})

test_that("named beta must supply every non-intercept coefficient", {
  expect_multisitedgp_error(
    gen_effects(J = 20L, sigma_tau = 0.2, formula = ~ prior + urban,
                beta = c(prior = 0.3), data = school()),
    "multisitedgp_arg_error"
  )
})

test_that("a named beta is reordered into model-matrix order", {
  draw <- function(beta) {
    withr::with_seed(1L, gen_effects(J = 20L, sigma_tau = 0.2,
                                     formula = ~ prior + urban,
                                     beta = beta, data = school()))
  }
  expect_equal(draw(c(urban = 0.5, prior = 0.3))$tau_j,
               draw(c(0, 0.3, 0.5))$tau_j)
})

test_that("a named beta may omit the intercept, like an unnamed one", {
  # .normalize_beta_names() emits "(Intercept)" but .resolve_named_beta() compared
  # against "(intercept)", so the intercept column was always counted missing and
  # this call was refused — with a message calling the intercept a *non-intercept*
  # coefficient. D-035.
  draw <- function(beta) {
    withr::with_seed(1L, gen_effects(J = 20L, sigma_tau = 0.2, formula = ~ prior,
                                     beta = beta, data = school()))
  }
  expect_equal(draw(c(prior = 0.3))$tau_j, draw(0.3)$tau_j)
  expect_equal(draw(c(prior = 0.3))$tau_j, draw(c(0, 0.3))$tau_j)
})

test_that("an unnamed beta is accepted with or without the intercept slot", {
  draw <- function(beta) {
    withr::with_seed(1L, gen_effects(J = 20L, sigma_tau = 0.2, formula = ~ prior,
                                     beta = beta, data = school()))
  }
  expect_equal(draw(c(0, 0.3))$tau_j, draw(0.3)$tau_j)
})

# ── 설계 클래스 검증 ──────────────────────────────────────────────────

test_that("theta_G must be a named list", {
  expect_multisitedgp_error(
    multisitedgp_design(J = 10L, sigma_tau = 0.2, nj_mean = 100, theta_G = "nu = 5"),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    multisitedgp_design(J = 10L, sigma_tau = 0.2, nj_mean = 100,
                        true_dist = "StudentT", theta_G = list(5)),
    "multisitedgp_arg_error"
  )
})

test_that("callback argument bundles must be lists", {
  expect_multisitedgp_error(
    multisitedgp_design(J = 10L, sigma_tau = 0.2, nj_mean = 100,
                        se_args = "not a list"),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    gen_effects_user(J = 10L, g_fn = function(J) stats::rnorm(J),
                     g_args = "not a list"),
    "multisitedgp_arg_error"
  )
})

test_that("direct-paradigm ranges are enforced", {
  direct <- function(...) {
    multisitedgp_design(paradigm = "direct", J = 10L, sigma_tau = 0.2, ...)
  }
  for (bad in list(list(I = 0), list(I = 1), list(I = 1.5))) {
    expect_multisitedgp_error(do.call(direct, c(bad, list(R = 2))),
                              "multisitedgp_arg_error")
  }
  expect_multisitedgp_error(direct(I = 0.5, R = 0.5), "multisitedgp_arg_error")
  expect_no_error(direct(I = 0.5, R = 2))
})

test_that("a non-positive dependence tolerance is refused", {
  expect_multisitedgp_error(
    multisitedgp_design(J = 10L, sigma_tau = 0.2, nj_mean = 100,
                        dependence = "rank", rank_corr = 0.2, tol = 0),
    "multisitedgp_arg_error"
  )
})

test_that("direct-only arguments are refused on a site-size design", {
  expect_multisitedgp_error(
    multisitedgp_design(J = 10L, sigma_tau = 0.2, nj_mean = 100, R = 2),
    "multisitedgp_coherence_error"
  )
  # shuffle defaults to TRUE, so the guard fires when it differs from the
  # default — the same rule R uses (default 1).
  expect_multisitedgp_error(
    multisitedgp_design(J = 10L, sigma_tau = 0.2, nj_mean = 100, shuffle = FALSE),
    "multisitedgp_coherence_error"
  )
})

test_that("a design with malformed nested specs fails validation", {
  design <- multisitedgp_design(J = 10L, sigma_tau = 0.2, nj_mean = 100)
  design$dependence_spec <- "not a list"
  expect_multisitedgp_error(validate_multisitedgp_design(design),
                            "multisitedgp_arg_error")
})

test_that("update_multisitedgp_design with no updates round-trips the design", {
  design <- multisitedgp_design(J = 10L, sigma_tau = 0.2, nj_mean = 100, seed = 1L)
  expect_equal(canonical_hash(update_multisitedgp_design(design)),
               canonical_hash(design))
})

test_that("the print method renders formulas, vectors and callbacks", {
  design <- multisitedgp_design(
    J = 10L, sigma_tau = 0.2, nj_mean = 100, seed = 1L,
    formula = ~ prior, beta = 0.3, data = school(10L),
    obs_fn = function(tau_j, se2_j) tau_j
  )
  out <- paste(capture.output(print(design)), collapse = "\n")

  expect_match(out, "prior", fixed = TRUE)      # formula deparsed
  expect_match(out, "<function>", fixed = TRUE) # callback by presence
})

# ── 콜백 계약 ─────────────────────────────────────────────────────────

test_that("a g_fn that raises is reported with the callback's own message", {
  err <- tryCatch(
    gen_effects(J = 10L, sigma_tau = 0.2,
                g_fn = function(J) stop("callback exploded")),
    error = conditionMessage
  )
  expect_match(err, "callback exploded", fixed = TRUE)
})

test_that("dependence_fn must return a usable se2_j or perm", {
  # dependence_fn 은 align_*_corr() 이 위임하는 훅이다. 계약을 어긴 반환은
  # 주변 분포 위반으로 거부되어야 한다 — permutation 이 아닌 se2_j 를 돌려주면
  # Layer 2 가 만든 주변 분포가 조용히 바뀐다.
  upstream <- gen_site_sizes(gen_effects(J = 10L, sigma_tau = 0.2),
                             J = 10L, nj_mean = 60, cv = 0.3)
  # 훅은 z_j / se2_j / target 으로 호출된다 (upstream 프레임이 아니다).
  align <- function(fn) {
    align_rank_corr(upstream, rank_corr = 0.3, dependence_fn = fn)
  }

  # 계약은 se2_j 와 perm 을 둘 다 요구한다 — 하나만 오면 거부된다.
  expect_multisitedgp_error(
    align(function(z_j, se2_j, target, ...) list(se2_j = rep(NA_real_, length(se2_j)))),
    "multisitedgp_marginal_violation_error"
  )
  expect_multisitedgp_error(
    align(function(z_j, se2_j, target, ...) list(perm = rep(1.5, length(se2_j)))),
    "multisitedgp_marginal_violation_error"
  )
  expect_multisitedgp_error(
    align(function(z_j, se2_j, target, ...) {
      p <- rev(seq_along(se2_j))
      list(se2_j = se2_j[p], perm = c(p[-1], NA_integer_))
    }),
    "multisitedgp_marginal_violation_error"
  )

  out <- align(function(z_j, se2_j, target, ...) {
    p <- rev(seq_along(se2_j))
    list(se2_j = se2_j[p], perm = p)
  })
  # 훅이 순열만 돌려주므로 주변 분포는 보존되어야 한다.
  expect_setequal(out$se2_j, upstream$se2_j)
})

# ── shape 별 필수 theta_G ─────────────────────────────────────────────

test_that("ALD and SkewN name the parameter they are missing", {
  expect_multisitedgp_error(gen_effects_ald(J = 10L), "multisitedgp_arg_error")
  expect_multisitedgp_error(gen_effects_skewn(J = 10L), "multisitedgp_arg_error")

  expect_match(
    tryCatch(gen_effects_ald(J = 10L), error = conditionMessage),
    "rho", fixed = TRUE
  )
  expect_match(
    tryCatch(gen_effects_skewn(J = 10L), error = conditionMessage),
    "slant", fixed = TRUE
  )
})
