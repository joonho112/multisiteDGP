# Step 4.1b — trunc_gamma_moments() 잔차 평가의 달성 가능 정밀도를 측정한다.
#
# 목적: post-solve tolerance 를 손으로 고르지 않고, 잔차를 실제로 평가할 수
# 있는 정밀도(노이즈 바닥)에서 유도하기 위한 근거를 만든다. 결정 D7.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
out_dir <- commandArgs(trailingOnly = TRUE)[1]
EPS <- .Machine$double.eps

cat("Step 4.1b — 잔차 평가 노이즈 바닥\n")
cat(R.version.string, "|", R.version$platform, "\n")
cat(strrep("=", 84), "\n\n")

## ── 1. 해석적 예측 ────────────────────────────────────────────────────
#
# residual$sd = moments$sd / (cv * n_bar) - 1 이고
# moments$sd = sqrt(E[X^2] - E[X]^2) 이다.
#
# (a) 원시 모멘트는 lgamma(alpha + k) - lgamma(alpha) 의 차분으로 계산된다.
#     alpha 가 크면 두 거대한 값을 빼서 작은 값을 얻으므로 상쇄가 일어난다.
#     log_moment 의 절대오차 ~ EPS * |lgamma(alpha)|
#     exp() 를 취하면 그것이 그대로 상대오차가 된다:  d ~ EPS * lgamma(alpha)
#
# (b) sd^2 = E[X^2] - E[X]^2 에서 또 한 번 상쇄가 일어난다.
#     E[X^2] ~ mean^2 이고 sd^2 = cv^2 * mean^2 이므로
#     sd^2 의 상대오차 ~ d * mean^2 / sd^2 = d / cv^2
#     sd 의 상대오차 ~ d / (2 cv^2)
#
# 따라서 잔차의 sd 성분이 도달할 수 있는 절대 정밀도는
#
#     noise_floor ~ EPS * lgamma(alpha) / (2 * cv^2)
#
# Gamma 분포에서 cv = 1/sqrt(alpha) 이므로 alpha ~ 1/cv^2 이고,
# cv 가 작아질수록 두 요인이 함께 나빠진다.

predict_floor <- function(alpha, cv) EPS * lgamma(alpha) / (2 * cv^2)

## ── 2. 경험적 측정 ────────────────────────────────────────────────────
# 해를 ULP 수준으로 흔들었을 때 잔차가 얼마나 움직이는지 본다. 그 산포가
# "이 지점에서 잔차를 평가할 수 있는 정밀도" 다.
empirical_floor <- function(n_bar, cv, n_min, reps = 24L) {
  fit <- tryCatch(
    solve_trunc_gamma(n_bar = n_bar, cv = cv, n_min = n_min,
                      tol = 1e6, max_starts = 5L, max_iter = 20000L),
    error = function(e) NULL
  )
  if (is.null(fit) || !is.finite(fit$alpha)) return(c(alpha = NA, emp = NA, pred = NA))

  la <- log(fit$alpha); lb <- log(fit$beta)
  vals <- vapply(seq_len(reps), function(i) {
    # 해를 log 스케일에서 i ULP 만큼 흔든다
    r <- .trunc_gamma_residual(c(la * (1 + i * EPS), lb * (1 + i * EPS)),
                               n_bar = n_bar, cv = cv, n_min = n_min)
    max(abs(r))
  }, numeric(1))
  c(alpha = fit$alpha,
    emp = max(vals) - min(vals),
    pred = predict_floor(fit$alpha, cv))
}

grid <- expand.grid(
  n_bar = c(50, 100, 250),
  cv    = c(0.002, 0.005, 0.01, 0.05, 0.1, 0.25, 0.5, 0.8),
  n_min = 5L
)
res <- t(vapply(seq_len(nrow(grid)), function(i)
  empirical_floor(grid$n_bar[i], grid$cv[i], grid$n_min[i]), numeric(3)))
out <- cbind(grid, as.data.frame(res))
out$ratio <- out$emp / out$pred
out$fixed_tol_ok <- out$emp < 1e-6

cat("── 격자: 경험적 노이즈 바닥 vs 해석적 예측 ──\n")
cat(sprintf("%6s %7s %12s %12s %12s %8s %s\n",
            "n_bar", "cv", "alpha", "경험적", "예측", "비율", "고정tol(1e-6) 충분?"))
for (i in seq_len(nrow(out))) {
  cat(sprintf("%6g %7g %12.4g %12.3e %12.3e %8.2f %s\n",
              out$n_bar[i], out$cv[i], out$alpha[i], out$emp[i], out$pred[i],
              out$ratio[i], ifelse(is.na(out$fixed_tol_ok[i]), "?",
                                   ifelse(out$fixed_tol_ok[i], "예", "아니오"))))
}

ok <- !is.na(out$ratio) & is.finite(out$ratio) & out$ratio > 0
cat(sprintf("\n예측 대비 경험 비율: 중앙값 %.2f, 범위 [%.2f, %.2f] (n = %d)\n",
            median(out$ratio[ok]), min(out$ratio[ok]), max(out$ratio[ok]), sum(ok)))
cat(sprintf("고정 tol = 1e-6 이 노이즈 바닥보다 작은 셀: %d / %d\n",
            sum(!out$fixed_tol_ok, na.rm = TRUE), sum(!is.na(out$fixed_tol_ok))))

write.csv(out, file.path(out_dir, "residual-noise-floor.csv"), row.names = FALSE)
cat("\n완료:", format(Sys.time(), "%F %T"), "\n")
