# StudentT(nu=2.1) 의 var(z_j)=0.438 이 MC 잡음인가 실제 계약 위반인가
suppressMessages(pkgload::load_all(".", quiet = TRUE))
N <- 200000L

cat("StudentT 표준화 — 시드별 var(z_j) 분포\n")
cat(strrep("=", 70), "\n\n")
for (nu in c(2.1, 2.5, 3, 4, 5, 10)) {
  v <- vapply(1:12, function(s)
    var(withr::with_seed(s, suppressWarnings(
      gen_effects(J = N, sigma_tau = 1, tau = 0, true_dist = "StudentT",
                  theta_G = list(nu = nu))))$z_j), numeric(1))
  cat(sprintf("  nu=%-5.1f  median %6.3f  min %6.3f  max %8.3f  IQR %7.3f\n",
              nu, median(v), min(v), max(v), IQR(v)))
}
cat("\n해석 기준: 무한 첨도(nu <= 4)에서는 표본분산 추정량 자체의 분산이 무한하다.\n")
cat("median 이 1 근처이고 max 가 크게 튀면 MC 잡음, median 이 계통적으로\n")
cat("1 에서 벗어나면 표준화 상수 오류다.\n\n")

# 이론값 대조: t(nu) 의 분산은 nu/(nu-2). 표준화는 sqrt((nu-2)/nu) 를 곱해야 한다.
cat("── 이론 대조: 표준화 전 raw t draw 의 분산 ──\n")
for (nu in c(2.1, 3, 5)) {
  raw <- withr::with_seed(1L, rt(N, df = nu))
  cat(sprintf("  nu=%-5.1f  이론 var = nu/(nu-2) = %8.3f   표본 var = %8.3f\n",
              nu, nu / (nu - 2), var(raw)))
}
