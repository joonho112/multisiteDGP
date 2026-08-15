suppressMessages(pkgload::load_all(".", quiet = TRUE))
st <- function(n_bar, cv, n_min) {
  tryCatch({ suppressWarnings(solve_trunc_gamma(n_bar=n_bar, cv=cv, n_min=n_min)); "ok" },
           error = function(e) "abort")
}
cat("=== 중단(abort) 경계만: 비율별 최대 해결 가능 cv ===\n")
for (r in c(0.02,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8)) {
  n_bar <- 100; n_min <- max(1L, as.integer(round(r*n_bar)))
  cvs <- seq(0.1, 4.0, by=0.05)
  ok <- cvs[vapply(cvs, function(c) st(n_bar,c,n_min)=="ok", logical(1))]
  cat(sprintf("  n_min/n_bar=%.2f  해결 가능 cv 상한 = %s\n", r,
      if (length(ok)) sprintf("%.2f", max(ok)) else "없음 (cv>=0.1 전부 중단)"))
}
cat("\n=== n_bar 자체의 영향 (비율 0.2 고정) ===\n")
for (nb in c(10,25,50,100,250,500)) {
  n_min <- max(1L, as.integer(round(0.2*nb)))
  cvs <- seq(0.1,4.0,by=0.05); ok <- cvs[vapply(cvs,function(c) st(nb,c,n_min)=="ok",logical(1))]
  cat(sprintf("  n_bar=%-4g n_min=%-3d  상한 = %s\n", nb, n_min, if(length(ok)) sprintf("%.2f",max(ok)) else "없음"))
}
