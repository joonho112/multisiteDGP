suppressMessages(pkgload::load_all(".", quiet = TRUE))
set.seed(20260815)
for (nu in c(2.5, 3, 4, 5, 10, 30)) {
  v <- replicate(400, {
    d <- suppressWarnings(gen_effects_studentt(J = 200L, sigma_tau = 1, nu = nu))
    stats::var(d$z_j)
  })
  cat(sprintf("nu=%-5g  median var(z_j)=%.3f  IQR=[%.3f, %.3f]  p95=%.2f  max=%.1f\n",
      nu, median(v), quantile(v,.25), quantile(v,.75), quantile(v,.95), max(v)))
}
