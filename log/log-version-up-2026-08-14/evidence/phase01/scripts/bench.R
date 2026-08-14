suppressMessages(devtools::load_all(".", quiet = TRUE))

# fn 은 무인자 함수로 받는다. promise 를 쓰면 R 이 첫 평가 결과를 캐시해
# 두 번째 반복부터 0 초가 찍힌다.
bench <- function(label, fn, reps = 7L) {
  invisible(fn())                                    # warm-up (JIT/첫 할당 제외)
  t <- vapply(seq_len(reps), function(i) system.time(fn())[["elapsed"]], numeric(1))
  cat(sprintf("%-46s  median %8.4f s   min %8.4f   max %8.4f\n",
              label, median(t), min(t), max(t)))
  invisible(median(t))
}
try_bench <- function(label, fn, reps = 7L) {
  tryCatch(bench(label, fn, reps),
           error = function(e) cat(sprintf("%-46s  ERROR: %s\n", label,
                                           sub("\n.*", "", conditionMessage(e)))))
}

cat("multisiteDGP v0.2.0 업그레이드 — 성능 베이스라인\n")
cat("Step 1.5, 2026-08-14 |", R.version.string, "|", R.version$platform, "\n")
cat(strrep("=", 86), "\n\n")

cat("── 대표 시나리오 ──\n")
bench("sim_multisite(preset_education_modest())", function() sim_multisite(preset_education_modest(), seed = 1L))
bench("sim_multisite(preset_jebs_paper())",       function() sim_multisite(preset_jebs_paper(), seed = 1L))
bench("sim_meta(preset_meta_modest())",           function() sim_meta(preset_meta_modest(), seed = 1L))

cat("\n── J 스케일링 (Gaussian, dependence = none) ──\n")
for (J in c(50L, 200L, 1000L, 5000L)) {
  local({
    JJ <- J
    bench(sprintf("sim_multisite(J = %5d)", JJ),
          function() sim_multisite(J = JJ, sigma_tau = 0.2, nj_mean = 100,
                                   true_dist = "Gaussian", seed = 1L))
  })
}

cat("\n── dependence injection 3종 (J = 200, target = -0.4) ──\n")
bench("dependence = rank    ", function() sim_multisite(J = 200L, sigma_tau = 0.2, nj_mean = 100,
                                                        dependence = "rank", rank_corr = -0.4, seed = 1L))
bench("dependence = copula  ", function() sim_multisite(J = 200L, sigma_tau = 0.2, nj_mean = 100,
                                                        dependence = "copula", pearson_corr = -0.4, seed = 1L))
bench("dependence = hybrid  ", function() sim_multisite(J = 200L, sigma_tau = 0.2, nj_mean = 100,
                                                        dependence = "hybrid", rank_corr = -0.4, seed = 1L))

cat("\n── 분포 shape (J = 200) — theta_G 없이 호출 시 동작도 함께 확인 ──\n")
shapes <- list(
  Gaussian      = NULL,
  StudentT      = list(nu = 5),
  SkewN         = list(slant = 3),
  ALD           = list(rho = 0.3),
  Mixture       = list(delta = 1.5, eps = 0.3, ups = 2),
  PointMassSlab = list(pi0 = 0.3)
)
for (nm in names(shapes)) {
  local({
    g <- nm; th <- shapes[[nm]]
    try_bench(sprintf("true_dist = %-14s (theta_G 지정)", g),
              function() do.call(sim_multisite,
                                 c(list(J = 200L, sigma_tau = 0.2, nj_mean = 100,
                                        true_dist = g, seed = 1L),
                                   if (is.null(th)) NULL else list(theta_G = th))))
    if (!is.null(th)) {
      try_bench(sprintf("true_dist = %-14s (theta_G 생략)", g),
                function() sim_multisite(J = 200L, sigma_tau = 0.2, nj_mean = 100,
                                         true_dist = g, seed = 1L), reps = 1L)
    }
  })
}

cat("\n── 진단 · 후처리 (J = 200 fixture) ──\n")
dat  <- sim_multisite(J = 200L, sigma_tau = 0.2, nj_mean = 100, seed = 1L)
grid <- design_grid(J = 50L, sigma_tau = c(0.10, 0.20), seed_root = 1L)
try_bench("scenario_audit(grid, M = 1L)", function() scenario_audit(grid, M = 1L), reps = 3L)
bench("canonical_hash()",  function() canonical_hash(dat))
bench("summary()",         function() capture.output(summary(dat)))

cat("\n── design_grid() sweep ──\n")
try_bench("design_grid(3 x 3 = 9 cells)",
          function() design_grid(sigma_tau = c(0.1, 0.2, 0.3),
                                 nj_mean = c(50, 100, 200), J = 50L,
                                 seed_root = 12345L), reps = 3L)

cat("\n완료:", format(Sys.time(), "%F %T"), "\n")
