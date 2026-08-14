# Step 2.5 + 2.6 — 통계 커널 감사: Layer 1 표준화 계약, Layer 2-4, solver 견고성
suppressMessages(pkgload::load_all(".", quiet = TRUE))
out_dir <- commandArgs(trailingOnly = TRUE)[1]
set.seed(1)

cat("Step 2.5 / 2.6 — 통계 커널 감사\n"); cat(strrep("=", 78), "\n\n")

## ── Layer 1: 표준화 계약을 테스트보다 넓은 격자에서 확인 ────────────
cat("── L1. 표준화 계약 (mean(z_j)=0, var(z_j)=1), N = 200,000 ──\n")
N <- 200000L
grid <- list(
  list(g = "Gaussian",      th = NULL),
  list(g = "StudentT",      th = list(nu = 2.1)),
  list(g = "StudentT",      th = list(nu = 3)),
  list(g = "StudentT",      th = list(nu = 5)),
  list(g = "StudentT",      th = list(nu = 30)),
  list(g = "SkewN",         th = list(slant = 0.5)),
  list(g = "SkewN",         th = list(slant = 3)),
  list(g = "SkewN",         th = list(slant = 20)),
  list(g = "ALD",           th = list(rho = 0.1)),
  list(g = "ALD",           th = list(rho = 0.5)),
  list(g = "ALD",           th = list(rho = 0.9)),
  list(g = "Mixture",       th = list(delta = 0.5, eps = 0.05, ups = 1)),
  list(g = "Mixture",       th = list(delta = 1.5, eps = 0.3,  ups = 2)),
  list(g = "Mixture",       th = list(delta = 3,   eps = 0.5,  ups = 5)),
  list(g = "PointMassSlab", th = list(pi0 = 0.05)),
  list(g = "PointMassSlab", th = list(pi0 = 0.5)),
  list(g = "PointMassSlab", th = list(pi0 = 0.95))
)
rows <- list()
for (i in seq_along(grid)) {
  gg <- grid[[i]]$g; th <- grid[[i]]$th
  r <- tryCatch({
    args <- c(list(J = N, sigma_tau = 1, tau = 0, true_dist = gg),
              if (is.null(th)) NULL else list(theta_G = th))
    e <- withr::with_seed(42L, do.call(gen_effects, args))
    z <- if ("z_j" %in% names(e)) e$z_j else (e$tau_j - 0) / 1
    list(ok = TRUE, m = mean(z), v = var(z), msg = "")
  }, error = function(err) list(ok = FALSE, m = NA, v = NA,
                                msg = sub("\n.*", "", conditionMessage(err))))
  lbl <- paste0(gg, if (is.null(th)) "" else paste0("(", paste(names(th), unlist(th), sep = "=", collapse = ","), ")"))
  rows[[i]] <- data.frame(shape = lbl, ok = r$ok, mean_z = r$m, var_z = r$v,
                          err = substr(r$msg, 1, 60), stringsAsFactors = FALSE)
  cat(sprintf("  %-34s %s  mean %+9.5f  var %8.5f  %s\n", lbl,
              if (r$ok) "OK   " else "ABORT",
              ifelse(is.na(r$m), 0, r$m), ifelse(is.na(r$v), 0, r$v), substr(r$msg, 1, 40)))
}
l1 <- do.call(rbind, rows)
write.csv(l1, file.path(out_dir, "layer1-standardization-grid.csv"), row.names = FALSE)
bad <- l1[l1$ok & (abs(l1$mean_z) > 0.02 | abs(l1$var_z - 1) > 0.03), ]
cat(sprintf("\n  격자 %d 셀 | 실행 %d | 계약 이탈 %d\n", nrow(l1), sum(l1$ok), nrow(bad)))
if (nrow(bad)) { cat("  이탈:\n"); print(bad[, c("shape","mean_z","var_z")], row.names = FALSE) }

## ── Layer 2: A2 solver 실패 영역 ────────────────────────────────────
cat("\n── L2. Engine A2 solver 실현 가능 영역 (nj_mean x cv) ──\n")
nj_grid <- c(10, 25, 50, 100, 250, 1000)
cv_grid <- c(0.05, 0.1, 0.25, 0.4, 0.6, 0.8, 1.0, 1.3)
sol <- expand.grid(nj_mean = nj_grid, cv = cv_grid)
sol$ok <- NA; sol$warn <- NA; sol$msg <- ""
for (i in seq_len(nrow(sol))) {
  r <- tryCatch({
    w <- character()
    withCallingHandlers(
      sim_multisite(J = 60L, sigma_tau = 0.2, nj_mean = sol$nj_mean[i],
                    cv = sol$cv[i], seed = 7L),
      warning = function(cond) { w <<- c(w, conditionMessage(cond)); invokeRestart("muffleWarning") })
    list(ok = TRUE, w = length(w), msg = if (length(w)) sub("\n.*", "", w[1]) else "")
  }, error = function(e) list(ok = FALSE, w = 0L, msg = sub("\n.*", "", conditionMessage(e))))
  sol$ok[i] <- r$ok; sol$warn[i] <- r$w; sol$msg[i] <- substr(r$msg, 1, 70)
}
cat("  격자", nrow(sol), "셀 | 성공", sum(sol$ok), "| 실패", sum(!sol$ok), "| 경고 동반", sum(sol$warn > 0), "\n")
m <- xtabs(ok ~ nj_mean + cv, data = sol)
cat("\n  성공 여부 (행 nj_mean, 열 cv):\n"); print(m)
if (any(!sol$ok)) { cat("\n  실패 셀:\n"); print(sol[!sol$ok, c("nj_mean","cv","msg")], row.names = FALSE) }
if (any(sol$warn > 0)) { cat("\n  경고 셀:\n"); print(unique(sol[sol$warn > 0, c("nj_mean","cv","msg")]), row.names = FALSE) }
write.csv(sol, file.path(out_dir, "solver-feasible-region.csv"), row.names = FALSE)

## ── Layer 3: dependence 목표 달성 오차 ──────────────────────────────
cat("\n── L3. dependence 목표 달성 오차 (J = 200, 시드 5개) ──\n")
targets <- c(-0.9, -0.6, -0.3, 0, 0.3, 0.6, 0.9)
dep_rows <- list(); k <- 0L
for (meth in c("rank", "copula", "hybrid")) {
  for (tg in targets) {
    ach <- c(); err <- ""
    # copula 의 pearson_corr 는 잠재 Gaussian copula 상관이므로
    # 실현 Spearman 의 기대값은 (6/pi) * asin(rho_P / 2) 이다 (roxygen 명시).
    tg_expected <- if (meth == "copula") (6 / pi) * asin(tg / 2) else tg
    for (s in 1:5) {
      r <- tryCatch({
        a <- list(J = 200L, sigma_tau = 0.2, nj_mean = 100, dependence = meth, seed = s)
        a[[if (meth == "copula") "pearson_corr" else "rank_corr"]] <- tg
        d <- do.call(sim_multisite, a)
        realized_rank_corr(d)
      }, error = function(e) { err <<- sub("\n.*", "", conditionMessage(e)); NA_real_ })
      ach <- c(ach, r)
    }
    k <- k + 1L
    dep_rows[[k]] <- data.frame(method = meth, target = tg,
                                expected = tg_expected,
                                achieved_mean = mean(ach, na.rm = TRUE),
                                max_abs_err = max(abs(ach - tg_expected), na.rm = TRUE),
                                n_fail = sum(is.na(ach)), err = substr(err, 1, 50),
                                stringsAsFactors = FALSE)
    cat(sprintf("  %-7s arg %+5.2f  기대 rho_S %+7.4f  실현 %+7.4f  최대오차 %7.4f  실패 %d  %s\n",
                meth, tg, tg_expected, mean(ach, na.rm = TRUE),
                max(abs(ach - tg_expected), na.rm = TRUE),
                sum(is.na(ach)), substr(err, 1, 40)))
  }
}
dep <- do.call(rbind, dep_rows)
write.csv(dep, file.path(out_dir, "dependence-achievement.csv"), row.names = FALSE)

## ── Layer 4 / wrapper: 두 front door 동치성 ─────────────────────────
cat("\n── L4. design 객체 경로 vs flat 인자 경로 동치성 ──\n")
d1 <- sim_multisite(J = 40L, sigma_tau = 0.2, nj_mean = 100, seed = 99L)
des <- multisitedgp_design(paradigm = "site_size", J = 40L, sigma_tau = 0.2,
                           nj_mean = 100, seed = 99L)
d2 <- sim_multisite(des)
cat("  canonical_hash 동일:", identical(canonical_hash(d1), canonical_hash(d2)), "\n")

cat("\n── L4. seed 지정 시 caller RNG 상태 보존 (T20) ──\n")
set.seed(123); before <- .Random.seed
invisible(sim_multisite(J = 30L, sigma_tau = 0.2, nj_mean = 100, seed = 5L))
cat("  RNG 상태 보존:", identical(before, .Random.seed), "\n")

cat("\n완료:", format(Sys.time(), "%F %T"), "\n")
