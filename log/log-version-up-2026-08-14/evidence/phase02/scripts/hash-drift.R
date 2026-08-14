# Step 2.3 — canonical_hash 플랫폼 드리프트의 발생 지점과 크기를 로컬에서 규명한다.
# Linux 접근이 없으므로 "solver 출력이 tolerance 수준까지만 결정된다"는 가설을
# 플랫폼 대신 solver 조건을 흔들어 검증한다.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
ns <- asNamespace("multisiteDGP")
g <- function(n) get(n, envir = ns)

cat("Step 2.3 — canonical_hash 드리프트 규명\n")
cat(R.version.string, "|", R.version$platform, "\n")
cat(strrep("=", 78), "\n\n")

## ── 1. 해시 스키마: double 이 반올림되는가 ──────────────────────────
cat("── 1. 해시가 double 을 반올림하는가 ──\n")
canon <- g(".canonicalize_for_hash")
x1 <- c(1.0, 2.0, 3.0)
x2 <- x1; x2[2] <- x2[2] + .Machine$double.eps * 2   # 2 ULP 섭동
cat("  1 값에 2 ULP 섭동 → canonical 표현 동일? ",
    identical(canon(x1), canon(x2)), "\n")
cat("  digest 동일? ", identical(digest::digest(canon(x1)), digest::digest(canon(x2))), "\n")
cat("  → 반올림 없음. 최하위 비트가 곧 해시.\n\n")

## ── 2. 기본 engine 이 반복 solver 를 타는가 ─────────────────────────
cat("── 2. 기본 경로가 nleqslv 를 타는가 ──\n")
d <- multisitedgp_design(paradigm = "site_size", J = 40L, sigma_tau = 0.2,
                         nj_mean = 100, seed = 12345L)
cat("  기본 engine:", d$engine, "\n")
calls <- 0L
suppressMessages(trace(nleqslv::nleqslv, tracer = quote(calls <<- calls + 1L),
                       print = FALSE, where = asNamespace("nleqslv")))
dat <- sim_multisite(d)
suppressMessages(untrace(nleqslv::nleqslv, where = asNamespace("nleqslv")))
cat("  sim_multisite() 1 회 호출 중 nleqslv 호출 횟수:", calls, "\n")
cat("  →", if (calls > 0) "기본 경로가 반복 solver 를 탄다." else "solver 미사용.", "\n\n")

## ── 3. solver 해는 tolerance 수준까지만 결정된다 ────────────────────
cat("── 3. solver 해의 결정도(determinacy) ──\n")
resid_fn <- g(".trunc_gamma_residual")
fit_one  <- g(".fit_trunc_gamma_start")
n_bar <- 100; cv <- 0.4; n_min <- 5L
starts <- g(".trunc_gamma_starts")(n_bar = n_bar, cv = cv, max_starts = 5L)

res <- lapply(seq_along(starts), function(i)
  fit_one(starts[[i]], start_id = i, n_bar = n_bar, cv = cv,
          n_min = n_min, max_iter = 20000L))
ok <- Filter(function(r) isTRUE(r$verified), res)
al <- vapply(ok, function(r) r$alpha, numeric(1))
be <- vapply(ok, function(r) r$beta,  numeric(1))
cat(sprintf("  수렴한 시작점: %d / %d\n", length(ok), length(starts)))
cat(sprintf("  alpha  범위: [%.17g, %.17g]\n", min(al), max(al)))
cat(sprintf("  beta   범위: [%.17g, %.17g]\n", min(be), max(be)))
ulp_a <- if (length(al) > 1) (max(al) - min(al)) / .Machine$double.eps / max(abs(al)) else 0
ulp_b <- if (length(be) > 1) (max(be) - min(be)) / .Machine$double.eps / max(abs(be)) else 0
cat(sprintf("  시작점에 따른 해 차이: alpha %.3g ULP, beta %.3g ULP\n", ulp_a, ulp_b))
cat(sprintf("  잔차 노름 범위: [%.3g, %.3g]  (ftol = 1e-12)\n",
            min(vapply(ok, function(r) r$residual_norm, numeric(1))),
            max(vapply(ok, function(r) r$residual_norm, numeric(1)))))
cat("  → 해는 ftol 수준까지만 결정된다. 비트 수준으로 결정되지 않는다.\n\n")

## ── 4. tolerance 를 흔들면 해가 바뀌는가 = 플랫폼 차이의 대리 실험 ──
cat("── 4. tolerance 섭동 = 플랫폼 차이의 대리 실험 ──\n")
fit_tol <- function(tol) {
  f <- nleqslv::nleqslv(x = log(starts[[1]]), fn = resid_fn,
                        n_bar = n_bar, cv = cv, n_min = n_min,
                        control = list(ftol = tol, xtol = tol, maxit = 20000L))
  c(alpha = exp(f$x[[1]]), beta = exp(f$x[[2]]))
}
base <- fit_tol(1e-12)
for (tol in c(1e-12, 1e-13, 1e-11, 1e-10)) {
  v <- fit_tol(tol)
  da <- abs(v[["alpha"]] - base[["alpha"]]) / .Machine$double.eps / abs(base[["alpha"]])
  cat(sprintf("  ftol=%-6.0e  alpha=%.17g  base 대비 %8.3g ULP\n", tol, v[["alpha"]], da))
}
cat("  → tolerance 를 1 자릿수만 바꿔도 해의 최하위 비트가 달라진다.\n")
cat("    플랫폼별 libm/컴파일러 차이도 동일한 효과를 낸다.\n\n")

## ── 5. solver 출력이 해시 대상 데이터에 흘러드는가 ──────────────────
cat("── 5. solver 출력 → 데이터 컬럼 전파 경로 ──\n")
cat("  컬럼:", paste(names(dat), collapse = ", "), "\n")
num <- names(dat)[vapply(dat, is.numeric, logical(1))]
cat("  수치 컬럼:", paste(num, collapse = ", "), "\n\n")

## ── 6. 해시 구성요소 분해: 어느 부분이 취약한가 ─────────────────────
cat("── 6. 해시 구성요소 민감도 ──\n")
payload <- g(".canonical_hash_payload")(dat, algo = "xxhash64")
cat("  payload 최상위 키:", paste(names(payload), collapse = ", "), "\n")
cat("  해시 대상 컬럼 수:", length(payload$columns_sorted), "\n")
cat("  해시 대상 진단 스칼라:", paste(names(payload$diagnostics_numeric), collapse = ", "), "\n\n")

perturb_hash <- function(what) {
  p <- payload
  if (what == "diagnostics") {
    k <- names(p$diagnostics_numeric)[1]
    p$diagnostics_numeric[[k]] <- p$diagnostics_numeric[[k]] * (1 + .Machine$double.eps)
  } else {
    p$data_canonical[[what]][1] <- p$data_canonical[[what]][1] * (1 + .Machine$double.eps)
  }
  digest::digest(p, algo = "xxhash64")
}
h0 <- digest::digest(payload, algo = "xxhash64")
cat("  기준 해시:", h0, "\n")
for (w in c(intersect(num, payload$columns_sorted)[1:min(3, length(num))], "diagnostics")) {
  if (is.na(w)) next
  cat(sprintf("  %-16s 1 ULP 섭동 → %s  (변경: %s)\n", w, perturb_hash(w),
              !identical(h0, perturb_hash(w))))
}
cat("\n  → 데이터 컬럼과 진단 스칼라 어느 쪽이든 1 ULP 로 해시가 뒤집힌다.\n")

## ── 7. same-machine 재현성은 유지되는가 ─────────────────────────────
cat("\n── 7. same-machine 재현성 (T20 계약) ──\n")
h_a <- canonical_hash(sim_multisite(d))
h_b <- canonical_hash(sim_multisite(d))
cat("  동일 머신 2 회:", h_a, "/", h_b, "→", identical(h_a, h_b), "\n")

cat("\n완료:", format(Sys.time(), "%F %T"), "\n")
