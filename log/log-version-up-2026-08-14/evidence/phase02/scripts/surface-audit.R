# Step 2.4 + 2.7 — 광고 표면 vs 구현 표면, 오류 카탈로그 도달 가능성
suppressMessages(pkgload::load_all(".", quiet = TRUE))
ns <- asNamespace("multisiteDGP")
out_dir <- commandArgs(trailingOnly = TRUE)[1]

cat("Step 2.4 / 2.7 — 표면 감사\n"); cat(strrep("=", 78), "\n\n")

## ── A. 광고: DESCRIPTION / NEWS / README 의 shape 개수 주장 ─────────
cat("── A. 분포 shape 개수 주장 위치 ──\n")
claims <- list()
scan_file <- function(path, pat = "eight|8 (distribution|shape)|여덟") {
  if (!file.exists(path)) return(NULL)
  ln <- readLines(path, warn = FALSE)
  hit <- grep(pat, ln, ignore.case = TRUE)
  if (length(hit)) data.frame(file = path, line = hit, text = substr(trimws(ln[hit]), 1, 110),
                              stringsAsFactors = FALSE) else NULL
}
for (p in c("DESCRIPTION", "NEWS.md", "README.md", "README.Rmd", "index.md", "_pkgdown.yml")) {
  r <- scan_file(p); if (!is.null(r)) claims[[p]] <- r
}
cl <- do.call(rbind, claims)
if (!is.null(cl)) print(cl, row.names = FALSE) else cat("  (없음)\n")

## ── B. 구현: true_dist 각 값이 실제로 동작하는가 ────────────────────
cat("\n── B. true_dist 실행 검증 ──\n")
theta <- list(Gaussian = NULL, StudentT = list(nu = 5), SkewN = list(slant = 3),
              ALD = list(rho = 0.3), Mixture = list(delta = 1.5, eps = 0.3, ups = 2),
              PointMassSlab = list(pi0 = 0.3), DPM = NULL, User = NULL)
rows <- list()
for (g in names(theta)) {
  args <- c(list(J = 20L, sigma_tau = 0.2, nj_mean = 100, true_dist = g, seed = 1L),
            if (is.null(theta[[g]])) NULL else list(theta_G = theta[[g]]))
  r <- tryCatch({ do.call(sim_multisite, args); list(ok = TRUE, msg = "") },
                error = function(e) list(ok = FALSE, msg = sub("\n.*", "", conditionMessage(e))))
  rows[[g]] <- data.frame(true_dist = g, works = r$ok, error = substr(r$msg, 1, 80),
                          stringsAsFactors = FALSE)
  cat(sprintf("  %-14s %s  %s\n", g, if (r$ok) "OK   " else "ABORT", substr(r$msg, 1, 70)))
}
impl <- do.call(rbind, rows)
cat(sprintf("\n  동작하는 shape: %d / %d\n", sum(impl$works), nrow(impl)))
write.csv(impl, file.path(out_dir, "true-dist-implementation.csv"), row.names = FALSE)

## ── C. 예약 인자 ────────────────────────────────────────────────────
cat("\n── C. 예약/미구현 인자 ──\n")
chk <- function(label, expr) {
  r <- tryCatch({ force(expr); "동작" },
                error = function(e) paste("ABORT:", sub("\n.*", "", conditionMessage(e))))
  cat(sprintf("  %-24s %s\n", label, substr(r, 1, 88)))
}
chk("target_marginal_rho", multisitedgp_design(J = 20L, sigma_tau = 0.2, nj_mean = 100,
                                               target_marginal_rho = 0.3))
chk("upstream (gen_effects)", gen_effects(J = 20L, sigma_tau = 0.2, upstream = list()))

## ── D. 오류 카탈로그 도달 가능성 ────────────────────────────────────
cat("\n── D. 타입드 오류 클래스 ──\n")
err_src <- readLines("R/00-errors-validation.R", warn = FALSE)
classes <- unique(unlist(regmatches(err_src, gregexpr('"multisitedgp_[a-z_]*error"', err_src))))
cat("  정의된 클래스:", length(classes), "\n  ", paste(gsub('"', "", classes), collapse = ", "), "\n")

abort_helpers <- unique(unlist(regmatches(err_src, gregexpr("\\.abort_[a-z_]+", err_src))))
cat("\n  abort helper:", paste(abort_helpers, collapse = ", "), "\n")
all_r <- list.files("R", pattern = "\\.R$", full.names = TRUE)
src <- unlist(lapply(all_r, readLines, warn = FALSE))
cat("\n  helper 별 호출 횟수 (R/ 전체):\n")
for (h in abort_helpers) {
  n <- sum(grepl(paste0("\\", h, "\\("), src))
  cat(sprintf("    %-28s %3d\n", h, n))
}

## ── E. traceability error-index 와 대조 ─────────────────────────────
cat("\n── E. error-index.csv 대조 ──\n")
ei_path <- "tools/traceability/error-index.csv"
if (file.exists(ei_path)) {
  ei <- read.csv(ei_path, stringsAsFactors = FALSE)
  cat("  등재된 오류:", nrow(ei), "| 열:", paste(names(ei), collapse = ", "), "\n")
  if ("status" %in% names(ei)) print(table(ei$status))
} else cat("  (파일 없음)\n")

## ── F. exported 함수의 @examples 실행 가능성은 R CMD check 가 검증 ──
cat("\n── F. export 표면 ──\n")
ex <- getNamespaceExports(ns)
cat("  export 수:", length(ex), "\n")
rd <- list.files("man", pattern = "\\.Rd$")
cat("  Rd 파일 수:", length(rd), "\n")
cat("\n완료:", format(Sys.time(), "%F %T"), "\n")
