suppressMessages(pkgload::load_all(".", quiet = TRUE))
dat <- sim_multisite(preset_jebs_paper(), seed = 4719L)
prev <- utils::read.csv("log/log-version-up-2026-08-14/artifacts/validation-2026-05/generated/br9-v12-full-pattern-evidence-results.csv",
                        stringsAsFactors = FALSE)
cat("가설: CSV 15 유효숫자 왕복 손실\n\n")
for (k in c("tau_j","tau_j_hat","se_j","se2_j")) {
  a <- signif(prev[[k]], 15); b <- signif(dat[[k]], 15)
  raw <- max(abs(prev[[k]] - dat[[k]]))
  rel <- max(abs(prev[[k]] - dat[[k]]) / pmax(abs(dat[[k]]), 1e-300))
  cat(sprintf("  %-10s 원본차 %.2e | 상대차 %.2e | 15자리 반올림 후 동일: %s\n",
      k, raw, rel, identical(a, b)))
}
cat("\n결정적 확인 — golden .rds fixture 는 정확 비교다:\n")
for (f in list.files("tools/jebs-golden-fixtures/generated", "\\.rds$", full.names = TRUE)) {
  ref <- readRDS(f)
  seed <- as.integer(sub(".*seed([0-9]+)\\.rds$", "\\1", f))
  act <- sim_multisite(preset_jebs_strict(), seed = seed)
  cols <- intersect(names(ref), names(act))
  same <- all(vapply(cols, function(k) identical(ref[[k]], act[[k]]), logical(1)))
  cat(sprintf("  seed=%-6d 열 %d개 bit-identical: %s\n", seed, length(cols), same))
}
