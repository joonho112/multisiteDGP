out_dir <- commandArgs(trailingOnly = TRUE)[1]
cov <- covr::package_coverage(".", type = "tests", quiet = TRUE)
saveRDS(cov, file.path(out_dir, "coverage.rds"))

cat("multisiteDGP v0.2.0 업그레이드 — 커버리지 베이스라인\n")
cat("Step 1.5, 2026-08-14 | MULTISITEDGP_RUN_SLOW/PROPERTY = true\n")
cat(strrep("=", 62), "\n\n")
cat(sprintf("전체 커버리지: %.2f%%\n\n", covr::percent_coverage(cov)))

df <- as.data.frame(cov)
agg <- do.call(rbind, lapply(split(df, df$filename), function(d) {
  data.frame(file = d$filename[1], n = nrow(d), hit = sum(d$value > 0),
             stringsAsFactors = FALSE)
}))
agg$pct <- round(100 * agg$hit / agg$n, 1)
agg <- agg[order(agg$pct, -agg$n), ]
cat("=== 파일별 (낮은 순) ===\n")
print(agg[, c("file", "pct", "hit", "n")], row.names = FALSE)

cat("\n=== 90% 미만 파일 ===\n")
low <- agg[agg$pct < 90, ]
if (nrow(low)) print(low[, c("file", "pct", "hit", "n")], row.names = FALSE) else cat("(없음)\n")

cat("\n=== 미커버 라인 수 상위 10 파일 ===\n")
agg$miss <- agg$n - agg$hit
top <- agg[order(-agg$miss), ][1:min(10, nrow(agg)), ]
print(top[, c("file", "miss", "pct")], row.names = FALSE)

write.csv(agg[, c("file", "pct", "hit", "n", "miss")],
          file.path(out_dir, "coverage-by-file.csv"), row.names = FALSE)
cat("\n완료:", format(Sys.time(), "%F %T"), "\n")
