# test-coverage.yaml 의 "Check exported-function coverage" 스텝을 그대로 재현한다.
# CI 환경변수: MULTISITEDGP_RUN_SLOW=false, MULTISITEDGP_RUN_PROPERTY=false
pkgload::load_all(".", quiet = TRUE)
ns <- asNamespace("multisiteDGP")
exported <- getNamespaceExports(ns)
internal <- setdiff(ls(ns), exported)
internal <- internal[vapply(internal, function(name) is.function(get(name, envir = ns)), logical(1))]
internal_regex <- paste0("^", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", internal), "$")

cat("exported 함수:", length(exported), " | internal 함수(제외 대상):", length(internal), "\n\n")

cov <- covr::package_coverage(
  type = "tests", quiet = TRUE, clean = FALSE,
  function_exclusions = internal_regex
)
line_cov <- covr::percent_coverage(cov, by = "line")
cat(sprintf("Exported-function line coverage: %.2f%%\n", line_cov))
cat(sprintf("CI 게이트 (>= 90%%): %s\n\n", if (line_cov < 90) "*** FAIL ***" else "PASS"))

df <- as.data.frame(cov)
agg <- do.call(rbind, lapply(split(df, df$filename), function(d)
  data.frame(file = d$filename[1], n = nrow(d), hit = sum(d$value > 0), stringsAsFactors = FALSE)))
agg$pct <- round(100 * agg$hit / agg$n, 1)
agg$miss <- agg$n - agg$hit
cat("=== 파일별 (낮은 순) ===\n")
print(agg[order(agg$pct), c("file", "pct", "hit", "n", "miss")], row.names = FALSE)
write.csv(agg, commandArgs(trailingOnly = TRUE)[1], row.names = FALSE)
