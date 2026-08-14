# Step 2.2 — tests/testthat/ 전수 스캔으로 skip 매트릭스를 만든다.
out_csv <- commandArgs(trailingOnly = TRUE)[1]

files <- list.files("tests/testthat", pattern = "^test-.*\\.R$", full.names = TRUE)
rows <- list()

for (f in files) {
  lines <- readLines(f, warn = FALSE)
  # 파일 최상단(첫 test_that 이전)의 skip 호출 = 파일 전체 게이트
  first_tt <- which(grepl("^\\s*test_that\\(", lines))[1]
  head_zone <- if (is.na(first_tt)) lines else lines[seq_len(max(1, first_tt - 1))]
  file_gate <- unique(unlist(regmatches(head_zone,
    gregexpr("skip_if_not_(slow|property|validation|linux_strict_hash)|skip_if_api_missing|skip_on_cran|skip_if_not_installed\\([^)]*\\)", head_zone))))

  tt_idx <- which(grepl("^\\s*test_that\\(", lines))
  if (!length(tt_idx)) next
  ends <- c(tt_idx[-1] - 1L, length(lines))

  for (i in seq_along(tt_idx)) {
    body <- lines[tt_idx[i]:ends[i]]
    title <- sub('^\\s*test_that\\(\\s*["\']([^"\']*)["\'].*$', "\\1", lines[tt_idx[i]])
    gates <- unique(unlist(regmatches(body,
      gregexpr("skip_if_not_(slow|property|validation|linux_strict_hash)|skip_if_api_missing|skip_on_cran|skip_if_not_installed\\([^)]*\\)", body))))
    gates <- unique(c(file_gate, gates))
    rows[[length(rows) + 1L]] <- data.frame(
      file       = basename(f),
      line       = tt_idx[i],
      title      = substr(title, 1, 90),
      gates      = if (length(gates)) paste(gates, collapse = "; ") else "",
      n_gates    = length(gates),
      runs_by_default = !any(grepl("skip_if_not_(slow|property|validation|linux_strict_hash)", gates)),
      stringsAsFactors = FALSE
    )
  }
}
m <- do.call(rbind, rows)
write.csv(m, out_csv, row.names = FALSE)

cat("=== 테스트 스위트 skip 매트릭스 ===\n")
cat("테스트 파일:", length(files), " | test_that 블록:", nrow(m), "\n")
cat("기본 실행:", sum(m$runs_by_default), " | 기본 게이트됨:", sum(!m$runs_by_default), "\n\n")

cat("=== 게이트 유형별 test_that 수 ===\n")
for (g in c("skip_if_not_slow", "skip_if_not_property", "skip_if_not_validation",
            "skip_if_not_linux_strict_hash", "skip_if_api_missing", "skip_on_cran", "skip_if_not_installed")) {
  n <- sum(grepl(g, m$gates, fixed = TRUE))
  if (n) cat(sprintf("  %-32s %3d\n", g, n))
}

cat("\n=== 게이트된 테스트 (파일별) ===\n")
gt <- m[!m$runs_by_default, ]
print(sort(table(gt$file), decreasing = TRUE))

cat("\n=== 게이트된 테스트 전체 목록 ===\n")
print(gt[, c("file", "line", "title", "gates")], row.names = FALSE)
