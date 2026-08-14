out_csv <- commandArgs(trailingOnly = TRUE)[1]
files <- list.files("vignettes", pattern = "[.]Rmd$", full.names = TRUE)
res <- data.frame()
for (f in files) {
  msg <- ""
  t <- tryCatch(
    system.time(rmarkdown::render(f, output_dir = tempdir(), quiet = TRUE))[["elapsed"]],
    error = function(e) { msg <<- sub("\n.*", "", conditionMessage(e)); NA_real_ }
  )
  if (nzchar(msg)) cat("      ERROR:", substr(msg, 1, 100), "\n")
  res <- rbind(res, data.frame(vignette = basename(f), elapsed_s = round(t, 1),
                               ok = !is.na(t), stringsAsFactors = FALSE))
  cat(sprintf("%-44s %8s  %s\n", basename(f),
              if (is.na(t)) "-" else sprintf("%.1f s", t),
              if (is.na(t)) "ERROR" else "ok"))
  flush.console()
}
cat("\n총 knit 시간:", round(sum(res$elapsed_s, na.rm = TRUE), 1), "초 /", nrow(res), "편\n")
cat("실패:", sum(!res$ok), "편\n\n")
res <- res[order(-res$elapsed_s), ]
cat("=== 느린 순 ===\n")
print(res, row.names = FALSE)
write.csv(res, out_csv, row.names = FALSE)

# 크기도 함께
h <- list.files("vignettes", pattern = "[.]html$", full.names = TRUE)
if (length(h)) {
  sz <- data.frame(file = basename(h), kb = round(file.size(h) / 1024, 1))
  sz <- sz[order(-sz$kb), ]
  cat("\n=== 체크인된 vignette HTML 크기 ===\n")
  print(head(sz, 20), row.names = FALSE)
  cat("총:", round(sum(sz$kb) / 1024, 1), "MB\n")
}
