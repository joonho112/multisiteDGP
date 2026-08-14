out_path <- commandArgs(trailingOnly = TRUE)[1]
d <- read.dcf("DESCRIPTION")
parse_f <- function(x) {
  if (is.na(x)) return(character())
  v <- strsplit(x, ",")[[1]]
  trimws(sub("[(].*", "", v))
}
dep <- parse_f(d[1, "Depends"])
imp <- parse_f(d[1, "Imports"])
sug <- parse_f(d[1, "Suggests"])
tools_ <- c("devtools", "roxygen2", "pkgdown", "lintr", "cyclocomp", "covr", "quarto")
inst <- rownames(installed.packages())

mk <- function(p, role) {
  if (!length(p)) return(NULL)
  data.frame(
    package   = p,
    role      = role,
    installed = vapply(p, function(x) x %in% inst, logical(1)),
    version   = vapply(p, function(x) if (x %in% inst) as.character(packageVersion(x)) else NA_character_, character(1)),
    row.names = NULL, stringsAsFactors = FALSE
  )
}
out <- do.call(rbind, list(
  mk(dep, "Depends"), mk(imp, "Imports"), mk(sug, "Suggests"),
  mk(setdiff(tools_, c(imp, sug)), "DevTool")
))
out$status <- ifelse(out$installed, "ok", "MISSING")
write.csv(out[, c("package", "role", "version", "status")], out_path, row.names = FALSE)

cat(sprintf("총 %d개 | 설치됨 %d | 누락 %d\n\n", nrow(out), sum(out$installed), sum(!out$installed)))
cat("누락:\n")
print(out[!out$installed, c("package", "role", "status")], row.names = FALSE)
cat("\n이번 Step에서 새로 설치:\n")
print(out[out$package %in% c("copula", "hedgehog"), c("package", "role", "version")], row.names = FALSE)
