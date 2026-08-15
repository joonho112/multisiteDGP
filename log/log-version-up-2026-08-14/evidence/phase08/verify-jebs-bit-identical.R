suppressMessages(pkgload::load_all(".", quiet = TRUE))
for (fn in list.files("tests/testthat", "^helper-", full.names = TRUE)) sys.source(fn, envir = globalenv())
for (seed in c(42L, 1L, 2024L, 12345L)) {
  ref <- readRDS(t_invariant_jebs_seed_file(seed))
  act <- t_invariant_jebs_plain_frame(sim_multisite(preset_jebs_strict(), seed = seed))
  cols <- intersect(names(ref), names(act))
  bad <- cols[!vapply(cols, function(k) identical(ref[[k]], act[[k]]), logical(1))]
  cat(sprintf("  seed=%-6d 열 %d개 | bit-identical: %-5s%s\n", seed, length(cols),
      length(bad) == 0L, if (length(bad)) paste0("  차이: ", paste(bad, collapse = ",")) else ""))
}
