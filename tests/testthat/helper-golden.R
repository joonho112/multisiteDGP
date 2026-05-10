golden_extdata_path <- function(...) {
  path <- system.file("extdata", "golden", ..., package = "multisiteDGP")
  testthat::skip_if_not(nzchar(path), "Step 8.1 golden extdata is unavailable.")
  path
}
