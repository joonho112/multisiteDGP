expect_multisitedgp_error <- function(expr, class = "multisitedgp_error", regexp = NULL) {
  err <- rlang::catch_cnd(expr)
  testthat::expect_s3_class(err, "multisitedgp_error")
  if (!identical(class, "multisitedgp_error")) {
    testthat::expect_s3_class(err, class)
  }
  if (!is.null(regexp)) {
    testthat::expect_match(conditionMessage(err), regexp)
  }
  invisible(err)
}

expect_rng_neutral_error <- function(expr, class = "multisitedgp_error", regexp = NULL, seed = 777L) {
  set.seed(seed)
  before <- get(".Random.seed", envir = .GlobalEnv)
  err <- expect_multisitedgp_error(expr, class, regexp)
  after <- get(".Random.seed", envir = .GlobalEnv)

  testthat::expect_identical(after, before)
  invisible(err)
}

multisitedgp_internal <- function(name) {
  getFromNamespace(name, "multisiteDGP")
}
