expect_multisitedgp_error <- function(expr, specific_class) {
  err <- rlang::catch_cnd(expr)
  testthat::expect_s3_class(err, specific_class)
  testthat::expect_s3_class(err, "multisitedgp_error")
  testthat::expect_s3_class(err, "rlang_error")
  testthat::expect_match(conditionMessage(err), "Try|Use|Pass|Remove")
  invisible(err)
}

expect_rng_neutral_error <- function(expr, specific_class, seed = 777L) {
  set.seed(seed)
  before <- get(".Random.seed", envir = .GlobalEnv)
  err <- expect_multisitedgp_error(expr, specific_class)
  after <- get(".Random.seed", envir = .GlobalEnv)

  testthat::expect_identical(after, before)
  invisible(err)
}

multisitedgp_internal <- function(name) {
  getFromNamespace(name, "multisiteDGP")
}
