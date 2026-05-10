test_that("cookbook audit parses and runs standalone recipes", {
  script <- system.file("scripts", "cookbook_audit.R", package = "multisiteDGP")
  expect_true(nzchar(script))

  env <- new.env(parent = globalenv())
  sys.source(script, envir = env)
  audit <- env$run_cookbook_audit(execute = TRUE, verbose = FALSE)

  expect_equal(nrow(audit), 9L)
  expect_true(all(audit$parsed))
  expect_true(all(audit$executed))
  expect_true(all(audit$marker_found))
  expect_true(all(is.na(audit$error)))
})
