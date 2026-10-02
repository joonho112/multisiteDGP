test_that("adapters are exported", {
  exports <- getNamespaceExports("multisiteDGP")
  expect_true(all(c("as_metafor", "as_baggr") %in% exports))
  # Removed in 0.3.0: the project it served is not an installable package.
  expect_false("as_multisitepower" %in% exports)
})

test_that("adapter rename helpers preserve values and strip multisiteDGP attributes", {
  dat <- sim_multisite(
    J = 25L,
    formula = ~ x_site,
    beta = 0.4,
    data = tibble::tibble(x_site = seq(-1, 1, length.out = 25L)),
    seed = 7301L
  )

  mf <- multisitedgp_internal(".as_metafor_rename")(dat)
  bg <- multisitedgp_internal(".as_baggr_rename")(dat, include_truth = TRUE)

  expect_s3_class(mf, "tbl_df")
  expect_false(inherits(mf, "multisitedgp_data"))
  expect_null(attr(mf, "design", exact = TRUE))
  expect_named(mf, c("yi", "vi", "sei", "x_site"))
  expect_equal(mf$yi, dat$tau_j_hat, tolerance = tol_deterministic)
  expect_equal(mf$vi, dat$se2_j, tolerance = tol_deterministic)
  expect_equal(mf$sei, dat$se_j, tolerance = tol_deterministic)
  expect_equal(compute_I(mf$vi, sigma_tau = 0.20), compute_I(dat$se2_j, sigma_tau = 0.20))

  expect_named(bg, c("tau", "se", "tau_true", "x_site"))
  expect_equal(bg$tau, dat$tau_j_hat, tolerance = tol_deterministic)
  expect_equal(bg$se, dat$se_j, tolerance = tol_deterministic)
  expect_equal(bg$tau_true, dat$tau_j, tolerance = tol_deterministic)
})

test_that("the adapters convert data from sim_meta(), which has no site sizes", {
  dat <- sim_meta(preset_meta_modest(), seed = 7302L)

  expect_named(as_metafor(dat), c("yi", "vi", "sei"))
  expect_equal(as_metafor(dat)$yi, dat$tau_j_hat, tolerance = tol_deterministic)
  expect_named(as_baggr(dat), c("tau", "se"))
  expect_equal(as_baggr(dat)$se, dat$se_j, tolerance = tol_deterministic)
})

test_that("the adapters return the renamed columns", {
  dat <- sim_multisite(J = 25L, seed = 7303L)

  expect_named(as_metafor(dat), c("yi", "vi", "sei"))
  expect_named(as_baggr(dat, include_truth = FALSE), c("tau", "se"))
  expect_named(as_baggr(dat, include_truth = TRUE), c("tau", "se", "tau_true"))
})

test_that("an adapter stops when a covariate has the name of a column it creates", {
  collision <- sim_multisite(
    J = 25L,
    formula = ~ sei,
    beta = 0.1,
    data = tibble::tibble(sei = seq_len(25L) / 25),
    seed = 7308L
  )
  expect_multisitedgp_error(as_metafor(collision))
  # `sei` is not a column of as_baggr(), so the covariate is carried along.
  expect_named(as_baggr(collision), c("tau", "se", "sei"))

  truth_collision <- sim_multisite(
    J = 25L,
    formula = ~ tau_true,
    beta = 0.1,
    data = tibble::tibble(tau_true = seq_len(25L) / 25),
    seed = 7307L
  )
  # `tau_true` is created only with include_truth = TRUE.
  expect_named(as_baggr(truth_collision), c("tau", "se", "tau_true"))
  expect_multisitedgp_error(as_baggr(truth_collision, include_truth = TRUE))
})

test_that("adapter defaults reject non multisitedgp_data inputs with classed errors", {
  expect_multisitedgp_error(as_metafor(tibble::tibble()))
  expect_multisitedgp_error(as_baggr(tibble::tibble()))
})

test_that("adapter reserved-name collision checks protect covariates", {
  withr::local_seed(7310L)
  checker <- multisitedgp_internal(".check_adapter_reserved_names")

  dat_metafor <- sim_multisite(
    J = 25L,
    formula = ~ yi,
    beta = 0.1,
    data = tibble::tibble(yi = stats::rnorm(25L)),
    seed = 7304L
  )
  dat_baggr <- sim_multisite(
    J = 25L,
    formula = ~ tau,
    beta = 0.1,
    data = tibble::tibble(tau = stats::rnorm(25L)),
    seed = 7305L
  )

  expect_multisitedgp_error(
    checker(dat_metafor, reserved = multisitedgp_internal(".RESERVED_METAFOR"), adapter = "metafor")
  )
  expect_multisitedgp_error(
    checker(dat_baggr, reserved = multisitedgp_internal(".RESERVED_BAGGR"), adapter = "baggr")
  )
})

test_that("no function of the package asks for an optional package", {
  # The adapters only rename columns, so nothing in the package may check
  # whether metafor, baggr or any other suggested package is installed.
  ns <- asNamespace("multisiteDGP")
  asks <- vapply(ls(ns, all.names = TRUE), function(name) {
    f <- get(name, envir = ns)
    is.function(f) &&
      any(c("requireNamespace", "loadNamespace", "require", "library") %in% all.names(body(f)))
  }, logical(1))

  expect_identical(names(asks)[asks], character())
})

test_that("as_metafor() and as_baggr() return their columns without metafor or baggr", {
  dat <- sim_multisite(J = 25L, seed = 7309L)

  # A function that needed its package would load it, or stop when the package
  # is not installed.
  loaded_before <- loadedNamespaces()
  mf <- as_metafor(dat)
  bg <- as_baggr(dat, include_truth = TRUE)
  newly_loaded <- setdiff(loadedNamespaces(), loaded_before)

  expect_false(any(c("metafor", "baggr") %in% newly_loaded))
  expect_named(mf, c("yi", "vi", "sei"))
  expect_equal(mf$yi, dat$tau_j_hat, tolerance = tol_deterministic)
  expect_equal(mf$vi, dat$se2_j, tolerance = tol_deterministic)
  expect_named(bg, c("tau", "se", "tau_true"))
  expect_equal(bg$tau, dat$tau_j_hat, tolerance = tol_deterministic)
  expect_equal(bg$se, dat$se_j, tolerance = tol_deterministic)
})
