test_that("Step 7.3 adapters are exported", {
  exports <- getNamespaceExports("multisiteDGP")
  expect_true(all(c("as_metafor", "as_baggr", "as_multisitepower") %in% exports))
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
  mp <- multisitedgp_internal(".as_multisitepower_rename")(dat)

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

  expect_named(mp, c("site", "estimate", "se", "n", "x_site"))
  expect_equal(mp$site, dat$site_index)
  expect_equal(mp$estimate, dat$tau_j_hat, tolerance = tol_deterministic)
  expect_equal(mp$se, dat$se_j, tolerance = tol_deterministic)
  expect_equal(mp$n, dat$n_j)
})

test_that("as_multisitepower omits n for direct meta outputs", {
  dat <- sim_meta(preset_meta_modest(), seed = 7302L)
  mp <- multisitedgp_internal(".as_multisitepower_rename")(dat)

  expect_named(mp, c("site", "estimate", "se"))
  expect_false("n" %in% names(mp))
  expect_equal(mp$estimate, dat$tau_j_hat, tolerance = tol_deterministic)
})

test_that("adapter public methods work when soft dependencies are installed", {
  dat <- sim_multisite(J = 25L, seed = 7303L)

  if (requireNamespace("metafor", quietly = TRUE)) {
    expect_named(as_metafor(dat), c("yi", "vi", "sei"))
  } else {
    expect_multisitedgp_error(as_metafor(dat), "multisitedgp_arg_error")
  }

  if (requireNamespace("baggr", quietly = TRUE)) {
    expect_named(as_baggr(dat, include_truth = FALSE), c("tau", "se"))
    expect_named(as_baggr(dat, include_truth = TRUE), c("tau", "se", "tau_true"))
  } else {
    expect_multisitedgp_error(as_baggr(dat), "multisitedgp_arg_error")
  }

  if (requireNamespace("multisitepower", quietly = TRUE)) {
    expect_named(as_multisitepower(dat), c("site", "estimate", "se", "n"))
  } else {
    expect_multisitedgp_error(as_multisitepower(dat), "multisitedgp_arg_error")
  }
})

test_that("as_multisitepower public path covers rename and reserved-name checks when dependency is available", {
  local_mocked_bindings(.require_namespace = function(package) TRUE, .package = "multisiteDGP")

  dat <- sim_multisite(J = 25L, seed = 7307L)
  mp <- as_multisitepower(dat)
  expect_named(mp, c("site", "estimate", "se", "n"))
  expect_equal(mp$site, dat$site_index)

  collision <- sim_multisite(
    J = 25L,
    formula = ~ site,
    beta = 0.1,
    data = tibble::tibble(site = seq_len(25L)),
    seed = 7308L
  )
  expect_multisitedgp_error(
    as_multisitepower(collision),
    "multisitedgp_arg_error"
  )
})

test_that("adapter defaults reject non multisitedgp_data inputs with classed errors", {
  expect_multisitedgp_error(as_metafor(tibble::tibble()), "multisitedgp_arg_error")
  expect_multisitedgp_error(as_baggr(tibble::tibble()), "multisitedgp_arg_error")
  expect_multisitedgp_error(as_multisitepower(tibble::tibble()), "multisitedgp_arg_error")
})

test_that("adapter reserved-name collision checks protect covariates", {
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
  dat_multisitepower <- sim_multisite(
    J = 25L,
    formula = ~ site,
    beta = 0.1,
    data = tibble::tibble(site = stats::rnorm(25L)),
    seed = 7306L
  )

  expect_multisitedgp_error(
    checker(dat_metafor, reserved = multisitedgp_internal(".RESERVED_METAFOR"), adapter = "metafor"),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    checker(dat_baggr, reserved = multisitedgp_internal(".RESERVED_BAGGR"), adapter = "baggr"),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    checker(
      dat_multisitepower,
      reserved = multisitedgp_internal(".RESERVED_MULTISITEPOWER"),
      adapter = "multisitepower"
    ),
    "multisitedgp_arg_error"
  )
})

test_that("adapter helper validation exposes soft dependency errors", {
  expect_multisitedgp_error(
    multisitedgp_internal(".require_soft_dependency")("definitelyMissingPkgForMultisiteDGP", "as_metafor"),
    "multisitedgp_arg_error"
  )
})

test_that("all adapter soft-dependency guards hard-reject missing packages", {
  dat <- sim_multisite(J = 25L, seed = 7309L)
  local_mocked_bindings(.require_namespace = function(package) FALSE, .package = "multisiteDGP")

  cases <- list(
    list(call = function() as_metafor(dat), package = "metafor", context = "as_metafor"),
    list(call = function() as_baggr(dat), package = "baggr", context = "as_baggr"),
    list(call = function() as_multisitepower(dat), package = "multisitepower", context = "as_multisitepower")
  )

  for (case in cases) {
    err <- expect_multisitedgp_error(case$call(), "multisitedgp_arg_error")
    expect_match(conditionMessage(err), sprintf("Package `%s` is required", case$package), fixed = TRUE)
    expect_match(conditionMessage(err), sprintf("`%s()`", case$context), fixed = TRUE)
    expect_match(conditionMessage(err), sprintf("install.packages(\"%s\")", case$package), fixed = TRUE)
  }
})

test_that("Step 7.3 adapter traceability statuses are synchronized", {
  trace_dir <- test_path("../../tools/traceability")
  skip_if_not(dir.exists(trace_dir), "Development-only traceability ledgers are not shipped in the package tarball.")

  api <- read.csv(file.path(trace_dir, "api-index.csv"), stringsAsFactors = FALSE)
  errors <- read.csv(file.path(trace_dir, "error-index.csv"), stringsAsFactors = FALSE)
  decisions <- read.csv(file.path(trace_dir, "decision-index.csv"), stringsAsFactors = FALSE)

  expect_true(all(api$status[match(c("API022", "API023", "API024"), api$id)] == "implemented"))
  expect_true(all(api$owner_step[match(c("API022", "API023", "API024"), api$id)] == "Step 7.3"))
  expect_true(all(errors$status[match(c("E26", "E27", "E30"), errors$id)] == "implemented"))
  expect_true(all(errors$validator_owner_step[match(c("E26", "E27", "E30"), errors$id)] == "Step 7.3"))
  expect_false("E31" %in% errors$id)
  expect_match(api$unresolved_conflict[match("API024", api$id)], "no E31", fixed = TRUE)
  expect_identical(decisions$status[match("Q7", decisions$id)], "resolved")
  expect_identical(decisions$status[match("Q11", decisions$id)], "resolved-rejected")
})
