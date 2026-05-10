# nolint start: object_usage_linter
observation_upstream <- function(J = 25L, engine = "A2_modern") {
  out <- tibble::tibble(
    site_index = seq_len(J),
    z_j = seq(-1, 1, length.out = J),
    tau_j = 0.2 * seq(-1, 1, length.out = J),
    n_j = as.integer(seq(40L, 40L + J - 1L)),
    se2_j = seq(0.02, 0.08, length.out = J),
    se_j = sqrt(seq(0.02, 0.08, length.out = J))
  )
  attr(out, "engine") <- engine
  attr(out, "kappa") <- 4
  out
}

reference_jebs_mixture_tau <- function(J, sigma_tau, delta, eps, ups) {
  scale <- sqrt((1 - eps) + eps * ups^2 + eps * (1 - eps) * delta^2)
  component_one <- stats::runif(J) < (1 - eps)
  tau_j <- component_one * stats::rnorm(J, -eps * delta / scale, sqrt(1 / scale^2)) +
    (1 - component_one) * stats::rnorm(J, (1 - eps) * delta / scale, sqrt(ups^2 / scale^2))
  tau_j * sigma_tau
}

reference_jebs_upstream <- function() {
  tau_j <- reference_jebs_mixture_tau(
    J = 100L,
    sigma_tau = 0.15,
    delta = 5,
    eps = 0.3,
    ups = 2
  )
  gen_site_sizes(
    tibble::tibble(
      site_index = seq_len(100L),
      z_j = tau_j / 0.15,
      tau_j = tau_j
    ),
    J = 100L,
    nj_mean = 80,
    cv = 0.50,
    nj_min = 4L,
    engine = "A1_legacy"
  )
}

test_that("gen_observations is exported and appends the L4 schema", {
  expect_true("gen_observations" %in% getNamespaceExports("multisiteDGP"))

  upstream <- observation_upstream()
  out <- withr::with_seed(901L, gen_observations(upstream))

  expect_s3_class(out, "tbl_df")
  expect_named(out, c("site_index", "z_j", "tau_j", "n_j", "se2_j", "se_j", "tau_j_hat"))
  expect_true(is.numeric(out$tau_j_hat))
  expect_equal(out$site_index, upstream$site_index, tolerance = tol_deterministic)
  expect_equal(out$z_j, upstream$z_j, tolerance = tol_deterministic)
  expect_equal(out$tau_j, upstream$tau_j, tolerance = tol_deterministic)
  expect_equal(out$n_j, upstream$n_j, tolerance = tol_deterministic)
  expect_equal(out$se2_j, upstream$se2_j, tolerance = tol_deterministic)
  expect_equal(out$se_j, upstream$se_j, tolerance = tol_deterministic)
  expect_identical(attr(out, "engine", exact = TRUE), "A2_modern")
})

test_that("default Gaussian path matches the active RNG stream", {
  upstream <- observation_upstream()

  expected <- withr::with_seed(
    902L,
    stats::rnorm(nrow(upstream), mean = upstream$tau_j, sd = sqrt(upstream$se2_j))
  )
  out <- withr::with_seed(902L, gen_observations(upstream))

  expect_equal(out$tau_j_hat, expected, tolerance = tol_deterministic)
})

test_that("obs_fn receives response-scale tau_j and se2_j by name", {
  upstream <- observation_upstream(engine = "A1_legacy")
  seen <- new.env(parent = emptyenv())
  obs_fn <- function(tau_j, se2_j, offset = 0) {
    seen$tau_j <- tau_j
    seen$se2_j <- se2_j
    tau_j + sqrt(se2_j) + offset
  }

  set.seed(903L)
  before <- .Random.seed
  out <- gen_observations(upstream, obs_fn = obs_fn, offset = 0.5)
  after <- .Random.seed

  expect_identical(after, before)
  expect_equal(seen$tau_j, upstream$tau_j, tolerance = tol_deterministic)
  expect_equal(seen$se2_j, upstream$se2_j, tolerance = tol_deterministic)
  expect_equal(out$se2_j, upstream$se2_j, tolerance = tol_deterministic)
  expect_null(attr(out, "observation_permutation_perm", exact = TRUE))
  expect_equal(out$tau_j_hat, upstream$tau_j + sqrt(upstream$se2_j) + 0.5, tolerance = tol_deterministic)
})

test_that("obs_fn output validation implements E18", {
  upstream <- observation_upstream()

  expect_multisitedgp_error(
    gen_observations(upstream, obs_fn = function(tau_j, se2_j) tau_j[-1]),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    gen_observations(upstream, obs_fn = function(tau_j, se2_j) c(tau_j, 0)),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    gen_observations(upstream, obs_fn = function(tau_j, se2_j) rep(NA_real_, length(tau_j))),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    gen_observations(upstream, obs_fn = function(tau_j, se2_j) as.character(tau_j)),
    "multisitedgp_arg_error"
  )
})

test_that("gen_observations validates Layer 4 upstream inputs", {
  upstream <- observation_upstream()

  expect_multisitedgp_error(gen_observations(), "multisitedgp_arg_error")
  expect_multisitedgp_error(gen_observations(upstream[, setdiff(names(upstream), "se2_j")]), "multisitedgp_arg_error")
  expect_multisitedgp_error(gen_observations(dplyr::mutate(upstream, tau_j_hat = tau_j)), "multisitedgp_arg_error")
  expect_multisitedgp_error(gen_observations(dplyr::mutate(upstream, tau_j = NA_real_)), "multisitedgp_arg_error")
  expect_multisitedgp_error(gen_observations(dplyr::mutate(upstream, se2_j = -se2_j)), "multisitedgp_arg_error")
})

test_that("A1 legacy default path matches JEBS observation-component hashes", {
  manifest_file <- test_path("../../tools/jebs-golden-fixtures/jebs-golden-fixture-manifest.csv")
  skip_if_not(file.exists(manifest_file), "Step 4.1 JEBS fixture manifest is not shipped in the package tarball.")
  manifest <- read.csv(manifest_file, stringsAsFactors = FALSE)

  for (seed in c(42L, 1L, 2024L, 12345L)) {
    out <- withr::with_seed(seed, gen_observations(reference_jebs_upstream()))
    manifest_row <- manifest[manifest$seed == seed, ]
    raw_observed <- data.frame(
      n_j = as.numeric(out$n_j),
      se2_j = out$se2_j,
      tau_j = out$tau_j,
      tau_j_hat = out$tau_j_hat
    )
    normalized <- data.frame(
      site_index = out$site_index,
      z_j = out$z_j,
      tau_j = out$tau_j,
      n_j = as.integer(out$n_j),
      se_j = out$se_j,
      se2_j = out$se2_j,
      tau_j_hat = out$tau_j_hat
    )

    expect_equal(nrow(manifest_row), 1L)
    expect_identical(canonical_hash(raw_observed), manifest_row$component_observed_raw_hash)
    expect_equal(normalized$se_j, sqrt(normalized$se2_j), tolerance = tol_deterministic)
    expect_identical(length(attr(out, "observation_permutation_perm", exact = TRUE)), nrow(out))
  }
})
# nolint end
