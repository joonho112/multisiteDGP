observation_upstream <- function(J = 25L, engine = "truncated_gamma") {
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
    engine = "censored"
  )
}

test_that("gen_observations is exported and appends tau_j_hat", {
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
  expect_identical(attr(out, "engine", exact = TRUE), "truncated_gamma")
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
  upstream <- observation_upstream(engine = "censored")
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

test_that("obs_fn output is validated", {
  upstream <- observation_upstream()

  expect_multisitedgp_error(
    gen_observations(upstream, obs_fn = function(tau_j, se2_j) tau_j[-1])
  )
  expect_multisitedgp_error(
    gen_observations(upstream, obs_fn = function(tau_j, se2_j) c(tau_j, 0))
  )
  expect_multisitedgp_error(
    gen_observations(upstream, obs_fn = function(tau_j, se2_j) rep(NA_real_, length(tau_j)))
  )
  expect_multisitedgp_error(
    gen_observations(upstream, obs_fn = function(tau_j, se2_j) as.character(tau_j))
  )
})

test_that("gen_observations validates its upstream data", {
  upstream <- observation_upstream()

  expect_multisitedgp_error(gen_observations())
  expect_multisitedgp_error(gen_observations(upstream[, setdiff(names(upstream), "se2_j")]))
  expect_multisitedgp_error(gen_observations(dplyr::mutate(upstream, tau_j_hat = tau_j)))
  expect_multisitedgp_error(gen_observations(dplyr::mutate(upstream, tau_j = NA_real_)))
  expect_multisitedgp_error(gen_observations(dplyr::mutate(upstream, se2_j = -se2_j)))
})

test_that("observed estimates reproduce the JEBS appendix with the censored site-size generator", {
  expected <- list(
    list(
      seed = 42L, sum_n = 7263L, head = c(0.249583801676, -0.0452138825603, 0.0386598851721),
      moments = c(0.01119308716689, 0.16999140787187, 0.00836756180938, 0.31923956892019, 0.07178314353686)
    ),
    list(
      seed = 1L, sum_n = 7845L, head = c(0.0265879791456, -0.2129543822617, -0.5096375292807),
      moments = c(0.002634301350229, 0.153212131448378, -0.000790780554044, 0.301090833984576, 0.073603709370388)
    ),
    list(
      seed = 2024L, sum_n = 7654L, head = c(0.279778928263, -0.559799944162, -0.211765164188),
      moments = c(-0.01232733424599, 0.14008489580989, 0.00179934499965, 0.30306437909533, 0.07126056949744)
    ),
    list(
      seed = 12345L, sum_n = 7737L, head = c(-0.163002502754, 0.119340473381, -0.13650938603),
      moments = c(0.0263867719543, 0.1552696566227, 0.0329442846227, 0.2747212661535, 0.0690807511832)
    )
  )
  for (case in expected) {
    out <- withr::with_seed(case$seed, gen_observations(reference_jebs_upstream()))
    label <- sprintf("seed = %d", case$seed)

    expect_identical(sum(out$n_j), case$sum_n, info = label)
    expect_equal(out$tau_j_hat[1:3], case$head, tolerance = 1e-8, info = label)
    expect_equal(
      c(mean(out$tau_j), stats::sd(out$tau_j), mean(out$tau_j_hat), stats::sd(out$tau_j_hat), mean(out$se2_j)),
      case$moments,
      tolerance = 1e-8,
      info = label
    )
    expect_equal(out$se_j, sqrt(out$se2_j), tolerance = tol_deterministic)
    expect_identical(length(attr(out, "observation_permutation_perm", exact = TRUE)), nrow(out))
  }
})

test_that("only the censored site-size generator shuffles precisions before the Gaussian draw", {
  effects <- withr::with_seed(455L, gen_effects_gaussian(J = 20L))
  censored <- withr::with_seed(456L, gen_site_sizes(
    effects,
    J = 20L,
    nj_mean = 40,
    cv = 0.5,
    engine = "censored"
  ))

  set.seed(457L)
  observed <- gen_observations(censored)
  observed_after <- .Random.seed
  set.seed(457L)
  expected_perm <- sample.int(nrow(censored), size = nrow(censored))
  expected_hat <- stats::rnorm(
    nrow(censored),
    mean = censored$tau_j,
    sd = sqrt(censored$se2_j[expected_perm])
  )
  expected_after <- .Random.seed

  expect_identical(
    attr(observed, "observation_permutation_perm", exact = TRUE),
    expected_perm
  )
  expect_identical(observed$se2_j, censored$se2_j[expected_perm])
  expect_identical(observed$tau_j_hat, expected_hat)
  expect_identical(observed_after, expected_after)

  truncated <- withr::with_seed(458L, gen_site_sizes(
    effects,
    J = 20L,
    nj_mean = 40,
    cv = 0.5,
    engine = "truncated_gamma"
  ))
  set.seed(459L)
  unshuffled <- gen_observations(truncated)
  unshuffled_after <- .Random.seed
  set.seed(459L)
  unshuffled_expected <- stats::rnorm(nrow(truncated), mean = truncated$tau_j, sd = sqrt(truncated$se2_j))

  expect_null(attr(unshuffled, "observation_permutation_perm", exact = TRUE))
  expect_identical(unshuffled$se2_j, truncated$se2_j)
  expect_identical(unshuffled$tau_j_hat, unshuffled_expected)
  expect_identical(unshuffled_after, .Random.seed)
})

test_that("gen_observations stops before the shuffle when a censored input has been aligned", {
  upstream <- observation_upstream(J = 20L, engine = "censored")
  reversed <- function(z_j, se2_j, target) {
    perm <- rev(seq_along(se2_j))
    list(perm = perm, se2_j = se2_j[perm])
  }
  # A target of 0 counts as an alignment too: the function was called.
  alignments <- list(
    rank = function(x) align_rank_corr(x, rank_corr = 0.6),
    copula = function(x) align_copula_corr(x, pearson_corr = 0.4),
    hybrid = function(x) align_hybrid_corr(x, rank_corr = 0.6, init = "rank"),
    rank_zero = function(x) align_rank_corr(x, rank_corr = 0),
    copula_zero = function(x) align_copula_corr(x, pearson_corr = 0),
    hybrid_zero = function(x) align_hybrid_corr(x, rank_corr = 0),
    user_supplied = function(x) align_rank_corr(x, rank_corr = 0.6, dependence_fn = reversed)
  )
  for (align in alignments) {
    aligned <- withr::with_seed(31L, align(upstream))
    expect_rng_neutral_error(gen_observations(aligned), regexp = "censored")
  }
})

test_that("aligned standard errors keep their order when gen_observations does not shuffle", {
  # A user-supplied obs_fn never shuffles, whichever generator made the sizes.
  for (engine in c("censored", "truncated_gamma")) {
    aligned <- align_rank_corr(observation_upstream(J = 20L, engine = engine), rank_corr = 0.6)
    out <- gen_observations(aligned, obs_fn = function(tau_j, se2_j) tau_j)
    expect_identical(out$se2_j, aligned$se2_j)
    expect_identical(out$tau_j_hat, aligned$tau_j)
    expect_identical(attr(out, "dependence_diagnostics"), attr(aligned, "dependence_diagnostics"))
    expect_false(attr(out, "observation_diagnostics")$precision_shuffled)
  }

  aligned <- align_rank_corr(observation_upstream(J = 20L), rank_corr = 0.6)
  out <- withr::with_seed(32L, gen_observations(aligned))
  expect_identical(out$se2_j, aligned$se2_j)
  expect_false(attr(out, "observation_diagnostics")$precision_shuffled)
})
