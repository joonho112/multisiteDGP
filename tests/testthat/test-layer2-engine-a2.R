# nolint start: object_usage_linter
a2_engine <- function(...) {
  multisitedgp_internal("engine_trunc_gamma_moment")(...)
}

a2_solve <- function(...) {
  multisitedgp_internal("solve_trunc_gamma")(...)
}

a2_moments <- function(...) {
  multisitedgp_internal("trunc_gamma_moments")(...)
}

a2_sampler <- function(...) {
  multisitedgp_internal("rtrunc_gamma")(...)
}

test_that("Engine A2 solver matches truncated-Gamma conditional moments analytically", {
  cases <- list(
    list(n_bar = 50, cv = 0.50, n_min = 5L),
    list(n_bar = 8, cv = 0.10, n_min = 5L),
    list(n_bar = 200, cv = 1.50, n_min = 5L)
  )

  for (case in cases) {
    solution <- suppressWarnings(a2_solve(
      n_bar = case$n_bar,
      cv = case$cv,
      n_min = case$n_min
    ))
    moments <- a2_moments(solution$alpha, solution$beta, case$n_min)

    expect_true(is.finite(solution$alpha))
    expect_true(is.finite(solution$beta))
    expect_gt(solution$alpha, 0)
    expect_gt(solution$beta, 0)
    expect_equal(moments$mean, case$n_bar, tolerance = 1e-6)
    expect_equal(moments$sd, case$cv * case$n_bar, tolerance = 1e-6)
  }
})

test_that("Engine A2 accepts tiny positive CV without a false feasibility floor", {
  solution <- suppressWarnings(a2_solve(n_bar = 6, cv = 0.01, n_min = 5L))
  moments <- a2_moments(solution$alpha, solution$beta, 5L)

  expect_equal(moments$mean, 6, tolerance = 1e-6)
  expect_equal(moments$sd, 0.06, tolerance = 1e-6)
})

test_that("Engine A2 sampler respects lower truncation and is reproducible", {
  solution <- a2_solve(n_bar = 50, cv = 0.50, n_min = 5L)

  draws_1 <- withr::with_seed(704L, a2_sampler(100000L, solution$alpha, solution$beta, 5L))
  draws_2 <- withr::with_seed(704L, a2_sampler(100000L, solution$alpha, solution$beta, 5L))

  expect_identical(draws_1, draws_2)
  expect_true(all(draws_1 >= 5L))
  expect_lt(abs(mean(draws_1) - 50) / 50, tol_mc_moment_n1e5)
  expect_lt(abs(stats::sd(draws_1) / mean(draws_1) - 0.50), tol_mc_moment_n1e5)
})

test_that("Engine A2 deterministic CV zero path consumes no RNG", {
  set.seed(705L)
  before <- .Random.seed
  out <- a2_engine(J = 25L, nj_mean = 50, cv = 0, nj_min = 5L)
  after <- .Random.seed

  expect_s3_class(out, "tbl_df")
  expect_identical(names(out), "n_j")
  expect_equal(out$n_j, rep(50, 25L), tolerance = tol_deterministic)
  expect_identical(before, after)
})

test_that("Engine A2 conditioning warnings are soft guidance", {
  warn_conditioning <- multisitedgp_internal(".warn_trunc_gamma_conditioning")

  expect_warning(warn_conditioning(n_bar = 50, cv = 0.0005, n_min = 5L), "very small positive")
  expect_warning(warn_conditioning(n_bar = 50, cv = 1.6, n_min = 5L), "large")
  expect_warning(warn_conditioning(n_bar = 6, cv = 0.5, n_min = 5L), "close to the lower")
})

test_that("Engine A2 accepts mocked solver term code when residuals verify", {
  exact <- suppressWarnings(a2_solve(n_bar = 50, cv = 0.5, n_min = 5L))

  testthat::local_mocked_bindings(
    .nleqslv_solve = function(...) {
      list(
        x = log(c(exact$alpha, exact$beta)),
        termcd = 5L,
        message = "mocked residual-verified non-success term code"
      )
    },
    .package = "multisiteDGP"
  )
  solution <- a2_solve(n_bar = 50, cv = 0.5, n_min = 5L)

  expect_equal(solution$mean, 50, tolerance = 1e-6)
  expect_equal(solution$sd, 25, tolerance = 1e-6)
  expect_identical(solution$termcd, 5L)
  expect_equal(solution$residual, c(mean = 0, sd = 0), tolerance = 1e-6)
})

test_that("Engine A2 solver failures use solver error class and never fallback silently", {
  expect_multisitedgp_error(
    suppressWarnings(a2_solve(n_bar = 6, cv = 1.8, n_min = 5L)),
    "multisitedgp_solver_error"
  )

  testthat::local_mocked_bindings(
    .nleqslv_solve = function(...) {
      list(
        x = log(c(4, 0.08)),
        termcd = 5L,
        message = "mocked no progress"
      )
    },
    .package = "multisiteDGP"
  )
  expect_multisitedgp_error(
    a2_solve(n_bar = 50, cv = 0.5, n_min = 5L),
    "multisitedgp_solver_error"
  )
})

test_that("Engine A2 validates internal inputs", {
  expect_multisitedgp_error(
    a2_engine(J = 5L, nj_mean = 50, cv = 0.5, nj_min = 5L),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    a2_solve(n_bar = 5, cv = 0.5, n_min = 5L),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    a2_solve(n_bar = 50, cv = -0.5, n_min = 5L),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    a2_sampler(0L, alpha = 1, beta = 1, n_min = 5L),
    "multisitedgp_arg_error"
  )
})
# nolint end
