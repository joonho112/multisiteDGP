site_size_upstream <- function(J = 25L) {
  tibble::tibble(
    site_index = seq_len(J),
    z_j = rep(0, J),
    tau_j = rep(0, J)
  )
}

# The censored site-size draw written out directly: a gamma draw, censored
# below at nj_min, then rounded.
reference_censored_n_j <- function(J, nj_mean, cv, nj_min) {
  if (identical(cv, 0) || isTRUE(all.equal(cv, 0))) {
    return(rep(as.integer(nj_mean), J))
  }
  shape <- 1 / cv^2
  rate <- 1 / (cv^2 * nj_mean)
  as.integer(round(pmax(nj_min, stats::rgamma(n = J, shape = shape, rate = rate)), 0))
}

censored_site_sizes <- function(...) {
  multisitedgp_internal(".site_sizes_censored")(...)
}

# Draws the site effects of preset_jebs_strict() and discards them, which
# leaves the RNG where the site-size draw of that design starts.
consume_mixture_stream <- function() {
  gen_effects_mixture(
    J = 100L,
    tau = 0,
    sigma_tau = 0.15,
    delta = 5,
    eps = 0.3,
    ups = 2
  )
  invisible(NULL)
}

truncated_gamma_site_sizes <- function(...) {
  multisitedgp_internal(".site_sizes_truncated_gamma")(...)
}

solve_truncated_gamma <- function(...) {
  multisitedgp_internal(".solve_truncated_gamma")(...)
}

truncated_gamma_moments <- function(...) {
  multisitedgp_internal(".trunc_gamma_moments")(...)
}

draw_truncated_gamma <- function(...) {
  multisitedgp_internal(".rtrunc_gamma")(...)
}

# An expression for trace() that appends `label` to a log of RNG calls kept in
# the global environment.
record_rng_call <- function(label) {
  substitute(
    assign(
      ".rng_calls",
      c(get(".rng_calls", envir = .GlobalEnv), value),
      envir = .GlobalEnv
    ),
    list(value = label)
  )
}

test_that("gen_site_sizes is exported and appends n_j, se2_j and se_j", {
  expect_true("gen_site_sizes" %in% getNamespaceExports("multisiteDGP"))

  upstream <- site_size_upstream()
  out <- withr::with_seed(
    801L,
    gen_site_sizes(upstream, J = 25L, nj_mean = 50, cv = 0.5, nj_min = 5L)
  )

  expect_s3_class(out, "tbl_df")
  expect_named(out, c("site_index", "z_j", "tau_j", "n_j", "se2_j", "se_j"))
  expect_true(is.integer(out$n_j))
  expect_true(is.numeric(out$se2_j))
  expect_true(is.numeric(out$se_j))
  expect_true(all(out$n_j >= 5L))
  expect_equal(out$se_j, sqrt(out$se2_j), tolerance = tol_deterministic)
  expect_identical(attr(out, "engine", exact = TRUE), "truncated_gamma")
  expect_identical(attr(out, "kappa", exact = TRUE), compute_kappa())
})

test_that("gen_site_sizes keeps se2_j times n_j equal to kappa for both generators", {
  upstream <- site_size_upstream()

  truncated <- withr::with_seed(
    802L,
    gen_site_sizes(upstream, J = 25L, nj_mean = 50, cv = 0.5, nj_min = 5L, engine = "truncated_gamma")
  )
  censored <- withr::with_seed(
    802L,
    gen_site_sizes(upstream, J = 25L, nj_mean = 50, cv = 0.5, nj_min = 5L, engine = "censored")
  )

  expect_equal(truncated$se2_j * truncated$n_j, rep(compute_kappa(), 25L), tolerance = tol_deterministic)
  expect_equal(censored$se2_j * censored$n_j, rep(compute_kappa(), 25L), tolerance = tol_deterministic)
  expect_true(all(truncated$n_j >= 5L))
  expect_true(all(censored$n_j >= 5L))
})

test_that("gen_site_sizes computes kappa from p, R2, and outcome variance", {
  upstream <- site_size_upstream()
  out <- withr::with_seed(
    804L,
    gen_site_sizes(
      upstream,
      J = 25L,
      nj_mean = 50,
      cv = 0.5,
      nj_min = 5L,
      p = 0.25,
      R2 = 0.2,
      var_outcome = 2,
      engine = "censored"
    )
  )
  kappa <- compute_kappa(p = 0.25, R2 = 0.2, var_outcome = 2)

  expect_identical(attr(out, "kappa", exact = TRUE), kappa)
  expect_equal(out$se2_j * out$n_j, rep(kappa, 25L), tolerance = tol_deterministic)
})

test_that("gen_site_sizes with the censored generator draws the site sizes of the JEBS code", {
  upstream <- site_size_upstream()

  actual <- withr::with_seed(
    803L,
    gen_site_sizes(
      upstream,
      J = 25L,
      nj_mean = 50,
      cv = 0.5,
      nj_min = 5L,
      engine = "censored"
    )$n_j
  )
  expected <- withr::with_seed(803L, reference_censored_n_j(25L, 50, 0.5, 5L))

  expect_identical(actual, expected)
})

test_that("gen_site_sizes with the truncated-gamma generator is reproducible under a fixed seed", {
  upstream <- site_size_upstream()

  out_1 <- withr::with_seed(
    805L,
    gen_site_sizes(upstream, J = 25L, nj_mean = 50, cv = 0.5, nj_min = 5L, engine = "truncated_gamma")
  )
  out_2 <- withr::with_seed(
    805L,
    gen_site_sizes(upstream, J = 25L, nj_mean = 50, cv = 0.5, nj_min = 5L, engine = "truncated_gamma")
  )

  expect_identical(out_1, out_2)
})

test_that("stochastic rounding in the truncated-gamma generator preserves the mean site size", {
  upstream <- site_size_upstream(100000L)

  out <- withr::with_seed(
    806L,
    suppressWarnings(gen_site_sizes(
      upstream,
      J = 100000L,
      nj_mean = 50,
      cv = 0.5,
      nj_min = 5L,
      engine = "truncated_gamma"
    ))
  )

  expect_lt(abs(mean(out$n_j) - 50) / 50, tol_mc_moment_n1e5)
  expect_true(all(out$n_j >= 5L))
  expect_true(is.integer(out$n_j))
})

test_that("stochastic rounding draws one uniform per non-integer size and none for integers", {
  stochastic_round <- multisitedgp_internal(".stochastic_round_site_sizes")

  set.seed(808L)
  before <- .Random.seed
  expect_identical(stochastic_round(c(10, 20, 30)), c(10L, 20L, 30L))
  after_integer <- .Random.seed
  expect_identical(after_integer, before)

  set.seed(809L)
  out <- stochastic_round(c(10.25, 20, 30.75))
  after_round <- .Random.seed
  set.seed(809L)
  uniforms <- stats::runif(2L)
  after_two_uniforms <- .Random.seed
  expected <- c(
    10L + as.integer(uniforms[[1L]] < 0.25),
    20L,
    30L + as.integer(uniforms[[2L]] < 0.75)
  )

  expect_identical(out, expected)
  expect_identical(after_round, after_two_uniforms)
})

test_that("gen_site_sizes CV zero path is deterministic and RNG neutral", {
  upstream <- site_size_upstream()

  set.seed(807L)
  before <- .Random.seed
  out <- gen_site_sizes(upstream, J = 25L, nj_mean = 50, cv = 0, nj_min = 5L, engine = "truncated_gamma")
  after <- .Random.seed

  expect_identical(out$n_j, rep(50L, 25L))
  expect_equal(out$se2_j * out$n_j, rep(compute_kappa(), 25L), tolerance = tol_deterministic)
  expect_identical(before, after)
})

test_that("gen_site_sizes validates its inputs", {
  upstream <- site_size_upstream()

  expect_multisitedgp_error(
    gen_site_sizes(J = 25L)
  )
  expect_multisitedgp_error(
    gen_site_sizes(upstream)
  )
  expect_multisitedgp_error(
    gen_site_sizes(1:25, J = 25L)
  )
  expect_multisitedgp_error(
    gen_site_sizes(upstream, J = 24L)
  )
  expect_multisitedgp_error(
    gen_site_sizes(upstream["site_index"], J = 25L)
  )
  expect_multisitedgp_error(
    gen_site_sizes(dplyr::mutate(upstream, se_j = 1), J = 25L)
  )
  expect_multisitedgp_error(
    gen_site_sizes(upstream, J = 25L, engine = "bad")
  )
  expect_multisitedgp_error(
    gen_site_sizes(upstream, J = 25L, cv = -0.5)
  )
  expect_multisitedgp_error(
    gen_site_sizes(upstream, J = 25L, nj_mean = 50.5, cv = 0)
  )
  expect_multisitedgp_error(
    gen_site_sizes(upstream, J = 25L, p = 0)
  )
  expect_multisitedgp_error(
    gen_site_sizes(upstream, J = 25L, R2 = 1)
  )
  expect_multisitedgp_error(
    gen_site_sizes(upstream, J = 25L, var_outcome = 0)
  )

  materialize_n_j <- multisitedgp_internal(".materialize_public_n_j")
  expect_multisitedgp_error(
    materialize_n_j(c(10, NA), nj_min = 5L, engine = "truncated_gamma")
  )
  expect_multisitedgp_error(
    suppressWarnings(materialize_n_j(c(10, .Machine$integer.max * 2), nj_min = 5L, engine = "truncated_gamma"))
  )
})

test_that("gen_site_sizes passes on the truncated-gamma solver error", {
  upstream <- site_size_upstream()

  expect_multisitedgp_error(
    suppressWarnings(gen_site_sizes(
      upstream,
      J = 25L,
      nj_mean = 6,
      cv = 1.8,
      nj_min = 5L,
      engine = "truncated_gamma"
    )),
    "multisitedgp_solver_error"
  )
})

test_that("the censored site-size generator reproduces the JEBS appendix site sizes after the mixture draws", {
  expected <- list(
    list(seed = 42L, sum_n = 7263L, range = c(16L, 204L), head = c(35L, 107L, 124L, 36L, 37L)),
    list(seed = 1L, sum_n = 7845L, range = c(7L, 204L), head = c(75L, 86L, 67L, 114L, 73L)),
    list(seed = 2024L, sum_n = 7654L, range = c(6L, 191L), head = c(150L, 77L, 114L, 60L, 115L)),
    list(seed = 12345L, sum_n = 7737L, range = c(10L, 198L), head = c(43L, 138L, 116L, 84L, 97L))
  )
  for (case in expected) {
    actual <- withr::with_seed(case$seed, {
      consume_mixture_stream()
      censored_site_sizes(J = 100L, nj_mean = 80, cv = 0.50, nj_min = 4L)$n_j
    })
    label <- sprintf("seed = %d", case$seed)
    expect_identical(sum(actual), case$sum_n, info = label)
    expect_identical(range(actual), case$range, info = label)
    expect_identical(actual[1:5], case$head, info = label)
  }
})

test_that("the censored site-size generator returns integer n_j of at least nj_min", {
  out <- withr::with_seed(701L, censored_site_sizes(J = 100L, nj_mean = 80, cv = 0.50, nj_min = 4L))
  se2_j <- compute_kappa() / out$n_j

  expect_s3_class(out, "tbl_df")
  expect_identical(names(out), "n_j")
  expect_true(is.integer(out$n_j))
  expect_true(all(out$n_j >= 4L))
  expect_equal(se2_j * out$n_j, rep(compute_kappa(), nrow(out)), tolerance = tol_deterministic)
})

test_that("the censored site-size generator with cv = 0 is deterministic and leaves the RNG untouched", {
  set.seed(702L)
  before <- .Random.seed
  out <- censored_site_sizes(J = 25L, nj_mean = 50, cv = 0, nj_min = 5L)
  after <- .Random.seed

  expect_identical(out$n_j, rep(50L, 25L))
  expect_identical(before, after)
})

test_that("the censored site-size generator makes exactly one rgamma call when cv is positive", {
  generator_seed <- withr::with_seed(703L, {
    censored_site_sizes(J = 25L, nj_mean = 50, cv = 0.5, nj_min = 5L)
    .Random.seed
  })
  reference_seed <- withr::with_seed(703L, {
    reference_censored_n_j(J = 25L, nj_mean = 50, cv = 0.5, nj_min = 5L)
    .Random.seed
  })

  expect_identical(generator_seed, reference_seed)
})

test_that("the censored site-size generator validates its inputs", {
  expect_multisitedgp_error(
    censored_site_sizes(J = 5L, nj_mean = 50, cv = 0.5, nj_min = 5L)
  )
  expect_multisitedgp_error(
    censored_site_sizes(J = 25L, nj_mean = 5, cv = 0.5, nj_min = 5L)
  )
  expect_multisitedgp_error(
    censored_site_sizes(J = 25L, nj_mean = 50, cv = -0.5, nj_min = 5L)
  )
  expect_multisitedgp_error(
    censored_site_sizes(J = 25L, nj_mean = 50.5, cv = 0, nj_min = 5L)
  )
})

test_that("the truncated-gamma site-size generator with cv = 0 draws no random numbers", {
  set.seed(705L)
  before <- .Random.seed
  out <- truncated_gamma_site_sizes(J = 25L, nj_mean = 50, cv = 0, nj_min = 5L)
  after <- .Random.seed

  expect_s3_class(out, "tbl_df")
  expect_identical(names(out), "n_j")
  expect_equal(out$n_j, rep(50, 25L), tolerance = tol_deterministic)
  expect_identical(before, after)
})

test_that("cv = 0 produces constant site sizes end to end", {
  n_j <- .site_sizes_truncated_gamma(J = 20L, nj_mean = 100, cv = 0)$n_j

  expect_length(n_j, 20L)
  expect_true(all(n_j == 100))
})

test_that("the truncated-gamma site-size generator warns on a large cv or a mean near the lower bound", {
  warn_conditioning <- multisitedgp_internal(".warn_trunc_gamma_conditioning")

  expect_silent(warn_conditioning(n_bar = 50, cv = 0.0005, n_min = 5L))
  expect_warning(warn_conditioning(n_bar = 50, cv = 1.6, n_min = 5L), "`cv` is above 1.5")
  expect_warning(warn_conditioning(n_bar = 6, cv = 0.5, n_min = 5L), "less than twice `nj_min`")
})

test_that("the truncated-gamma site-size generator validates its inputs", {
  expect_multisitedgp_error(
    truncated_gamma_site_sizes(J = 5L, nj_mean = 50, cv = 0.5, nj_min = 5L)
  )
  expect_multisitedgp_error(
    solve_truncated_gamma(n_bar = 5, cv = 0.5, n_min = 5L)
  )
  expect_multisitedgp_error(
    solve_truncated_gamma(n_bar = 50, cv = -0.5, n_min = 5L)
  )
  expect_multisitedgp_error(
    draw_truncated_gamma(0L, alpha = 1, beta = 1, n_min = 5L)
  )
})

test_that(".site_sizes_truncated_gamma rejects a negative cv", {
  expect_error(
    .site_sizes_truncated_gamma(J = 20L, nj_mean = 100, cv = -0.1),
    class = "multisitedgp_error"
  )
})

test_that("the truncated-gamma solver matches the target mean and sd", {
  cases <- list(
    list(n_bar = 50, cv = 0.50, n_min = 5L),
    list(n_bar = 8, cv = 0.10, n_min = 5L),
    list(n_bar = 200, cv = 1.50, n_min = 5L)
  )

  for (case in cases) {
    solution <- suppressWarnings(solve_truncated_gamma(
      n_bar = case$n_bar,
      cv = case$cv,
      n_min = case$n_min
    ))
    moments <- truncated_gamma_moments(solution$alpha, solution$beta, case$n_min)

    expect_true(is.finite(solution$alpha))
    expect_true(is.finite(solution$beta))
    expect_gt(solution$alpha, 0)
    expect_gt(solution$beta, 0)
    expect_equal(moments$mean, case$n_bar, tolerance = 1e-6)
    expect_equal(moments$sd, case$cv * case$n_bar, tolerance = 1e-6)
  }
})

test_that("the truncated-gamma solver accepts a tiny positive cv", {
  solution <- suppressWarnings(solve_truncated_gamma(n_bar = 6, cv = 0.01, n_min = 5L))
  moments <- truncated_gamma_moments(solution$alpha, solution$beta, 5L)

  expect_equal(moments$mean, 6, tolerance = 1e-6)
  expect_equal(moments$sd, 0.06, tolerance = 1e-6)
})

test_that("the truncated-gamma solver verifies both moments at cv = 1e-5", {
  solution <- solve_truncated_gamma(n_bar = 100, cv = 1e-5, n_min = 5L)
  moments <- truncated_gamma_moments(solution$alpha, solution$beta, 5L)

  expect_equal(moments$mean, 100, tolerance = 1e-8)
  expect_equal(moments$sd, 0.001, tolerance = 1e-8)
  expect_lte(abs(solution$residual[["mean"]]), solution$tol_effective[["mean"]])
  expect_lte(abs(solution$residual[["sd"]]), solution$tol_effective[["sd"]])
  expect_identical(solution$tol_effective[["mean"]], 1e-6)
})

test_that("stable recurrence handles log-spaced tiny CV values", {
  cvs <- 10^seq(-5, -1, length.out = 5L)
  for (cv in cvs) {
    out <- .solve_truncated_gamma(n_bar = 100, cv = cv, n_min = 5L)
    moments <- .trunc_gamma_moments(out$alpha, out$beta, 5L)

    expect_equal(moments$mean, 100, tolerance = 1e-6, info = sprintf("cv=%g mean", cv))
    expect_equal(moments$sd, 100 * cv, tolerance = 1e-6, info = sprintf("cv=%g sd", cv))
    expect_identical(out$tol_effective[["mean"]], 1e-6)
    expect_lte(out$tol_effective[["sd"]], 1e-3)
  }
})

test_that("the solver gives no warning at small cv", {
  expect_silent(.solve_truncated_gamma(n_bar = 100, cv = 0.001, n_min = 5L))
  expect_silent(.solve_truncated_gamma(n_bar = 100, cv = 0.005, n_min = 5L))
  expect_silent(.solve_truncated_gamma(n_bar = 100, cv = 0.05, n_min = 5L))
})

test_that(".solve_truncated_gamma succeeds well inside the feasible region", {
  # cv = 0.05 is well inside the region where the solver converges. Closer to
  # the boundary (cv = 0.005) the largest residual depends on the platform:
  # 7.44e-07 on macOS and 1.91e-06 on Linux, against a tolerance of 1e-06.
  out <- .solve_truncated_gamma(n_bar = 100, cv = 0.05, n_min = 5L)

  expect_true(is.finite(out$alpha))
  expect_true(is.finite(out$beta))
  expect_lt(max(abs(out$residual)), 1e-6)
})

test_that("the solver aborts on the measured infeasible region", {
  # The corner with a small mean and a large cv, where the solver finds no
  # verified solution.
  infeasible <- list(
    list(nj_mean = 10, cv = 0.6),
    list(nj_mean = 10, cv = 1.0),
    list(nj_mean = 25, cv = 1.3)
  )
  for (case in infeasible) {
    expect_error(
      .solve_truncated_gamma(n_bar = case$nj_mean, cv = case$cv, n_min = 5L),
      class = "multisitedgp_error",
      info = sprintf("n_bar = %s, cv = %s", case$nj_mean, case$cv)
    )
  }
})

test_that("the solver succeeds just inside the feasible boundary", {
  # Cells next to the infeasible ones on the same grid. They solve, so the
  # solver does not fail indiscriminately.
  feasible <- list(
    list(nj_mean = 10, cv = 0.4),
    list(nj_mean = 25, cv = 1.0),
    list(nj_mean = 50, cv = 1.3)
  )
  for (case in feasible) {
    out <- .solve_truncated_gamma(n_bar = case$nj_mean, cv = case$cv, n_min = 5L)
    expect_true(is.finite(out$alpha),
                info = sprintf("n_bar = %s, cv = %s", case$nj_mean, case$cv))
    expect_true(is.finite(out$beta))
    expect_lt(max(abs(out$residual)), 1e-6)
  }
})

test_that("cv = 0 returns the degenerate solution without calling the solver", {
  out <- .solve_truncated_gamma(n_bar = 100, cv = 0, n_min = 5L)

  expect_identical(out$alpha, Inf)
  expect_identical(out$beta, Inf)
  expect_identical(out$mean, 100)
  expect_identical(out$sd, 0)
  expect_identical(out$cv, 0)
  expect_identical(unname(out$residual), c(0, 0))
  expect_identical(out$message, "deterministic")
})

test_that("truncated-gamma solver failures raise the solver error and never fall back silently", {
  expect_multisitedgp_error(
    suppressWarnings(solve_truncated_gamma(n_bar = 6, cv = 1.8, n_min = 5L)),
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
    solve_truncated_gamma(n_bar = 50, cv = 0.5, n_min = 5L),
    "multisitedgp_solver_error"
  )
})

test_that("the truncated-gamma solver does not apply the sd tolerance to the mean", {
  # The mocked solution has mean 5 and sd 0.001: the sd is on target and the
  # mean is 95% off. The looser sd tolerance must not be applied to the mean.
  testthat::local_mocked_bindings(
    .nleqslv_solve = function(...) {
      list(
        x = log(c(2.5e7, 5e6)),
        termcd = 1L,
        message = "mocked bad mean / correct SD"
      )
    },
    .package = "multisiteDGP"
  )

  expect_multisitedgp_error(
    solve_truncated_gamma(n_bar = 100, cv = 1e-5, n_min = 5L),
    "multisitedgp_solver_error"
  )
})

test_that("the truncated-gamma solver accepts a non-success termination code when the residuals verify", {
  exact <- suppressWarnings(solve_truncated_gamma(n_bar = 50, cv = 0.5, n_min = 5L))

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
  solution <- solve_truncated_gamma(n_bar = 50, cv = 0.5, n_min = 5L)

  expect_equal(solution$mean, 50, tolerance = 1e-6)
  expect_equal(solution$sd, 25, tolerance = 1e-6)
  expect_identical(solution$termcd, 5L)
  expect_equal(solution$residual, c(mean = 0, sd = 0), tolerance = 1e-6)
})

test_that(".solve_truncated_gamma requires n_bar to exceed n_min", {
  expect_error(.solve_truncated_gamma(n_bar = 5, cv = 0.4, n_min = 5L),
               class = "multisitedgp_error")
  expect_error(.solve_truncated_gamma(n_bar = 100, cv = 0.4, n_min = 0L),
               class = "multisitedgp_error")
})

test_that(".solve_truncated_gamma rejects a negative cv", {
  expect_error(.solve_truncated_gamma(n_bar = 100, cv = -0.1, n_min = 5L),
               class = "multisitedgp_error")
})

test_that(".solve_truncated_gamma rejects a non-positive tol", {
  expect_error(
    .solve_truncated_gamma(n_bar = 100, cv = 0.4, n_min = 5L, tol = 0),
    class = "multisitedgp_error"
  )
  expect_error(
    .solve_truncated_gamma(n_bar = 100, cv = 0.4, n_min = 5L, tol = -1e-6),
    class = "multisitedgp_error"
  )
})

test_that(".solve_truncated_gamma requires at least one solver start", {
  expect_error(
    .solve_truncated_gamma(n_bar = 100, cv = 0.4, n_min = 5L, max_starts = 0L),
    class = "multisitedgp_error"
  )
})

test_that(".solve_truncated_gamma requires a positive iteration budget", {
  expect_error(
    .solve_truncated_gamma(n_bar = 100, cv = 0.4, n_min = 5L, max_iter = 0L),
    class = "multisitedgp_error"
  )
})

test_that(".trunc_gamma_residual returns the sentinel for invalid parameters", {
  sentinel <- c(mean = .Machine$double.xmax, sd = .Machine$double.xmax)

  # exp(log_par) underflows to 0 here, which takes the alpha <= 0 branch.
  expect_identical(
    .trunc_gamma_residual(c(-1e6, -1e6), n_bar = 100, cv = 0.4, n_min = 5L),
    sentinel
  )
  # exp(log_par) overflows to Inf here, which gets the same sentinel.
  expect_identical(
    .trunc_gamma_residual(c(1e6, 1e6), n_bar = 100, cv = 0.4, n_min = 5L),
    sentinel
  )
})

test_that(".fit_trunc_gamma_start reports a failed fit instead of propagating", {
  failed <- .failed_trunc_gamma_fit(message = "synthetic failure", start_id = 3L)

  expect_false(failed$verified)
  expect_identical(failed$start, 3L)
  expect_identical(failed$message, "synthetic failure")
  expect_true(is.na(failed$alpha))
  expect_identical(failed$residual_norm, Inf)
})

test_that("stable recurrence agrees with raw incomplete-Gamma moments when conditioned", {
  alpha <- 6
  beta <- 0.06
  n_min <- 5L
  moments <- .trunc_gamma_moments(alpha, beta, n_min)
  # k-th raw moment of a Gamma(alpha, beta) truncated below n_min, from the
  # ratio of two upper incomplete-Gamma tails.
  raw_moment <- function(k) {
    log_tail <- stats::pgamma(n_min, shape = alpha, rate = beta, lower.tail = FALSE, log.p = TRUE)
    log_shifted_tail <- stats::pgamma(n_min, shape = alpha + k, rate = beta, lower.tail = FALSE, log.p = TRUE)
    exp(lgamma(alpha + k) - lgamma(alpha) - k * log(beta) + log_shifted_tail - log_tail)
  }
  raw_first <- raw_moment(1)
  raw_second <- raw_moment(2)

  expect_equal(moments$mean, raw_first, tolerance = 1e-12)
  expect_equal(moments$variance, raw_second - raw_first^2, tolerance = 1e-10)
})

test_that("unverifiable recurrence configurations report non-finite precision", {
  moments <- .trunc_gamma_moments(alpha = 1e3, beta = 2e5, n_min = 5L)
  expect_false(is.finite(moments$sd_relative_error_bound))
})

test_that(".trunc_gamma_moments rejects a non-positive n_min", {
  expect_error(
    .trunc_gamma_moments(alpha = 2, beta = 0.02, n_min = 0L),
    class = "multisitedgp_error"
  )
})

test_that(".trunc_gamma_moments rejects non-positive shape or rate", {
  expect_error(.trunc_gamma_moments(alpha = 0, beta = 0.02, n_min = 5L),
               class = "multisitedgp_error")
  expect_error(.trunc_gamma_moments(alpha = 2, beta = -1, n_min = 5L),
               class = "multisitedgp_error")
})

test_that("the truncated-gamma sampler respects the lower bound and is reproducible", {
  solution <- solve_truncated_gamma(n_bar = 50, cv = 0.50, n_min = 5L)

  draws_1 <- withr::with_seed(704L, draw_truncated_gamma(100000L, solution$alpha, solution$beta, 5L))
  draws_2 <- withr::with_seed(704L, draw_truncated_gamma(100000L, solution$alpha, solution$beta, 5L))

  expect_identical(draws_1, draws_2)
  expect_true(all(draws_1 >= 5L))
  expect_lt(abs(mean(draws_1) - 50) / 50, tol_mc_moment_n1e5)
  expect_lt(abs(stats::sd(draws_1) / mean(draws_1) - 0.50), tol_mc_moment_n1e5)
})

test_that(".rtrunc_gamma respects the lower bound on ordinary inputs", {
  draws <- withr::with_seed(101L, .rtrunc_gamma(n = 200L, alpha = 6, beta = 0.06, n_min = 5L))

  expect_length(draws, 200L)
  expect_true(all(draws >= 5))
  expect_true(all(is.finite(draws)))
})

test_that(".rtrunc_gamma rejects a non-positive n_min", {
  expect_error(
    .rtrunc_gamma(n = 5L, alpha = 2, beta = 0.02, n_min = 0L),
    class = "multisitedgp_error"
  )
})

test_that(".rtrunc_gamma aborts when the upper tail underflows", {
  # With n_min far to the right of the distribution's mass, the survival
  # probability underflows to 0.
  expect_error(
    .rtrunc_gamma(n = 5L, alpha = 2, beta = 50, n_min = 100000L),
    class = "multisitedgp_error"
  )
})

test_that(".validate_positive_scalar_number rejects zero and negatives", {
  expect_error(.validate_positive_scalar_number(0, "alpha"),
               class = "multisitedgp_error")
  expect_error(.validate_positive_scalar_number(-1, "alpha"),
               class = "multisitedgp_error")
  expect_identical(.validate_positive_scalar_number(2.5, "alpha"), 2.5)
})

test_that("`sim_multisite()` site sizes respect nj_min and average nj_mean over replications", {
  # 20 replications of 8000 sites. The mean of 160000 site sizes with standard
  # deviation 25 has standard error 25 / sqrt(160000) = 0.0625, so the
  # tolerance of 0.5 percent of 50 is 4 standard errors.
  seeds <- replication_seeds(20L, 821001L)
  mean_estimates <- vapply(seeds, function(seed) {
    df <- sim_default(seed = seed, J = 8000L, nj_mean = 50, nj_min = 5L)
    expect_true(all(df$n_j >= 5L))
    mean(df$n_j)
  }, numeric(1))

  expect_lt(abs(mean(mean_estimates) - 50) / 50, 0.005)
})

test_that("`sim_multisite()` output has se2_j times n_j equal to the default kappa of 4", {
  df <- sim_default(seed = 820201L)

  expect_equal(compute_kappa(), 4, tolerance = tol_deterministic)
  expect_equal(df$se2_j * df$n_j, rep(4, nrow(df)), tolerance = tol_deterministic)
})

test_that("`sim_multisite()` keeps se2_j times n_j equal to kappa across cv values", {
  for (cv in c(0, 0.25, 0.50, 0.75)) {
    df <- sim_default(seed = 820300L + as.integer(cv * 100), cv = cv)
    products <- df$se2_j * df$n_j

    expect_lt(stats::var(products), tol_deterministic)
    expect_equal(products, rep(compute_kappa(), length(products)), tolerance = tol_deterministic)
  }
})

test_that("`sim_multisite()` with the censored site-size generator draws in the order of the JEBS code", {
  assign(".rng_calls", character(), envir = .GlobalEnv)
  suppressMessages(trace(stats::runif, tracer = record_rng_call("runif"), print = FALSE))
  suppressMessages(trace(stats::rnorm, tracer = record_rng_call("rnorm"), print = FALSE))
  suppressMessages(trace(stats::rgamma, tracer = record_rng_call("rgamma"), print = FALSE))
  suppressMessages(trace(base::sample.int, tracer = record_rng_call("sample.int"), print = FALSE))
  on.exit({
    suppressMessages(untrace(stats::runif))
    suppressMessages(untrace(stats::rnorm))
    suppressMessages(untrace(stats::rgamma))
    suppressMessages(untrace(base::sample.int))
    rm(".rng_calls", envir = .GlobalEnv)
  }, add = TRUE)

  invisible(sim_multisite(preset_jebs_strict(), seed = 42L))
  rng_calls <- get(".rng_calls", envir = .GlobalEnv)

  # preset_jebs_strict() makes every kind of random draw: the mixture component
  # indicator, the two component normals, the gamma site sizes, the precision
  # shuffle, and the observation noise. An added or reordered call changes the
  # data that a seed gives.
  expect_identical(
    rng_calls,
    c("runif", "rnorm", "rnorm", "rgamma", "sample.int", "rnorm")
  )
})

test_that("`sim_multisite()` with the censored site-size generator restores the caller's RNG state", {
  set.seed(832012L)
  before <- .Random.seed

  out <- sim_multisite(preset_jebs_strict(), seed = 42L)
  diagnostics <- attr(out, "diagnostics", exact = TRUE)

  expect_identical(.Random.seed, before)
  expect_identical(diagnostics$observation_diagnostics$method, "gaussian")
  expect_true(diagnostics$observation_diagnostics$precision_shuffled)
  expect_identical(diagnostics$observation_diagnostics$rng_draws, nrow(out))
})
