# nolint start: object_usage_linter
.t12_record_call <- function(label) {
  substitute(
    assign(
      ".t12_rng_calls",
      c(get(".t12_rng_calls", envir = .GlobalEnv), value),
      envir = .GlobalEnv
    ),
    list(value = label)
  )
}

test_that("T12 Engine A1 full path preserves the complete legacy RNG vector", {
  assign(".t12_rng_calls", character(), envir = .GlobalEnv)
  suppressMessages(trace(stats::runif, tracer = .t12_record_call("runif"), print = FALSE))
  suppressMessages(trace(stats::rnorm, tracer = .t12_record_call("rnorm"), print = FALSE))
  suppressMessages(trace(stats::rgamma, tracer = .t12_record_call("rgamma"), print = FALSE))
  suppressMessages(trace(base::sample.int, tracer = .t12_record_call("sample.int"), print = FALSE))
  on.exit({
    suppressMessages(untrace(stats::runif))
    suppressMessages(untrace(stats::rnorm))
    suppressMessages(untrace(stats::rgamma))
    suppressMessages(untrace(base::sample.int))
    rm(".t12_rng_calls", envir = .GlobalEnv)
  }, add = TRUE)

  invisible(sim_multisite(preset_jebs_strict(), seed = 42L))
  rng_calls <- get(".t12_rng_calls", envir = .GlobalEnv)

  # JEBS strict is the complete A1 mixture path: component indicator, two
  # component-normal streams, Gamma site sizes, legacy precision shuffle, and
  # observation noise. Any added or reordered RNG call is a T12 regression.
  expect_identical(
    rng_calls,
    c("runif", "rnorm", "rnorm", "rgamma", "sample.int", "rnorm")
  )
})

test_that("T12 Engine A1 seeded wrapper restores caller RNG state", {
  set.seed(832012L)
  before <- .Random.seed

  out <- sim_multisite(preset_jebs_strict(), seed = 42L)
  diagnostics <- attr(out, "diagnostics", exact = TRUE)

  expect_identical(.Random.seed, before)
  expect_identical(diagnostics$observation_diagnostics$method, "gaussian")
  expect_true(diagnostics$observation_diagnostics$legacy_a1_shuffle)
  expect_identical(diagnostics$observation_diagnostics$rng_draws, nrow(out))
})
# nolint end
