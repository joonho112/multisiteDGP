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

test_that("T12 Engine A1 full path preserves contract-level RNG milestones", {
  assign(".t12_rng_calls", character(), envir = .GlobalEnv)
  suppressMessages(trace(stats::rnorm, tracer = .t12_record_call("rnorm"), print = FALSE))
  suppressMessages(trace(stats::rgamma, tracer = .t12_record_call("rgamma"), print = FALSE))
  suppressMessages(trace(base::sample.int, tracer = .t12_record_call("sample.int"), print = FALSE))
  on.exit({
    suppressMessages(untrace(stats::rnorm))
    suppressMessages(untrace(stats::rgamma))
    suppressMessages(untrace(base::sample.int))
    rm(".t12_rng_calls", envir = .GlobalEnv)
  }, add = TRUE)

  invisible(sim_multisite(preset_jebs_strict(), seed = 42L))
  rng_calls <- get(".t12_rng_calls", envir = .GlobalEnv)
  rgamma_pos <- which(rng_calls == "rgamma")
  sample_pos <- which(rng_calls == "sample.int")

  expect_length(rgamma_pos, 1L)
  expect_length(sample_pos, 1L)
  expect_gt(sample_pos, rgamma_pos)

  # Contract boundary: A1 sizes use one Gamma stream, then Layer 4 applies one
  # precision shuffle before the Gaussian observation draw. Layer 1 internals are
  # intentionally not frozen as an exact runif/rnorm prefix.
  post_shuffle_rnorm <- which(rng_calls == "rnorm" & seq_along(rng_calls) > sample_pos)
  expect_gt(length(post_shuffle_rnorm), 0L)
  if (length(post_shuffle_rnorm) > 0L) {
    expect_identical(post_shuffle_rnorm[[1L]], sample_pos + 1L)
  }
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
