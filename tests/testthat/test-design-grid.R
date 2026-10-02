test_that("design_grid is exported and returns a classed cell grid", {
  exports <- getNamespaceExports("multisiteDGP")
  expect_true("design_grid" %in% exports)

  grid <- design_grid(
    J = c(20L, 40L),
    sigma_tau = c(0.1, 0.2),
    seed_root = 7501L
  )

  expect_s3_class(grid, "multisitedgp_design_grid")
  expect_s3_class(grid, "tbl_df")
  expect_equal(nrow(grid), 4L)
  expect_identical(names(grid), c("J", "sigma_tau", "design", "seed"))
  expect_false("rep" %in% names(grid))
  expect_identical(grid$J, c(20L, 20L, 40L, 40L))
  expect_identical(grid$sigma_tau, c(0.1, 0.2, 0.1, 0.2))
  expect_true(all(vapply(grid$design, is_multisitedgp_design, logical(1))))
  expect_identical(
    vapply(grid$design, `[[`, integer(1), "J"),
    grid$J
  )
  expect_identical(
    vapply(grid$design, `[[`, numeric(1), "sigma_tau"),
    grid$sigma_tau
  )
  expect_true(all(vapply(grid$design, function(design) is.null(design$seed), logical(1))))
})

test_that("design_grid seed stream is deterministic and does not touch caller RNG", {
  set.seed(7502L)
  before <- get(".Random.seed", envir = .GlobalEnv)
  one <- design_grid(J = c(20L, 40L, 60L), seed_root = 12345L)
  after <- get(".Random.seed", envir = .GlobalEnv)
  two <- design_grid(J = c(20L, 40L, 60L), seed_root = 12345L)

  expect_identical(after, before)
  expect_identical(one$seed, two$seed)
  expect_type(one$seed, "integer")
  expect_length(unique(one$seed), nrow(one))
  expect_identical(one$design[[1L]], two$design[[1L]])
})

test_that("design_grid can be used without a seed stream", {
  grid <- design_grid(
    paradigm = "direct",
    I = c(0.2, 0.4),
    R = c(1, 3),
    seed_stream = FALSE
  )

  expect_s3_class(grid, "multisitedgp_design_grid")
  expect_equal(nrow(grid), 4L)
  expect_identical(names(grid), c("paradigm", "I", "R", "design"))
  expect_false("seed" %in% names(grid))
  expect_true(all(vapply(grid$design, is_multisitedgp_design, logical(1))))
  expect_true(all(vapply(grid$design, `[[`, character(1), "paradigm") == "direct"))
  expect_true(all(vapply(grid$design, function(design) is.null(design$seed), logical(1))))
})

test_that("design_grid updates a base design without mutating it", {
  base <- preset_education_modest(seed = 99L)
  grid <- design_grid(
    J = c(30L, 50L),
    sigma_tau = 0.25,
    base_design = base,
    seed_root = 7503L
  )

  expect_equal(nrow(grid), 2L)
  expect_identical(base$J, 50L)
  expect_identical(base$sigma_tau, 0.20)
  expect_identical(base$seed, 99L)
  expect_identical(grid$design[[1L]]$J, 30L)
  expect_identical(grid$design[[2L]]$J, 50L)
  expect_identical(grid$design[[1L]]$sigma_tau, 0.25)
  expect_identical(grid$design[[1L]]$nj_min, base$nj_min)
  expect_identical(grid$design[[1L]]$seed, base$seed)
})

test_that("design_grid supports constructor list arguments as single grid columns", {
  grid <- design_grid(
    true_dist = "StudentT",
    theta_G = list(list(nu = 5), list(nu = 8)),
    seed_stream = FALSE
  )

  expect_equal(nrow(grid), 2L)
  expect_identical(grid$design[[1L]]$theta_G, list(nu = 5))
  expect_identical(grid$design[[2L]]$theta_G, list(nu = 8))
})

test_that("design_grid validates seed stream and argument shape with classed errors", {
  expect_rng_neutral_error(
    design_grid(J = c(20L, 40L), seed_stream = TRUE)
  )
  expect_multisitedgp_error(
    design_grid(J = c(20L, 40L), seed_root = -1L)
  )
  expect_multisitedgp_error(
    design_grid(J = c(20L, 40L), seed_stream = NA, seed_root = 7505L)
  )
  expect_multisitedgp_error(
    design_grid(J = c(20L, 40L), seed = 1L, seed_root = 7505L)
  )
  expect_multisitedgp_error(
    design_grid(unknown_arg = 1, seed_root = 7505L)
  )
  expect_multisitedgp_error(
    design_grid(1, seed_root = 7505L)
  )
  expect_multisitedgp_error(
    design_grid(J = integer(), seed_root = 7505L)
  )
  expect_multisitedgp_error(
    design_grid(J = c(8L, 20L), seed_root = 7505L)
  )
  expect_multisitedgp_error(
    design_grid(J = 20L, base_design = list(), seed_root = 7505L)
  )
})

test_that("design_grid rejects arguments that are not vectors or lists", {
  expect_multisitedgp_error(
    design_grid(g_fn = function(J) stats::rnorm(J), seed_root = 7505L),
    regexp = "must be a vector or a list"
  )
  expect_multisitedgp_error(
    design_grid(data = data.frame(x = seq_len(20L)), seed_root = 7505L),
    regexp = "must be a vector or a list"
  )
})
