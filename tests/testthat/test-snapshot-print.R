# nolint start: object_usage_linter
local_print_snapshot_output <- function() {
  testthat::local_reproducible_output(width = 80, unicode = FALSE)
  withr::local_options(list(
    cli.width = 80,
    width = 80,
    pillar.print_min = 6L,
    pillar.print_max = 20L,
    pillar.sigfig = 3L
  ))
}

snapshot_print_transform <- function(x) {
  x <- gsub("R=[0-9.]+", "R=<R>", x)
  gsub("multisiteDGP [0-9.]+", "multisiteDGP <VERSION>", x)
}

snapshot_preset_designs <- function() {
  list(
    education_small = preset_education_small(),
    education_modest = preset_education_modest(),
    education_substantial = preset_education_substantial(),
    jebs_paper = preset_jebs_paper(),
    jebs_strict = preset_jebs_strict(),
    walters_2024 = preset_walters_2024(),
    twin_towers = preset_twin_towers(),
    meta_modest = preset_meta_modest(),
    small_area_estimation = preset_small_area_estimation()
  )
}

snapshot_preset_outputs <- function() {
  list(
    jebs_paper = sim_multisite(preset_jebs_paper(), seed = 4719L),
    jebs_strict = sim_multisite(preset_jebs_strict(), seed = 4719L),
    education_modest = sim_multisite(preset_education_modest(), seed = 12345L),
    walters_2024 = sim_multisite(preset_walters_2024(), seed = 1L),
    small_area_estimation = sim_meta(preset_small_area_estimation(), seed = 42L)
  )
}

snapshot_covariate_data <- function() {
  covariates <- tibble::tibble(x_site = seq(-1, 1, length.out = 25L))
  sim_multisite(
    J = 25L,
    formula = ~x_site,
    beta = 0.15,
    data = covariates,
    dependence = "rank",
    rank_corr = 0.3,
    seed = 8601L
  )
}

test_that("print.multisitedgp_design snapshots all preset designs", {
  local_print_snapshot_output()
  designs <- snapshot_preset_designs()

  for (name in names(designs)) {
    expect_snapshot({
      cat(sprintf("## %s\n", name))
      print(designs[[name]])
    })
  }
})

test_that("print.multisitedgp_data snapshots golden preset outputs", {
  local_print_snapshot_output()
  outputs <- snapshot_preset_outputs()

  for (name in names(outputs)) {
    expect_snapshot(
      {
        cat(sprintf("## %s\n", name))
        print(outputs[[name]], n = 3L)
      },
      transform = snapshot_print_transform
    )
  }
})

test_that("summary.multisitedgp_data snapshots golden preset outputs", {
  local_print_snapshot_output()
  outputs <- snapshot_preset_outputs()

  for (name in names(outputs)) {
    expect_snapshot(
      {
        cat(sprintf("## %s\n", name))
        summary(outputs[[name]])
      },
      transform = snapshot_print_transform
    )
  }
})

test_that("print and summary snapshot a direct meta-analysis preset", {
  local_print_snapshot_output()
  out <- sim_meta(preset_meta_modest(), seed = 8602L)

  expect_snapshot(print(out, n = 3L), transform = snapshot_print_transform)
  expect_snapshot(summary(out), transform = snapshot_print_transform)
})

test_that("print and summary snapshot covariate two-number reporting", {
  local_print_snapshot_output()
  out <- snapshot_covariate_data()

  expect_snapshot(print(out, n = 3L), transform = snapshot_print_transform)
  expect_snapshot(summary(out), transform = snapshot_print_transform)
})

test_that("summary snapshots row-subset diagnostic recomputation", {
  local_print_snapshot_output()
  out <- sim_multisite(J = 25L, seed = 8603L)
  sub <- suppressWarnings(out[seq_len(12L), ])

  expect_snapshot(summary(sub), transform = snapshot_print_transform)
})
# nolint end
