test_that("Step 7.4 plot helpers are exported", {
  exports <- getNamespaceExports("multisiteDGP")
  expect_true(all(c("plot_effects", "plot_funnel", "plot_dependence") %in% exports))
})

test_that("plot helpers return composable ggplot objects for site-size data", {
  dat <- sim_multisite(preset_education_modest(), seed = 7401L)

  plots <- list(
    effects_caterpillar = plot_effects(dat),
    effects_density = plot_effects(dat, type = "density"),
    funnel = plot_funnel(dat),
    dependence = plot_dependence(dat)
  )

  for (p in plots) {
    expect_s3_class(p, "ggplot")
    expect_silent(ggplot2::ggplot_build(p))
  }

  augmented <- plot_dependence(dat) + ggplot2::labs(title = "Custom title")
  expect_s3_class(augmented, "ggplot")
  expect_equal(augmented$labels$title, "Custom title")
  expect_match(plots$effects_caterpillar$labels$subtitle, "I_hat", fixed = TRUE)
  expect_match(plots$effects_caterpillar$labels$subtitle, "R_hat", fixed = TRUE)
  expect_match(plots$effects_caterpillar$labels$subtitle, "rho_S", fixed = TRUE)
  expect_match(plots$effects_caterpillar$labels$caption, "canonical_hash=", fixed = TRUE)
})

test_that("plot helpers build for direct meta outputs", {
  dat <- sim_meta(preset_meta_modest(), seed = 7402L)

  expect_s3_class(dat, "multisitedgp_meta")
  expect_silent(ggplot2::ggplot_build(plot_effects(dat, truth = FALSE)))
  expect_silent(ggplot2::ggplot_build(plot_funnel(dat, reference = "tau")))
  expect_silent(ggplot2::ggplot_build(plot_dependence(dat, smoother = FALSE, by_residual = FALSE)))
})

test_that("plot options validate and change expected labels/layers", {
  dat <- sim_multisite(
    multisitedgp_design(J = 40L, dependence = "rank", rank_corr = -0.3),
    seed = 7403L
  )

  no_caption <- plot_effects(dat, caption = FALSE)
  mono_density <- plot_effects(dat, type = "density", monochrome = TRUE)
  funnel_no_env <- plot_funnel(dat, envelope = FALSE, monochrome = TRUE)
  design_tau <- update_multisitedgp_design(
    attr(dat, "design", exact = TRUE),
    tau = 0.4
  )
  funnel_tau <- plot_funnel(
    sim_multisite(design_tau, seed = 7404L),
    reference = "tau"
  )
  dep_marginal <- plot_dependence(dat, by_residual = FALSE, envelope = FALSE)
  dep_residual <- plot_dependence(dat, by_residual = TRUE, envelope = TRUE)

  expect_null(no_caption$labels$caption)
  expect_s3_class(mono_density, "ggplot")
  expect_equal(length(funnel_no_env$layers), 2L)
  expect_equal(unique(ggplot2::ggplot_build(funnel_tau)$data[[2L]]$xintercept), 0.4)
  expect_match(dep_marginal$labels$x, "tau_j", fixed = TRUE)
  expect_match(dep_residual$labels$x, "z_j", fixed = TRUE)
  expect_gte(length(dep_residual$layers), 3L)

  expect_multisitedgp_error(plot_effects(dat, type = "bad"), "multisitedgp_arg_error")
  expect_multisitedgp_error(plot_funnel(dat, reference = "bad"), "multisitedgp_arg_error")
  expect_multisitedgp_error(plot_dependence(dat, smoother = "yes"), "multisitedgp_arg_error")
})

test_that("plot helpers reject non multisitedgp_data inputs with classed errors", {
  expect_multisitedgp_error(plot_effects(tibble::tibble()), "multisitedgp_arg_error")
  expect_multisitedgp_error(plot_funnel(tibble::tibble()), "multisitedgp_arg_error")
  expect_multisitedgp_error(plot_dependence(tibble::tibble()), "multisitedgp_arg_error")
})

test_that("plot palette and theme helpers follow accessibility policy", {
  palette_small <- multisitedgp_internal(".default_palette")(2L)
  palette_large <- multisitedgp_internal(".default_palette")(9L)
  palette_cont <- multisitedgp_internal(".default_palette")(1L, type = "continuous")
  theme <- multisitedgp_internal(".theme_multisitedgp")()

  expect_length(palette_small, 2L)
  expect_length(palette_large, 9L)
  expect_length(palette_cont, 256L)
  expect_s3_class(theme, "theme")
  expect_multisitedgp_error(
    multisitedgp_internal(".default_palette")(0L),
    "multisitedgp_arg_error"
  )
})

test_that("Step 7.4 plot traceability statuses are synchronized", {
  trace_dir <- test_path("../../tools/traceability")
  skip_if_not(dir.exists(trace_dir), "Development-only traceability ledgers are not shipped in the package tarball.")

  api <- read.csv(file.path(trace_dir, "api-index.csv"), stringsAsFactors = FALSE)
  ids <- c("API040", "API041", "API042")

  expect_true(all(api$status[match(ids, api$id)] == "implemented"))
  expect_true(all(api$owner_step[match(ids, api$id)] == "Step 7.4"))
})
