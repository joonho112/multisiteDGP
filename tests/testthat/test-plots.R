test_that("plot helpers are exported", {
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
})

test_that("the subtitle of plot_effects() and plot_funnel() uses the words of print() and summary()", {
  dat <- sim_multisite(preset_education_modest(), seed = 7401L)
  diag <- attr(dat, "diagnostics", exact = TRUE)
  expected <- sprintf(
    "50 sites, informativeness %.3f, SE heterogeneity %.3f, rank correlation %.3f",
    diag$I_hat, diag$R_hat, diag$rho_S_residual
  )

  expect_identical(plot_effects(dat)$labels$subtitle, expected)
  expect_identical(plot_effects(dat, type = "density")$labels$subtitle, expected)
  expect_identical(plot_funnel(dat)$labels$subtitle, expected)

  # Equal site sizes give equal standard errors, so there is no rank
  # correlation to report.
  equal_sizes <- sim_multisite(J = 20L, nj_mean = 50, nj_min = 50L, cv = 0, seed = 7405L)
  expect_identical(
    plot_funnel(equal_sizes)$labels$subtitle,
    sprintf("20 sites, informativeness %.3f, SE heterogeneity 1.000", attr(equal_sizes, "diagnostics")$I_hat)
  )
  expect_identical(plot_dependence(equal_sizes)$labels$subtitle, "rank correlation not defined")
})

test_that("data without stored diagnostics for their rows get no subtitle", {
  dat <- sim_multisite(J = 25L, seed = 7411L)
  expect_false(is.null(plot_effects(dat)$labels$subtitle))

  # rbind() and add_row() keep the diagnostics of their first argument, which
  # describe 25 sites. A changed value drops the diagnostics.
  stacked <- rbind(dat, sim_multisite(J = 25L, seed = 7412L))
  grown <- tibble::add_row(
    dat, site_index = 26L, z_j = 9, tau_j = 1.8, tau_j_hat = 1.8, se_j = 0.3, se2_j = 0.09, n_j = 44L
  )
  changed <- dat
  changed$se2_j <- changed$se2_j * 4
  changed$se_j <- sqrt(changed$se2_j)
  not_a_list <- dat
  attr(not_a_list, "diagnostics") <- "none"
  for (x in list(stacked, grown, changed, not_a_list)) {
    expect_null(plot_effects(x)$labels$subtitle)
    expect_null(plot_funnel(x)$labels$subtitle)
    expect_null(plot_dependence(x, smoother = FALSE)$labels$subtitle)
  }
})

test_that("the subtitle of plot_dependence() shows the target that the design has", {
  subtitle <- function(..., by_residual = TRUE) {
    dat <- sim_multisite(preset_education_modest(...), seed = 7406L)
    rho <- attr(dat, "diagnostics", exact = TRUE)$rho_S_residual
    list(
      shown = plot_dependence(dat, smoother = FALSE, by_residual = by_residual)$labels$subtitle,
      realized = sprintf("rank correlation %.3f", rho)
    )
  }

  rank <- subtitle(dependence = "rank", rank_corr = 0.4)
  expect_identical(rank$shown, paste0(rank$realized, ", target 0.400"))
  hybrid <- subtitle(dependence = "hybrid", rank_corr = -0.3)
  expect_identical(hybrid$shown, paste0(hybrid$realized, ", target -0.300"))
  copula <- subtitle(dependence = "copula", pearson_corr = 0.3)
  expect_identical(copula$shown, paste0(copula$realized, ", target Pearson correlation 0.300"))

  # No target without dependence, and none when a user-supplied function
  # replaces the built-in method.
  none <- subtitle()
  expect_identical(none$shown, none$realized)
  reversed <- function(z_j, se2_j, target) {
    perm <- rev(seq_along(se2_j))
    list(se2_j = se2_j[perm], perm = perm)
  }
  user_fn <- subtitle(dependence = "rank", rank_corr = 0.4, dependence_fn = reversed)
  expect_identical(user_fn$shown, user_fn$realized)

  marginal <- subtitle(dependence = "rank", rank_corr = 0.4, by_residual = FALSE)
  expect_identical(marginal$shown, paste0("marginal ", marginal$realized, ", target 0.400"))

  # With covariates the target is for the residual correlation only.
  sites <- data.frame(x = rep(0:1, 25L))
  design <- preset_education_modest(dependence = "rank", rank_corr = 0.4, formula = ~x, beta = 0.3, data = sites)
  dat <- sim_multisite(design, seed = 7406L)
  diag <- attr(dat, "diagnostics", exact = TRUE)
  expect_identical(
    plot_dependence(dat, smoother = FALSE)$labels$subtitle,
    sprintf("rank correlation %.3f, target 0.400", diag$rho_S_residual)
  )
  expect_identical(
    plot_dependence(dat, smoother = FALSE, by_residual = FALSE)$labels$subtitle,
    sprintf("marginal rank correlation %.3f", diag$rho_S_marginal)
  )
  expect_false(isTRUE(all.equal(diag$rho_S_residual, diag$rho_S_marginal)))
})

test_that("titles and axis labels are in plain words, with the column in parentheses", {
  dat <- sim_multisite(preset_education_modest(), seed = 7401L)
  labels_of <- function(p) unlist(p$labels[c("title", "x", "y")], use.names = FALSE)

  expect_identical(
    labels_of(plot_effects(dat)),
    c("Site estimates with 95% intervals", "Estimate with 95% interval (tau_j_hat)", "Site")
  )
  expect_identical(
    labels_of(plot_effects(dat, type = "density")),
    c("Distribution of site estimates and true effects", "Estimate (tau_j_hat) or true effect (tau_j)", "Density")
  )
  expect_identical(
    labels_of(plot_effects(dat, type = "density", truth = FALSE)),
    c("Distribution of site estimates", "Estimate (tau_j_hat)", "Density")
  )
  expect_identical(
    labels_of(plot_funnel(dat)),
    c("Funnel plot of the site estimates", "Estimate (tau_j_hat)", "Standard error (se_j)")
  )
  expect_identical(
    labels_of(plot_dependence(dat, smoother = FALSE)),
    c("Dependence between effects and precision", "Standardized site effect (z_j)", "Sampling variance (se2_j)")
  )
  expect_identical(
    labels_of(plot_dependence(dat, smoother = FALSE, by_residual = FALSE))[[2L]],
    "Site effect (tau_j)"
  )
})

test_that("plot_effects() names the estimates and the true effects in a legend", {
  dat <- sim_multisite(preset_education_modest(), seed = 7401L)
  legend_of <- function(p, aesthetic) {
    ggplot2::ggplot_build(p)$plot$scales$get_scales(aesthetic)$get_limits()
  }
  layer_values <- function(p, layer, column) unique(ggplot2::ggplot_build(p)$data[[layer]][[column]])

  with_truth <- plot_effects(dat)
  expect_identical(legend_of(with_truth, "colour"), c("estimate", "true effect"))
  expect_identical(legend_of(with_truth, "shape"), c("estimate", "true effect"))
  # The estimates keep the first palette color and the true effects the second.
  palette <- multisitedgp_internal(".default_palette")(2L)
  expect_identical(layer_values(with_truth, 1L, "colour"), palette[[1L]])
  expect_identical(layer_values(with_truth, 2L, "colour"), palette[[1L]])
  expect_identical(layer_values(with_truth, 4L, "colour"), palette[[2L]])

  # In shades of gray the true effects are also set apart by their shape.
  gray <- plot_effects(dat, monochrome = TRUE)
  expect_identical(legend_of(gray, "colour"), c("estimate", "true effect"))
  expect_false(identical(layer_values(gray, 2L, "shape"), layer_values(gray, 4L, "shape")))

  # One set of points needs no legend.
  without_truth <- plot_effects(dat, truth = FALSE)
  expect_false(without_truth$scales$has_scale("colour"))
  expect_identical(layer_values(without_truth, 2L, "colour"), palette[[1L]])

  # The density plot uses the same two colors for the same two series. Its
  # first group is the true effects.
  density <- plot_effects(dat, type = "density")
  expect_setequal(legend_of(density, "colour"), c("estimate", "true effect"))
  density_data <- ggplot2::ggplot_build(density)$data[[1L]]
  expect_identical(unique(density_data$colour[density_data$group == 1L]), palette[[2L]])
  expect_identical(unique(density_data$colour[density_data$group == 2L]), palette[[1L]])
  estimates_only <- plot_effects(dat, type = "density", truth = FALSE)
  expect_identical(layer_values(estimates_only, 1L, "colour"), palette[[1L]])
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

  expect_s3_class(mono_density, "ggplot")
  expect_equal(length(funnel_no_env$layers), 2L)
  expect_equal(unique(ggplot2::ggplot_build(funnel_tau)$data[[2L]]$xintercept), 0.4)
  expect_match(dep_marginal$labels$x, "tau_j", fixed = TRUE)
  expect_match(dep_residual$labels$x, "z_j", fixed = TRUE)
  expect_gte(length(dep_residual$layers), 3L)

  expect_multisitedgp_error(plot_effects(dat, type = "bad"))
  expect_multisitedgp_error(plot_funnel(dat, reference = "bad"))
  expect_multisitedgp_error(plot_dependence(dat, smoother = "yes"))
})

test_that("plot helpers reject non multisitedgp_data inputs with classed errors", {
  expect_multisitedgp_error(plot_effects(tibble::tibble()))
  expect_multisitedgp_error(plot_funnel(tibble::tibble()))
  expect_multisitedgp_error(plot_dependence(tibble::tibble()))
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
    multisitedgp_internal(".default_palette")(0L)
  )
})

test_that("the caterpillar y axis thins its labels on large designs", {
  # The y axis has one discrete level per site, so without thinning every
  # label is drawn, and at J = 50 the labels already overlap.
  breaks_for <- function(J) {
    dat <- sim_multisite(
      multisitedgp_design(J = J, sigma_tau = 0.2, nj_mean = 100, seed = 1L)
    )
    .caterpillar_site_breaks(.strip_multisitedgp_data(dat))
  }

  # Up to the limit every site keeps its label, so each site can be picked out.
  expect_length(breaks_for(10L), 10L)
  expect_length(breaks_for(25L), 25L)

  # Above the limit the number of labels is capped.
  for (J in c(50L, 200L)) {
    expect_lte(length(breaks_for(J)), .CATERPILLAR_MAX_LABELS)
  }
})

test_that("thinned labels stay a subset of the real site indices, in effect order", {
  dat <- sim_multisite(
    multisitedgp_design(J = 60L, sigma_tau = 0.2, nj_mean = 100, seed = 1L)
  )
  plot_data <- .strip_multisitedgp_data(dat)
  breaks <- .caterpillar_site_breaks(plot_data)

  expect_true(all(breaks %in% as.character(plot_data$site_index)))
  expect_false(anyDuplicated(breaks) > 0L)

  # The axis is ordered by effect size, so the labels that remain must follow
  # that order.
  ordered <- as.character(plot_data$site_index[order(plot_data$tau_j_hat)])
  expect_identical(breaks, ordered[match(breaks, ordered)])
})

test_that("plot_effects still renders with the thinned axis", {
  dat <- sim_multisite(
    multisitedgp_design(J = 80L, sigma_tau = 0.2, nj_mean = 100, seed = 1L)
  )
  expect_s3_class(plot_effects(dat), "ggplot")
  expect_s3_class(plot_effects(dat, truth = FALSE), "ggplot")
})
