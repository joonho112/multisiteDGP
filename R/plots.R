# nolint start: object_usage_linter
#' Visualization helpers for multisiteDGP simulations
#'
#' @encoding UTF-8
#'
#' @description
#' Three plot helpers for visualizing a `multisitedgp_data` simulation.
#' Each returns a bare `ggplot2::ggplot` object so the caller can add
#' themes, labels, facets, or other layers downstream.
#'
#' \describe{
#'   \item{\code{\link{plot_effects}}}{Caterpillar (default) or density view of latent and observed site effects. Use to read off the effect-size ordering and the spread of `tau_j_hat` vs `tau_j`.}
#'   \item{\code{\link{plot_funnel}}}{Meta-analysis funnel — `tau_j_hat` (x-axis) vs `se_j` (y-axis, inverted). Use to spot precision-effect dependence and check whether large-SE sites have the same effect distribution as small-SE sites.}
#'   \item{\code{\link{plot_dependence}}}{Scatter of `z_j` (or `tau_j`) against `se2_j`. Use to verify that Layer 3 dependence alignment hit its target — the realized rank correlation should match the design.}
#' }
#'
#' For the four-question diagnostic rubric and worked applications of
#' these plots, see the
#' \href{../articles/a3-diagnostics-in-practice.html}{A3 Diagnostics in
#' practice} vignette. For end-to-end case studies that use these plots,
#' see the \href{../articles/a6-case-study-multisite.html}{A6 multisite
#' trial} and \href{../articles/a7-case-study-meta-analysis.html}{A7
#' meta-analysis} case studies.
#'
#' @param x A `multisitedgp_data` object from \code{\link{sim_multisite}}
#'   or \code{\link{sim_meta}}.
#' @param ... Reserved for future extensions.
#'
#' @return A `ggplot2::ggplot` object.
#' @importFrom rlang .data
#' @family family-plots
#' @seealso
#'   \code{\link{realized_rank_corr}}, \code{\link{informativeness}},
#'   \code{\link{heterogeneity_ratio}} for scalar diagnostics that
#'   complement the visualizations;
#'   the \href{../articles/a3-diagnostics-in-practice.html}{A3 Diagnostics
#'   in practice} vignette.
#' @references
#' Lee, J., Che, J., Rabe-Hesketh, S., Feller, A., & Miratrix, L. (2025).
#' Improving the estimation of site-specific effects and their distribution
#' in multisite trials. \emph{Journal of Educational and Behavioral Statistics},
#' \bold{50}(5), 731--764. \doi{10.3102/10769986241254286}.
#' @name plots
#' @examples
#' dat <- sim_multisite(J = 30L, seed = 1L)
#' plot_effects(dat)         # caterpillar
#' plot_funnel(dat)          # funnel
#' plot_dependence(dat)      # z_j vs se2_j scatter
NULL

#' @describeIn plots Caterpillar (default) or density view of `tau_j_hat` (and optionally latent `tau_j`). Use to read off effect-size ordering and spread.
#' @param type Character. Effect-plot view — `"caterpillar"` (default) or `"density"`.
#' @param truth Logical. Show latent `tau_j` overlay alongside observed `tau_j_hat`. Default `TRUE`.
#' @param monochrome Logical. Use grayscale-safe styling. Default `FALSE`.
#' @param caption Logical. Include diagnostic / provenance labels in the plot caption. Default `TRUE`.
#' @export
plot_effects <- function(
    x,
    type = c("caterpillar", "density"),
    truth = TRUE,
    monochrome = FALSE,
    caption = TRUE,
    ...) {
  UseMethod("plot_effects")
}

#' @export
plot_effects.default <- function(
    x,
    type = c("caterpillar", "density"),
    truth = TRUE,
    monochrome = FALSE,
    caption = TRUE,
    ...) {
  .abort_plot_data("plot_effects", x)
}

#' @export
plot_effects.multisitedgp_data <- function(
    x,
    type = c("caterpillar", "density"),
    truth = TRUE,
    monochrome = FALSE,
    caption = TRUE,
    ...) {
  type <- .match_choice(type, "type", c("caterpillar", "density"))
  truth <- .validate_scalar_logical(truth, "truth")
  monochrome <- .validate_scalar_logical(monochrome, "monochrome")
  caption <- .validate_scalar_logical(caption, "caption")

  if (identical(type, "density")) {
    return(.plot_effects_density(x, truth = truth, monochrome = monochrome, caption = caption))
  }
  .plot_effects_caterpillar(x, truth = truth, monochrome = monochrome, caption = caption)
}

#' @describeIn plots Meta-analysis funnel — `tau_j_hat` (x-axis) vs `se_j` (y-axis, inverted). Use to spot precision-effect dependence and read whether large-SE sites have the same effect distribution as small-SE sites.
#' @param reference Character. Funnel reference line — `"zero"` (default) or `"tau"` (the design grand mean).
#' @param envelope Logical. Show the 1.96-SE funnel envelope (the band where 95 percent of estimates would fall under no heterogeneity). Default `TRUE`.
#' @export
plot_funnel <- function(
    x,
    reference = c("zero", "tau"),
    envelope = TRUE,
    monochrome = FALSE,
    caption = TRUE,
    ...) {
  UseMethod("plot_funnel")
}

#' @export
plot_funnel.default <- function(
    x,
    reference = c("zero", "tau"),
    envelope = TRUE,
    monochrome = FALSE,
    caption = TRUE,
    ...) {
  .abort_plot_data("plot_funnel", x)
}

#' @export
plot_funnel.multisitedgp_data <- function(
    x,
    reference = c("zero", "tau"),
    envelope = TRUE,
    monochrome = FALSE,
    caption = TRUE,
    ...) {
  reference <- .match_choice(reference, "reference", c("zero", "tau"))
  envelope <- .validate_scalar_logical(envelope, "envelope")
  monochrome <- .validate_scalar_logical(monochrome, "monochrome")
  caption <- .validate_scalar_logical(caption, "caption")
  ref_value <- .plot_reference_value(x, reference = reference)
  point_color <- if (isTRUE(monochrome)) "grey25" else .default_palette(1L)

  p <- ggplot2::ggplot(.strip_multisitedgp_data(x), ggplot2::aes(x = .data$tau_j_hat, y = .data$se_j)) +
    ggplot2::geom_point(color = point_color[[1L]], alpha = 0.78, size = 2.2) +
    ggplot2::geom_vline(xintercept = ref_value, linewidth = 0.8, linetype = "dashed", color = "grey45") +
    ggplot2::scale_y_reverse() +
    ggplot2::labs(
      title = "Funnel plot of observed site effects",
      subtitle = .plot_diagnostic_subtitle(x),
      x = "Observed site effect (tau_j_hat)",
      y = "Sampling SE (se_j, reversed)",
      caption = .plot_caption(x, caption)
    ) +
    .theme_multisitedgp()

  if (isTRUE(envelope)) {
    se_grid <- seq(min(x$se_j), max(x$se_j), length.out = 100L)
    env <- tibble::tibble(
      se_j = c(se_grid, se_grid),
      tau_j_hat = c(ref_value - 1.96 * se_grid, ref_value + 1.96 * se_grid),
      bound = rep(c("lower", "upper"), each = length(se_grid))
    )
    p <- p + ggplot2::geom_line(
      data = env,
      ggplot2::aes(x = .data$tau_j_hat, y = .data$se_j, group = .data$bound),
      inherit.aes = FALSE,
      linewidth = 0.8,
      linetype = "dotted",
      color = "grey50"
    )
  }
  p
}

#' @describeIn plots Scatter of latent effect (`z_j` by default, or `tau_j` if `by_residual = FALSE`) against `se2_j`. Use to verify that Layer 3 dependence alignment hit its target — the realized rank correlation should match the design's `rank_corr` (within Monte Carlo noise).
#' @param smoother Logical. Add a loess smooth to highlight the trend. Default `TRUE`.
#' @param by_residual Logical. `TRUE` (default) plots residual-scale `z_j`; `FALSE` plots response-scale `tau_j`. Residual-scale matches the design target of Layer 3 aligners.
#' @export
plot_dependence <- function(
    x,
    smoother = TRUE,
    envelope = TRUE,
    by_residual = TRUE,
    monochrome = FALSE,
    caption = TRUE,
    ...) {
  UseMethod("plot_dependence")
}

#' @export
plot_dependence.default <- function(
    x,
    smoother = TRUE,
    envelope = TRUE,
    by_residual = TRUE,
    monochrome = FALSE,
    caption = TRUE,
    ...) {
  .abort_plot_data("plot_dependence", x)
}

#' @export
plot_dependence.multisitedgp_data <- function(
    x,
    smoother = TRUE,
    envelope = TRUE,
    by_residual = TRUE,
    monochrome = FALSE,
    caption = TRUE,
    ...) {
  smoother <- .validate_scalar_logical(smoother, "smoother")
  envelope <- .validate_scalar_logical(envelope, "envelope")
  by_residual <- .validate_scalar_logical(by_residual, "by_residual")
  monochrome <- .validate_scalar_logical(monochrome, "monochrome")
  caption <- .validate_scalar_logical(caption, "caption")

  plot_data <- .strip_multisitedgp_data(x)
  x_col <- if (isTRUE(by_residual)) "z_j" else "tau_j"
  point_color <- if (isTRUE(monochrome)) "grey25" else .default_palette(1L)[[1L]]
  smooth_color <- if (isTRUE(monochrome)) "grey10" else .default_palette(2L)[[2L]]

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[x_col]], y = .data$se2_j)) +
    ggplot2::geom_point(color = point_color, alpha = 0.78, size = 2.2) +
    ggplot2::labs(
      title = "Effect-precision dependence",
      subtitle = .plot_dependence_subtitle(x, by_residual = by_residual),
      x = if (isTRUE(by_residual)) "Standardized residual effect (z_j)" else "Latent site effect (tau_j)",
      y = "Sampling variance (se2_j)",
      caption = .plot_caption(x, caption)
    ) +
    .theme_multisitedgp()

  if (isTRUE(smoother) && isTRUE(envelope)) {
    ribbon <- .dependence_bootstrap_envelope(plot_data[[x_col]], plot_data$se2_j)
    if (!is.null(ribbon)) {
      p <- p + ggplot2::geom_ribbon(
        data = ribbon,
        ggplot2::aes(x = .data$x, ymin = .data$ymin, ymax = .data$ymax),
        inherit.aes = FALSE,
        fill = if (isTRUE(monochrome)) "grey80" else smooth_color,
        alpha = 0.18
      )
    }
  }

  if (isTRUE(smoother)) {
    p <- p + ggplot2::geom_smooth(
      method = "loess",
      formula = y ~ x,
      se = FALSE,
      linewidth = 0.9,
      color = smooth_color
    )
  }
  p
}

.plot_effects_caterpillar <- function(x, truth, monochrome, caption) {
  plot_data <- .strip_multisitedgp_data(x)
  plot_data$lower <- plot_data$tau_j_hat - 1.96 * plot_data$se_j
  plot_data$upper <- plot_data$tau_j_hat + 1.96 * plot_data$se_j
  colors <- .plot_discrete_colors(if (isTRUE(truth)) 2L else 1L, monochrome)

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(y = stats::reorder(.data$site_index, .data$tau_j_hat))) +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = .data$lower, xmax = .data$upper),
      width = 0.15,
      linewidth = 0.75,
      color = colors[[1L]]
    ) +
    ggplot2::geom_point(ggplot2::aes(x = .data$tau_j_hat), color = colors[[1L]], size = 2.1, alpha = 0.85) +
    ggplot2::geom_vline(xintercept = 0, linewidth = 0.7, linetype = "dashed", color = "grey55") +
    ggplot2::labs(
      title = "Site-level observed effects",
      subtitle = .plot_diagnostic_subtitle(x),
      x = "Observed site effect (tau_j_hat, 95% CI)",
      y = "Site",
      caption = .plot_caption(x, caption)
    ) +
    .theme_multisitedgp()

  if (isTRUE(truth)) {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(x = .data$tau_j),
      color = colors[[2L]],
      shape = if (isTRUE(monochrome)) 17L else 16L,
      size = 1.9,
      alpha = 0.75
    )
  }
  p
}

.plot_effects_density <- function(x, truth, monochrome, caption) {
  plot_data <- tibble::tibble(
    effect = x$tau_j_hat,
    series = "tau_j_hat"
  )
  if (isTRUE(truth)) {
    plot_data <- dplyr::bind_rows(plot_data, tibble::tibble(effect = x$tau_j, series = "tau_j"))
  }
  colors <- .plot_discrete_colors(length(unique(plot_data$series)), monochrome)

  ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$effect, color = .data$series, fill = .data$series)) +
    ggplot2::geom_density(alpha = if (isTRUE(monochrome)) 0.15 else 0.28, linewidth = 0.9) +
    ggplot2::geom_vline(xintercept = 0, linewidth = 0.7, linetype = "dashed", color = "grey55") +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::labs(
      title = "Distribution of site effects",
      subtitle = .plot_diagnostic_subtitle(x),
      x = "Effect size",
      y = "Density",
      color = NULL,
      fill = NULL,
      caption = .plot_caption(x, caption)
    ) +
    .theme_multisitedgp()
}

.abort_plot_data <- function(plotter, x) {
  .abort_arg(
    sprintf("`x` must be a multisitedgp_data object for `%s()`.", plotter),
    sprintf("Got object with class: %s.", paste(class(x), collapse = "/")),
    "Use `sim_multisite()` or `sim_meta()` before calling the plot helper."
  )
}

.plot_reference_value <- function(x, reference) {
  if (identical(reference, "zero")) {
    return(0)
  }
  design <- attr(x, "design", exact = TRUE)
  if (is.list(design) && !is.null(design$tau)) {
    return(design$tau)
  }
  0
}

.plot_diagnostic_subtitle <- function(x) {
  diag <- attr(x, "diagnostics", exact = TRUE)
  if (!is.list(diag)) {
    return(NULL)
  }
  sprintf(
    "J = %s | I_hat = %s | R_hat = %s | rho_S = %s (%s)",
    .format_plot_value(diag$J, digits = 0L),
    .format_plot_value(diag$I_hat),
    .format_plot_value(diag$R_hat),
    .format_plot_value(diag$rho_S_residual),
    if (is.null(diag$dependence_method)) "unknown" else diag$dependence_method
  )
}

.plot_dependence_subtitle <- function(x, by_residual) {
  diag <- attr(x, "diagnostics", exact = TRUE)
  if (!is.list(diag)) {
    return(NULL)
  }
  rho <- if (isTRUE(by_residual)) diag$rho_S_residual else diag$rho_S_marginal
  basis <- if (isTRUE(by_residual)) "residual" else "marginal"
  sprintf(
    "%s rho_S = %s | target = %s | method = %s",
    basis,
    .format_plot_value(rho),
    .format_plot_value(diag$target_rank_corr),
    if (is.null(diag$dependence_method)) "unknown" else diag$dependence_method
  )
}

.plot_caption <- function(x, caption) {
  if (!isTRUE(caption)) {
    return(NULL)
  }
  provenance_string(x)
}

.format_plot_value <- function(x, digits = 3L) {
  if (is.null(x) || length(x) == 0L || is.na(x[[1L]]) || !is.finite(x[[1L]])) {
    return("NA")
  }
  formatC(x[[1L]], digits = digits, format = "f")
}

.plot_discrete_colors <- function(n, monochrome) {
  if (isTRUE(monochrome)) {
    return(grDevices::gray.colors(n, start = 0.2, end = 0.65))
  }
  .default_palette(n)
}

.default_palette <- function(n, type = c("discrete", "continuous")) {
  type <- .match_choice(type, "type", c("discrete", "continuous"))
  n <- .validate_scalar_integer(n, "n")
  if (n < 1L) {
    .abort_arg(
      "`n` must be at least 1.",
      "`n` controls the number of colors requested from the multisiteDGP palette.",
      "Use `n = 1L` or another positive integer."
    )
  }
  if (identical(type, "continuous")) {
    return(viridisLite::viridis(256L, option = "D", end = 0.9))
  }
  if (n <= 8L) {
    return(RColorBrewer::brewer.pal(max(3L, n), "Set2")[seq_len(n)])
  }
  viridisLite::viridis(n, option = "D", end = 0.9)
}

.dependence_bootstrap_envelope <- function(x, y, grid_n = 80L, B = 80L) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]
  y <- y[ok]
  if (length(x) < 10L || length(unique(x)) < 4L) {
    return(NULL)
  }
  grid <- seq(min(x), max(x), length.out = grid_n)
  fits <- withr::with_seed(15074L, replicate(B, {
    idx <- sample.int(length(x), replace = TRUE)
    .predict_loess_envelope(x[idx], y[idx], grid)
  }))
  if (is.null(dim(fits))) {
    return(NULL)
  }
  valid <- rowSums(is.finite(fits)) >= max(5L, floor(0.25 * B))
  if (!any(valid)) {
    return(NULL)
  }
  ymin <- rep(NA_real_, length(grid))
  ymax <- rep(NA_real_, length(grid))
  ymin[valid] <- apply(fits[valid, , drop = FALSE], 1L, stats::quantile, probs = 0.025, na.rm = TRUE)
  ymax[valid] <- apply(fits[valid, , drop = FALSE], 1L, stats::quantile, probs = 0.975, na.rm = TRUE)
  tibble::tibble(x = grid, ymin = ymin, ymax = ymax)
}

.predict_loess_envelope <- function(x, y, grid) {
  fit <- tryCatch(
    suppressWarnings(stats::loess(y ~ x, span = 0.75, control = stats::loess.control(surface = "direct"))),
    error = function(e) NULL
  )
  if (is.null(fit)) {
    return(rep(NA_real_, length(grid)))
  }
  out <- tryCatch(
    suppressWarnings(stats::predict(fit, newdata = data.frame(x = grid))),
    error = function(e) rep(NA_real_, length(grid))
  )
  as.numeric(out)
}

.theme_multisitedgp <- function(base_size = 14) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      axis.title = ggplot2::element_text(face = "bold"),
      strip.text = ggplot2::element_text(face = "bold", size = base_size + 1),
      legend.position = "bottom",
      plot.title = ggplot2::element_text(face = "bold", size = base_size + 2),
      plot.caption = ggplot2::element_text(size = base_size - 2, colour = "grey40", hjust = 0)
    )
}
# nolint end
