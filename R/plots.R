#' Plot simulated site-level data
#'
#' `plot_effects()` draws the estimate of each site with its 95% confidence
#' interval, or the density of the estimates. `plot_funnel()` plots the
#' standard errors against the estimates, with the smallest at the top.
#' `plot_dependence()` plots the sampling variances against the site effects.
#'
#' @param x Simulated data from [sim_multisite()] or [sim_meta()].
#' @param type Character. `"caterpillar"` (the default) draws one row per site,
#'   ordered by estimate. `"density"` draws a kernel density of the estimates.
#' @param truth Logical. If `TRUE` (the default), the true site effects
#'   `tau_j` are added as a second point per site or as a second density.
#' @param monochrome Logical. If `TRUE`, the plot uses shades of gray instead
#'   of colors. Default `FALSE`.
#' @param reference Character. Where `plot_funnel()` draws its dashed vertical
#'   line: `"zero"` (the default) or `"tau"`, the grand mean of the design.
#' @param envelope Logical. If `TRUE` (the default), `plot_funnel()` adds
#'   dotted lines at the reference plus and minus `1.96 * se_j`, and
#'   `plot_dependence()` adds a 95% bootstrap band around the loess curve.
#' @param smoother Logical. If `TRUE` (the default), a loess curve is added.
#' @param by_residual Logical. If `TRUE` (the default), the horizontal axis
#'   shows the standardized site effects `z_j`. If `FALSE`, it shows `tau_j`.
#' @param ... Not used.
#'
#' @return A `ggplot` object, which can be modified with `+`. The subtitle
#'   gives the number of sites, the informativeness, the heterogeneity ratio,
#'   and the realized rank correlation, or for `plot_dependence()` the realized
#'   rank correlation and the target of the design, if it has one. Data without
#'   stored diagnostics, or with more or fewer rows than those describe, get no
#'   subtitle. That is the case after a row selection, a changed value, or
#'   `rbind()`.
#'
#' @seealso [sim_multisite()], [sim_meta()], `vignette("multisiteDGP")`.
#' @examples
#' # Rank correlation of 0.5 between site effects and sampling variances
#' design <- preset_education_modest(dependence = "rank", rank_corr = 0.5)
#' dat <- sim_multisite(design, seed = 1)
#' plot_effects(dat)
#' plot_funnel(dat)
#' plot_dependence(dat, smoother = FALSE)
#' @importFrom rlang .data
#' @name plots
NULL

#' @rdname plots
#' @export
plot_effects <- function(
  x,
  type = c("caterpillar", "density"),
  truth = TRUE,
  monochrome = FALSE,
  ...
) {
  UseMethod("plot_effects")
}

#' @export
plot_effects.default <- function(
  x,
  type = c("caterpillar", "density"),
  truth = TRUE,
  monochrome = FALSE,
  ...
) {
  .abort_not_simulated_data(x)
}

#' @export
plot_effects.multisitedgp_data <- function(
  x,
  type = c("caterpillar", "density"),
  truth = TRUE,
  monochrome = FALSE,
  ...
) {
  type <- .match_choice(type, "type", c("caterpillar", "density"))
  truth <- .validate_scalar_logical(truth, "truth")
  monochrome <- .validate_scalar_logical(monochrome, "monochrome")

  if (identical(type, "density")) {
    return(.plot_effects_density(x, truth = truth, monochrome = monochrome))
  }
  .plot_effects_caterpillar(x, truth = truth, monochrome = monochrome)
}

#' @rdname plots
#' @export
plot_funnel <- function(
  x,
  reference = c("zero", "tau"),
  envelope = TRUE,
  monochrome = FALSE,
  ...
) {
  UseMethod("plot_funnel")
}

#' @export
plot_funnel.default <- function(
  x,
  reference = c("zero", "tau"),
  envelope = TRUE,
  monochrome = FALSE,
  ...
) {
  .abort_not_simulated_data(x)
}

#' @export
plot_funnel.multisitedgp_data <- function(
  x,
  reference = c("zero", "tau"),
  envelope = TRUE,
  monochrome = FALSE,
  ...
) {
  reference <- .match_choice(reference, "reference", c("zero", "tau"))
  envelope <- .validate_scalar_logical(envelope, "envelope")
  monochrome <- .validate_scalar_logical(monochrome, "monochrome")
  ref_value <- .plot_reference_value(x, reference = reference)
  point_color <- if (isTRUE(monochrome)) "grey25" else .default_palette(1L)

  p <- ggplot2::ggplot(.strip_multisitedgp_data(x), ggplot2::aes(x = .data$tau_j_hat, y = .data$se_j)) +
    ggplot2::geom_point(color = point_color[[1L]], alpha = 0.78, size = 2.2) +
    ggplot2::geom_vline(xintercept = ref_value, linewidth = 0.8, linetype = "dashed", color = "grey45") +
    ggplot2::scale_y_reverse() +
    ggplot2::labs(
      title = "Funnel plot of the site estimates",
      subtitle = .plot_diagnostic_subtitle(x),
      x = "Estimate (tau_j_hat)",
      y = "Standard error (se_j)"
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

#' @rdname plots
#' @export
plot_dependence <- function(
  x,
  smoother = TRUE,
  envelope = TRUE,
  by_residual = TRUE,
  monochrome = FALSE,
  ...
) {
  UseMethod("plot_dependence")
}

#' @export
plot_dependence.default <- function(
  x,
  smoother = TRUE,
  envelope = TRUE,
  by_residual = TRUE,
  monochrome = FALSE,
  ...
) {
  .abort_not_simulated_data(x)
}

#' @export
plot_dependence.multisitedgp_data <- function(
  x,
  smoother = TRUE,
  envelope = TRUE,
  by_residual = TRUE,
  monochrome = FALSE,
  ...
) {
  smoother <- .validate_scalar_logical(smoother, "smoother")
  envelope <- .validate_scalar_logical(envelope, "envelope")
  by_residual <- .validate_scalar_logical(by_residual, "by_residual")
  monochrome <- .validate_scalar_logical(monochrome, "monochrome")

  plot_data <- .strip_multisitedgp_data(x)
  x_col <- if (isTRUE(by_residual)) "z_j" else "tau_j"
  point_color <- if (isTRUE(monochrome)) "grey25" else .default_palette(1L)[[1L]]
  smooth_color <- if (isTRUE(monochrome)) "grey10" else .default_palette(2L)[[2L]]

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[x_col]], y = .data$se2_j)) +
    ggplot2::geom_point(color = point_color, alpha = 0.78, size = 2.2) +
    ggplot2::labs(
      title = "Dependence between effects and precision",
      subtitle = .plot_dependence_subtitle(x, by_residual = by_residual),
      x = if (isTRUE(by_residual)) "Standardized site effect (z_j)" else "Site effect (tau_j)",
      y = "Sampling variance (se2_j)"
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

.plot_effects_caterpillar <- function(x, truth, monochrome) {
  plot_data <- .strip_multisitedgp_data(x)
  plot_data$lower <- plot_data$tau_j_hat - 1.96 * plot_data$se_j
  plot_data$upper <- plot_data$tau_j_hat + 1.96 * plot_data$se_j
  colors <- .plot_discrete_colors(if (isTRUE(truth)) 2L else 1L, monochrome)

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(y = stats::reorder(.data$site_index, .data$tau_j_hat)))
  if (isTRUE(truth)) {
    # Color and shape are mapped to a label so that the two sets of points get
    # a legend. The label is a constant within each layer.
    p <- p +
      ggplot2::geom_errorbar(
        ggplot2::aes(xmin = .data$lower, xmax = .data$upper, color = "estimate"),
        width = 0.15,
        linewidth = 0.75,
        show.legend = FALSE
      ) +
      ggplot2::geom_point(
        ggplot2::aes(x = .data$tau_j_hat, color = "estimate", shape = "estimate"),
        size = 2.1,
        alpha = 0.85
      )
  } else {
    p <- p +
      ggplot2::geom_errorbar(
        ggplot2::aes(xmin = .data$lower, xmax = .data$upper),
        width = 0.15,
        linewidth = 0.75,
        color = colors[[1L]]
      ) +
      ggplot2::geom_point(ggplot2::aes(x = .data$tau_j_hat), color = colors[[1L]], size = 2.1, alpha = 0.85)
  }
  p <- p +
    ggplot2::geom_vline(xintercept = 0, linewidth = 0.7, linetype = "dashed", color = "grey55") +
    ggplot2::labs(
      title = "Site estimates with 95% intervals",
      subtitle = .plot_diagnostic_subtitle(x),
      x = "Estimate with 95% interval (tau_j_hat)",
      y = "Site"
    ) +
    .theme_multisitedgp() +
    ggplot2::scale_y_discrete(breaks = .caterpillar_site_breaks(plot_data))

  if (isTRUE(truth)) {
    p <- p +
      ggplot2::geom_point(
        ggplot2::aes(x = .data$tau_j, color = "true effect", shape = "true effect"),
        size = 1.9,
        alpha = 0.75
      ) +
      ggplot2::scale_color_manual(
        values = c("estimate" = colors[[1L]], "true effect" = colors[[2L]]),
        name = NULL
      ) +
      ggplot2::scale_shape_manual(
        values = c("estimate" = 19L, "true effect" = if (isTRUE(monochrome)) 17L else 16L),
        name = NULL
      )
  }
  p
}

# The y axis has one label per site, and the labels collide beyond a few dozen
# sites, so keep at most .CATERPILLAR_MAX_LABELS of them, every k-th site in
# effect order.
.CATERPILLAR_MAX_LABELS <- 25L

.caterpillar_site_breaks <- function(plot_data) {
  ordered_sites <- plot_data$site_index[order(plot_data$tau_j_hat)]
  n <- length(ordered_sites)
  if (n <= .CATERPILLAR_MAX_LABELS) {
    return(as.character(ordered_sites))
  }
  keep <- seq(1L, n, by = ceiling(n / .CATERPILLAR_MAX_LABELS))
  as.character(ordered_sites[keep])
}

.plot_effects_density <- function(x, truth, monochrome) {
  plot_data <- tibble::tibble(
    effect = x$tau_j_hat,
    series = "estimate"
  )
  if (isTRUE(truth)) {
    plot_data <- dplyr::bind_rows(plot_data, tibble::tibble(effect = x$tau_j, series = "true effect"))
  }
  series_levels <- intersect(c("true effect", "estimate"), plot_data$series)
  plot_data$series <- factor(plot_data$series, levels = series_levels)
  # Named, so that the estimates and the true effects have the colors they
  # have in the caterpillar plot.
  colors <- stats::setNames(.plot_discrete_colors(2L, monochrome), c("estimate", "true effect"))[series_levels]

  ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$effect, color = .data$series, fill = .data$series)) +
    ggplot2::geom_density(alpha = if (isTRUE(monochrome)) 0.15 else 0.28, linewidth = 0.9) +
    ggplot2::geom_vline(xintercept = 0, linewidth = 0.7, linetype = "dashed", color = "grey55") +
    ggplot2::scale_color_manual(values = colors, breaks = rev(series_levels)) +
    ggplot2::scale_fill_manual(values = colors, breaks = rev(series_levels)) +
    ggplot2::labs(
      title = if (isTRUE(truth)) {
        "Distribution of site estimates and true effects"
      } else {
        "Distribution of site estimates"
      },
      subtitle = .plot_diagnostic_subtitle(x),
      x = if (isTRUE(truth)) "Estimate (tau_j_hat) or true effect (tau_j)" else "Estimate (tau_j_hat)",
      y = "Density",
      color = NULL,
      fill = NULL
    ) +
    .theme_multisitedgp()
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

# The number of sites and three diagnostics, in the words of `print()` and
# `summary()`. A diagnostic without a finite value is left out.
.plot_diagnostic_subtitle <- function(x) {
  diag <- .stored_diagnostics(x)
  if (!is.list(diag)) {
    return(NULL)
  }
  parts <- c(
    .plot_subtitle_part("%s sites", diag$J, digits = 0L),
    .plot_subtitle_part("informativeness %s", diag$I_hat),
    .plot_subtitle_part("SE heterogeneity %s", diag$R_hat),
    .plot_subtitle_part("rank correlation %s", diag$rho_S_residual)
  )
  if (length(parts) == 0L) {
    return(NULL)
  }
  paste(parts, collapse = ", ")
}

# The realized rank correlation, followed by the target of the design when the
# built-in method has one.
.plot_dependence_subtitle <- function(x, by_residual) {
  diag <- .stored_diagnostics(x)
  if (!is.list(diag)) {
    return(NULL)
  }
  rho <- if (isTRUE(by_residual)) diag$rho_S_residual else diag$rho_S_marginal
  label <- if (isTRUE(by_residual)) "rank correlation" else "marginal rank correlation"
  if (!.is_finite_number(rho)) {
    return(paste(label, "not defined"))
  }
  method <- diag$dependence_method
  user_fn <- identical(diag$dependence_diagnostics$method, "custom")
  # The target of a design is for the correlation with z_j. With covariates
  # the correlation with tau_j is a different quantity and has no target.
  has_covariates <- !is.null(attr(x, "design", exact = TRUE)$formula)
  target <- if (user_fn || is.null(method) || (!isTRUE(by_residual) && has_covariates)) {
    NULL
  } else if (method %in% c("rank", "hybrid")) {
    .plot_subtitle_part("target %s", diag$target_rank_corr)
  } else if (identical(method, "copula")) {
    .plot_subtitle_part("target Pearson correlation %s", diag$target_pearson_corr)
  }
  paste(c(sprintf("%s %s", label, .format_plot_value(rho)), target), collapse = ", ")
}

.plot_subtitle_part <- function(template, value, digits = 3L) {
  if (!.is_finite_number(value)) {
    return(NULL)
  }
  sprintf(template, .format_plot_value(value, digits = digits))
}

.format_plot_value <- function(x, digits = 3L) {
  formatC(x[[1L]], digits = digits, format = "f")
}

.plot_discrete_colors <- function(n, monochrome) {
  if (isTRUE(monochrome)) {
    return(grDevices::gray.colors(n, start = 0.2, end = 0.65))
  }
  .default_palette(n)
}

# The colors are stored here so that the package needs no palette package.
# .PALETTE_SET2 is the ColorBrewer "Set2" palette. .PALETTE_VIRIDIS is the
# viridis scale in 256 steps, cut at 0.9 to leave out the lightest yellow.
.PALETTE_SET2 <- c(
  "#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3"
)
.PALETTE_VIRIDIS <- c(
  "#440154FF", "#440256FF", "#450457FF", "#450558FF", "#46065AFF", "#46085BFF", "#46095DFF", "#460A5DFF",
  "#460B5EFF", "#470D60FF", "#470E61FF", "#471063FF", "#471164FF", "#471265FF", "#481466FF", "#481568FF",
  "#481768FF", "#481769FF", "#48186AFF", "#481A6CFF", "#481B6DFF", "#481C6EFF", "#481D6FFF", "#481E70FF",
  "#482070FF", "#482072FF", "#482274FF", "#482374FF", "#482475FF", "#482576FF", "#482677FF", "#482878FF",
  "#482979FF", "#472A7AFF", "#472B7AFF", "#472D7AFF", "#472D7BFF", "#472E7CFF", "#472F7DFF", "#46307EFF",
  "#46327EFF", "#46337FFF", "#463480FF", "#453581FF", "#453681FF", "#453881FF", "#453883FF", "#443983FF",
  "#443A83FF", "#443B84FF", "#433D84FF", "#433E85FF", "#423F85FF", "#424086FF", "#424186FF", "#414186FF",
  "#414387FF", "#414487FF", "#404588FF", "#404688FF", "#3F4788FF", "#3F4889FF", "#3E4989FF", "#3E4A89FF",
  "#3E4B8AFF", "#3D4D8AFF", "#3D4D8AFF", "#3D4E8AFF", "#3C4F8AFF", "#3C508BFF", "#3B518BFF", "#3B528BFF",
  "#3A538BFF", "#3A548CFF", "#39558CFF", "#39558CFF", "#39578CFF", "#38588CFF", "#38598CFF", "#375A8CFF",
  "#375B8DFF", "#365C8DFF", "#365D8DFF", "#355E8DFF", "#355F8DFF", "#35608DFF", "#34608DFF", "#34618DFF",
  "#33628DFF", "#33638DFF", "#32648EFF", "#32658EFF", "#31668EFF", "#31678EFF", "#31688EFF", "#30688EFF",
  "#30698EFF", "#306A8EFF", "#2F6B8EFF", "#2F6C8EFF", "#2E6D8EFF", "#2E6E8EFF", "#2E6F8EFF", "#2D708EFF",
  "#2D718EFF", "#2D718EFF", "#2C718EFF", "#2C728EFF", "#2C738EFF", "#2B748EFF", "#2B758EFF", "#2A768EFF",
  "#2A778EFF", "#2A788EFF", "#29798EFF", "#29798EFF", "#297A8EFF", "#297B8EFF", "#287C8EFF", "#287D8EFF",
  "#277E8EFF", "#277F8EFF", "#27808EFF", "#26818EFF", "#26828EFF", "#26828EFF", "#26828EFF", "#25838EFF",
  "#25848EFF", "#25858EFF", "#24868EFF", "#24878EFF", "#23888EFF", "#23898EFF", "#238A8DFF", "#238B8DFF",
  "#228B8DFF", "#228C8DFF", "#228D8DFF", "#218E8DFF", "#218F8DFF", "#21908DFF", "#21918CFF", "#20928CFF",
  "#20928CFF", "#20928CFF", "#20938CFF", "#1F948CFF", "#1F958BFF", "#1F968BFF", "#1F978BFF", "#1F988BFF",
  "#1F998AFF", "#1F9A8AFF", "#1E9B8AFF", "#1E9B8AFF", "#1E9C89FF", "#1E9D89FF", "#1F9E89FF", "#1F9F88FF",
  "#1FA088FF", "#1FA188FF", "#1FA187FF", "#1FA287FF", "#20A386FF", "#20A386FF", "#20A486FF", "#21A585FF",
  "#21A685FF", "#22A785FF", "#22A884FF", "#23A983FF", "#24AA83FF", "#25AB82FF", "#25AC82FF", "#25AD82FF",
  "#26AD81FF", "#27AD81FF", "#28AE80FF", "#29AF7FFF", "#2AB07FFF", "#2CB17EFF", "#2DB27DFF", "#2EB37CFF",
  "#2EB47CFF", "#30B47CFF", "#31B57BFF", "#33B67AFF", "#34B679FF", "#35B779FF", "#37B878FF", "#38B977FF",
  "#3ABA76FF", "#3BBB75FF", "#3CBC74FF", "#3EBC73FF", "#3FBC73FF", "#40BD72FF", "#42BE71FF", "#44BF70FF",
  "#46C06FFF", "#48C16EFF", "#4AC16DFF", "#4BC26CFF", "#4DC36BFF", "#4FC36AFF", "#51C46AFF", "#53C569FF",
  "#54C568FF", "#56C667FF", "#58C765FF", "#5AC864FF", "#5CC863FF", "#5DC962FF", "#5FCA61FF", "#62CB5FFF",
  "#64CB5FFF", "#66CB5DFF", "#67CC5CFF", "#69CD5BFF", "#6CCD5AFF", "#6ECE58FF", "#70CF57FF", "#72D056FF",
  "#74D055FF", "#76D054FF", "#78D152FF", "#7BD151FF", "#7DD250FF", "#7FD34EFF", "#81D34DFF", "#84D44BFF",
  "#86D549FF", "#88D548FF", "#8AD647FF", "#8DD645FF", "#8FD644FF", "#91D742FF", "#93D741FF", "#95D840FF",
  "#98D83EFF", "#9BD93CFF", "#9DD93BFF", "#9FDA3AFF", "#A1DA38FF", "#A3DA37FF", "#A6DB35FF", "#A9DB33FF",
  "#ABDC32FF", "#ADDC30FF", "#B0DD2FFF", "#B2DD2DFF", "#B4DE2BFF", "#B7DE29FF", "#B9DE28FF", "#BBDF27FF"
)

.default_palette <- function(n, type = c("discrete", "continuous")) {
  type <- .match_choice(type, "type", c("discrete", "continuous"))
  n <- .validate_scalar_integer(n, "n")
  if (n < 1L) {
    .abort("`n` must be at least 1.", .you_passed(n))
  }
  if (identical(type, "continuous")) {
    return(.PALETTE_VIRIDIS)
  }
  if (n <= 8L) {
    return(.PALETTE_SET2[seq_len(n)])
  }
  # The Set2 palette has eight colors, so a larger request takes evenly spaced
  # steps of the viridis scale.
  .PALETTE_VIRIDIS[round(seq(1, length(.PALETTE_VIRIDIS), length.out = n))]
}

.dependence_bootstrap_envelope <- function(x, y, grid_n = 80L, n_boot = 80L) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]
  y <- y[ok]
  if (length(x) < 10L || length(unique(x)) < 4L) {
    return(NULL)
  }
  grid <- seq(min(x), max(x), length.out = grid_n)
  fits <- .with_reproducible_seed(15074L, replicate(n_boot, {
    idx <- sample.int(length(x), replace = TRUE)
    .predict_loess_envelope(x[idx], y[idx], grid)
  }))
  if (is.null(dim(fits))) {
    return(NULL)
  }
  valid <- rowSums(is.finite(fits)) >= max(5L, floor(0.25 * n_boot))
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
      # At the base size the subtitle does not fit a figure 7 inches wide.
      plot.subtitle = ggplot2::element_text(size = base_size - 3)
    )
}
