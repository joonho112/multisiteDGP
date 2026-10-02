#' Preset designs for common settings
#'
#' Each function returns a design with preset values. Print it to see them all.
#'
#' @section Preset values:
#' | Preset | `J` | `true_dist` | `sigma_tau` | Standard errors | Source |
#' |---|--:|---|--:|---|---|
#' | `preset_education_small()` | 50 | Gaussian | 0.05 | from truncated-gamma site sizes, mean 40 | Weiss et al. (2017), in part |
#' | `preset_education_modest()` | 50 | Gaussian | 0.2 | from truncated-gamma site sizes, mean 50 | Weiss et al. (2017), in part |
#' | `preset_education_substantial()` | 100 | Gaussian | 0.3 | from truncated-gamma site sizes, mean 80 | Weiss et al. (2017), in part |
#' | `preset_jebs_paper()` | 50 | Mixture | 0.2 | from censored site sizes, mean 40 | Lee et al. (2025) |
#' | `preset_jebs_strict()` | 100 | Mixture | 0.15 | from censored site sizes, mean 80 | Lee et al. (2025) |
#' | `preset_walters_2024()` | 46 | Gaussian | 0.197 | from truncated-gamma site sizes, mean 240 | Walters (2024), in part |
#' | `preset_twin_towers()` | 1000 | Mixture | 2 | from equal site sizes of 100 | assumed |
#' | `preset_meta_modest()` | 50 | Gaussian | 0.2 | set directly, `I = 0.3`, `R = 1.5` | assumed |
#' | `preset_small_area_estimation()` | 30 | Gaussian | 0.2 | set directly, `I = 0.2`, `R = 3` | assumed |
#'
#' * The education presets take `sigma_tau` from the small end, the middle, and
#'   the upper end of the range of cross-site standard deviations of effects
#'   that Weiss et al. (2017) report for 16 multisite randomized trials.
#'   Everything else in these presets is an assumption of the package.
#' * Two presets are named after Lee et al. (2025), published in the Journal
#'   of Educational and Behavioral Statistics (JEBS). Both are cells of the
#'   paper's simulation grid, with its two-component mixture and its censored
#'   site-size generator. For a seed, each returns the numbers of the paper's
#'   code once the sites are sorted by size, as that code sorts them.
#' * `preset_walters_2024()` takes `J`, `sigma_tau`, and an average sampling
#'   variance of 0.010 from Walters (2024). `nj_mean = 240` gives a site of
#'   that size this variance with `p = 0.5`, `R2 = 0.4`, and `var_outcome = 1`.
#'   The other values are assumed, and the preset has no dependence between
#'   effects and precision. `attr(design, "preset_metadata")` lists which
#'   values come from the source.
#' * `preset_twin_towers()` is a bimodal mixture with equal site sizes, of the
#'   kind used to test methods that recover the shape of an effect distribution.
#' * `preset_small_area_estimation()` has a wider spread of precision than
#'   `preset_meta_modest()`, as is typical of small-area estimates.
#' @param ... Arguments of [multisitedgp_design()], given by name, that replace
#'   the preset's values. A replacement that conflicts with the rest of the
#'   preset is an error, for example `nj_mean` in `preset_meta_modest()`.
#' @return A design as returned by [multisitedgp_design()], for [sim_meta()] if
#'   the standard errors are set directly and for [sim_multisite()] otherwise.
#' @seealso [multisitedgp_design()], [sim_multisite()], [sim_meta()],
#'   `vignette("diagnostics-and-presets")`.
#' @references Lee, J., Che, J., Rabe-Hesketh, S., Feller, A., & Miratrix, L.
#' (2025). Improving the estimation of site-specific effects and their
#' distribution in multisite trials. *Journal of Educational and Behavioral
#' Statistics*, 50(5), 731-764. \doi{10.3102/10769986241254286}
#'
#' Walters, C. (2024). Empirical Bayes methods in labor economics. In
#' *Handbook of Labor Economics* (Vol. 5, pp. 183-260). Elsevier.
#' \doi{10.1016/bs.heslab.2024.11.001}
#'
#' Weiss, M. J., Bloom, H. S., Verbitsky-Savitz, N., Gupta, H., Vigil, A. E.,
#' & Cullinan, D. N. (2017). How much do the effects of education and
#' training programs vary across sites? Evidence from past multisite
#' randomized trials. *Journal of Research on Educational Effectiveness*,
#' 10(4), 843-876. \doi{10.1080/19345747.2017.1300719}
#' @name presets
#' @examples
#' preset_education_modest()
#' # Arguments given by name replace the preset's values
#' sim_multisite(preset_education_modest(J = 100, sigma_tau = 0.1), seed = 1)
NULL

#' @rdname presets
#' @export
preset_education_small <- function(...) {
  .preset_design(
    list(
      J = 50L,
      paradigm = "site_size",
      true_dist = "Gaussian",
      tau = 0,
      sigma_tau = 0.05,
      nj_mean = 40,
      cv = 0.50,
      nj_min = 5L,
      p = 0.5,
      R2 = 0,
      engine = "truncated_gamma"
    ),
    list(...)
  )
}

#' @rdname presets
#' @export
preset_education_modest <- function(...) {
  .preset_design(
    list(
      J = 50L,
      paradigm = "site_size",
      true_dist = "Gaussian",
      tau = 0,
      sigma_tau = 0.20,
      nj_mean = 50,
      cv = 0.50,
      nj_min = 10L,
      p = 0.5,
      R2 = 0,
      engine = "truncated_gamma"
    ),
    list(...)
  )
}

#' @rdname presets
#' @export
preset_education_substantial <- function(...) {
  .preset_design(
    list(
      J = 100L,
      paradigm = "site_size",
      true_dist = "Gaussian",
      tau = 0,
      sigma_tau = 0.30,
      nj_mean = 80,
      cv = 0.50,
      nj_min = 10L,
      p = 0.5,
      R2 = 0,
      engine = "truncated_gamma"
    ),
    list(...)
  )
}

#' @rdname presets
#' @export
preset_jebs_paper <- function(...) {
  .preset_design(
    list(
      J = 50L,
      paradigm = "site_size",
      true_dist = "Mixture",
      theta_G = list(delta = 5, eps = 0.3, ups = 2),
      tau = 0,
      sigma_tau = 0.20,
      nj_mean = 40,
      cv = 0.50,
      nj_min = 5L,
      p = 0.5,
      R2 = 0,
      engine = "censored",
      dependence = "none"
    ),
    list(...)
  )
}

#' @rdname presets
#' @export
preset_jebs_strict <- function(...) {
  .preset_design(
    list(
      J = 100L,
      paradigm = "site_size",
      true_dist = "Mixture",
      theta_G = list(delta = 5, eps = 0.3, ups = 2),
      tau = 0,
      sigma_tau = 0.15,
      nj_mean = 80,
      cv = 0.50,
      nj_min = 5L,
      p = 0.5,
      R2 = 0,
      engine = "censored",
      dependence = "none"
    ),
    list(...)
  )
}

#' @rdname presets
#' @export
preset_walters_2024 <- function(...) {
  overrides <- list(...)
  design <- .preset_design(
    list(
      J = 46L,
      paradigm = "site_size",
      true_dist = "Gaussian",
      tau = 0,
      sigma_tau = 0.197,
      nj_mean = 240,
      cv = 0.30,
      nj_min = 50L,
      p = 0.5,
      R2 = 0.40,
      engine = "truncated_gamma",
      dependence = "none"
    ),
    overrides
  )
  attr(design, "preset_name") <- "preset_walters_2024"
  attr(design, "preset_metadata") <- list(
    status = "Walters-anchored proxy; not a chapter-replication design",
    direct = list(
      J = 46L,
      average_se2 = 0.010,
      sigma_tau = 0.197,
      precision_dependence = "weak"
    ),
    derived = list(
      nj_mean = 240,
      rule = "kappa / average_se2 with kappa = 2.4 under assumed p, R2, and var_outcome"
    ),
    assumed = list(
      true_dist = "Gaussian",
      p = 0.5,
      outcome_R2 = 0.40,
      var_outcome = 1,
      cv = 0.30,
      nj_min = 50L
    ),
    excluded_source_quantity = list(
      conditional_prior_R2 = 0.502,
      note = "Walters' conditional-prior R2 is not the preset's outcome R2"
    ),
    overrides = names(overrides)
  )
  design
}

#' @rdname presets
#' @export
preset_twin_towers <- function(...) {
  .preset_design(
    list(
      J = 1000L,
      paradigm = "site_size",
      true_dist = "Mixture",
      theta_G = list(delta = 4, eps = 0.5, ups = 1),
      tau = 0,
      sigma_tau = 2.0,
      nj_mean = 100,
      cv = 0,
      nj_min = 100L,
      p = 0.5,
      R2 = 0,
      engine = "truncated_gamma",
      dependence = "none"
    ),
    list(...)
  )
}

#' @rdname presets
#' @export
preset_meta_modest <- function(...) {
  .preset_design(
    list(
      J = 50L,
      paradigm = "direct",
      true_dist = "Gaussian",
      tau = 0,
      sigma_tau = 0.20,
      I = 0.30,
      R = 1.5,
      shuffle = TRUE,
      dependence = "none"
    ),
    list(...)
  )
}

#' @rdname presets
#' @export
preset_small_area_estimation <- function(...) {
  .preset_design(
    list(
      J = 30L,
      paradigm = "direct",
      true_dist = "Gaussian",
      tau = 0,
      sigma_tau = 0.20,
      I = 0.20,
      R = 3.0,
      shuffle = TRUE,
      dependence = "none"
    ),
    list(...)
  )
}

.preset_design <- function(base, overrides) {
  if (length(overrides) == 0L) {
    return(do.call(multisitedgp_design, base))
  }

  supplied <- names(overrides)
  if (is.null(supplied)) {
    supplied <- rep("", length(overrides))
  }
  supplied[is.na(supplied)] <- ""
  if (any(!nzchar(supplied))) {
    .abort(
      "Every argument in `...` must be named.",
      "Each one is an argument of `multisitedgp_design()`, such as `J = 100`."
    )
  }

  bad <- setdiff(supplied, names(formals(multisitedgp_design)))
  if (length(bad) > 0L) {
    .abort_unknown_design_args(bad)
  }

  base[supplied] <- overrides
  do.call(multisitedgp_design, base)
}
