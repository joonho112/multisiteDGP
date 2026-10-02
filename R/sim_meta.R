#' Simulate site-level summary data with standard errors set directly
#'
#' Simulates one estimated effect and one standard error per site, in four
#' steps: site effects, standard errors set directly from a target
#' informativeness `I` and heterogeneity ratio `R`, an optional dependence
#' between effects and precision, and observed site estimates. By default the
#' standard errors are fixed values that give exactly the requested `I` and
#' `R`. In a meta-analysis the sites are the studies. Use [sim_multisite()] to
#' derive the standard errors from simulated site sizes instead.
#'
#' @param design A design from [multisitedgp_design()] with
#'   `paradigm = "direct"`, or a preset such as [preset_meta_modest()]. If
#'   `NULL` (the default), the design is built from `...`.
#' @param ... Arguments passed to [multisitedgp_design()] with
#'   `paradigm = "direct"` when `design` is `NULL`, which must include `I`.
#'   Giving both `design` and `...` is an error.
#' @param seed Integer seed, or `NULL` (the default). With a seed the same
#'   call gives the same data in any R session and leaves the random number
#'   generator of your session as it was. Overrides `design$seed`.
#'
#' @return A tibble of class `multisitedgp_meta`, which inherits from
#'   [multisitedgp_data], with the same columns as [sim_multisite()] returns.
#'   `n_j` is `NA` unless a user-supplied `se_fn` returns site sizes.
#'
#' @seealso [gen_se_direct()], [informativeness()], [heterogeneity_ratio()],
#'   `vignette("site-sizes-and-standard-errors")`.
#' @family simulation
#' @examples
#' sim_meta(preset_meta_modest(), seed = 1)
#'
#' # Build the design from arguments, then check the realized I and R
#' dat <- sim_meta(J = 20, I = 0.3, R = 2, seed = 1)
#' informativeness(dat)
#' heterogeneity_ratio(dat$se2_j)
#' @export
sim_meta <- function(design = NULL, ..., seed = NULL) {
  dots <- list(...)
  design <- .resolve_sim_meta_design(design = design, dots = dots, seed = seed)

  run_steps <- function() {
    out <- gen_effects(
      J = design$J,
      true_dist = design$true_dist,
      tau = design$tau,
      sigma_tau = design$sigma_tau,
      theta_G = design$theta_G,
      formula = design$formula,
      beta = design$beta,
      data = design$data,
      g_fn = design$g_fn,
      g_returns = design$g_returns
    )
    out <- .apply_direct_precision(out, design = design)
    out <- .apply_design_dependence(out, design = design)
    gen_observations(out, obs_fn = design$obs_spec$obs_fn)
  }

  out <- if (is.null(design$seed)) {
    run_steps()
  } else {
    .with_reproducible_seed(design$seed, run_steps())
  }

  diagnostics <- .initial_meta_diagnostics(out, design = design)
  out <- .new_multisitedgp_meta(.order_sim_columns(out, design = design), design = design, diagnostics = diagnostics)
  .attach_core_diagnostics_table(out)
}

.resolve_sim_meta_design <- function(design, dots, seed = NULL) {
  if (is.null(design)) {
    .preflight_sim_meta_dots(dots)
    design <- do.call(multisitedgp_design, c(list(paradigm = "direct"), dots))
  } else {
    .validate_empty_wrapper_dots(dots, caller = "sim_meta")
    if (!is_multisitedgp_design(design)) {
      .abort(
        "`design` must be a design from `multisitedgp_design()` or a preset.",
        .you_passed(design),
        "The arguments of a design can also be given to `sim_meta()` directly."
      )
    }
    validate_multisitedgp_design(design)
    if (!identical(design$paradigm, "direct")) {
      .abort(
        "`sim_meta()` requires `paradigm = \"direct\"`.",
        sprintf("The design has `paradigm = \"%s\"`.", design$paradigm),
        "Use `sim_multisite()` for this design."
      )
    }
  }

  if (!is.null(seed)) {
    design <- update_multisitedgp_design(design, seed = seed)
  }
  design
}

.preflight_sim_meta_dots <- function(dots) {
  if (length(dots) == 0L) {
    return(invisible(TRUE))
  }
  supplied <- names(dots)
  if (is.null(supplied)) {
    supplied <- rep("", length(dots))
  }
  supplied[is.na(supplied)] <- ""
  supplied[!nzchar(supplied)] <- "<unnamed>"

  if ("<unnamed>" %in% supplied) {
    .abort(
      "Every argument in `...` must be named.",
      "Each one is an argument of `multisitedgp_design()`, such as `J = 50` or `I = 0.3`."
    )
  }

  if ("set_seed" %in% supplied) {
    .abort(
      "`set_seed` is not supported by `sim_meta()`.",
      fix = "Give the seed as `seed`."
    )
  }

  misplaced <- intersect(
    supplied,
    c("nj_mean", "cv", "nj_min", "p", "R2", "var_outcome", "engine", "n_j", "n_per_site")
  )
  if (length(misplaced) > 0L) {
    .abort(
      "`sim_meta()` does not take site-size arguments.",
      sprintf("You passed: %s.", paste(misplaced, collapse = ", ")),
      "Use `sim_multisite()` to derive the standard errors from site sizes."
    )
  }

  if ("paradigm" %in% supplied) {
    .abort(
      "`sim_meta()` does not accept `paradigm`.",
      "It always sets the standard errors directly.",
      "Use `sim_multisite()` to derive the standard errors from site sizes."
    )
  }

  invisible(TRUE)
}

.apply_direct_precision <- function(upstream, design) {
  gen_se_direct(
    upstream,
    J = design$J,
    I = design$I,
    R = design$R,
    shuffle = design$shuffle,
    sigma_tau = design$sigma_tau,
    se_fn = design$se_fn,
    se_args = design$se_args
  )
}

.initial_meta_diagnostics <- function(x, design) {
  diagnostics <- .initial_sim_diagnostics(x, design = design)
  direct_se_diagnostics <- attr(x, "direct_se_diagnostics", exact = TRUE)
  se_generator <- attr(x, "engine", exact = TRUE)
  if (!is.null(se_generator)) {
    diagnostics$engine <- se_generator
  }
  diagnostics$target_I <- design$I
  diagnostics$target_R <- design$R
  diagnostics$shuffle <- design$shuffle
  diagnostics$direct_se_method <- if (is.null(design$se_fn)) "grid" else "custom"
  diagnostics$I_error <- if (identical(diagnostics$direct_se_method, "grid")) {
    unname(diagnostics$I_hat - design$I)
  } else {
    NA_real_
  }
  diagnostics$R_error <- if (identical(diagnostics$direct_se_method, "grid")) {
    unname(diagnostics$R_hat - design$R)
  } else {
    NA_real_
  }
  diagnostics$I_exact <- if (identical(diagnostics$direct_se_method, "grid")) {
    is.finite(diagnostics$I_error) && abs(diagnostics$I_error) <= .Machine$double.eps^0.5
  } else {
    NA
  }
  diagnostics$R_exact <- if (identical(diagnostics$direct_se_method, "grid")) {
    is.finite(diagnostics$R_error) && abs(diagnostics$R_error) <= .Machine$double.eps^0.5
  } else {
    NA
  }
  diagnostics$direct_se_diagnostics <- direct_se_diagnostics
  diagnostics
}
