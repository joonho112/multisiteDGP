#' Simulate site-level summary data for a multisite trial
#'
#' Simulates one estimated effect and one standard error per site, in four
#' steps: site effects, site sizes and the standard errors derived from them,
#' an optional dependence between effects and precision, and observed site
#' estimates. Use [sim_meta()] to set the standard errors directly from a
#' target informativeness `I` and heterogeneity ratio `R` instead.
#'
#' @details
#' Each step is a function of its own: [gen_effects()], [gen_site_sizes()], a
#' dependence function such as [align_rank_corr()], and [gen_observations()].
#'
#' @param design A design from [multisitedgp_design()] with
#'   `paradigm = "site_size"`, or a preset such as [preset_education_modest()].
#'   If `NULL` (the default), the design is built from `...`.
#' @param ... Arguments passed to [multisitedgp_design()] with
#'   `paradigm = "site_size"` when `design` is `NULL`. Giving both `design`
#'   and `...` is an error.
#' @param seed Integer seed, or `NULL` (the default). With a seed the same
#'   call gives the same data in any R session and leaves the random number
#'   generator of your session as it was. Overrides `design$seed`.
#'
#' @return A tibble of class [multisitedgp_data] with one row per site and
#'   columns `site_index`, `z_j` (standardized site effect), `tau_j` (true
#'   site effect), `tau_j_hat` (observed estimate), `se_j` (standard error),
#'   `se2_j` (sampling variance), and `n_j` (site size), followed by the
#'   columns of the design's `data`, if any. The attributes `design` and
#'   `diagnostics` hold the design and the diagnostics that `summary()` prints.
#'
#' @seealso [multisitedgp_design()], [presets], `vignette("multisiteDGP")`.
#' @family simulation
#' @examples
#' dat <- sim_multisite(preset_education_modest(), seed = 1)
#' dat
#'
#' # Build the design from arguments instead of a preset
#' sim_multisite(J = 20, sigma_tau = 0.1, nj_mean = 80, seed = 1)
#' @export
sim_multisite <- function(design = NULL, ..., seed = NULL) {
  dots <- list(...)
  design <- .resolve_sim_multisite_design(design = design, dots = dots, seed = seed)

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
    out <- gen_site_sizes(
      out,
      J = design$J,
      nj_mean = design$nj_mean,
      cv = design$cv,
      nj_min = design$nj_min,
      p = design$p,
      R2 = design$R2,
      var_outcome = design$var_outcome,
      engine = design$engine
    )
    out <- .apply_design_dependence(out, design = design)
    gen_observations(out, obs_fn = design$obs_spec$obs_fn)
  }

  out <- if (is.null(design$seed)) {
    run_steps()
  } else {
    .with_reproducible_seed(design$seed, run_steps())
  }

  diagnostics <- .initial_sim_diagnostics(out, design = design)
  out <- .new_multisitedgp_data(.order_sim_columns(out, design = design), design = design, diagnostics = diagnostics)
  .attach_core_diagnostics_table(out)
}

.resolve_sim_multisite_design <- function(design, dots, seed = NULL) {
  if (is.null(design)) {
    .preflight_sim_multisite_dots(dots)
    design <- do.call(multisitedgp_design, c(list(paradigm = "site_size"), dots))
  } else {
    .validate_empty_wrapper_dots(dots, caller = "sim_multisite")
    if (!is_multisitedgp_design(design)) {
      .abort(
        "`design` must be a design from `multisitedgp_design()` or a preset.",
        .you_passed(design),
        "The arguments of a design can also be given to `sim_multisite()` directly."
      )
    }
    validate_multisitedgp_design(design)
    if (!identical(design$paradigm, "site_size")) {
      .abort(
        "`sim_multisite()` requires `paradigm = \"site_size\"`.",
        sprintf("The design has `paradigm = \"%s\"`.", design$paradigm),
        "Use `sim_meta()` for this design."
      )
    }
  }

  if (!is.null(seed)) {
    design <- update_multisitedgp_design(design, seed = seed)
  }
  design
}

.preflight_sim_multisite_dots <- function(dots) {
  if (length(dots) == 0L) {
    return(invisible(TRUE))
  }
  supplied <- names(dots)
  if (is.null(supplied)) {
    supplied <- rep("", length(dots))
  }
  supplied[is.na(supplied)] <- ""
  supplied[!nzchar(supplied)] <- "<unnamed>"

  if ("set_seed" %in% supplied) {
    .abort(
      "`set_seed` is not supported by `sim_multisite()`.",
      fix = "Give the seed as `seed`."
    )
  }

  misplaced <- intersect(supplied, c("I", "R", "shuffle", "se_fn", "se_args"))
  if (length(misplaced) > 0L) {
    .abort(
      "`sim_multisite()` does not take `I`, `R`, `shuffle`, `se_fn`, or `se_args`.",
      sprintf("You passed: %s.", paste(misplaced, collapse = ", ")),
      "Use `sim_meta()` to set the standard errors directly."
    )
  }

  if ("paradigm" %in% supplied) {
    .abort(
      "`sim_multisite()` does not accept `paradigm`.",
      "It always derives the standard errors from site sizes.",
      "Use `sim_meta()` to set the standard errors directly."
    )
  }

  invisible(TRUE)
}

.validate_empty_wrapper_dots <- function(dots, caller) {
  if (length(dots) > 0L) {
    supplied <- names(dots)
    if (is.null(supplied)) {
      supplied <- rep("", length(dots))
    }
    supplied[!nzchar(supplied)] <- "<unnamed>"
    .abort(
      sprintf("`...` must be empty when `design` is supplied to `%s()`.", caller),
      sprintf("You also passed: %s.", paste(supplied, collapse = ", ")),
      "Change the design with `update_multisitedgp_design()`, or give all arguments without `design`."
    )
  }
  invisible(TRUE)
}

.apply_design_dependence <- function(upstream, design) {
  spec <- design$dependence_spec
  switch(spec$method,
    none = upstream,
    rank = align_rank_corr(
      upstream,
      rank_corr = spec$rank_corr,
      max_iter = spec$max_iter,
      tol = spec$tol,
      dependence_fn = spec$dependence_fn
    ),
    copula = align_copula_corr(
      upstream,
      pearson_corr = spec$pearson_corr,
      dependence_fn = spec$dependence_fn
    ),
    hybrid = align_hybrid_corr(
      upstream,
      rank_corr = spec$rank_corr,
      init = spec$hybrid_init,
      polish = spec$hybrid_polish,
      max_iter = spec$max_iter,
      tol = spec$tol,
      dependence_fn = spec$dependence_fn
    )
  )
}

.order_sim_columns <- function(x, design = NULL) {
  x <- tibble::as_tibble(x)
  if (!is.null(design) && is.null(design$formula) && "tau_j" %in% names(x)) {
    # Without covariates, keep z_j equal to tau_j standardized by `tau` and
    # `sigma_tau`.
    x$z_j <- (x$tau_j - design$tau) / design$sigma_tau
  }
  leading <- .data_columns()
  leading <- leading[leading %in% names(x)]
  trailing <- setdiff(names(x), c(leading, "latent_component"))
  x[c(leading, trailing)]
}

.initial_sim_diagnostics <- function(x, design) {
  dependence_diagnostics <- attr(x, "dependence_diagnostics", exact = TRUE)
  observation_diagnostics <- attr(x, "observation_diagnostics", exact = TRUE)
  perm <- attr(x, "permutation_perm", exact = TRUE)

  rho_s_residual <- .safe_cor(x$z_j, x$se2_j, method = "spearman")
  rho_s_marginal <- .safe_cor(x$tau_j, x$se2_j, method = "spearman")
  rho_p_residual <- .safe_cor(x$z_j, x$se2_j, method = "pearson")
  rho_p_marginal <- .safe_cor(x$tau_j, x$se2_j, method = "pearson")

  list(
    J = nrow(x),
    paradigm = design$paradigm,
    true_dist = design$true_dist,
    engine = design$engine,
    seed = design$seed,
    I_hat = compute_I(x$se2_j, sigma_tau = design$sigma_tau, tau_j = x$tau_j),
    R_hat = max(x$se2_j) / min(x$se2_j),
    GM_se2 = exp(mean(log(x$se2_j))),
    rho_S_residual = unname(rho_s_residual),
    rho_S_marginal = unname(rho_s_marginal),
    rho_P_residual = unname(rho_p_residual),
    rho_P_marginal = unname(rho_p_marginal),
    target_sigma_tau_resid = design$sigma_tau,
    sigma_tau_resid = if (nrow(x) > 1L) stats::sd(x$z_j) * design$sigma_tau else NA_real_,
    sigma_tau_marg = if (nrow(x) > 1L) stats::sd(x$tau_j) else NA_real_,
    dependence_method = design$dependence_spec$method,
    target_rank_corr = design$dependence_spec$rank_corr,
    target_pearson_corr = design$dependence_spec$pearson_corr,
    dependence_diagnostics = dependence_diagnostics,
    observation_diagnostics = observation_diagnostics,
    permutation = .diagnostic_permutation(perm, design = design)
  )
}

.diagnostic_permutation <- function(perm, design) {
  if (identical(design$dependence_spec$method, "none")) {
    return("identity")
  }
  if (isTRUE(design$record_permutation)) {
    return(perm)
  }
  "not_recorded"
}

.safe_cor <- function(x, y, method) {
  out <- suppressWarnings(stats::cor(x, y, method = method))
  if (length(out) != 1L || !is.finite(out)) {
    return(NA_real_)
  }
  out
}
