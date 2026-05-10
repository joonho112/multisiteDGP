.abort_multisitedgp <- function(message, info, fix, class) {
  cli::cli_abort(
    c(
      "x" = message,
      "i" = info,
      ">" = fix
    ),
    class = c(class, "multisitedgp_error")
  )
}

.abort_arg <- function(message, info, fix) {
  .abort_multisitedgp(message, info, fix, "multisitedgp_arg_error")
}

.abort_coherence <- function(message, info, fix) {
  .abort_multisitedgp(message, info, fix, "multisitedgp_coherence_error")
}

.abort_engine_dependence <- function(message, info, fix) {
  .abort_multisitedgp(message, info, fix, "multisitedgp_engine_dependence_error")
}

.abort_solver <- function(message, info, fix) {
  .abort_multisitedgp(message, info, fix, "multisitedgp_solver_error")
}

.abort_dependence_solver <- function(message, info, fix) {
  .abort_multisitedgp(message, info, fix, "multisitedgp_dependence_solver_error")
}

.abort_marginal_violation <- function(message, info, fix) {
  .abort_multisitedgp(message, info, fix, "multisitedgp_marginal_violation_error")
}

.validate_scalar_integer <- function(x, arg) {
  if (length(x) != 1L || is.na(x) || !is.numeric(x) || !is.finite(x) || x != floor(x)) {
    .abort_arg(
      sprintf("`%s` must be a single finite integer.", arg),
      sprintf("`%s` is checked before simulation state is created.", arg),
      sprintf("Pass `%s` as one integer-like value.", arg)
    )
  }
  invisible(as.integer(x))
}

.validate_seed <- function(x, arg = "seed") {
  x <- .validate_scalar_integer(x, arg)
  if (x < 0L || x > .Machine$integer.max) {
    .abort_arg(
      sprintf("`%s` must be between 0 and .Machine$integer.max.", arg),
      "R's RNG seed contract accepts one non-negative integer seed.",
      sprintf("Use `%s = 1L`, `%s = 42L`, or another non-negative integer.", arg, arg)
    )
  }
  invisible(x)
}

.validate_scalar_number <- function(x, arg) {
  if (length(x) != 1L || is.na(x) || !is.numeric(x) || !is.finite(x)) {
    .abort_arg(
      sprintf("`%s` must be a single finite number.", arg),
      sprintf("`%s` is a scalar design parameter.", arg),
      sprintf("Pass `%s` as one finite numeric value.", arg)
    )
  }
  invisible(as.numeric(x))
}

.validate_scalar_logical <- function(x, arg) {
  if (length(x) != 1L || is.na(x) || !is.logical(x)) {
    .abort_arg(
      sprintf("`%s` must be TRUE or FALSE.", arg),
      sprintf("`%s` controls a single design option.", arg),
      sprintf("Pass `%s = TRUE` or `%s = FALSE`.", arg, arg)
    )
  }
  x
}

.validate_function_or_null <- function(x, arg) {
  if (!is.null(x) && !is.function(x)) {
    .abort_arg(
      sprintf("`%s` must be NULL or a function.", arg),
      sprintf("`%s` is a user hook and must be callable when supplied.", arg),
      sprintf("Pass `%s = NULL` or provide a function.", arg)
    )
  }
  invisible(x)
}

.require_namespace <- function(package) {
  requireNamespace(package, quietly = TRUE)
}

.require_soft_dependency <- function(package, context) {
  if (!.require_namespace(package)) {
    .abort_arg(
      sprintf("Package `%s` is required for `%s()`.", package, context),
      sprintf("`%s` is a multisiteDGP Suggests dependency for this distribution.", package),
      sprintf("Use `install.packages(\"%s\")` and try again.", package)
    )
  }
  invisible(TRUE)
}

.validate_named_list <- function(x, arg) {
  if (!is.list(x) || is.null(names(x))) {
    .abort_arg(
      sprintf("`%s` must be a named list.", arg),
      "Distribution-specific parameters are read by name.",
      sprintf("Pass `%s = list(...)` with named entries.", arg)
    )
  }
  invisible(x)
}

.validate_j <- function(j) {
  j <- .validate_scalar_integer(j, "J")
  if (j < 10L) {
    .abort_arg(
      sprintf("`J` must be at least 10; you passed %s.", j),
      "J >= 10 is required for stable (I, R) reporting.",
      "Try `J = 25L` for a small design or `J = 50L` for the default design."
    )
  }
  invisible(j)
}

.validate_sigma_tau <- function(sigma_tau) {
  sigma_tau <- .validate_scalar_number(sigma_tau, "sigma_tau")
  if (sigma_tau <= 0) {
    .abort_arg(
      sprintf("`sigma_tau` must be > 0; you passed %s.", sigma_tau),
      "`sigma_tau == 0` is a degenerate point-mass G distribution.",
      "Try `sigma_tau = 0.20` for modest heterogeneity or `sigma_tau = 0.05` for very low heterogeneity."
    )
  }
  invisible(sigma_tau)
}

.validate_nj_mean_nj_min <- function(nj_mean, nj_min, allow_equal = FALSE) {
  nj_mean <- .validate_scalar_number(nj_mean, "nj_mean")
  nj_min <- .validate_scalar_integer(nj_min, "nj_min")
  allow_equal <- .validate_scalar_logical(allow_equal, "allow_equal")
  if (nj_min < 1L) {
    .abort_arg(
      sprintf("`nj_min` must be >= 1; you passed %s.", nj_min),
      "`n_j` is a site size and must be a positive integer.",
      "Try `nj_min = 5L` or `nj_min = 10L`."
    )
  }
  invalid_relation <- if (isTRUE(allow_equal)) nj_mean < nj_min else nj_mean <= nj_min
  if (invalid_relation) {
    .abort_arg(
      sprintf("`nj_mean` (%s) must exceed `nj_min` (%s).", nj_mean, nj_min),
      "The truncated-Gamma site-size generator requires `nj_mean > nj_min`.",
      "Try `nj_mean = 40, nj_min = 5L` or `nj_mean = 60, nj_min = 10L`."
    )
  }
  invisible(list(nj_mean = nj_mean, nj_min = nj_min))
}

.validate_engine_dependence <- function(engine, dependence, rank_corr = 0) {
  if (identical(engine, "A1_legacy") && !identical(dependence, "none")) {
    .abort_engine_dependence(
      sprintf("Engine `A1_legacy` is not compatible with `dependence = \"%s\"`.", dependence),
      "Engine A1 is reserved for JEBS legacy bit-identical reproduction.",
      sprintf("Use `engine = \"A2_modern\", dependence = \"%s\", rank_corr = %s`.", dependence, rank_corr)
    )
  }
  invisible(list(engine = engine, dependence = dependence, rank_corr = rank_corr))
}

.validate_direct_args <- function(nj_mean = NULL, cv = NULL, nj_min = NULL) {
  supplied <- c(
    nj_mean = !is.null(nj_mean),
    cv = !is.null(cv),
    nj_min = !is.null(nj_min)
  )
  if (any(supplied)) {
    bad_args <- paste(names(supplied)[supplied], collapse = ", ")
    .abort_coherence(
      sprintf("`sim_meta()` is for direct (I, R) designs and does not accept: %s.", bad_args),
      "Direct meta-analysis designs specify precision through `I` and `R`, not site-size arguments.",
      "Use `sim_multisite()` with site-size arguments or call `sim_meta(I = ..., R = ...)`."
    )
  }
  invisible(TRUE)
}

.validate_g_fn_true_dist <- function(g_fn = NULL, true_dist = "Gaussian") {
  .validate_function_or_null(g_fn, "g_fn")
  if (!is.null(g_fn) && !true_dist %in% c("User", "DPM")) {
    .abort_coherence(
      sprintf("`g_fn` and `true_dist = \"%s\"` were both supplied.", true_dist),
      "`g_fn` defines a user distribution, so `true_dist` must be `User`, `DPM`, or omitted.",
      "Remove `true_dist`, set `true_dist = \"User\"`, or use `true_dist = \"DPM\"` as an explicit bridge."
    )
  }
  invisible(list(g_fn = g_fn, true_dist = true_dist))
}

.validate_studentt_theta <- function(theta_g) {
  .validate_named_list(theta_g, "theta_G")
  if (!"nu" %in% names(theta_g)) {
    .abort_arg(
      "`true_dist = \"StudentT\"` requires `theta_G$nu`.",
      "Student-t standardization requires `nu > 2` for finite variance.",
      "Use `theta_G = list(nu = 5)`."
    )
  }
  nu <- .validate_scalar_number(theta_g$nu, "theta_G$nu")
  if (nu <= 2) {
    .abort_arg(
      sprintf("`theta_G$nu` must be > 2; you passed %s.", nu),
      "Student-t standardization requires finite variance.",
      "Use `theta_G = list(nu = 5)`."
    )
  }
  invisible(nu)
}

.validate_corr_exclusive <- function(rank_corr = 0, pearson_corr = 0, dependence = NULL) {
  rank_corr <- .validate_scalar_number(rank_corr, "rank_corr")
  pearson_corr <- .validate_scalar_number(pearson_corr, "pearson_corr")
  if (abs(rank_corr) > 1 || abs(pearson_corr) > 1) {
    .abort_arg(
      "`rank_corr` and `pearson_corr` must each be in [-1, 1].",
      "Correlation targets outside [-1, 1] are not mathematically valid.",
      "Use a target such as `0.3`, `0`, or `-0.3`."
    )
  }
  rank_nonzero <- !isTRUE(all.equal(rank_corr, 0))
  pearson_nonzero <- !isTRUE(all.equal(pearson_corr, 0))
  if (rank_nonzero && pearson_nonzero) {
    .abort_arg(
      "`rank_corr` and `pearson_corr` were both specified; choose one.",
      "`rank_corr` targets Spearman rank alignment; `pearson_corr` targets Gaussian copula alignment.",
      "Use `dependence = \"rank\", rank_corr = 0.3` or `dependence = \"copula\", pearson_corr = 0.3`."
    )
  }
  if (!is.null(dependence)) {
    if (dependence %in% c("rank", "hybrid") && pearson_nonzero) {
      .abort_arg(
        sprintf("`pearson_corr` is not used when `dependence = \"%s\"`.", dependence),
        "`rank` and `hybrid` dependence target residual Spearman correlation through `rank_corr`.",
        sprintf(
          "Use `dependence = \"%s\", rank_corr = %s`, or switch to `dependence = \"copula\"`.",
          dependence,
          pearson_corr
        )
      )
    }
    if (identical(dependence, "copula") && rank_nonzero) {
      .abort_arg(
        "`rank_corr` is not used when `dependence = \"copula\"`.",
        "`copula` dependence targets latent Gaussian Pearson correlation through `pearson_corr`.",
        sprintf(
          "Use `dependence = \"copula\", pearson_corr = %s`, or switch to `dependence = \"rank\"` or `\"hybrid\"`.",
          rank_corr
        )
      )
    }
    if (identical(dependence, "none") && (rank_nonzero || pearson_nonzero)) {
      .abort_arg(
        "`rank_corr` and `pearson_corr` are not used when `dependence = \"none\"`.",
        "The independence path does not run a dependence aligner.",
        "Remove the correlation target or choose `dependence = \"rank\"`, `\"copula\"`, or `\"hybrid\"`."
      )
    }
  }
  invisible(list(rank_corr = rank_corr, pearson_corr = pearson_corr))
}
