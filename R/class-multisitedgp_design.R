#' Create a simulation design
#'
#' Checks the settings of a simulation and stores them in one object: the
#' number of sites, the distribution of the site effects, the way the standard
#' errors are obtained, the dependence between effects and precision, and the
#' seed. [sim_multisite()] and [sim_meta()] also accept these arguments
#' directly, without a design.
#'
#' @details
#' `paradigm` decides which arguments are used. With `"site_size"`, a value
#' other than the default for `I`, `R`, `shuffle`, or `se_fn` is an error.
#' With `"direct"`, the same holds for `nj_mean`, `cv`, `nj_min`, `p`, `R2`,
#' and `engine`.
#'
#' A design stores each argument under its own name, with two exceptions. The
#' arguments from `dependence` to `tol` are in the element `dependence_spec`,
#' where `dependence` is named `method`, and `obs_fn` is in `obs_spec`.
#'
#' @param J Integer (at least 10). Number of sites. Default `50L`.
#' @param paradigm How the standard errors are obtained: `"site_size"` (the
#'   default), from simulated site sizes, as [sim_multisite()] requires, or
#'   `"direct"`, from `I` and `R`, as [sim_meta()] requires.
#' @param true_dist Distribution of the site effects: `"Gaussian"` (the
#'   default), `"StudentT"`, `"SkewN"`, `"ALD"`, `"Mixture"`,
#'   `"PointMassSlab"`, or `"User"`. See [gen_effects()].
#' @param tau,sigma_tau Numbers. Grand mean of the site effects and their
#'   cross-site standard deviation (above 0). Defaults `0` and `0.2`.
#' @param theta_G Named list of parameters of the site-effect distribution,
#'   which the model writes G. It must hold `nu` for `"StudentT"`, `slant` for
#'   `"SkewN"`, `rho` for `"ALD"`, `delta`, `eps`, and `ups` for `"Mixture"`,
#'   and `pi0` for `"PointMassSlab"`. Default `list()`. See [gen_effects()].
#' @param formula,beta,data Site-level covariates for the mean of the site
#'   effects: a one-sided formula, its coefficients, and a data frame with `J`
#'   rows. Default `NULL` (no covariates). They are checked when the data are
#'   simulated. See [gen_effects()].
#' @param g_fn,g_returns Used when `true_dist = "User"`, which is selected if
#'   `g_fn` is given and `true_dist` is not. `g_fn` (default `NULL`) is a
#'   function called as `g_fn(J, ...)`, with the entries of `theta_G` as
#'   further arguments, that returns `J` draws. `g_returns` says whether the
#'   draws have mean 0 and variance 1 (`"standardized"`, the default) or are
#'   the site effects themselves (`"raw"`). See [gen_effects_user()].
#' @param nj_mean,cv,nj_min Used when standard errors come from site sizes.
#'   Mean of the site sizes, their coefficient of variation (at least 0),
#'   and the smallest site size allowed (integer, at least 1). `nj_mean` must
#'   exceed `nj_min`, or equal it when `cv = 0`. Defaults `50`, `0.5`, `5L`.
#' @param p,R2,var_outcome Used when standard errors come from site sizes.
#'   Proportion of individuals treated (strictly between 0 and 1), share of
#'   outcome variance explained by covariates (at least 0, below 1), and
#'   outcome variance (above 0). A site of size `n_j` has sampling variance
#'   `kappa / n_j`, see [compute_kappa()]. Defaults `0.5`, `0`, `1`.
#' @param engine Used when standard errors come from site sizes. The site-size
#'   generator: `"truncated_gamma"` (the default) or `"censored"`, which
#'   requires `dependence = "none"`. See [gen_site_sizes()].
#' @param I,R,shuffle Used when standard errors are set directly. `I` is the
#'   target informativeness (strictly between 0 and 1, required, default
#'   `NULL`). `R` is the target heterogeneity ratio, the largest sampling
#'   variance over the smallest (at least 1, default `1`). `shuffle` (default
#'   `TRUE`) puts the standard errors in random order across sites. See
#'   [gen_se_direct()].
#' @param se_fn,se_args Used when standard errors are set directly. `se_fn`
#'   (default `NULL`) is a function, called as `se_fn(J, ...)` with the
#'   entries of the list `se_args` (default `list()`), that returns the
#'   sampling variances in place of those computed from `I` and `R`. See
#'   [gen_se_direct()].
#' @param dependence Dependence between site effects and precision: `"none"`
#'   (the default), `"rank"`, `"copula"`, or `"hybrid"`. See
#'   [align_rank_corr()], [align_copula_corr()], and [align_hybrid_corr()].
#' @param rank_corr,pearson_corr Numbers between -1 and 1. Default `0`.
#'   `rank_corr` is the target Spearman correlation between the standardized
#'   site effects and the sampling variances, for `"rank"` and `"hybrid"`.
#'   `pearson_corr` is the correlation of the Gaussian copula, for
#'   `"copula"`. Only the one in use may differ from 0.
#' @param dependence_fn Function that reorders the standard errors in place of
#'   the built-in method, or `NULL` (the default). See [align_rank_corr()].
#' @param hybrid_init,hybrid_polish Used when `dependence = "hybrid"`. The
#'   starting arrangement (`"copula"`, the default, or `"rank"`) and its
#'   refinement (`"hill_climb"`, the default, or `"none"`).
#' @param max_iter,tol Used by `"rank"` and `"hybrid"`, which swap standard
#'   errors between sites until the rank correlation is within `tol` (above
#'   0, default `0.02`) of its target. `max_iter` (integer, at least 100,
#'   default `20000L`) bounds the number of swaps.
#' @param obs_fn Function, called as `obs_fn(tau_j, se2_j)`, that returns the
#'   observed estimates in place of normal draws with mean `tau_j` and
#'   variance `se2_j`. Default `NULL`. See [gen_observations()].
#' @param seed Integer seed (at least 0), or `NULL` (the default). Used by
#'   [sim_multisite()] and [sim_meta()] unless their own `seed` is given.
#' @param record_permutation `TRUE`, `FALSE`, or `NULL` (the default: `TRUE`
#'   unless `dependence = "none"`). Whether the simulated data keep a record of
#'   how the dependence step reordered the standard errors, as
#'   `attr(x, "diagnostics")$permutation`.
#' @param x For `is_multisitedgp_design()`, any object. For `print()` and
#'   `format()`, a design.
#' @param ... Not used.
#'
#' @return `multisitedgp_design()` returns a design: a list of class
#'   `multisitedgp_design` that holds the checked arguments and is passed to
#'   [sim_multisite()], [sim_meta()], or [design_grid()] (as `base_design`).
#'   `is_multisitedgp_design()` returns `TRUE` or `FALSE`, `format()` returns
#'   the printed text as one string, and `print()` returns `x` invisibly.
#'
#' @seealso [presets], [design_grid()], `vignette("multisiteDGP")`.
#' @family design
#' @examples
#' design <- multisitedgp_design(J = 30, sigma_tau = 0.15, seed = 1)
#' design
#' sim_multisite(design)
#'
#' # Standard errors set directly, site effects from a t distribution
#' meta <- multisitedgp_design(
#'   paradigm = "direct", I = 0.4, R = 3,
#'   true_dist = "StudentT", theta_G = list(nu = 5), seed = 1
#' )
#' meta
#' @export
multisitedgp_design <- function(
  J = 50L,
  paradigm = c("site_size", "direct"),
  true_dist = c("Gaussian", "StudentT", "SkewN", "ALD", "Mixture", "PointMassSlab", "User"),
  tau = 0,
  sigma_tau = 0.20,
  theta_G = list(),
  formula = NULL,
  beta = NULL,
  data = NULL,
  g_fn = NULL,
  g_returns = c("standardized", "raw"),
  nj_mean = 50,
  cv = 0.50,
  nj_min = 5L,
  p = 0.5,
  R2 = 0,
  var_outcome = 1,
  engine = c("truncated_gamma", "censored"),
  I = NULL,
  R = 1,
  shuffle = TRUE,
  se_fn = NULL,
  se_args = list(),
  dependence = c("none", "rank", "copula", "hybrid"),
  rank_corr = 0,
  pearson_corr = 0,
  dependence_fn = NULL,
  hybrid_init = c("copula", "rank"),
  hybrid_polish = c("hill_climb", "none"),
  max_iter = 20000L,
  tol = 0.02,
  obs_fn = NULL,
  seed = NULL,
  record_permutation = NULL
) {
  true_dist_missing <- missing(true_dist)
  if (!is.null(g_fn) && true_dist_missing) {
    true_dist <- "User"
  }

  paradigm <- .match_choice(paradigm, "paradigm", c("site_size", "direct"))
  true_dist <- .match_choice(
    true_dist,
    "true_dist",
    .generated_g_shapes()
  )
  g_returns <- .match_choice(g_returns, "g_returns", c("standardized", "raw"))
  engine <- .match_choice(engine, "engine", c("truncated_gamma", "censored"))
  dependence <- .match_choice(dependence, "dependence", c("none", "rank", "copula", "hybrid"))
  hybrid_init <- .match_choice(hybrid_init, "hybrid_init", c("copula", "rank"))
  hybrid_polish <- .match_choice(hybrid_polish, "hybrid_polish", c("hill_climb", "none"))

  J <- .validate_j(J)
  tau <- .validate_scalar_number(tau, "tau")
  sigma_tau <- .validate_sigma_tau(sigma_tau)
  theta_G <- .validate_theta_g_container(theta_G)
  g_fn <- .validate_function_or_null(g_fn, "g_fn")
  se_fn <- .validate_function_or_null(se_fn, "se_fn")
  dependence_fn <- .validate_function_or_null(dependence_fn, "dependence_fn")
  obs_fn <- .validate_function_or_null(obs_fn, "obs_fn")
  .validate_g_fn_true_dist(g_fn, true_dist)
  .validate_user_g_fn(g_fn, true_dist)

  cv <- .validate_scalar_number(cv, "cv")
  nj <- .validate_nj_mean_nj_min(nj_mean, nj_min, allow_equal = .is_zero_cv(cv))
  p <- .validate_scalar_number(p, "p")
  R2 <- .validate_scalar_number(R2, "R2")
  var_outcome <- .validate_scalar_number(var_outcome, "var_outcome")

  if (!is.null(I)) {
    I <- .validate_scalar_number(I, "I")
  }
  R <- .validate_scalar_number(R, "R")
  shuffle <- .validate_scalar_logical(shuffle, "shuffle")
  se_args <- .validate_list(se_args, "se_args")

  .validate_engine_dependence(engine, dependence, rank_corr)
  corr <- .validate_corr_exclusive(rank_corr, pearson_corr, dependence = dependence)
  max_iter <- .validate_scalar_integer(max_iter, "max_iter")
  tol <- .validate_scalar_number(tol, "tol")
  .validate_design_ranges(
    paradigm = paradigm,
    true_dist = true_dist,
    theta_G = theta_G,
    cv = cv,
    p = p,
    R2 = R2,
    I = I,
    R = R,
    max_iter = max_iter,
    tol = tol
  )
  .validate_design_coherence(
    paradigm = paradigm,
    nj_mean = nj$nj_mean,
    cv = cv,
    nj_min = nj$nj_min,
    p = p,
    R2 = R2,
    engine = engine,
    I = I,
    R = R,
    shuffle = shuffle,
    se_fn = se_fn
  )

  if (!is.null(seed)) {
    seed <- .validate_seed(seed, "seed")
  }
  record_permutation <- .resolve_record_permutation(record_permutation, dependence)

  .new_multisitedgp_design(
    list(
      J = J,
      paradigm = paradigm,
      true_dist = true_dist,
      tau = tau,
      sigma_tau = sigma_tau,
      theta_G = theta_G,
      formula = formula,
      beta = beta,
      data = data,
      g_fn = g_fn,
      g_returns = g_returns,
      nj_mean = nj$nj_mean,
      cv = cv,
      nj_min = nj$nj_min,
      p = p,
      R2 = R2,
      var_outcome = var_outcome,
      engine = engine,
      I = I,
      R = R,
      shuffle = shuffle,
      se_fn = se_fn,
      se_args = se_args,
      dependence_spec = list(
        method = dependence,
        rank_corr = corr$rank_corr,
        pearson_corr = corr$pearson_corr,
        dependence_fn = dependence_fn,
        hybrid_init = hybrid_init,
        hybrid_polish = hybrid_polish,
        max_iter = max_iter,
        tol = tol
      ),
      obs_spec = list(obs_fn = obs_fn),
      seed = seed,
      record_permutation = record_permutation
    )
  )
}

#' @rdname multisitedgp_design
#' @export
is_multisitedgp_design <- function(x) {
  inherits(x, "multisitedgp_design")
}

#' @rdname update_multisitedgp_design
#' @order 2
#' @export
validate_multisitedgp_design <- function(design) {
  if (!is_multisitedgp_design(design)) {
    .abort(
      "`design` must be a design from `multisitedgp_design()` or a preset.",
      .you_passed(design)
    )
  }
  do.call(multisitedgp_design, .design_to_constructor_args(design))
  invisible(design)
}

#' Change or check a design
#'
#' `update_multisitedgp_design()` builds a new design from an existing one
#' with some arguments changed, and checks it as [multisitedgp_design()] does.
#' `validate_multisitedgp_design()` runs the same checks on a design as it is,
#' for example after its elements were changed with `$`.
#'
#' @details
#' If `dependence` changes and `record_permutation` was at its default,
#' `record_permutation` takes the default for the new `dependence`. Giving
#' `g_fn` without `true_dist` sets `true_dist` to `"User"`.
#'
#' @param design A design from [multisitedgp_design()] or one of the [presets].
#' @param ... Arguments of [multisitedgp_design()] to change, as
#'   `name = value`. Any other name is an error.
#'
#' @return `update_multisitedgp_design()` returns the new design.
#'   `validate_multisitedgp_design()` returns `design` invisibly, or stops
#'   with an error that says what is wrong.
#'
#' @seealso [design_grid()], [presets], `vignette("multisiteDGP")`.
#' @family design
#' @examples
#' design <- preset_education_modest(seed = 1)
#' update_multisitedgp_design(design, J = 100, sigma_tau = 0.3)
#'
#' # A change made with `$` is checked only when the design is validated or used
#' design$sigma_tau <- -1
#' try(validate_multisitedgp_design(design))
#' @order 1
#' @export
update_multisitedgp_design <- function(design, ...) {
  validate_multisitedgp_design(design)
  updates <- list(...)
  args <- .design_to_constructor_args(design)

  if (length(updates) == 0L) {
    return(.restore_preset_metadata(do.call(multisitedgp_design, args), design))
  }

  update_names <- names(updates)
  if (is.null(update_names)) {
    update_names <- rep("", length(updates))
  }
  bad <- setdiff(update_names, names(args))
  if (length(bad) > 0L || any(!nzchar(update_names))) {
    .abort_unknown_design_args(bad)
  }

  args[names(updates)] <- updates
  if (!is.null(updates$g_fn) && !"true_dist" %in% names(updates)) {
    args$true_dist <- "User"
  }
  old_record_default <- !identical(design$dependence_spec$method, "none")
  if (
    "dependence" %in% names(updates) &&
      !"record_permutation" %in% names(updates) &&
      identical(design$record_permutation, old_record_default)
  ) {
    args$record_permutation <- NULL
  }
  .restore_preset_metadata(do.call(multisitedgp_design, args), design)
}

# For a `...` that takes arguments of multisitedgp_design(). `bad` holds the
# names that are not arguments, with "" for an entry that has no name.
.abort_unknown_design_args <- function(bad) {
  named <- bad[nzchar(bad)]
  .abort(
    "`...` must hold arguments of `multisitedgp_design()`, given by name.",
    if (length(named) > 0L) {
      sprintf("Not an argument: %s.", paste(named, collapse = ", "))
    } else {
      "At least one argument has no name."
    }
  )
}

.restore_preset_metadata <- function(out, source) {
  for (field in c("preset_name", "preset_metadata")) {
    value <- attr(source, field, exact = TRUE)
    if (!is.null(value)) {
      attr(out, field) <- value
    }
  }
  out
}

.new_multisitedgp_design <- function(x) {
  structure(x, class = c("multisitedgp_design", "list"))
}

#' @rdname multisitedgp_design
#' @export
print.multisitedgp_design <- function(x, ...) {
  cat(format(x, ...), "\n", sep = "")
  invisible(x)
}

#' @rdname multisitedgp_design
#' @export
format.multisitedgp_design <- function(x, ...) {
  field <- function(name, value) sprintf("  %s: %s", name, value)
  dep <- x$dependence_spec

  site_effects <- c(
    "Site effects",
    field("true_dist", x$true_dist),
    field("tau", .format_design_scalar(x$tau)),
    field("sigma_tau", .format_design_scalar(x$sigma_tau)),
    if (length(x$theta_G) > 0L) field("theta_G", .format_design_pairs(x$theta_G)),
    if (!is.null(x$formula)) field("formula", .format_design_scalar(x$formula)),
    if (!is.null(x$beta)) field("beta", .format_design_vector(x$beta)),
    if (!is.null(x$g_fn)) field("g_fn", "<function>"),
    if (identical(x$true_dist, "User")) field("g_returns", x$g_returns)
  )
  standard_errors <- if (identical(x$paradigm, "direct")) {
    c(
      "Standard errors",
      field("I", .format_design_scalar(x$I)),
      field("R", .format_design_scalar(x$R)),
      field("shuffle", .format_design_scalar(x$shuffle)),
      if (!is.null(x$se_fn)) field("se_fn", "<function>")
    )
  } else {
    c(
      "Site sizes and standard errors",
      field("engine", x$engine),
      field("nj_mean", .format_design_scalar(x$nj_mean)),
      field("cv", .format_design_scalar(x$cv)),
      field("nj_min", .format_design_scalar(x$nj_min)),
      field("p", .format_design_scalar(x$p)),
      field("R2", .format_design_scalar(x$R2)),
      field("var_outcome", .format_design_scalar(x$var_outcome))
    )
  }
  dependence <- c(
    "Dependence",
    field("dependence", dep$method),
    if (dep$method %in% c("rank", "hybrid")) field("rank_corr", .format_design_scalar(dep$rank_corr)),
    if (identical(dep$method, "copula")) field("pearson_corr", .format_design_scalar(dep$pearson_corr)),
    if (identical(dep$method, "hybrid")) {
      c(field("hybrid_init", dep$hybrid_init), field("hybrid_polish", dep$hybrid_polish))
    },
    if (!is.null(dep$dependence_fn)) field("dependence_fn", "<function>")
  )
  observations <- if (!is.null(x$obs_spec$obs_fn)) {
    c("Observations", field("obs_fn", "<function>"))
  }

  paste(c(
    "<multisitedgp_design>",
    paste0("J = ", .format_design_headline(x$J, x$paradigm, x$seed)),
    "", site_effects,
    "", standard_errors,
    "", dependence,
    if (!is.null(observations)) c("", observations)
  ), collapse = "\n")
}

# The number of sites, where the standard errors come from, and the seed, in
# the wording shared by the design print, the data print, and the summary title.
.format_design_headline <- function(J, paradigm, seed) {
  sprintf(
    "%s sites, %s, %s",
    format(J, scientific = FALSE),
    if (identical(paradigm, "direct")) "standard errors set directly" else "standard errors from site sizes",
    if (is.null(seed)) "no seed" else paste("seed", format(seed, scientific = FALSE))
  )
}

.format_design_scalar <- function(x, null = "NULL") {
  if (is.null(x)) {
    return(null)
  }
  if (inherits(x, "formula")) {
    return(paste(deparse(x), collapse = " "))
  }
  if (length(x) == 0L) {
    return(null)
  }
  if (is.logical(x)) {
    return(if (isTRUE(x)) "TRUE" else "FALSE")
  }
  .format_design_number(x[[1L]])
}

.format_design_vector <- function(x) {
  if (is.null(x)) {
    return("NULL")
  }
  if (is.numeric(x)) {
    return(paste(vapply(x, .format_design_number, character(1)), collapse = ", "))
  }
  paste(as.character(x), collapse = ", ")
}

# A computed value such as 0.189020952141661 is shown as 0.189021. Whole
# numbers keep all their digits.
.format_design_number <- function(x) {
  if (is.numeric(x) && is.finite(x) && x != round(x)) {
    return(format(x, digits = 6L))
  }
  as.character(x)
}

# A named list as `name = value` pairs, for `theta_G`.
.format_design_pairs <- function(x) {
  values <- vapply(x, function(value) {
    if (is.atomic(value) && length(value) == 1L) {
      return(.format_design_scalar(value))
    }
    paste(deparse(value), collapse = " ")
  }, character(1))
  paste(sprintf("%s = %s", names(x), values), collapse = ", ")
}

.validate_theta_g_container <- function(theta_g) {
  if (!is.list(theta_g)) {
    .abort(
      "`theta_G` must be a list.",
      .you_passed(theta_g),
      "For example, `theta_G = list(nu = 5)`."
    )
  }
  if (length(theta_g) > 0L && !rlang::is_named(theta_g)) {
    .abort(
      "Every entry of `theta_G` must be named.",
      fix = "For example, `theta_G = list(nu = 5)`."
    )
  }
  theta_g
}

.validate_list <- function(x, arg) {
  if (!is.list(x)) {
    .abort(
      sprintf("`%s` must be a list.", arg),
      .you_passed(x)
    )
  }
  x
}

.resolve_record_permutation <- function(record_permutation, dependence) {
  if (is.null(record_permutation)) {
    return(!identical(dependence, "none"))
  }
  .validate_scalar_logical(record_permutation, "record_permutation")
}

.match_choice <- function(x, arg, choices) {
  if (identical(x, choices)) {
    return(choices[[1L]])
  }
  if (length(x) != 1L || is.na(x) || !is.character(x) || !x %in% choices) {
    .abort(
      sprintf("`%s` must be one of %s.", arg, .format_choices(choices)),
      .you_passed(x)
    )
  }
  x
}

# The allowed values of an argument, each in quotes, joined by commas and "or".
.format_choices <- function(choices) {
  quoted <- sprintf("\"%s\"", choices)
  n <- length(quoted)
  if (n <= 2L) {
    return(paste(quoted, collapse = " or "))
  }
  paste0(paste(quoted[-n], collapse = ", "), ", or ", quoted[[n]])
}

.design_to_constructor_args <- function(design) {
  required <- c(
    "J", "paradigm", "true_dist", "tau", "sigma_tau", "theta_G",
    "formula", "beta", "data", "g_fn", "g_returns", "nj_mean", "cv",
    "nj_min", "p", "R2", "var_outcome", "engine", "I", "R", "shuffle",
    "se_fn", "se_args", "dependence_spec", "obs_spec", "seed",
    "record_permutation"
  )
  missing_fields <- setdiff(required, names(design))
  extra_fields <- setdiff(names(design), required)
  advice <- "Build a design with `multisitedgp_design()` and change it with `update_multisitedgp_design()`."
  if (length(missing_fields) > 0L || length(extra_fields) > 0L) {
    .abort(
      "`design` does not have the elements of a design.",
      paste(c(
        if (length(missing_fields) > 0L) sprintf("Missing: %s.", paste(missing_fields, collapse = ", ")),
        if (length(extra_fields) > 0L) sprintf("Not a design element: %s.", paste(extra_fields, collapse = ", "))
      ), collapse = " "),
      advice
    )
  }
  if (!is.list(design$dependence_spec) || !is.list(design$obs_spec)) {
    .abort(
      "`design$dependence_spec` and `design$obs_spec` must be lists.",
      fix = advice
    )
  }
  dependence_names <- c(
    "method", "rank_corr", "pearson_corr", "dependence_fn",
    "hybrid_init", "hybrid_polish", "max_iter", "tol"
  )
  if (
    !identical(names(design$dependence_spec), dependence_names) ||
      !identical(names(design$obs_spec), "obs_fn")
  ) {
    .abort(
      "`design$dependence_spec` or `design$obs_spec` does not have the elements of a design.",
      sprintf(
        "`dependence_spec` holds %s, in this order, and `obs_spec` holds obs_fn.",
        paste(dependence_names, collapse = ", ")
      ),
      advice
    )
  }

  c(
    design[c(
      "J", "paradigm", "true_dist", "tau", "sigma_tau", "theta_G",
      "formula", "beta", "data", "g_fn", "g_returns", "nj_mean", "cv",
      "nj_min", "p", "R2", "var_outcome", "engine", "I", "R", "shuffle",
      "se_fn", "se_args"
    )],
    list(
      dependence = design$dependence_spec$method,
      rank_corr = design$dependence_spec$rank_corr,
      pearson_corr = design$dependence_spec$pearson_corr,
      dependence_fn = design$dependence_spec$dependence_fn,
      hybrid_init = design$dependence_spec$hybrid_init,
      hybrid_polish = design$dependence_spec$hybrid_polish,
      max_iter = design$dependence_spec$max_iter,
      tol = design$dependence_spec$tol,
      obs_fn = design$obs_spec$obs_fn,
      seed = design$seed,
      record_permutation = design$record_permutation
    )
  )
}

.validate_design_ranges <- function(paradigm, true_dist, theta_G, cv, p, R2, I, R, max_iter, tol) {
  if (cv < 0) {
    .abort("`cv` must be at least 0.", .you_passed(cv))
  }
  if (p <= 0 || p >= 1) {
    .abort("`p` must be strictly between 0 and 1.", .you_passed(p))
  }
  if (R2 < 0 || R2 >= 1) {
    .abort("`R2` must be at least 0 and below 1.", .you_passed(R2))
  }
  if (!is.null(I) && (I <= 0 || I >= 1)) {
    .abort("`I` must be strictly between 0 and 1.", .you_passed(I))
  }
  if (R < 1) {
    .abort("`R` must be at least 1.", .you_passed(R))
  }
  if (max_iter < 100L) {
    .abort("`max_iter` must be at least 100.", .you_passed(max_iter))
  }
  if (tol <= 0) {
    .abort("`tol` must be positive.", .you_passed(tol))
  }
  if (identical(true_dist, "StudentT")) {
    .validate_studentt_theta(theta_G)
  }
  if (identical(true_dist, "SkewN")) {
    .validate_skewn_theta(theta_G)
  }
  if (identical(true_dist, "ALD")) {
    .validate_ald_theta(theta_G)
  }
  if (identical(true_dist, "Mixture")) {
    .validate_mixture_theta(theta_G)
  }
  if (identical(true_dist, "PointMassSlab")) {
    .validate_pmslab_theta(theta_G)
  }
  if (identical(paradigm, "direct") && is.null(I)) {
    .abort(
      "`I` is required when the standard errors are set directly.",
      "It is the target informativeness, a number strictly between 0 and 1."
    )
  }
}

.validate_design_coherence <- function(
  paradigm,
  nj_mean,
  cv,
  nj_min,
  p,
  R2,
  engine,
  I,
  R,
  shuffle,
  se_fn
) {
  if (identical(paradigm, "site_size")) {
    rule <- "With `paradigm = \"site_size\"` the standard errors come from the site sizes."
    if (!is.null(I)) {
      .abort("`I` is for `paradigm = \"direct\"`, not `paradigm = \"site_size\"`.", rule)
    }
    if (!identical(R, 1)) {
      .abort("`R` is for `paradigm = \"direct\"`, not `paradigm = \"site_size\"`.", rule)
    }
    if (!identical(shuffle, TRUE)) {
      .abort("`shuffle` is for `paradigm = \"direct\"`, not `paradigm = \"site_size\"`.", rule)
    }
    if (!is.null(se_fn)) {
      .abort("`se_fn` is for `paradigm = \"direct\"`, not `paradigm = \"site_size\"`.", rule)
    }
  }
  if (identical(paradigm, "direct")) {
    bad <- c(
      if (!identical(nj_mean, 50)) "nj_mean",
      if (!identical(cv, 0.50)) "cv",
      if (!identical(nj_min, 5L)) "nj_min",
      if (!identical(p, 0.5)) "p",
      if (!identical(R2, 0)) "R2",
      if (!identical(engine, "truncated_gamma")) "engine"
    )
    if (length(bad) > 0L) {
      .abort(
        sprintf("Site-size arguments are not used when `paradigm = \"direct\"`: %s.", paste(bad, collapse = ", ")),
        "The standard errors are then set by `I` and `R`, or by `se_fn`."
      )
    }
  }
  invisible(TRUE)
}
