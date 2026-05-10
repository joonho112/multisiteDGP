# nolint start: object_name_linter, object_usage_linter
#' Construct an immutable multisite simulation design
#'
#' @encoding UTF-8
#'
#' @description
#' Build a frozen `multisitedgp_design` configuration object that
#' captures every choice for a multisite-trial simulation: site count,
#' latent-effect distribution, sample-size or precision margin,
#' optional precision dependence, observation model, and seed. Pass the
#' result to \code{\link{sim_multisite}} or \code{\link{sim_meta}} for
#' a single simulation, or to \code{\link{design_grid}} for a scenario
#' sweep.
#'
#' @details
#' \strong{When to construct a design vs. pass flat args.} For a single
#' simulation call, you can pass flat arguments directly to
#' \code{\link{sim_multisite}} (or \code{\link{sim_meta}}) and skip
#' `multisitedgp_design()`. Construct a design once when you want to
#' reuse it across multiple simulation calls, sweep a
#' \code{\link{design_grid}}, or audit a fixed configuration with
#' \code{\link{validate_multisitedgp_design}}.
#'
#' \strong{Site-size-driven (Paradigm A) vs direct-precision
#' (Paradigm B).} `paradigm = "site_size"` (default) uses the
#' `nj_mean`, `cv`, `nj_min`, `p`, `R2`, `var_outcome`, `engine`
#' arguments to derive sampling variance from site sizes (Layer 2 via
#' \code{\link{gen_site_sizes}}). `paradigm = "direct"` uses `I`, `R`,
#' `shuffle`, `se_fn`, `se_args` to specify the precision distribution
#' directly (Layer 2 via \code{\link{gen_se_direct}}). Mixing arguments
#' from the two paradigms triggers a coherence error.
#'
#' For the formal specification of every slot and the design-class
#' contract, see the
#' \href{../articles/m1-statistical-dgp.html}{The two-stage DGP — formal
#' specification} vignette and Appendix A.
#'
#' @param J Integer (\eqn{\ge 10}). Number of sites. Default `50L`.
#' @param paradigm Character. Margin / SE paradigm — `"site_size"`
#'   (default) or `"direct"`.
#' @param true_dist Character. Latent-effect distribution — one of
#'   `"Gaussian"`, `"StudentT"`, `"SkewN"`, `"ALD"`, `"Mixture"`,
#'   `"PointMassSlab"`, `"User"`, `"DPM"`. Default `"Gaussian"`.
#' @param tau Numeric. Grand mean of the site-effect distribution.
#'   Default `0`.
#' @param sigma_tau Numeric (\eqn{\ge 0}). Between-site SD on the
#'   response scale. Default `0.20`.
#' @param variance Numeric. Legacy Gaussian variance argument. Default
#'   `1`. Constrained to `1` by the unit-variance Layer 1 convention.
#' @param theta_G Named list of distribution-specific parameters. See
#'   \code{\link{gen_effects}} for keys per shape.
#' @param formula,beta,data Optional covariate specification. `formula`
#'   is a one-sided formula; `beta` is the matching coefficient vector;
#'   `data` is a `data.frame` with the predictors.
#' @param g_fn Optional user-supplied effect-generator callback for
#'   `true_dist = "User"` or the bridged `"DPM"` route.
#' @param g_returns Character. `"standardized"` (Convention A, default)
#'   or `"raw"` (Convention B). See \code{\link{gen_effects_user}}.
#' @param nj_mean,cv,nj_min,p,R2,var_outcome Site-size margin parameters
#'   (Paradigm A only). See \code{\link{gen_site_sizes}}.
#' @param engine Character. Site-size engine — `"A2_modern"` (default,
#'   recommended) or `"A1_legacy"` (JEBS bit-parity reproduction only).
#' @param I,R,shuffle,se_fn,se_args Direct-precision parameters
#'   (Paradigm B only). See \code{\link{gen_se_direct}}.
#' @param dependence Character. Layer 3 dependence injection method —
#'   `"none"` (default), `"rank"`, `"copula"`, or `"hybrid"`.
#' @param rank_corr,pearson_corr Numeric in `[-1, 1]`. Dependence
#'   targets. `rank_corr` is used for `"rank"` and `"hybrid"`;
#'   `pearson_corr` is used for `"copula"`.
#' @param dependence_fn Optional custom dependence callback. See
#'   \code{\link{align_rank_corr}} for the contract.
#' @param hybrid_init,hybrid_polish Hybrid-aligner initialization and
#'   polishing controls. See \code{\link{align_hybrid_corr}}.
#' @param max_iter,tol Iteration budget and convergence tolerance for
#'   the rank / hybrid hill-climb. Defaults `20000L` and `0.02`.
#' @param target_marginal_rho Reserved for future marginal-correlation
#'   target; not implemented in the current release.
#' @param obs_fn Optional Layer 4 observation callback. See
#'   \code{\link{gen_observations}}.
#' @param framing Character. Sampling-frame interpretation —
#'   `"superpopulation"` (default) or `"finite"`.
#' @param seed Optional integer. Single seed for the full pipeline; if
#'   non-`NULL`, the wrapper wraps the call in
#'   \code{\link[withr]{with_seed}} for bit-identical replays.
#' @param record_permutation Logical. Retain Layer 3 permutation
#'   metadata for inspection. Default depends on `dependence`.
#' @param x A `multisitedgp_design` object — used by `print` and
#'   `format` methods.
#' @param ... Additional arguments for `print` / `format` methods.
#'
#' @return A list with class `c("multisitedgp_design", "list")` carrying
#'   every locked configuration choice. Pass to \code{\link{sim_multisite}}
#'   or \code{\link{sim_meta}}.
#'
#' @family family-design
#' @seealso
#'   \code{\link{sim_multisite}} and \code{\link{sim_meta}} for the
#'   wrappers that consume designs;
#'   \code{\link{validate_multisitedgp_design}} for replaying the
#'   constructor contract;
#'   \code{\link{update_multisitedgp_design}} for functional updates;
#'   \code{\link{is_multisitedgp_design}} for the predicate;
#'   \code{\link{presets}} for defensible starting designs;
#'   \code{\link{design_grid}} for scenario-grid sweeps.
#' @references
#' Lee, J., Che, J., Rabe-Hesketh, S., Feller, A., & Miratrix, L. (2025).
#' Improving the estimation of site-specific effects and their distribution
#' in multisite trials. \emph{Journal of Educational and Behavioral Statistics},
#' \bold{50}(5), 731--764. \doi{10.3102/10769986241254286}.
#' @examples
#' # Minimal site-size-driven design.
#' design <- multisitedgp_design(J = 20L, sigma_tau = 0.20, seed = 1L)
#' is_multisitedgp_design(design)
#'
#' # Direct-precision design for meta-analysis.
#' meta_design <- multisitedgp_design(
#'   paradigm = "direct", J = 30L, I = 0.30, R = 2,
#'   sigma_tau = 0.20, seed = 1L
#' )
#' @export
multisitedgp_design <- function(
  J = 50L,
  paradigm = c("site_size", "direct"),
  true_dist = c("Gaussian", "StudentT", "SkewN", "ALD", "Mixture", "PointMassSlab", "User", "DPM"),
  tau = 0,
  sigma_tau = 0.20,
  variance = 1,
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
  engine = c("A2_modern", "A1_legacy"),
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
  target_marginal_rho = NULL,
  obs_fn = NULL,
  framing = c("superpopulation", "finite"),
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
    c("Gaussian", "StudentT", "SkewN", "ALD", "Mixture", "PointMassSlab", "User", "DPM")
  )
  g_returns <- .match_choice(g_returns, "g_returns", c("standardized", "raw"))
  engine <- .match_choice(engine, "engine", c("A2_modern", "A1_legacy"))
  dependence <- .match_choice(dependence, "dependence", c("none", "rank", "copula", "hybrid"))
  hybrid_init <- .match_choice(hybrid_init, "hybrid_init", c("copula", "rank"))
  hybrid_polish <- .match_choice(hybrid_polish, "hybrid_polish", c("hill_climb", "none"))
  framing <- .match_choice(framing, "framing", c("superpopulation", "finite"))

  J <- .validate_j(J)
  tau <- .validate_scalar_number(tau, "tau")
  sigma_tau <- .validate_sigma_tau(sigma_tau)
  variance <- .validate_scalar_number(variance, "variance")
  theta_G <- .validate_theta_g_container(theta_G)
  g_fn <- .validate_function_or_null(g_fn, "g_fn")
  se_fn <- .validate_function_or_null(se_fn, "se_fn")
  dependence_fn <- .validate_function_or_null(dependence_fn, "dependence_fn")
  obs_fn <- .validate_function_or_null(obs_fn, "obs_fn")
  .validate_g_fn_true_dist(g_fn, true_dist)
  .validate_user_and_dpm_presence(g_fn, true_dist)

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
    se_fn = se_fn,
    target_marginal_rho = target_marginal_rho
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
      variance = variance,
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
      framing = framing,
      seed = seed,
      record_permutation = record_permutation
    )
  )
}

#' Test whether an object is a multisite simulation design
#'
#' @encoding UTF-8
#'
#' @description
#' Predicate test — returns `TRUE` if `x` carries the
#' `multisitedgp_design` S3 class (i.e., was produced by
#' \code{\link{multisitedgp_design}} or by a preset such as
#' \code{\link{preset_education_modest}}), `FALSE` otherwise.
#'
#' @details
#' Use this in defensive code, custom wrappers, or before calling
#' \code{\link{validate_multisitedgp_design}}. A failed predicate means
#' the input is not a locked design object — typically a plain `list`
#' or a `multisitedgp_data` object passed by mistake.
#'
#' @param x Object to test.
#'
#' @return A single logical value.
#'
#' @family family-design
#' @seealso
#'   \code{\link{multisitedgp_design}} for the constructor;
#'   \code{\link{validate_multisitedgp_design}} for the constructor-
#'   replay validation;
#'   \code{\link{is_multisitedgp_data}} for the data-class predicate.
#'
#' @references
#' Lee, J., Che, J., Rabe-Hesketh, S., Feller, A., & Miratrix, L. (2025).
#' Improving the estimation of site-specific effects and their distribution
#' in multisite trials. \emph{Journal of Educational and Behavioral Statistics},
#' \bold{50}(5), 731--764. \doi{10.3102/10769986241254286}.
#'
#' @examples
#' # A real design object: TRUE.
#' is_multisitedgp_design(multisitedgp_design(J = 20L))
#'
#' # Plain lists / data frames / NULL: FALSE.
#' is_multisitedgp_design(list(J = 20L))
#' is_multisitedgp_design(NULL)
#' @export
is_multisitedgp_design <- function(x) {
  inherits(x, "multisitedgp_design")
}

#' Validate a multisite simulation design
#'
#' @encoding UTF-8
#'
#' @description
#' Replay the `multisitedgp_design()` constructor contract on an
#' existing design object. Useful as a sanity check after a non-trivial
#' \code{\link{update_multisitedgp_design}}, or before passing a design
#' that has been deserialized from disk into
#' \code{\link{sim_multisite}} or \code{\link{sim_meta}}.
#'
#' @details
#' Validation enforces every constraint the constructor enforces — for
#' example, the Decision-C contract (engine A1 with non-trivial
#' precision dependence is rejected), the eight-shape `true_dist`
#' allowlist, the `theta_G` per-shape required keys, and the unit-
#' variance convention. Errors are emitted with full
#' message + info + fix hints from the typed-error catalog (see
#' `R/00-errors-validation.R`).
#'
#' On success, returns the original `design` invisibly so the function
#' chains naturally with `|>`.
#'
#' @param design A `multisitedgp_design` object.
#'
#' @return `design`, invisibly, if validation succeeds. Aborts with a
#'   typed error otherwise.
#'
#' @family family-design
#' @seealso
#'   \code{\link{multisitedgp_design}} for the constructor;
#'   \code{\link{update_multisitedgp_design}} for functional updates
#'   (each automatically re-validates);
#'   \code{\link{is_multisitedgp_design}} for the predicate test.
#'
#' @references
#' Lee, J., Che, J., Rabe-Hesketh, S., Feller, A., & Miratrix, L. (2025).
#' Improving the estimation of site-specific effects and their distribution
#' in multisite trials. \emph{Journal of Educational and Behavioral Statistics},
#' \bold{50}(5), 731--764. \doi{10.3102/10769986241254286}.
#'
#' @examples
#' design <- multisitedgp_design(J = 20L, sigma_tau = 0.20)
#' validate_multisitedgp_design(design)   # silent; returns design invisibly
#'
#' # Chain-friendly with the pipe.
#' design |> validate_multisitedgp_design() |> sim_multisite(seed = 1L)
#' @export
validate_multisitedgp_design <- function(design) {
  if (!is_multisitedgp_design(design)) {
    .abort_arg(
      "`design` must be a multisitedgp_design object.",
      "Validation replays the constructor contract for an existing design object.",
      "Use `multisitedgp_design()` to create a design before validation."
    )
  }
  do.call(multisitedgp_design, .design_to_constructor_args(design))
  invisible(design)
}

#' Functionally update a multisite simulation design
#'
#' @encoding UTF-8
#'
#' @description
#' Return a new `multisitedgp_design` with one or more fields changed,
#' preserving immutability. Useful when sweeping a single parameter
#' across a scenario grid, when correcting a constructor argument
#' without rebuilding from scratch, or when handing a partially-updated
#' design to \code{\link{design_grid}} as a `base_design`.
#'
#' @details
#' Updates are replayed through `multisitedgp_design()` so the result
#' is fully validated. When `dependence` changes and
#' `record_permutation` equals the old method's constructor default,
#' the new method's default is recomputed; values that differ from the
#' old method's default are carried forward. The design stores only the
#' resolved value, not whether it was supplied explicitly.
#'
#' Unknown update field names trigger a typed error pointing the
#' caller to the canonical `multisitedgp_design()` argument list — no
#' silent acceptance.
#'
#' @param design A `multisitedgp_design` object.
#' @param ... Fields to update using flat \code{\link{multisitedgp_design}}
#'   argument names. Unknown names error with a friendly hint.
#'
#' @return A new `multisitedgp_design` object (the original is
#'   unchanged).
#'
#' @family family-design
#' @seealso
#'   \code{\link{multisitedgp_design}} for the constructor;
#'   \code{\link{validate_multisitedgp_design}} for the standalone
#'   constructor-replay validator;
#'   \code{\link{design_grid}} for the `base_design` reuse pattern that
#'   relies on this function under the hood;
#'   \code{\link{is_multisitedgp_design}} for the predicate test.
#'
#' @references
#' Lee, J., Che, J., Rabe-Hesketh, S., Feller, A., & Miratrix, L. (2025).
#' Improving the estimation of site-specific effects and their distribution
#' in multisite trials. \emph{Journal of Educational and Behavioral Statistics},
#' \bold{50}(5), 731--764. \doi{10.3102/10769986241254286}.
#'
#' @examples
#' # Update a single field.
#' design <- multisitedgp_design(J = 20L, sigma_tau = 0.20)
#' design2 <- update_multisitedgp_design(design, sigma_tau = 0.25)
#' design2$sigma_tau   # 0.25
#' design$sigma_tau    # 0.20 (original unchanged)
#'
#' # Switch dependence method; record_permutation default updates accordingly.
#' design3 <- update_multisitedgp_design(design, dependence = "rank",
#'                                       rank_corr = 0.3)
#' design3$dependence_spec$method   # "rank"
#' @export
update_multisitedgp_design <- function(design, ...) {
  validate_multisitedgp_design(design)
  updates <- list(...)
  args <- .design_to_constructor_args(design)

  if (length(updates) == 0L) {
    return(do.call(multisitedgp_design, args))
  }

  bad <- setdiff(names(updates), names(args))
  if (length(bad) > 0L || any(!nzchar(names(updates)))) {
    bad_label <- paste(bad, collapse = ", ")
    .abort_arg(
      "Unknown design update field.",
      sprintf("Update fields must match `multisitedgp_design()` arguments; got: %s.", bad_label),
      "Use flat argument names such as `J`, `sigma_tau`, `dependence`, or `rank_corr`."
    )
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
  do.call(multisitedgp_design, args)
}

.new_multisitedgp_design <- function(x) {
  structure(x, class = c("multisitedgp_design", "list"))
}

#' @describeIn multisitedgp_design Print a compact multi-line design summary.
#' @export
print.multisitedgp_design <- function(x, ...) {
  cat(format(x, ...), "\n", sep = "")
  invisible(x)
}

#' @describeIn multisitedgp_design Format a compact multi-line design summary.
#' @export
format.multisitedgp_design <- function(x, ...) {
  paste(c(
    "<multisitedgp_design>",
    sprintf("Paradigm: %s    Engine: %s    Framing: %s", x$paradigm, x$engine, x$framing),
    sprintf("J: %s    Seed: %s    Lifecycle: experimental", x$J, .format_design_scalar(x$seed, null = "NULL (active RNG)")),
    "",
    "[ Layer 1: G-effects ]",
    sprintf("  true_dist:  %s", x$true_dist),
    sprintf("  tau:        %s", .format_design_scalar(x$tau)),
    sprintf("  sigma_tau:  %s", .format_design_scalar(x$sigma_tau)),
    sprintf("  formula:    %s", .format_design_scalar(x$formula)),
    sprintf("  beta:       %s", .format_design_vector(x$beta)),
    sprintf("  g_fn:       %s", .format_design_fn(x$g_fn)),
    "",
    sprintf("[ Layer 2: Margin (%s) ]", if (identical(x$paradigm, "direct")) "Paradigm B" else "Paradigm A"),
    if (identical(x$paradigm, "direct")) {
      c(
        sprintf("  I:          %s", .format_design_scalar(x$I)),
        sprintf("  R:          %s", .format_design_scalar(x$R)),
        sprintf("  shuffle:    %s", .format_design_scalar(x$shuffle)),
        sprintf("  se_fn:      %s", .format_design_fn(x$se_fn))
      )
    } else {
      c(
        sprintf("  nj_mean:    %s", .format_design_scalar(x$nj_mean)),
        sprintf("  cv:         %s", .format_design_scalar(x$cv)),
        sprintf("  nj_min:     %s", .format_design_scalar(x$nj_min)),
        sprintf("  p:          %s", .format_design_scalar(x$p)),
        sprintf("  R2:         %s", .format_design_scalar(x$R2)),
        sprintf("  var_outcome:%s", paste0(" ", .format_design_scalar(x$var_outcome)))
      )
    },
    "",
    "[ Layer 3: Dependence ]",
    sprintf("  method:        %s", x$dependence_spec$method),
    sprintf("  rank_corr:     %s", .format_design_scalar(x$dependence_spec$rank_corr)),
    sprintf("  pearson_corr:  %s", .format_design_scalar(x$dependence_spec$pearson_corr)),
    sprintf("  hybrid_init:   %s", .format_design_scalar(x$dependence_spec$hybrid_init)),
    sprintf("  hybrid_polish: %s", .format_design_scalar(x$dependence_spec$hybrid_polish)),
    sprintf("  dependence_fn: %s", .format_design_fn(x$dependence_spec$dependence_fn)),
    "",
    "[ Layer 4: Observation ]",
    sprintf("  obs_fn: %s", .format_design_fn(x$obs_spec$obs_fn)),
    "",
    "Use sim_multisite(design) or sim_meta(design) to simulate."
  ), collapse = "\n")
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
  as.character(x[[1L]])
}

.format_design_vector <- function(x) {
  if (is.null(x)) {
    return("NULL")
  }
  paste(as.character(x), collapse = ", ")
}

.format_design_fn <- function(x) {
  if (is.null(x)) {
    return("NULL")
  }
  "<function>"
}

.validate_theta_g_container <- function(theta_g) {
  if (!is.list(theta_g)) {
    .abort_arg(
      "`theta_G` must be a list.",
      "Distribution-specific parameters are stored as a list even when empty.",
      "Pass `theta_G = list()` or a named list such as `theta_G = list(nu = 5)`."
    )
  }
  if (length(theta_g) > 0L && !rlang::is_named(theta_g)) {
    .abort_arg(
      "`theta_G` entries must be named.",
      "Distribution-specific parameters are read by name.",
      "Use names such as `nu`, `slant`, `rho`, `delta`, `eps`, or `ups`."
    )
  }
  theta_g
}

.validate_list <- function(x, arg) {
  if (!is.list(x)) {
    .abort_arg(
      sprintf("`%s` must be a list.", arg),
      sprintf("`%s` stores optional callback arguments.", arg),
      sprintf("Pass `%s = list()` or a named list.", arg)
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
    .abort_arg(
      sprintf("`%s` must be one of: %s.", arg, paste(choices, collapse = ", ")),
      sprintf("`%s` is matched before the design object is created.", arg),
      sprintf("Use `%s = \"%s\"` or another listed value.", arg, choices[[1L]])
    )
  }
  x
}

.design_to_constructor_args <- function(design) {
  required <- c(
    "J", "paradigm", "true_dist", "tau", "sigma_tau", "variance", "theta_G",
    "formula", "beta", "data", "g_fn", "g_returns", "nj_mean", "cv",
    "nj_min", "p", "R2", "var_outcome", "engine", "I", "R", "shuffle",
    "se_fn", "se_args", "dependence_spec", "obs_spec", "framing", "seed",
    "record_permutation"
  )
  missing_fields <- setdiff(required, names(design))
  extra_fields <- setdiff(names(design), required)
  if (length(missing_fields) > 0L || length(extra_fields) > 0L) {
    .abort_arg(
      "Design object has an invalid field inventory.",
      sprintf(
        "Missing fields: %s. Extra fields: %s.",
        paste(missing_fields, collapse = ", "),
        paste(extra_fields, collapse = ", ")
      ),
      "Use `multisitedgp_design()` or `update_multisitedgp_design()` to create complete design objects."
    )
  }
  if (!is.list(design$dependence_spec) || !is.list(design$obs_spec)) {
    .abort_arg(
      "Design object has malformed nested specs.",
      "`dependence_spec` and `obs_spec` must be lists.",
      "Use `multisitedgp_design()` to rebuild the design from flat arguments."
    )
  }
  if (!identical(names(design$dependence_spec), c("method", "rank_corr", "pearson_corr", "dependence_fn", "hybrid_init", "hybrid_polish", "max_iter", "tol")) ||
    !identical(names(design$obs_spec), "obs_fn")) {
    .abort_arg(
      "Design object has malformed nested spec names.",
      "`dependence_spec` and `obs_spec` must use the canonical Step 2.1 field names.",
      "Use `multisitedgp_design()` to rebuild the design from flat arguments."
    )
  }

  c(
    design[c(
      "J", "paradigm", "true_dist", "tau", "sigma_tau", "variance", "theta_G",
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
      target_marginal_rho = NULL,
      obs_fn = design$obs_spec$obs_fn,
      framing = design$framing,
      seed = design$seed,
      record_permutation = design$record_permutation
    )
  )
}

.validate_design_ranges <- function(paradigm, true_dist, theta_G, cv, p, R2, I, R, max_iter, tol) {
  if (cv < 0) {
    .abort_arg(
      "`cv` must be >= 0.",
      "The site-size coefficient of variation cannot be negative.",
      "Use `cv = 0` for homogeneous site sizes or a positive value such as `cv = 0.5`."
    )
  }
  if (p <= 0 || p >= 1) {
    .abort_arg(
      "`p` must be strictly between 0 and 1.",
      "The treatment allocation factor diverges at the boundary.",
      "Pass a value such as `p = 0.5`."
    )
  }
  if (R2 < 0 || R2 >= 1) {
    .abort_arg(
      "`R2` must be in [0, 1).",
      "`R2 = 1` makes the sampling-variance scale degenerate.",
      "Pass `R2 = 0` for no covariate adjustment or a value less than 1."
    )
  }
  if (!is.null(I) && (I <= 0 || I >= 1)) {
    .abort_arg(
      "`I` must be strictly between 0 and 1.",
      "Direct designs use `I` as an informativeness proportion.",
      "Pass a value such as `I = 0.30` or `I = 0.50`."
    )
  }
  if (R < 1) {
    .abort_arg(
      "`R` must be >= 1.",
      "Direct designs use `R` as a max/min sampling-variance ratio.",
      "Pass `R = 1` for homogeneous precision or a value greater than 1."
    )
  }
  if (max_iter < 100L) {
    .abort_arg(
      "`max_iter` must be at least 100.",
      "Dependence solvers need a minimal iteration budget.",
      "Use `max_iter = 20000L` unless you have a reason to tune it."
    )
  }
  if (tol <= 0) {
    .abort_arg(
      "`tol` must be > 0.",
      "Dependence solvers require a positive tolerance.",
      "Pass `tol = 0.02` for the default tolerance."
    )
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
    .abort_arg(
      "`I` is required when `paradigm = \"direct\"`.",
      "Direct designs specify standard errors through exact `(I, R)` targets.",
      "Pass `I = 0.30` or use `paradigm = \"site_size\"`."
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
  se_fn,
  target_marginal_rho
) {
  if (!is.null(target_marginal_rho)) {
    .abort_arg(
      "`target_marginal_rho` is not supported in v1.0.",
      "multisiteDGP v1 uses residual interpretation for covariate-dependent effects.",
      "Remove `target_marginal_rho`; the marginal API is deferred to v2."
    )
  }
  if (identical(paradigm, "site_size")) {
    if (!is.null(I)) {
      .abort_coherence(
        "`I` is for `paradigm = \"direct\"`, not `paradigm = \"site_size\"`.",
        "Site-size designs derive precision from `nj_mean`, `cv`, `nj_min`, `p`, and `R2`.",
        "Use `paradigm = \"direct\"` or remove `I`."
      )
    }
    if (!identical(R, 1)) {
      .abort_coherence(
        "`R` is for `paradigm = \"direct\"`, not `paradigm = \"site_size\"`.",
        "Site-size designs derive precision ratios from generated site sizes.",
        "Use `paradigm = \"direct\"` or remove `R`."
      )
    }
    if (!identical(shuffle, TRUE)) {
      .abort_coherence(
        "`shuffle` is for `paradigm = \"direct\"`, not `paradigm = \"site_size\"`.",
        "Site-size designs generate site precision through the margin engine.",
        "Use `paradigm = \"direct\"` or remove `shuffle`."
      )
    }
    if (!is.null(se_fn)) {
      .abort_coherence(
        "`se_fn` is for `paradigm = \"direct\"`, not `paradigm = \"site_size\"`.",
        "Site-size designs use the Layer 2 margin engine to create standard errors.",
        "Use `paradigm = \"direct\", se_fn = my_se_fn` or remove `se_fn`."
      )
    }
  }
  if (identical(paradigm, "direct")) {
    bad <- c(
      if (!identical(nj_mean, 50)) "nj_mean",
      if (!identical(cv, 0.50)) "cv",
      if (!identical(nj_min, 5L)) "nj_min",
      if (!identical(p, 0.5)) "p",
      if (!identical(R2, 0)) "R2",
      if (!identical(engine, "A2_modern")) "engine"
    )
    if (length(bad) > 0L) {
      .abort_coherence(
        sprintf("`paradigm = \"direct\"` does not accept site-size margin args: %s.", paste(bad, collapse = ", ")),
        "Direct designs specify precision through `(I, R)` or a direct `se_fn` hook.",
        "Remove the site-size args or use `paradigm = \"site_size\"`."
      )
    }
  }
  invisible(TRUE)
}
# nolint end
