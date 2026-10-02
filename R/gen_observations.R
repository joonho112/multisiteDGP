#' Generate observed site estimates
#'
#' Draws the observed estimate `tau_j_hat` of each site from a normal
#' distribution with mean `tau_j` and variance `se2_j`, or obtains it from
#' `obs_fn`. This is the last step of [sim_multisite()] and [sim_meta()].
#'
#' @details
#' If `upstream` comes from [gen_site_sizes()] with `engine = "censored"` and
#' `obs_fn` is `NULL`, the function first shuffles the columns `n_j`, `se2_j`,
#' and `se_j` together across sites, as in Lee et al. (2025).
#' It stops before drawing if a dependence function has already aligned them,
#' because this shuffle would undo that alignment. Use `engine =
#' "truncated_gamma"` for such a pipeline. A custom `obs_fn` does not shuffle
#' the standard errors and can use the aligned data.
#'
#' @param upstream The data from the previous step: a data frame with one row
#'   per site and columns `site_index`, `z_j`, `tau_j`, `n_j`, `se2_j`, and
#'   `se_j`, as returned by [gen_site_sizes()], [gen_se_direct()], or a
#'   dependence function.
#' @param obs_fn Function with arguments `tau_j` and `se2_j`, or `NULL` (the
#'   default) for the normal draw. It receives those columns of `upstream` and
#'   must return one finite number per site.
#' @param ... Further arguments passed to `obs_fn`.
#'
#' @return The input data as a tibble with the column `tau_j_hat` added. The
#'   attribute `observation_diagnostics` records the method (`"gaussian"` or
#'   `"custom"`) and whether the standard errors were shuffled.
#'
#' @seealso [gen_effects()], [gen_site_sizes()], [gen_se_direct()],
#'   [align_rank_corr()], `vignette("model")`.
#' @examples
#' set.seed(1)
#' sites <- gen_site_sizes(gen_effects(J = 10), J = 10)
#' gen_observations(sites)
#' # Estimation errors from a t distribution with 5 df, variance se2_j
#' obs_t <- function(tau_j, se2_j, df) {
#'   tau_j + sqrt(se2_j * (df - 2) / df) * rt(length(tau_j), df)
#' }
#' gen_observations(sites, obs_fn = obs_t, df = 5)
#' @export
gen_observations <- function(upstream, obs_fn = NULL, ...) {
  if (missing(upstream)) {
    .abort(
      "`upstream` is required.",
      "It is the result of `gen_site_sizes()`, `gen_se_direct()`, or a dependence function."
    )
  }
  obs_fn <- .validate_function_or_null(obs_fn, "obs_fn")
  upstream <- .validate_observation_upstream(upstream, caller = "gen_observations")

  observation_permutation <- NULL
  if (is.null(obs_fn) && .uses_precision_shuffle(upstream)) {
    dependence <- attr(upstream, "dependence_diagnostics", exact = TRUE)
    if (is.list(dependence) && isTRUE(dependence$method %in% c("rank", "copula", "hybrid", "custom"))) {
      .abort(
        "`upstream` has a dependence alignment and `engine = \"censored\"`.",
        "The censored observation shuffle would undo that alignment.",
        "Use `engine = \"truncated_gamma\"` for a pipeline with dependence."
      )
    }
    observation_permutation <- base::sample.int(nrow(upstream), size = nrow(upstream))
    upstream <- .apply_observation_permutation(upstream, observation_permutation)
  }

  tau_j_hat <- if (is.null(obs_fn)) {
    stats::rnorm(nrow(upstream), mean = upstream$tau_j, sd = sqrt(upstream$se2_j))
  } else {
    out <- obs_fn(tau_j = upstream$tau_j, se2_j = upstream$se2_j, ...)
    .validate_obs_fn_output(out, J = nrow(upstream))
  }

  out <- dplyr::mutate(upstream, tau_j_hat = as.numeric(tau_j_hat))
  out <- .restore_custom_attrs(out, upstream)
  attr(out, "observation_diagnostics") <- list(
    method = if (is.null(obs_fn)) "gaussian" else "custom",
    precision_shuffled = !is.null(observation_permutation),
    rng_draws = if (is.null(obs_fn)) nrow(out) else NA_integer_
  )
  if (!is.null(observation_permutation)) {
    attr(out, "observation_permutation_perm") <- observation_permutation
  }
  out
}

.validate_observation_upstream <- function(upstream, caller = "gen_observations") {
  if (!inherits(upstream, "data.frame")) {
    .abort(
      "`upstream` must be a data frame.",
      .you_passed(upstream),
      sprintf(
        "`%s()` takes the result of `gen_site_sizes()`, `gen_se_direct()`, or a dependence function.",
        caller
      )
    )
  }
  if ("tau_j_hat" %in% names(upstream)) {
    .abort("`upstream` must not have a column `tau_j_hat`, which this function adds.")
  }

  out <- .validate_precision_upstream(upstream, caller = caller)
  out <- .restore_custom_attrs(out, upstream)
  .validate_numeric_vector(out$tau_j, "upstream$tau_j", min_length = nrow(out))
  out
}

.validate_obs_fn_output <- function(out, J) {
  if (!is.numeric(out) || length(out) != J || anyNA(out) || any(!is.finite(out))) {
    .abort(
      "`obs_fn` must return one finite number per site.",
      sprintf("For %s sites it returned %s.", J, .describe_returned(out, J, "missing or infinite values"))
    )
  }
  as.numeric(out)
}

.uses_precision_shuffle <- function(upstream) {
  identical(attr(upstream, "engine", exact = TRUE), "censored")
}

.apply_observation_permutation <- function(upstream, perm) {
  perm <- .validate_permutation(perm, nrow(upstream), "perm")
  out <- tibble::as_tibble(upstream)
  out <- .restore_custom_attrs(out, upstream)
  for (column in c("n_j", "se2_j", "se_j")) {
    out[[column]] <- out[[column]][perm]
  }
  out
}

.restore_custom_attrs <- function(out, source) {
  custom_attrs <- setdiff(names(attributes(source)), c("names", "row.names", "class"))
  for (attr_name in custom_attrs) {
    attr(out, attr_name) <- attr(source, attr_name, exact = TRUE)
  }
  out
}
