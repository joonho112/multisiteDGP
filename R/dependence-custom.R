.apply_custom_dependence_fn <- function(
  upstream,
  dependence_fn,
  target,
  target_type,
  caller,
  ...
) {
  upstream <- .validate_precision_upstream(upstream, caller = caller)
  dependence_fn <- .validate_function_or_null(dependence_fn, "dependence_fn")
  returned <- dependence_fn(
    z_j = upstream$z_j,
    se2_j = upstream$se2_j,
    target = target,
    ...
  )
  perm <- .validate_dependence_fn_output(returned, upstream$se2_j)
  out <- .apply_dependence_permutation(upstream, perm, caller = caller)
  achieved_spearman <- .realized_spearman(out$z_j, out$se2_j)
  achieved <- if (identical(target_type, "residual_spearman")) {
    achieved_spearman
  } else {
    NA_real_
  }
  residual <- if (identical(target_type, "residual_spearman")) {
    achieved_spearman - target
  } else {
    NA_real_
  }

  attr(out, "dependence_diagnostics") <- list(
    method = "custom",
    target_type = target_type,
    target = unname(target),
    achieved = unname(achieved),
    achieved_spearman = unname(achieved_spearman),
    residual = unname(residual),
    preservation = "empirical_multiset",
    ties_method = "average",
    rng_draws = NA_integer_,
    converged = NA,
    iterations = NA_integer_,
    tol = NA_real_,
    custom = TRUE
  )
  out
}

.validate_dependence_fn_output <- function(returned, se2_j) {
  if (!is.list(returned) || !all(c("se2_j", "perm") %in% names(returned))) {
    .abort(
      "`dependence_fn` must return `list(se2_j = se2_j[perm], perm = perm)`.",
      sprintf("It returned %s.", .describe_value(returned))
    )
  }
  out_se2 <- returned$se2_j
  if (!is.numeric(out_se2) || length(out_se2) != length(se2_j) || anyNA(out_se2) || any(!is.finite(out_se2))) {
    .abort(
      "`dependence_fn` must return `se2_j` as one finite number per site.",
      sprintf(
        "For %s sites it returned %s.",
        length(se2_j), .describe_returned(out_se2, length(se2_j), "missing or infinite values")
      )
    )
  }
  perm <- .validate_dependence_fn_perm(returned$perm, length(se2_j))
  if (!identical(unname(sort(out_se2)), unname(sort(se2_j)))) {
    .abort(
      "`dependence_fn` changed the values of `se2_j`.",
      "The returned `se2_j` must hold the values it was given, in a new order."
    )
  }
  if (!identical(out_se2, se2_j[perm])) {
    .abort(
      "`dependence_fn` returned `se2_j` and `perm` that do not agree.",
      "The returned `se2_j` must equal `se2_j[perm]`, with the `se2_j` it was given."
    )
  }
  perm
}

.validate_dependence_fn_perm <- function(perm, J) {
  if (length(perm) != J || anyNA(perm) || !is.numeric(perm) || any(!is.finite(perm))) {
    .abort(
      sprintf("`dependence_fn` must return `perm` as %s finite numbers, one for each site.", J),
      sprintf("It returned %s.", .describe_returned(perm, J, "missing or infinite values"))
    )
  }
  if (any(perm != floor(perm))) {
    .abort("`dependence_fn` must return whole numbers in `perm`.")
  }
  perm <- as.integer(perm)
  if (!identical(sort(perm), seq_len(J))) {
    .abort(
      sprintf("`dependence_fn` must return each number from 1 to %s exactly once in `perm`.", J)
    )
  }
  perm
}
