# nolint start: object_usage_linter
.apply_custom_dependence_fn <- function(
  upstream,
  dependence_fn,
  target,
  target_type,
  caller,
  ...
) {
  upstream <- .validate_l3_upstream(upstream, caller = caller)
  dependence_fn <- .validate_function_or_null(dependence_fn, "dependence_fn")
  hook_out <- dependence_fn(
    z_j = upstream$z_j,
    se2_j = upstream$se2_j,
    target = target,
    ...
  )
  perm <- .validate_dependence_fn_output(hook_out, upstream$se2_j)
  out <- .apply_l3_permutation(upstream, perm, caller = caller)
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

.validate_dependence_fn_output <- function(hook_out, se2_j) {
  if (!is.list(hook_out) || !all(c("se2_j", "perm") %in% names(hook_out))) {
    .abort_marginal_violation(
      "`dependence_fn` must return `list(se2_j = ..., perm = ...)`.",
      "Numeric-only returns cannot recover paired permutations when precision values are tied.",
      "Use `list(se2_j = se2_j[perm], perm = perm)` with an explicit permutation index."
    )
  }
  out_se2 <- hook_out$se2_j
  if (!is.numeric(out_se2) || length(out_se2) != length(se2_j) || anyNA(out_se2) || any(!is.finite(out_se2))) {
    .abort_marginal_violation(
      "`dependence_fn` returned invalid `se2_j`.",
      "`se2_j` must be a finite numeric vector with one value per site.",
      "Use `se2_j = input_se2_j[perm]` without changing length or values."
    )
  }
  perm <- .validate_dependence_fn_perm(hook_out$perm, length(se2_j))
  if (!identical(unname(sort(out_se2)), unname(sort(se2_j)))) {
    .abort_marginal_violation(
      "Custom `dependence_fn` violated marginal preservation.",
      "`sort(out$se2_j)` must be bit-identical to `sort(se2_j)`.",
      "Use `se2_j[perm]`; do not mutate precision values."
    )
  }
  if (!identical(out_se2, se2_j[perm])) {
    .abort_marginal_violation(
      "`dependence_fn` returned inconsistent `se2_j` and `perm`.",
      "`out$se2_j` must equal the input `se2_j[perm]` exactly.",
      "Use a `perm` that matches the actual `se2_j` permutation."
    )
  }
  perm
}

.validate_dependence_fn_perm <- function(perm, J) {
  if (length(perm) != J || anyNA(perm) || !is.numeric(perm) || any(!is.finite(perm))) {
    .abort_marginal_violation(
      "`dependence_fn` returned invalid `perm`.",
      sprintf("Expected length %s with finite integer-like values.", J),
      "Use `perm` as a complete permutation of `seq_len(J)`."
    )
  }
  if (any(perm != floor(perm))) {
    .abort_marginal_violation(
      "`dependence_fn` returned non-integer `perm` values.",
      "Layer 3 requires explicit integer permutation indices.",
      "Use `perm` as integer-like values in `seq_len(J)`."
    )
  }
  perm <- as.integer(perm)
  if (!identical(sort(perm), seq_len(J))) {
    .abort_marginal_violation(
      "`dependence_fn` returned `perm` that is not a complete 1:J permutation.",
      "Duplicate, missing, zero, or out-of-range indices break paired-column preservation.",
      "Use each integer in `seq_len(J)` exactly once."
    )
  }
  perm
}
# nolint end
