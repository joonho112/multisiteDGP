# nolint start: object_usage_linter
.mdgp_rank <- function(x) {
  x <- .validate_numeric_vector(x, "x", min_length = 2L)
  base::rank(x, ties.method = "average")
}

.validate_permutation <- function(perm, J, arg = "perm") {
  J <- .validate_scalar_integer(J, "J")
  if (length(perm) != J || anyNA(perm) || !is.numeric(perm) || any(!is.finite(perm))) {
    .abort_arg(
      sprintf("`%s` must be a complete permutation of 1:J.", arg),
      sprintf("Expected length %s with integer-like values.", J),
      sprintf("Pass `%s = sample.int(J)` or another permutation containing each index once.", arg)
    )
  }
  if (any(perm != floor(perm))) {
    .abort_arg(
      sprintf("`%s` must contain integer-like values.", arg),
      "Layer 3 uses explicit integer permutation indices to preserve paired columns.",
      sprintf("Pass `%s` as values in `seq_len(J)`.", arg)
    )
  }

  perm <- as.integer(perm)
  expected <- seq_len(J)
  if (!identical(sort(perm), expected)) {
    .abort_arg(
      sprintf("`%s` must contain each value in 1:J exactly once.", arg),
      "Duplicate, missing, zero, or out-of-range permutation indices break marginal preservation.",
      sprintf("Pass `%s` as a true permutation of `seq_len(J)`.", arg)
    )
  }
  perm
}

.validate_l3_upstream <- function(upstream, caller = "Layer 3 dependence") {
  if (!inherits(upstream, "data.frame")) {
    .abort_arg(
      "`upstream` must be a data frame.",
      sprintf("`%s()` permutes Layer 2 precision columns using tibble semantics.", caller),
      "Pass the output of `gen_site_sizes()` or `gen_se_direct()`."
    )
  }
  required <- c("site_index", "z_j", "tau_j", "n_j", "se2_j", "se_j")
  missing_required <- setdiff(required, names(upstream))
  if (length(missing_required) > 0L) {
    .abort_arg(
      "`upstream` is missing required Layer 2 columns.",
      sprintf("Missing columns: %s.", paste(missing_required, collapse = ", ")),
      "Pass the output of `gen_site_sizes()` or `gen_se_direct()`."
    )
  }
  if (nrow(upstream) < 2L) {
    .abort_arg(
      "`upstream` must contain at least two rows.",
      "Rank-correlation alignment requires at least two sites.",
      "Pass a Layer 2 frame generated with `J >= 10`."
    )
  }
  .validate_numeric_vector(upstream$z_j, "upstream$z_j", min_length = nrow(upstream))
  .validate_se2_vector(upstream$se2_j, min_length = nrow(upstream))
  .validate_positive_numeric_vector(upstream$se_j, "upstream$se_j", min_length = nrow(upstream))
  if (!is.integer(upstream$n_j)) {
    .abort_arg(
      "`upstream$n_j` must be integer.",
      "`n_j` is a paired Layer 2 column, with `NA_integer_` used for direct meta designs.",
      "Pass canonical Layer 2 output before applying dependence."
    )
  }
  tibble::as_tibble(upstream)
}

reorder_for_spearman <- function(x, y, target_corr = 1) {
  x <- .validate_numeric_vector(x, "x", min_length = 2L)
  y <- .validate_numeric_vector(y, "y", min_length = 2L)
  target_corr <- .validate_scalar_number(target_corr, "target_corr")
  if (length(x) != length(y)) {
    .abort_arg(
      "`x` and `y` must have the same length.",
      "Spearman reordering pairs one precision value with each target residual.",
      "Pass vectors from the same Layer 2 frame."
    )
  }
  if (abs(target_corr) > 1) {
    .abort_arg(
      "`target_corr` must be in [-1, 1].",
      "Spearman correlation targets outside [-1, 1] are not valid.",
      "Pass a target such as `0.3`, `0`, or `-0.3`."
    )
  }
  if (identical(target_corr, 0) || isTRUE(all.equal(target_corr, 0))) {
    return(seq_along(x))
  }

  x_order <- order(.mdgp_rank(x), seq_along(x))
  y_order <- order(.mdgp_rank(y), seq_along(y))
  if (target_corr < 0) {
    y_order <- rev(y_order)
  }
  perm <- integer(length(x))
  perm[x_order] <- y_order
  perm
}

.apply_l3_permutation <- function(upstream, perm, caller = "Layer 3 dependence") {
  upstream <- .validate_l3_upstream(upstream, caller = caller)
  perm <- .validate_permutation(perm, nrow(upstream), "perm")

  for (column in c("se2_j", "se_j", "n_j")) {
    upstream[[column]] <- upstream[[column]][perm]
  }
  attr(upstream, "permutation_perm") <- perm
  upstream
}

.realized_spearman <- function(x, y) {
  x <- .validate_numeric_vector(x, "x", min_length = 2L)
  y <- .validate_numeric_vector(y, "y", min_length = 2L)
  if (length(x) != length(y)) {
    .abort_arg(
      "`x` and `y` must have the same length.",
      "Realized Spearman correlation is computed pairwise.",
      "Pass two vectors with one value per site."
    )
  }
  x_rank <- .mdgp_rank(x)
  y_rank <- .mdgp_rank(y)
  if (length(unique(x_rank)) < 2L || length(unique(y_rank)) < 2L) {
    return(NA_real_)
  }
  stats::cor(x_rank, y_rank, method = "pearson")
}
# nolint end
