.mdgp_rank <- function(x) {
  x <- .validate_numeric_vector(x, "x", min_length = 2L)
  base::rank(x, ties.method = "average")
}

.validate_permutation <- function(perm, J, arg = "perm") {
  J <- .validate_scalar_integer(J, "J")
  if (length(perm) != J || anyNA(perm) || !is.numeric(perm) || any(!is.finite(perm))) {
    .abort(
      sprintf("`%s` must be a permutation of the numbers 1 to %s.", arg, J),
      .you_passed(perm)
    )
  }
  if (any(perm != floor(perm))) {
    .abort(sprintf("`%s` must hold whole numbers.", arg))
  }

  perm <- as.integer(perm)
  expected <- seq_len(J)
  if (!identical(sort(perm), expected)) {
    .abort(sprintf("`%s` must hold each number from 1 to %s exactly once.", arg, J))
  }
  perm
}

.validate_precision_upstream <- function(upstream, caller = "align_rank_corr") {
  takes <- sprintf("`%s()` takes the result of `gen_site_sizes()` or `gen_se_direct()`.", caller)
  if (!inherits(upstream, "data.frame")) {
    .abort(
      "`upstream` must be a data frame.",
      .you_passed(upstream),
      takes
    )
  }
  required <- c("site_index", "z_j", "tau_j", "n_j", "se2_j", "se_j")
  missing_required <- setdiff(required, names(upstream))
  if (length(missing_required) > 0L) {
    .abort(
      "`upstream` must have the columns `site_index`, `z_j`, `tau_j`, `n_j`, `se2_j`, and `se_j`.",
      sprintf("Missing columns: %s.", paste(missing_required, collapse = ", ")),
      takes
    )
  }
  if (nrow(upstream) < 2L) {
    .abort(
      "`upstream` must have at least two rows.",
      sprintf("`nrow(upstream)` is %s.", nrow(upstream))
    )
  }
  .validate_numeric_vector(upstream$z_j, "upstream$z_j", min_length = nrow(upstream))
  .validate_se2_vector(upstream$se2_j, min_length = nrow(upstream))
  .validate_positive_numeric_vector(upstream$se_j, "upstream$se_j", min_length = nrow(upstream))
  if (!is.integer(upstream$n_j)) {
    .abort(
      "`upstream$n_j` must be an integer vector.",
      sprintf("It has class %s.", class(upstream$n_j)[[1L]]),
      "Convert it with `as.integer()`, or use `NA_integer_` where there is no site size."
    )
  }
  tibble::as_tibble(upstream)
}

.reorder_for_spearman <- function(x, y, target_corr = 1) {
  x <- .validate_numeric_vector(x, "x", min_length = 2L)
  y <- .validate_numeric_vector(y, "y", min_length = 2L)
  target_corr <- .validate_scalar_number(target_corr, "target_corr")
  if (length(x) != length(y)) {
    .abort(
      "`x` and `y` must have the same length.",
      sprintf("Their lengths are %s and %s.", length(x), length(y))
    )
  }
  if (abs(target_corr) > 1) {
    .abort(
      "`target_corr` must be between -1 and 1.",
      .you_passed(target_corr)
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

.apply_dependence_permutation <- function(upstream, perm, caller = "align_rank_corr") {
  upstream <- .validate_precision_upstream(upstream, caller = caller)
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
    .abort(
      "`x` and `y` must have the same length.",
      sprintf("Their lengths are %s and %s.", length(x), length(y))
    )
  }
  x_rank <- .mdgp_rank(x)
  y_rank <- .mdgp_rank(y)
  if (length(unique(x_rank)) < 2L || length(unique(y_rank)) < 2L) {
    return(NA_real_)
  }
  stats::cor(x_rank, y_rank, method = "pearson")
}
