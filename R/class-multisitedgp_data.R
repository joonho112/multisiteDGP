#' Simulated site-level summary data
#'
#' [sim_multisite()] and [sim_meta()] return a tibble of class
#' `multisitedgp_data` with one row per site. The attributes `design` and
#' `diagnostics` hold the design and a list of diagnostics, whose element
#' `target_vs_realized` is the table that `summary()` prints.
#'
#' @format Seven columns, then the columns of the design's `data`, if any:
#' \describe{
#'   \item{`site_index`}{Site number, from 1 to the number of sites.}
#'   \item{`z_j`}{Standardized site effect, drawn with mean 0 and variance 1.}
#'   \item{`tau_j`}{True site effect.}
#'   \item{`tau_j_hat`}{Observed estimate of `tau_j`.}
#'   \item{`se_j`}{Standard error of `tau_j_hat`.}
#'   \item{`se2_j`}{Sampling variance, `se_j^2`.}
#'   \item{`n_j`}{Site size, or `NA` when standard errors are set directly.}
#' }
#' @details `print()` shows the number of sites, the source of the standard
#' errors, the seed, and up to three diagnostics (`sigma_tau`, rank
#' correlation, informativeness), above the first `n` rows. With covariates
#' the rank correlation line is labeled `(z, se)`, because it is the
#' correlation with the standardized site effects `z_j`, which then differs
#' from the one with `tau_j`. `summary()` prints every diagnostic that has a
#' value, by group (scale, dependence, shape, precision), with its target and
#' status (`pass`, `warn`, `fail`), if any. The section below defines the
#' rows.
#'
#' `as_tibble()` drops the class and both attributes. So does `[`, an
#' assignment, or a renaming that removes any of the seven columns. Selecting
#' or reordering columns and adding a column keep the class, `design`, and
#' `diagnostics`. Selecting rows, as in `x[i, ]`, drops `diagnostics` with a
#' warning. Changing a value in one of the seven columns drops `diagnostics`
#' without a warning, by assignment (`x$se_j <- `) or with `dplyr::mutate()`.
#' `print()` and `summary()` then compute the diagnostics from the data as
#' they are. They do the same when the number of rows no longer matches the
#' stored diagnostics, as after `rbind()`.
#'
#' @section Rows of summary():
#' A status is `pass` or `fail` by the table and `warn` in between. Rows
#' without a target or a rule have no status. A status in the scale,
#' dependence, or shape group compares one simulated data set with its design.
#' Where a row depends on random draws, its rule allows for the sampling error
#' of `J` sites. The `sigma_tau` row, a rank correlation with target 0, the
#' copula row, and the KS distance each pass a data set simulated from the
#' design about 95 times in 100 or more. The `sigma_tau` row passes less often
#' when a few sites carry most of the variance, as in a mixture with a rare
#' and distant component (70 times in 100 with `eps = 0.05`, `delta = 50`, and
#' 25 sites).
#' A status in the precision group describes the design.
#'
#' | Row | Value | `pass` | `fail` |
#' |---|---|---|---|
#' | informativeness I | [informativeness()] | within 0.02 of a target `I` | 0.05 or more away |
#' | SE heterogeneity R | [heterogeneity_ratio()] | within 10% of a target `R` | 25% or more away |
#' | sigma_tau | `sd(z_j) * sigma_tau` | within 2 SE of `sigma_tau` | more than 3 SE away |
#' | geometric mean of se^2 | of `se2_j` | within 5% of the value that a target `I` implies | 15% or more away |
#' | rank correlation, residual | Spearman correlation of `z_j` and `se2_j` | see below | see below |
#' | rank correlation, marginal | the same with `tau_j` | | |
#' | Pearson correlation, copula latent | of the copula's scores | within 2 SE of `pearson_corr` | more than 3 SE away |
#' | Pearson correlation, residual and marginal | of `se2_j` with `z_j` and with `tau_j` | | |
#' | KS distance | [ks_distance()], with the p-value of the test | p above 0.05 | p at most 0.01 |
#' | Bhattacharyya coefficient | [bhattacharyya_coef()] | | |
#' | Q-Q residual | largest gap between the sorted `z_j` and the design's quantiles | | |
#' | mean shrinkage | [mean_shrinkage()] | from 0.10 to 0.95 | below 0.05 or above 0.99 |
#' | average margin of error | `1.96 * mean(se_j)` | at most `2 * sigma_tau` | above `4 * sigma_tau` |
#'
#' SE is a standard error. For the `sigma_tau` row it is
#' `sigma_tau * sqrt((k - 1 + 2 / (J - 1)) / (4 * J))`, where `k` is the
#' kurtosis of the design's distribution. For the normal distribution `k` is 3
#' and the SE is `sigma_tau / sqrt(2 * (J - 1))`. The row has no status when
#' the kurtosis is unknown or infinite (`true_dist = "User"`, a Student t
#' distribution with `nu` of 4 or less) and when the SE is half of `sigma_tau`
#' or more. For the copula row the SE is `(1 - pearson_corr^2) / sqrt(J - 1)`,
#' the large-sample value for two random scores. The actual SE is at most
#' that, so the row passes at least as often.
#' A residual rank correlation with target 0 passes up to `2 / sqrt(J - 1)` in
#' absolute value and fails above `3 / sqrt(J - 1)`. With another target it
#' passes within `max(0.02, tol)` of the target and fails beyond
#' `max(0.05, 2 * tol)`. The three shape rows appear only for
#' `true_dist = "Gaussian"` and `"StudentT"`.
#'
#' @param x,object A `multisitedgp_data` object. `is_multisitedgp_data()`
#'   takes any object.
#' @param n Integer. Number of rows shown by `print()` (at least 1). Default 6.
#' @param i,j Row and column indices, as for a tibble.
#' @param drop Logical. `TRUE` returns one column as a vector. Default `FALSE`.
#' @param .preserve_class Logical. If `TRUE`, `as_tibble()` returns `x`
#'   unchanged. Default `FALSE`.
#' @param ... Passed to the tibble method. Not used by `summary()`.
#' @return `is_multisitedgp_data()` returns `TRUE` or `FALSE`. `print()` and
#'   `summary()` return their argument invisibly, and `as_tibble()` and `[`
#'   return a tibble.
#' @seealso [plots], [adapters], `vignette("multisiteDGP")`.
#' @family simulation
#' @examples
#' dat <- sim_multisite(preset_education_modest(), seed = 1)
#' print(dat, n = 3)
#' summary(dat)
#' tibble::as_tibble(dat)
#' is_multisitedgp_data(dat)
#' @name multisitedgp_data
#' @rdname multisitedgp_data
#' @export
is_multisitedgp_data <- function(x) {
  inherits(x, "multisitedgp_data")
}

#' @rdname multisitedgp_data
#' @importFrom tibble as_tibble
#' @export
as_tibble.multisitedgp_data <- function(x, ..., .preserve_class = FALSE) {
  .preserve_class <- .validate_scalar_logical(.preserve_class, ".preserve_class")
  if (.preserve_class) {
    return(x)
  }
  .strip_multisitedgp_data(x, ...)
}

#' @rdname multisitedgp_data
#' @export
"[.multisitedgp_data" <- function(x, i, j, ..., drop = FALSE) {
  n_real_args <- nargs() - !missing(drop)
  row_subset <- n_real_args > 2L && !missing(i)
  out <- NextMethod()
  .restore_multisitedgp_data_subset(x, out, row_subset = row_subset)
}

#' @rawNamespace S3method(dplyr::dplyr_row_slice,multisitedgp_data)
dplyr_row_slice.multisitedgp_data <- function(data, i, ...) {
  out <- NextMethod()
  .restore_multisitedgp_data_subset(data, out, row_subset = TRUE)
}

#' @rawNamespace S3method(dplyr::dplyr_reconstruct,multisitedgp_data)
dplyr_reconstruct.multisitedgp_data <- function(data, template) {
  .restore_multisitedgp_data_values(template, data)
}

#' @export
"$<-.multisitedgp_data" <- function(x, name, value) {
  out <- NextMethod()
  .drop_stale_diagnostics(x, out)
}

#' @export
"[[<-.multisitedgp_data" <- function(x, i, j, ..., value) {
  out <- NextMethod()
  .drop_stale_diagnostics(x, out)
}

#' @export
"[<-.multisitedgp_data" <- function(x, i, j, ..., value) {
  out <- NextMethod()
  .drop_stale_diagnostics(x, out)
}

#' @export
"names<-.multisitedgp_data" <- function(x, value) {
  out <- NextMethod()
  .drop_stale_diagnostics(x, out)
}

#' @rdname multisitedgp_data
#' @export
print.multisitedgp_data <- function(x, n = 6L, ...) {
  n <- .validate_print_n(n)
  cat(paste(.format_multisitedgp_print_header(x), collapse = "\n"), "\n", sep = "")
  print(.strip_multisitedgp_data(x), n = n, ...)
  invisible(x)
}

#' @rdname multisitedgp_data
#' @export
summary.multisitedgp_data <- function(object, ...) {
  cat(paste(.format_multisitedgp_summary(object), collapse = "\n"), "\n", sep = "")
  invisible(object)
}

.new_multisitedgp_data <- function(x, design = NULL, diagnostics = list(), meta = FALSE) {
  out <- .as_plain_tibble(x)
  .validate_multisitedgp_data_columns(out)
  if (!is.null(design)) {
    validate_multisitedgp_design(design)
    expected_paradigm <- if (isTRUE(meta)) "direct" else "site_size"
    if (!identical(design$paradigm, expected_paradigm)) {
      .abort(
        sprintf(
          "`design$paradigm` must be \"%s\" for data of class `%s`.",
          expected_paradigm,
          if (isTRUE(meta)) "multisitedgp_meta" else "multisitedgp_data"
        ),
        sprintf("It is \"%s\".", design$paradigm)
      )
    }
  }
  diagnostics <- .validate_list(diagnostics, "diagnostics")

  attr(out, "design") <- design
  attr(out, "diagnostics") <- diagnostics
  class(out) <- .data_class_vector(meta)
  out
}

.new_multisitedgp_meta <- function(x, design = NULL, diagnostics = list()) {
  .new_multisitedgp_data(x, design = design, diagnostics = diagnostics, meta = TRUE)
}

.data_columns <- function() {
  c("site_index", "z_j", "tau_j", "tau_j_hat", "se_j", "se2_j", "n_j")
}

.data_class_vector <- function(meta = FALSE) {
  if (isTRUE(meta)) {
    return(c("multisitedgp_meta", "multisitedgp_data", "tbl_df", "tbl", "data.frame"))
  }
  c("multisitedgp_data", "tbl_df", "tbl", "data.frame")
}

.is_meta_class <- function(x) {
  inherits(x, "multisitedgp_meta")
}

.strip_multisitedgp_data <- function(x, ...) {
  .as_plain_tibble(x, ...)
}

.as_plain_tibble <- function(x, ...) {
  out <- as.data.frame(x)
  keep <- intersect(c("names", "row.names", "class"), names(attributes(out)))
  attributes(out) <- attributes(out)[keep]
  tibble::as_tibble(out, ...)
}

.restore_multisitedgp_data_subset <- function(x, out, row_subset = FALSE) {
  if (!inherits(out, "data.frame")) {
    return(out)
  }
  if (!all(.data_columns() %in% names(out))) {
    return(.strip_multisitedgp_data(out))
  }

  out <- .as_plain_tibble(out)
  attr(out, "design") <- attr(x, "design", exact = TRUE)
  attr(out, "diagnostics") <- attr(x, "diagnostics", exact = TRUE)

  if (isTRUE(row_subset)) {
    had_diagnostics <- !is.null(attr(out, "diagnostics", exact = TRUE))
    attr(out, "diagnostics") <- NULL
    if (had_diagnostics) {
      cli::cli_warn(c(
        "!" = "Diagnostics dropped because rows were selected.",
        "i" = "`print()` and `summary()` compute them from the remaining rows."
      ))
    }
  }

  class(out) <- .data_class_vector(.is_meta_class(x))
  out
}

# Stored diagnostics describe the seven columns as they were simulated. Once a
# value in one of them has changed, they are dropped so that `print()` and
# `summary()` compute them again.
.same_data_columns <- function(x, y) {
  same <- vapply(
    .data_columns(),
    function(column) identical(x[[column]], y[[column]]),
    logical(1)
  )
  all(same)
}

# For dplyr, which hands over a bare data frame: the class and the attributes
# are put back first.
.restore_multisitedgp_data_values <- function(x, out) {
  out <- .restore_multisitedgp_data_subset(x, out)
  if (is_multisitedgp_data(out) && !.same_data_columns(out, x)) {
    attr(out, "diagnostics") <- NULL
  }
  out
}

# For an assignment or a renaming, after which `out` still has the class and
# every attribute of `x`. Two columns that exchanged their names count as
# changed values.
.drop_stale_diagnostics <- function(x, out) {
  if (!all(.data_columns() %in% names(out))) {
    return(.remove_multisitedgp_class(out))
  }
  if (!.same_data_columns(out, x)) {
    attr(out, "diagnostics") <- NULL
  }
  out
}

# Takes the class and the two attributes off without building the tibble
# again. Names that a tibble accepts from `names<-`, such as the missing names
# after `unname()`, would not pass the checks of `tibble::as_tibble()`.
.remove_multisitedgp_class <- function(out) {
  attr(out, "design") <- NULL
  attr(out, "diagnostics") <- NULL
  class(out) <- setdiff(class(out), c("multisitedgp_meta", "multisitedgp_data"))
  out
}

.validate_multisitedgp_data_columns <- function(x) {
  missing_columns <- setdiff(.data_columns(), names(x))
  if (length(missing_columns) > 0L) {
    .abort(
      "`x` must have the columns of simulated data.",
      sprintf("Missing columns: %s.", paste(missing_columns, collapse = ", "))
    )
  }
  if (!is.integer(x$site_index)) {
    .abort(
      "Column `site_index` must be an integer vector.",
      sprintf("It has class %s.", class(x$site_index)[[1L]])
    )
  }
  numeric_columns <- c("z_j", "tau_j", "tau_j_hat", "se_j", "se2_j")
  non_numeric <- numeric_columns[!vapply(x[numeric_columns], is.numeric, logical(1))]
  if (length(non_numeric) > 0L) {
    .abort(
      "Columns `z_j`, `tau_j`, `tau_j_hat`, `se_j`, and `se2_j` must be numeric.",
      sprintf("Not numeric: %s.", paste(non_numeric, collapse = ", "))
    )
  }
  if (!is.integer(x$n_j)) {
    .abort(
      "Column `n_j` must be an integer vector, with `NA` where there is no site size.",
      sprintf("It has class %s.", class(x$n_j)[[1L]])
    )
  }
  invisible(x)
}

.validate_print_n <- function(n) {
  n <- .validate_scalar_integer(n, "n")
  if (n < 1L) {
    .abort(
      "`n` must be at least 1.",
      .you_passed(n)
    )
  }
  n
}

.format_multisitedgp_print_header <- function(x) {
  first <- paste0("# multisitedgp_data: ", .describe_simulation(x))
  tab <- .diagnostics_table_for_reporting(x)
  if (is.null(tab)) {
    return(first)
  }
  # The row is the correlation with z_j. With covariates that is not the
  # correlation with tau_j, so the label names z.
  has_covariates <- !is.null(attr(x, "design", exact = TRUE)$formula)
  rank_label <- if (has_covariates) "rank correlation (z, se)" else "rank correlation (tau, se)"
  c(
    first,
    .format_headline_row(tab, "sigma_tau", "residual", "sigma_tau"),
    .format_headline_row(tab, "realized_rank_corr", "residual", rank_label),
    .format_headline_row(tab, "informativeness", "overall", "informativeness I")
  )
}

.format_multisitedgp_summary <- function(x) {
  title <- .format_summary_title(x)
  tab <- .diagnostics_table_for_reporting(x)
  if (is.null(tab)) {
    return(c(title, "Diagnostics are not available for this object."))
  }

  rows <- .summary_report_rows(tab)
  blocks <- lapply(unique(rows$block), function(block) {
    c("", block, rows$line[rows$block == block])
  })
  c(
    title,
    unlist(blocks),
    "",
    sprintf(
      "%d pass, %d warn, %d fail",
      sum(rows$status == "pass"), sum(rows$status == "warn"), sum(rows$status == "fail")
    )
  )
}

.format_summary_title <- function(x) {
  preset <- attr(attr(x, "design", exact = TRUE), "preset_name", exact = TRUE)
  has_preset <- is.character(preset) && length(preset) == 1L && !is.na(preset) && nzchar(preset)
  sprintf(
    "multisiteDGP simulation%s: %s",
    if (has_preset) sprintf(" (%s)", preset) else "",
    .describe_simulation(x)
  )
}

# The number of sites, where the standard errors come from, and the seed. An
# object without a design can only report its number of sites.
.describe_simulation <- function(x) {
  design <- attr(x, "design", exact = TRUE)
  if (is.null(design)) {
    return(sprintf("%s sites", nrow(x)))
  }
  .format_design_headline(nrow(x), design$paradigm, design$seed)
}

# The stored diagnostics, or NULL when they are not a list or describe another
# number of sites than the data have now. `rbind()` and other functions that
# add or remove rows without calling `[` leave the attribute as it was.
.stored_diagnostics <- function(x) {
  diagnostics <- attr(x, "diagnostics", exact = TRUE)
  if (!is.list(diagnostics)) {
    return(NULL)
  }
  stored_sites <- diagnostics[["J"]]
  if (.is_finite_number(stored_sites) && stored_sites != nrow(x)) {
    return(NULL)
  }
  diagnostics
}

.diagnostics_table_for_reporting <- function(x) {
  diagnostics <- .stored_diagnostics(x)
  tab <- diagnostics$target_vs_realized
  if (!is.null(tab)) {
    return(tab)
  }
  if (is.null(diagnostics)) {
    # Nothing stored applies, so the table is computed from the data alone.
    attr(x, "diagnostics") <- NULL
  }
  tryCatch(
    .apply_diagnostic_thresholds(.compute_diagnostics_table(x), x = x),
    error = function(e) NULL
  )
}

.format_headline_row <- function(tab, diagnostic, basis, label) {
  row <- .report_row(tab, diagnostic, basis)
  if (is.null(row) || !.is_finite_number(row[["realized"]])) {
    return(NULL)
  }
  realized <- .fmt_report_number(row[["realized"]])
  if (.is_finite_number(row[["target"]])) {
    return(sprintf("# %s: target %s, realized %s", label, .fmt_report_number(row[["target"]]), realized))
  }
  sprintf("# %s: %s", label, realized)
}

# The rows of the summary in print order: the block heading, the diagnostic and
# basis that identify the row of the diagnostics table, and the printed label.
.summary_row_spec <- function() {
  tibble::tribble(
    ~block,        ~diagnostic,             ~basis,          ~label,
    "Scale",       "informativeness",       "overall",       "informativeness I",
    "Scale",       "heterogeneity_ratio",   "overall",       "SE heterogeneity R",
    "Scale",       "sigma_tau",             "residual",      "sigma_tau",
    "Scale",       "GM_se2",                "overall",       "geometric mean of se^2",
    "Dependence",  "realized_rank_corr",    "residual",      "rank correlation, residual",
    "Dependence",  "realized_rank_corr",    "marginal",      "rank correlation, marginal",
    "Dependence",  "realized_pearson_corr", "copula_latent", "Pearson correlation, copula latent",
    "Dependence",  "raw_pearson_corr",      "residual",      "Pearson correlation, residual",
    "Dependence",  "raw_pearson_corr",      "marginal",      "Pearson correlation, marginal",
    "Shape",       "ks_distance",           "target_G",      "KS distance",
    "Shape",       "bhattacharyya_coef",    "target_G",      "Bhattacharyya coefficient",
    "Shape",       "qq_residuals",          "target_G",      "Q-Q residual",
    "Precision",   "mean_shrinkage",        "overall",       "mean shrinkage",
    "Precision",   "average_moe",           "overall",       "average margin of error"
  )
}

# One entry per row that has a finite realized value, with its printed line.
.summary_report_rows <- function(tab) {
  rows <- .summary_row_spec()
  found <- lapply(seq_len(nrow(rows)), function(i) {
    .report_row(tab, rows$diagnostic[[i]], rows$basis[[i]])
  })
  keep <- vapply(found, function(row) {
    !is.null(row) && .is_finite_number(row[["realized"]])
  }, logical(1))
  rows <- rows[keep, , drop = FALSE]
  found <- found[keep]

  rows$value <- vapply(found, function(row) .fmt_report_number(row[["realized"]]), character(1))
  rows$reference <- vapply(found, .summary_row_reference, character(1))
  rows$status <- vapply(found, .summary_row_status, character(1))
  rows$line <- .format_summary_lines(rows)
  rows
}

# The column after the realized value: the p-value for the KS row, otherwise
# the target. The shape rows are compared with an ideal value (0 or 1) that the
# design does not set, so they show no target.
.summary_row_reference <- function(row) {
  if (identical(row[["diagnostic"]], "ks_distance") && .is_finite_number(row[["p_value"]])) {
    return(paste("p =", .fmt_report_number(row[["p_value"]])))
  }
  if (.is_finite_number(row[["target"]]) && !identical(row[["target_source"]], "ideal")) {
    return(paste("target", .fmt_report_number(row[["target"]])))
  }
  ""
}

.summary_row_status <- function(row) {
  status <- row[["status"]]
  if (is.character(status) && length(status) == 1L && !is.na(status)) {
    return(tolower(status))
  }
  ""
}

# Label, realized value, target or p-value, status: padded to common widths,
# with the reference column left out when no row has one.
.format_summary_lines <- function(rows) {
  if (nrow(rows) == 0L) {
    return(character())
  }
  pad_right <- function(x) formatC(x, width = max(nchar(x)), flag = "-")
  pad_left <- function(x) formatC(x, width = max(nchar(x)))
  columns <- list(pad_right(rows$label), pad_left(rows$value))
  if (any(nzchar(rows$reference))) {
    columns <- c(columns, list(pad_right(rows$reference)))
  }
  columns <- c(columns, list(rows$status))
  lines <- do.call(paste, c(columns, list(sep = "   ")))
  sub(" +$", "", paste0("  ", lines))
}

.report_row <- function(tab, diagnostic, basis) {
  rows <- tab[tab$diagnostic == diagnostic & tab$basis == basis, , drop = FALSE]
  if (nrow(rows) == 0L) {
    return(NULL)
  }
  rows[1L, , drop = FALSE]
}

.is_finite_number <- function(x) {
  is.numeric(x) && length(x) == 1L && is.finite(x)
}

.fmt_report_number <- function(x) {
  formatC(x, format = "f", digits = 3L)
}
