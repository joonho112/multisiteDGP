# nolint start: object_length_linter, object_name_linter, object_usage_linter
#' Test for multisite simulation data objects
#'
#' @encoding UTF-8
#'
#' @description
#' Predicate test for the `multisitedgp_data` S3 class — `TRUE` for
#' objects produced by \code{\link{sim_multisite}} or
#' \code{\link{sim_meta}}, `FALSE` otherwise.
#'
#' @param x Object to test.
#' @return A single logical value.
#' @family family-design
#' @examples
#' dat <- sim_multisite(J = 10L, seed = 1L)
#' is_multisitedgp_data(dat)        # TRUE
#' is_multisitedgp_data(iris)       # FALSE
#' @export
is_multisitedgp_data <- function(x) {
  inherits(x, "multisitedgp_data")
}

#' Coerce multisite simulation data to a plain tibble
#'
#' @encoding UTF-8
#'
#' @description
#' Strip the `multisitedgp_data` class and attributes (design,
#' diagnostics, provenance) and return a plain `tbl_df`. Useful when
#' handing off to a downstream tool that should not see the
#' multisiteDGP attributes.
#'
#' @param x A `multisitedgp_data` object.
#' @param i,j Row and column indices passed to base subsetting.
#' @param ... Passed to \code{\link[tibble]{as_tibble}}.
#' @param drop Included for base subsetting compatibility; multisiteDGP
#'   data objects use tibble-style non-dropping behavior.
#' @param .preserve_class Logical. If `TRUE`, return `x` unchanged
#'   (preserves the class and attributes). Default `FALSE`.
#'
#' @return A plain `tbl_df` (with class and attributes stripped) unless
#'   `.preserve_class = TRUE`, in which case `x` is returned unchanged.
#' @importFrom tibble as_tibble
#' @family family-design
#' @seealso \code{\link{adapters}} for downstream package-specific
#'   coercions (`as_metafor`, `as_baggr`, `as_multisitepower`).
#' @examples
#' dat <- sim_multisite(J = 10L, seed = 1L)
#' plain <- tibble::as_tibble(dat)
#' attributes(plain)   # only standard tibble attributes
#' @export
as_tibble.multisitedgp_data <- function(x, ..., .preserve_class = FALSE) {
  .preserve_class <- .validate_scalar_logical(.preserve_class, ".preserve_class")
  if (.preserve_class) {
    return(x)
  }
  .strip_multisitedgp_data(x, ...)
}

#' @describeIn as_tibble.multisitedgp_data Subset data while preserving class
#'   only when canonical columns remain; row subsets drop stale diagnostics.
#' @export
"[.multisitedgp_data" <- function(x, i, j, ..., drop = FALSE) {
  out <- NextMethod()
  .restore_multisitedgp_data_subset(x, out, row_subset = !missing(i))
}

#' @rawNamespace S3method(dplyr::dplyr_row_slice,multisitedgp_data)
dplyr_row_slice.multisitedgp_data <- function(data, i, ...) {
  out <- NextMethod()
  .restore_multisitedgp_data_subset(data, out, row_subset = TRUE)
}

#' Print method for multisite simulation data
#'
#' @encoding UTF-8
#'
#' @description
#' Print a `multisitedgp_data` object with a four-axis "realized vs.
#' intended" header summarizing the most-relevant diagnostics, followed
#' by the underlying tibble. The header lists, for each canonical
#' diagnostic axis (informativeness `I`, heterogeneity ratio `R`,
#' \eqn{\sigma_\tau}, dependence \eqn{\rho_S}), the realized value and
#' the design target (or "no target" when the design did not constrain
#' that axis), plus a per-axis pass / warn / fail flag.
#'
#' @details
#' The header is a one-glance summary — when something looks off, run
#' \code{summary(x)} for the full diagnostic report including all
#' Group A/B/C/D scalars, the realized residual / marginal Spearman /
#' Pearson correlations, and the feasibility status.
#'
#' @param x A `multisitedgp_data` object.
#' @param n Integer. Number of rows to show from the underlying tibble.
#'   Default `6L`.
#' @param ... Passed to tibble printing.
#' @return Invisibly returns `x`.
#' @family family-design
#' @seealso \code{\link{summary.multisitedgp_data}} for the full
#'   diagnostic report.
#' @examples
#' dat <- sim_multisite(J = 10L, seed = 1L)
#' print(dat, n = 3)
#' @export
print.multisitedgp_data <- function(x, n = 6L, ...) {
  n <- .validate_print_n(n)
  cat(paste(.format_multisitedgp_print_header(x), collapse = "\n"), "\n", sep = "")
  print(.strip_multisitedgp_data(x), n = n, ...)
  cat("# Use summary(df) for the full diagnostic report.\n")
  invisible(x)
}

#' Summary method for multisite simulation diagnostics
#'
#' @encoding UTF-8
#'
#' @description
#' Print the full diagnostic report for a `multisitedgp_data` object —
#' the four-axis "realized vs. intended" header expanded to include all
#' Group A (precision and feasibility), Group B (realized dependence on
#' residual and marginal scales), Group C (distributional fit), and
#' Group D (shrinkage downstream) diagnostics, plus the threshold-rubric
#' pass / warn / fail status for each.
#'
#' @details
#' Use `summary()` when you need to read every diagnostic at once —
#' typically before a long simulation run, when calibrating a design,
#' or when investigating why a simulation under-performs vs. the design
#' target. Use \code{print()} for a fast at-a-glance check.
#'
#' @param object A `multisitedgp_data` object.
#' @param ... Reserved for future extensions.
#' @return Invisibly returns `object`.
#' @family family-design
#' @seealso \code{\link{print.multisitedgp_data}} for the at-a-glance
#'   header; \code{\link{compute_I}}, \code{\link{realized_rank_corr}},
#'   \code{\link{bhattacharyya_coef}}, \code{\link{ks_distance}} for
#'   the underlying scalar diagnostics.
#' @examples
#' dat <- sim_multisite(J = 10L, seed = 1L)
#' summary(dat)
#' @export
summary.multisitedgp_data <- function(object, ...) {
  cat(paste(.format_multisitedgp_summary(object), collapse = "\n"), "\n", sep = "")
  invisible(object)
}

.new_multisitedgp_data <- function(x, design = NULL, diagnostics = list(), meta = FALSE) {
  out <- .as_plain_tibble(x)
  .validate_multisitedgp_data_schema(out)
  if (!is.null(design)) {
    validate_multisitedgp_design(design)
    expected_paradigm <- if (isTRUE(meta)) "direct" else "site_size"
    if (!identical(design$paradigm, expected_paradigm)) {
      .abort_arg(
        "Design paradigm does not match the requested data class.",
        sprintf(
          "`%s` objects require `design$paradigm == \"%s\"`.",
          if (isTRUE(meta)) "multisitedgp_meta" else "multisitedgp_data",
          expected_paradigm
        ),
        "Use the matching constructor for the design paradigm."
      )
    }
  }
  diagnostics <- .validate_list(diagnostics, "diagnostics")
  paradigm <- if (is.null(design)) {
    if (isTRUE(meta)) "direct" else "site_size"
  } else {
    design$paradigm
  }

  attr(out, "design") <- design
  attr(out, "diagnostics") <- diagnostics
  attr(out, "multisitedgp_version") <- .multisitedgp_version()
  attr(out, "paradigm") <- paradigm
  attr(out, "provenance") <- .new_multisitedgp_provenance(design = design)
  class(out) <- .data_class_vector(meta)
  out
}

.new_multisitedgp_meta <- function(x, design = NULL, diagnostics = list()) {
  .new_multisitedgp_data(x, design = design, diagnostics = diagnostics, meta = TRUE)
}

.canonical_data_columns <- function() {
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
  if (!all(.canonical_data_columns() %in% names(out))) {
    return(.strip_multisitedgp_data(out))
  }

  out <- .as_plain_tibble(out)
  attr(out, "design") <- attr(x, "design", exact = TRUE)
  attr(out, "multisitedgp_version") <- attr(x, "multisitedgp_version", exact = TRUE)
  attr(out, "paradigm") <- attr(x, "paradigm", exact = TRUE)
  attr(out, "diagnostics") <- attr(x, "diagnostics", exact = TRUE)
  attr(out, "provenance") <- attr(x, "provenance", exact = TRUE)

  if (isTRUE(row_subset)) {
    attr(out, "diagnostics") <- NULL
    provenance <- attr(out, "provenance", exact = TRUE)
    if (is.list(provenance)) {
      provenance$canonical_hash <- NULL
      provenance$row_subset <- TRUE
      attr(out, "provenance") <- provenance
    }
    cli::cli_warn(c(
      "!" = "Diagnostics dropped after row subset.",
      "i" = "Original diagnostics may no longer describe the subset rows.",
      ">" = "Recompute diagnostics before reporting subset summaries."
    ))
  }

  class(out) <- .data_class_vector(.is_meta_class(x))
  out
}

.validate_multisitedgp_data_schema <- function(x) {
  missing_columns <- setdiff(.canonical_data_columns(), names(x))
  if (length(missing_columns) > 0L) {
    .abort_arg(
      "Data is missing required multisiteDGP columns.",
      sprintf("Missing columns: %s.", paste(missing_columns, collapse = ", ")),
      "Use the canonical columns before constructing `multisitedgp_data`."
    )
  }
  if (!is.integer(x$site_index)) {
    .abort_arg(
      "`site_index` must be integer.",
      "`site_index` is the canonical site identifier.",
      "Use `site_index = seq_len(J)`."
    )
  }
  numeric_columns <- c("z_j", "tau_j", "tau_j_hat", "se_j", "se2_j")
  non_numeric <- numeric_columns[!vapply(x[numeric_columns], is.numeric, logical(1))]
  if (length(non_numeric) > 0L) {
    .abort_arg(
      "Canonical effect and precision columns must be numeric.",
      sprintf("Non-numeric columns: %s.", paste(non_numeric, collapse = ", ")),
      "Use numeric vectors for `z_j`, `tau_j`, `tau_j_hat`, `se_j`, and `se2_j`."
    )
  }
  if (!is.integer(x$n_j)) {
    .abort_arg(
      "`n_j` must be integer.",
      "`n_j` stores site sizes, or `NA_integer_` for direct meta designs.",
      "Use integer site sizes or `rep(NA_integer_, J)`."
    )
  }
  invisible(x)
}

.multisitedgp_version <- function() {
  tryCatch(
    as.character(utils::packageVersion("multisiteDGP")),
    error = function(e) NA_character_
  )
}

.validate_print_n <- function(n) {
  n <- .validate_scalar_integer(n, "n")
  if (n < 1L) {
    .abort_arg(
      "`n` must be at least 1 for printing.",
      "`n` controls the number of data rows shown below the diagnostics header.",
      "Use `n = 6L` or another positive integer."
    )
  }
  n
}

.format_multisitedgp_print_header <- function(x) {
  tab <- .diagnostics_table_for_reporting(x)
  paradigm <- attr(x, "paradigm", exact = TRUE)
  paradigm <- if (is.null(paradigm)) "unknown" else paradigm
  lines <- c(
    sprintf("# A %s: %s sites, paradigm = \"%s\"", class(x)[[1L]], nrow(x), paradigm),
    "# Realized vs intended:"
  )
  if (is.null(tab)) {
    return(c(lines, "#   Diagnostics unavailable for this object."))
  }
  c(
    lines,
    paste0("#   ", .format_headline_row(tab, "informativeness", "overall", "I")),
    paste0("#   ", .format_headline_row(tab, "heterogeneity_ratio", "overall", "R")),
    paste0("#   ", .format_headline_row(tab, "sigma_tau", "residual", "sigma_tau")),
    paste0("#   ", .format_headline_row(tab, "realized_rank_corr", "residual", "rho_S")),
    paste0("#   ", .format_headline_row(tab, "realized_rank_corr", "marginal", "rho_S_marg")),
    paste0("#   ", .format_feasibility_headline(tab))
  )
}

.format_multisitedgp_summary <- function(x) {
  tab <- .diagnostics_table_for_reporting(x)
  title <- .format_summary_title(x)
  if (is.null(tab)) {
    return(c(
      title,
      "------------------------------------------------------------",
      "Diagnostics unavailable for this object.",
      paste0("Provenance: ", provenance_string(x))
    ))
  }

  report <- .summary_report_rows(tab)
  counts <- .status_counts(report)
  c(
    title,
    "------------------------------------------------------------",
    "A. Realized vs Intended",
    .format_summary_row(report, "informativeness", "overall", "   I (informativeness)"),
    .format_summary_row(report, "heterogeneity_ratio", "overall", "   R (SE heterogeneity)"),
    .format_summary_row(report, "sigma_tau", "residual", "   sigma_tau"),
    .format_summary_row(report, "GM_se2", "overall", "   GM(se^2)"),
    "",
    "B. Dependence",
    .format_summary_row(report, "realized_rank_corr", "residual", "   rank_corr residual"),
    .format_summary_row(report, "realized_rank_corr", "marginal", "   rank_corr marginal"),
    .format_summary_row(report, "realized_pearson_corr", "residual", "   pearson_corr residual"),
    .format_summary_row(report, "realized_pearson_corr", "marginal", "   pearson_corr marginal"),
    "",
    "C. G shape fit",
    .format_summary_row(report, "ks_distance", "target_G", "   KS distance D_J"),
    .format_summary_row(report, "bhattacharyya_coef", "target_G", "   Bhattacharyya BC"),
    .format_summary_row(report, "qq_residuals", "target_G", "   Q-Q residual"),
    "",
    "D. Operational feasibility",
    .format_summary_row(report, "mean_shrinkage", "overall", "   mean shrinkage S"),
    .format_summary_row(report, "average_moe", "overall", "   avg MOE (95%)"),
    .format_summary_row(report, "feasibility_index", "efron", "   feasibility_index"),
    "------------------------------------------------------------",
    sprintf("Overall: %s PASS, %s WARN, %s FAIL.", counts[["PASS"]], counts[["WARN"]], counts[["FAIL"]]),
    paste0("Provenance: ", provenance_string(x))
  )
}

.format_summary_title <- function(x) {
  title <- "multisiteDGP simulation diagnostics"
  provenance <- attr(x, "provenance", exact = TRUE)
  preset <- if (is.list(provenance)) provenance$preset else NULL
  if (is.null(preset) || length(preset) != 1L || is.na(preset) || !nzchar(preset)) {
    return(title)
  }
  sprintf("%s (%s)", title, preset)
}

.diagnostics_table_for_reporting <- function(x) {
  diagnostics <- attr(x, "diagnostics", exact = TRUE)
  tab <- diagnostics$target_vs_realized
  if (!is.null(tab)) {
    return(tab)
  }
  tryCatch(
    .apply_diagnostic_thresholds(.compute_diagnostics_table(x), x = x),
    error = function(e) NULL
  )
}

.format_headline_row <- function(tab, diagnostic, basis, label) {
  row <- .report_row(tab, diagnostic, basis)
  if (is.null(row)) {
    return(sprintf("%s: unavailable", label))
  }
  target <- row$target[[1L]]
  status <- row$status[[1L]]
  if (is.null(target) || length(target) == 0L || is.na(target) || !is.finite(target)) {
    if (is.null(status) || length(status) == 0L || is.na(status)) {
      return(sprintf(
        "%s: realized=%s (no target)",
        label,
        .fmt_report_number(row$realized[[1L]])
      ))
    }
    return(sprintf(
      "%s: realized=%s, %s (no target)",
      label,
      .fmt_report_number(row$realized[[1L]]),
      .fmt_report_status(status)
    ))
  }
  sprintf(
    "%s: target=%s, realized=%s, %s",
    label,
    .fmt_report_number(target),
    .fmt_report_number(row$realized[[1L]]),
    .fmt_report_status(status)
  )
}

.format_feasibility_headline <- function(tab) {
  row <- .report_row(tab, "feasibility_index", "efron")
  if (is.null(row)) {
    return("Feasibility: unavailable")
  }
  sprintf("Feasibility: %s (n_eff=%s)", .fmt_report_status(row$status[[1L]]), .fmt_report_number(row$realized[[1L]]))
}

.summary_report_rows <- function(tab) {
  wanted <- list(
    c("informativeness", "overall"),
    c("heterogeneity_ratio", "overall"),
    c("sigma_tau", "residual"),
    c("GM_se2", "overall"),
    c("realized_rank_corr", "residual"),
    c("realized_rank_corr", "marginal"),
    c("realized_pearson_corr", "residual"),
    c("realized_pearson_corr", "marginal"),
    c("ks_distance", "target_G"),
    c("bhattacharyya_coef", "target_G"),
    c("qq_residuals", "target_G"),
    c("mean_shrinkage", "overall"),
    c("average_moe", "overall"),
    c("feasibility_index", "efron")
  )
  rows <- lapply(wanted, function(key) .report_row(tab, key[[1L]], key[[2L]]))
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0L) {
    return(tab[0, , drop = FALSE])
  }
  dplyr::bind_rows(rows)
}

.format_summary_row <- function(tab, diagnostic, basis, label) {
  row <- .report_row(tab, diagnostic, basis)
  if (is.null(row)) {
    return(sprintf("%s: unavailable", label))
  }
  extra <- .format_summary_extra(row)
  sprintf(
    "%-28s %8s  (target %s)  %-4s  %s",
    paste0(label, ":"),
    .fmt_report_number(row$realized[[1L]]),
    .fmt_report_number(row$target[[1L]]),
    .fmt_report_status(row$status[[1L]]),
    extra
  )
}

.format_summary_extra <- function(row) {
  if (identical(row$diagnostic[[1L]], "ks_distance") && is.finite(row$p_value[[1L]])) {
    return(sprintf("[p=%s]", .fmt_report_number(row$p_value[[1L]])))
  }
  if (is.finite(row$rel_delta[[1L]])) {
    return(sprintf("[rel=%s%%]", .fmt_report_number(100 * row$rel_delta[[1L]], digits = 1L)))
  }
  if (is.finite(row$delta[[1L]])) {
    return(sprintf("[delta=%s]", .fmt_report_number(row$delta[[1L]])))
  }
  note <- row$threshold_note[[1L]]
  if (!is.null(note) && !is.na(note) && nzchar(note)) {
    return(sprintf("[%s]", note))
  }
  "[no target]"
}

.report_row <- function(tab, diagnostic, basis) {
  rows <- tab[tab$diagnostic == diagnostic & tab$basis == basis, , drop = FALSE]
  if (nrow(rows) == 0L) {
    return(NULL)
  }
  rows[1L, , drop = FALSE]
}

.status_counts <- function(tab) {
  statuses <- tab$status
  statuses <- statuses[!is.na(statuses)]
  c(
    PASS = sum(statuses == "PASS"),
    WARN = sum(statuses == "WARN"),
    FAIL = sum(statuses == "FAIL")
  )
}

.fmt_report_status <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x)) {
    return("N/A")
  }
  x
}

.fmt_report_number <- function(x, digits = 3L) {
  if (is.null(x) || length(x) == 0L || is.na(x) || !is.finite(x)) {
    return("N/A")
  }
  formatC(x, format = "f", digits = digits)
}
# nolint end
