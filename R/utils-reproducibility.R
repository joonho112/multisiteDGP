# nolint start: object_name_linter, object_length_linter, cyclocomp_linter, object_usage_linter
#' Canonical numerical-content hash for reproducibility checks
#'
#' @encoding UTF-8
#'
#' @description
#' Compute a stable numerical-content hash of a multisiteDGP simulation
#' object. The hash is *canonical*: it normalizes column order, drops row
#' names, rounds doubles to nine significant digits, excludes derived
#' diagnostics by default, and replaces callback functions with presence
#' sentinels. Equal hashes mean equal canonical payloads under the named
#' schema; they do not prove byte-identical R objects or serialized files.
#'
#' @details
#' \strong{The hash is a numerical equivalence contract.} Seeded package
#' front doors pin the RNG algorithm, normal generator, and sample generator.
#' Supported-platform CI checks that representative designs produce the same
#' nine-significant-digit payload. Decimal rounding is discontinuous, so a
#' negligible difference at a rounding boundary can still move the hash, while
#' a genuine difference below the declared precision can be intentionally
#' treated as equivalent.
#'
#' \strong{The package version is not part of numerical content.} The producing
#' version remains in provenance. Changes to payload meaning advance
#' `hash_schema_version`; changes to exact serialized bytes belong in a
#' separate artifact SHA-256. See
#' `system.file("REPRODUCIBILITY.md", package = "multisiteDGP")` for the full
#' installed policy.
#'
#' \strong{Use cases.} (1) Save the hash alongside a published
#' simulation result so future readers can verify reproduction.
#' (2) Pin a regression test fixture so unintended pipeline changes are
#' caught. (3) Detect whether two parallel workers produced the same
#' output.
#'
#' For a worked reproducibility walkthrough see the
#' \href{../articles/m7-reproducibility-provenance.html}{Reproducibility
#' and provenance} vignette.
#'
#' @param x A `multisitedgp_data`, `multisitedgp_design`, data frame, or
#'   other R object.
#' @param algo Character. Hash algorithm passed to
#'   \code{\link[digest]{digest}}. Default `"xxhash64"` — fast, 16-hex
#'   output, suitable for typical reproducibility checks.
#' @param columns_to_include Optional character vector of columns to
#'   include for data-frame-like objects. Columns are sorted before
#'   hashing. Default `NULL` (all canonical columns).
#' @param diagnostics_to_include Optional character vector of diagnostic
#'   names to include. Default `NULL` (no derived diagnostics).
#'
#' @return A single algorithm-dependent character hash string.
#'
#' @family family-reproducibility
#' @seealso
#'   \code{\link{provenance_string}} for the human-readable one-line
#'   provenance summary;
#'   the \href{../articles/m7-reproducibility-provenance.html}{M7
#'   Reproducibility and provenance} vignette.
#' @references
#' Lee, J., Che, J., Rabe-Hesketh, S., Feller, A., & Miratrix, L. (2025).
#' Improving the estimation of site-specific effects and their distribution
#' in multisite trials. \emph{Journal of Educational and Behavioral Statistics},
#' \bold{50}(5), 731--764. \doi{10.3102/10769986241254286}.
#' @examples
#' dat <- sim_multisite(J = 10L, seed = 1L)
#' canonical_hash(dat)
#'
#' # Same seeded design and schema -> same canonical numerical payload.
#' identical(canonical_hash(dat), canonical_hash(sim_multisite(J = 10L, seed = 1L)))
#' @export
canonical_hash <- function(
  x,
  algo = "xxhash64",
  columns_to_include = NULL,
  diagnostics_to_include = NULL
) {
  algo <- .validate_scalar_string(algo, "algo")
  payload <- .canonical_hash_payload(
    x,
    columns_to_include = columns_to_include,
    diagnostics_to_include = diagnostics_to_include,
    algo = algo
  )
  digest::digest(payload, algo = algo)
}

#' One-line human-readable provenance string
#'
#' @encoding UTF-8
#'
#' @description
#' Format the key reproducibility fields of a multisiteDGP object as a
#' single pipe-delimited string suitable for reports, preregistration
#' appendices, paper appendices, and reviewer-facing figure captions.
#' Includes package version, paradigm, seed, canonical hash, design hash,
#' schema, producer runtime/platform/RNG, verifier runtime/platform, and
#' custom-callback hooks.
#'
#' @details
#' Output format:
#' \preformatted{multisiteDGP 0.2.0 | paradigm=site_size | seed=1 | canonical_hash=... | design_hash=... | hash_schema=... | producer_R=4.6.0 | producer_platform=... | producer_rng=Mersenne-Twister/Inversion/Rejection | verifier_R=4.6.0 | verifier_platform=... | hooks=none}
#'
#' Drop the result into a paper appendix or a vignette caption to make
#' your simulation cite-able and reproducible. For an end-to-end
#' reproducibility workflow see the
#' \href{../articles/m7-reproducibility-provenance.html}{Reproducibility
#' and provenance} vignette.
#'
#' @param x A `multisitedgp_data`, `multisitedgp_design`, or hashable
#'   object.
#' @param ... Passed to methods.
#'
#' @return A single character string.
#' @family family-reproducibility
#' @seealso
#'   \code{\link{canonical_hash}} for the underlying content hash;
#'   the \href{../articles/m7-reproducibility-provenance.html}{M7
#'   Reproducibility and provenance} vignette.
#' @references
#' Lee, J., Che, J., Rabe-Hesketh, S., Feller, A., & Miratrix, L. (2025).
#' Improving the estimation of site-specific effects and their distribution
#' in multisite trials. \emph{Journal of Educational and Behavioral Statistics},
#' \bold{50}(5), 731--764. \doi{10.3102/10769986241254286}.
#' @examples
#' dat <- sim_multisite(J = 10L, seed = 1L)
#' provenance_string(dat)
#' @export
provenance_string <- function(x, ...) {
  UseMethod("provenance_string")
}

#' @export
provenance_string.multisitedgp_data <- function(x, ...) {
  provenance <- attr(x, "provenance", exact = TRUE)
  design <- attr(x, "design", exact = TRUE)
  .warn_provenance_runtime_mismatch(provenance)
  parts <- c(
    sprintf("multisiteDGP %s", .provenance_version(x, provenance)),
    sprintf("paradigm=%s", .provenance_field(provenance$paradigm, attr(x, "paradigm", exact = TRUE))),
    if (!is.null(provenance$preset)) sprintf("preset=%s", provenance$preset),
    sprintf("seed=%s", .format_provenance_value(provenance$seed)),
    sprintf("canonical_hash=%s", canonical_hash(x)),
    sprintf("design_hash=%s", .provenance_field(provenance$design_hash, .canonical_design_hash(design, .provenance_hash_algo(provenance)))),
    sprintf("hash_algo=%s", .provenance_hash_algo(provenance)),
    sprintf("hash_schema=%s", .provenance_field(provenance$hash_schema_version, .hash_schema_version())),
    sprintf("producer_R=%s", .short_stored_r_version(provenance$R_version)),
    sprintf("producer_platform=%s", .provenance_field(provenance$platform)),
    sprintf("producer_rng=%s", .format_rng_kind(provenance$rng_kind)),
    sprintf("verifier_R=%s", .short_r_version()),
    sprintf("verifier_platform=%s", R.version$platform),
    sprintf("hooks=%s", .format_hooks(.provenance_hooks(provenance, design)))
  )
  paste(parts, collapse = " | ")
}

#' @export
provenance_string.multisitedgp_design <- function(x, ...) {
  preset_name <- attr(x, "preset_name", exact = TRUE)
  parts <- c(
    sprintf("multisiteDGP %s", .multisitedgp_version()),
    "object=multisitedgp_design",
    sprintf("paradigm=%s", x$paradigm),
    if (!is.null(preset_name)) sprintf("preset=%s", preset_name),
    sprintf("engine=%s", x$engine),
    sprintf("seed=%s", .format_provenance_value(x$seed)),
    sprintf("design_hash=%s", canonical_hash(x)),
    sprintf("hash_algo=%s", "xxhash64"),
    sprintf("hash_schema=%s", .hash_schema_version()),
    sprintf("R=%s", .short_r_version()),
    sprintf("rng=%s", .format_rng_kind(.provenance_rng_kind(x$seed))),
    sprintf("hooks=%s", .format_hooks(.custom_hook_names(x)))
  )
  paste(parts, collapse = " | ")
}

#' @export
provenance_string.default <- function(x, ...) {
  parts <- c(
    sprintf("multisiteDGP %s", .multisitedgp_version()),
    sprintf("object=%s", .object_hash_type(x)),
    sprintf("canonical_hash=%s", canonical_hash(x)),
    sprintf("hash_algo=%s", "xxhash64"),
    sprintf("R=%s", .short_r_version())
  )
  paste(parts, collapse = " | ")
}

.canonical_hash_payload <- function(
  x,
  columns_to_include = NULL,
  diagnostics_to_include = NULL,
  algo = "xxhash64"
) {
  if (is_multisitedgp_design(x)) {
    return(list(
      hash_schema_version = .hash_schema_version(),
      object_type = "multisitedgp_design",
      design = .canonicalize_for_hash(x)
    ))
  }

  if (inherits(x, "data.frame")) {
    df <- as.data.frame(x)
    cols <- .select_hash_columns(df, columns_to_include)
    data_canonical <- lapply(df[cols], .canonicalize_for_hash)
    return(list(
      hash_schema_version = .hash_schema_version(),
      object_type = "data_frame_like",
      columns_sorted = cols,
      data_canonical = data_canonical,
      diagnostics_numeric = .canonicalize_diagnostics(
        attr(x, "diagnostics", exact = TRUE),
        diagnostics_to_include
      ),
      manifest = .hash_manifest(x, algo)
    ))
  }

  list(
    hash_schema_version = .hash_schema_version(),
    object_type = .object_hash_type(x),
    object = .canonicalize_for_hash(x),
    diagnostics_numeric = .canonicalize_diagnostics(
      attr(x, "diagnostics", exact = TRUE),
      diagnostics_to_include
    ),
    manifest = .hash_manifest(x, algo)
  )
}

# The manifest deliberately omits the package version. A hash is quoted in a
# manuscript or an issue as an anchor for "this data, this design", and a version
# nothing about the data moved. v0.1.x papered over that with a hardcoded lineage
# bucket that collapsed 0.0.0.9000 and 0.1.* to one string; it had no rule for
# 0.2 and the bump moved every hash. Provenance is already carried by
# `design_hash` and `hash_schema_version` — the schema version is what moves when
# the package changes what the hash means, and that is the thing worth pinning.
# The producing version is still recorded on the object's provenance attribute
# and printed; it is just not hashed.
.hash_manifest <- function(x, algo) {
  list(
    paradigm = .attr_as_character(x, "paradigm"),
    design_hash = .canonical_design_hash(attr(x, "design", exact = TRUE), algo),
    hash_schema_version = .hash_schema_version(),
    function_exclusion_policy = .function_exclusion_policy(),
    custom_hooks = .custom_hook_names(attr(x, "design", exact = TRUE))
  )
}

.canonical_design_hash <- function(design, algo = "xxhash64") {
  if (is.null(design)) {
    return(NULL)
  }
  digest::digest(list(
    hash_schema_version = .hash_schema_version(),
    object_type = "multisitedgp_design",
    design = .canonicalize_for_hash(design)
  ), algo = algo)
}

.new_multisitedgp_provenance <- function(
  design = NULL,
  seed = NULL,
  algo = "xxhash64",
  canonical_hash = NULL,
  call = NULL,
  preset = NULL
) {
  if (is.null(seed) && !is.null(design) && !is.null(design$seed)) {
    seed <- design$seed
  }
  preset_name <- if (is.null(preset)) {
    attr(design, "preset_name", exact = TRUE)
  } else {
    preset
  }
  list(
    seed = seed,
    multisitedgp_version = .multisitedgp_version(),
    R_version = R.version.string,
    platform = R.version$platform,
    rng_kind = .provenance_rng_kind(seed),
    rng_policy = if (is.null(seed)) "caller-controlled" else "package-pinned",
    canonical_hash = canonical_hash,
    design_hash = .canonical_design_hash(design, algo),
    hash_algo = algo,
    hash_schema_version = .hash_schema_version(),
    paradigm = if (!is.null(design)) design$paradigm else NULL,
    preset = preset_name,
    preset_metadata = attr(design, "preset_metadata", exact = TRUE),
    call = call,
    function_exclusion_policy = .function_exclusion_policy(),
    custom_hooks = .custom_hook_names(design)
  )
}

.canonicalize_for_hash <- function(x, path = character()) {
  if (is.null(x)) {
    return(list(kind = "NULL"))
  }
  if (is.function(x)) {
    return(list(
      kind = "function_excluded",
      hook = .path_label(path),
      present = TRUE
    ))
  }
  if (inherits(x, "formula")) {
    return(list(
      kind = "formula",
      expression = paste(deparse(x), collapse = "\n")
    ))
  }
  if (is.factor(x)) {
    return(list(
      kind = if (is.ordered(x)) "ordered_factor" else "factor",
      levels = enc2utf8(levels(x)),
      values = enc2utf8(as.character(x))
    ))
  }
  if (is.environment(x)) {
    return(list(
      kind = "environment_excluded",
      hook = .path_label(path),
      present = TRUE
    ))
  }
  if (is.data.frame(x)) {
    cols <- .stable_sort_character(names(x))
    return(list(
      kind = "data.frame",
      columns_sorted = cols,
      data = lapply(x[cols], .canonicalize_for_hash)
    ))
  }
  if (is.pairlist(x)) {
    x <- as.list(x)
  }
  if (is.list(x)) {
    x <- unclass(x)
    nm <- names(x)
    if (!is.null(nm) && all(nzchar(nm))) {
      x <- x[.stable_sort_character(nm)]
    }
    out <- vector("list", length(x))
    names(out) <- names(x)
    for (idx in seq_along(x)) {
      child <- names(x)[[idx]]
      if (is.null(child) || identical(child, "")) {
        child <- as.character(idx)
      }
      out[[idx]] <- .canonicalize_for_hash(x[[idx]], c(path, child))
    }
    return(out)
  }
  if (is.language(x)) {
    return(list(
      kind = "language",
      expression = paste(deparse(x), collapse = "\n")
    ))
  }
  if (is.atomic(x)) {
    out <- unname(x)
    attributes(out) <- NULL
    if (is.double(out)) {
      out <- signif(out, .HASH_SIGNIF_DIGITS)
    }
    return(out)
  }
  unname(x)
}

.select_hash_columns <- function(x, columns_to_include = NULL) {
  if (is.null(columns_to_include)) {
    return(.stable_sort_character(names(x)))
  }
  columns_to_include <- .validate_character_vector(columns_to_include, "columns_to_include")
  missing_columns <- setdiff(columns_to_include, names(x))
  if (length(missing_columns) > 0L) {
    .abort_arg(
      "`columns_to_include` contains unknown columns.",
      sprintf("Missing columns: %s.", paste(missing_columns, collapse = ", ")),
      "Use column names that are present in `x`."
    )
  }
  .stable_sort_character(columns_to_include)
}

.canonicalize_diagnostics <- function(diagnostics, diagnostics_to_include = NULL) {
  if (is.null(diagnostics) || !is.list(diagnostics)) {
    return(NULL)
  }
  keys <- if (is.null(diagnostics_to_include)) {
    .canonical_diagnostics_allowlist()
  } else {
    .validate_character_vector(diagnostics_to_include, "diagnostics_to_include")
  }
  keys <- .stable_sort_character(unique(keys))
  selected <- diagnostics[intersect(keys, names(diagnostics))]
  numeric_scalar <- vapply(selected, function(x) {
    is.numeric(x) && length(x) == 1L && is.finite(x)
  }, logical(1))
  lapply(selected[numeric_scalar], .canonicalize_for_hash)
}

# Empty since schema v3 and retained in v4. The diagnostics these named are derived from the
# hashed data and the hashed design, so they added no provenance — only the
# platform-dependent accumulation noise of `cor()` and `sd()`.
#
# `canonical_hash(x, diagnostics_to_include = ...)` still accepts an explicit
# set for callers who want a diagnostic pinned deliberately; they just are not
# part of the default contract any more.
.canonical_diagnostics_allowlist <- function() {
  character()
}

.custom_hook_names <- function(design) {
  if (is.null(design)) {
    return(character())
  }
  hooks <- character()
  if (is.function(design$g_fn)) {
    hooks <- c(hooks, "g_fn")
  }
  if (is.function(design$se_fn)) {
    hooks <- c(hooks, "se_fn")
  }
  if (is.list(design$dependence_spec) && is.function(design$dependence_spec$dependence_fn)) {
    hooks <- c(hooks, "dependence_fn")
  }
  if (is.list(design$obs_spec) && is.function(design$obs_spec$obs_fn)) {
    hooks <- c(hooks, "obs_fn")
  }
  unique(hooks)
}

.function_exclusion_policy <- function() {
  "callback bodies, bytecode, and environments are excluded; presence sentinels are hashed"
}

# Hash schema v4 — canonical numerical content, not byte identity.
#
# v1 hashed raw IEEE-754 doubles including a set of derived diagnostics, and a
# single ULP flipped the hash. That made the cross-platform contract
# unachievable, but not for the reason it looked like: the *data* is platform
# not generally bit-portable, and the diagnostics were also unstable.
#
#   data columns  `n_j` is rounded to an integer; seeded front doors pin R's
#                 RNG triple. Other numerical columns can still drift below
#                 nine significant digits on supported platforms.
#   diagnostics   `I_hat`, `R_hat`, `rho_*` and `sigma_tau_*` are computed with
#                 `cor()`, `sd()` and `mean()`, whose accumulation order varies
#                 by platform. The gap shows even on one machine, where
#                 `rho_P_marginal` and `rho_P_residual` are mathematically
#                 identical for a covariate-free design yet differ in their
#                 last digits.
#
# v2 papered over this by rounding every hashed double to nine significant
# digits. That worked, but the nine was arbitrary and it blunted the hash
# against genuine regressions below 1e-9.
#
# v3 did both. It dropped the diagnostics, which are computed from the data and
# from a design already covered by `design_hash`, so they added no provenance —
# only noise. And it keeps the rounding for what remains.
#
# The rounding is still needed. An earlier v3 attempt dropped the diagnostics
# and hashed the rest exactly after a limited fixture set appeared byte-stable.
# Broader CI disproved the generalization: print examples and snapshot presets
# still drifted below 1e-9.
#
# v4 retains the nine-digit content policy, canonicalizes factor labels and
# orderedness instead of their internal integer codes, and applies the same
# rounding rule to explicitly requested diagnostics. The schema advance also
# records the pinned RNG triple in run provenance. Equal v4 hashes identify
# equal canonical payloads only; exact artifact identity is a separate SHA-256.
.HASH_SIGNIF_DIGITS <- 9L

.hash_schema_version <- function() {
  "multisiteDGP-canonical-hash-v4"
}

.object_hash_type <- function(x) {
  cls <- class(x)
  if (length(cls) == 0L) {
    return(typeof(x))
  }
  cls[[1]]
}

.attr_as_character <- function(x, which) {
  value <- attr(x, which, exact = TRUE)
  if (is.null(value)) {
    return(NULL)
  }
  as.character(value)
}

.path_label <- function(path) {
  if (length(path) == 0L) {
    return("<root>")
  }
  paste(path, collapse = ".")
}

.validate_scalar_string <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    .abort_arg(
      sprintf("`%s` must be a single non-empty string.", arg),
      sprintf("`%s` controls canonical hash serialization.", arg),
      sprintf("Use `%s = \"xxhash64\"` unless a different digest algorithm is required.", arg)
    )
  }
  x
}

.validate_character_vector <- function(x, arg) {
  if (!is.character(x) || anyNA(x) || any(!nzchar(x))) {
    .abort_arg(
      sprintf("`%s` must be a character vector with non-empty names.", arg),
      sprintf("`%s` selects fields for canonical hashing.", arg),
      sprintf("Use `%s = c(\"site_index\", \"tau_j_hat\")` or leave it `NULL`.", arg)
    )
  }
  x
}

.provenance_version <- function(x, provenance) {
  .provenance_field(
    provenance$multisitedgp_version,
    attr(x, "multisitedgp_version", exact = TRUE),
    .multisitedgp_version()
  )
}

.provenance_hash_algo <- function(provenance) {
  .provenance_field(provenance$hash_algo, "xxhash64")
}

.provenance_hooks <- function(provenance, design) {
  hooks <- provenance$custom_hooks
  if (!is.null(hooks)) {
    return(hooks)
  }
  .custom_hook_names(design)
}

.warn_provenance_runtime_mismatch <- function(provenance) {
  if (!is.list(provenance)) {
    return(invisible(FALSE))
  }
  mismatch <- character()
  if (!is.null(provenance$R_version) &&
      !identical(as.character(provenance$R_version), R.version.string)) {
    mismatch <- c(mismatch, sprintf(
      "producer R `%s` differs from verifier R `%s`",
      .short_stored_r_version(provenance$R_version),
      .short_r_version()
    ))
  }
  if (!is.null(provenance$platform) &&
      !identical(as.character(provenance$platform), R.version$platform)) {
    mismatch <- c(mismatch, sprintf(
      "producer platform `%s` differs from verifier platform `%s`",
      provenance$platform,
      R.version$platform
    ))
  }
  if (length(mismatch) == 0L) {
    return(invisible(FALSE))
  }
  cli::cli_warn(c(
    "!" = "Stored producer runtime differs from the current verifier runtime.",
    "i" = paste(mismatch, collapse = "; "),
    ">" = "Use the stored producer fields for replay and the verifier fields for the current check."
  ))
  invisible(TRUE)
}

.provenance_field <- function(...) {
  values <- list(...)
  for (value in values) {
    if (!is.null(value) && length(value) > 0L && !all(is.na(value))) {
      return(as.character(value[[1L]]))
    }
  }
  "NA"
}

.format_provenance_value <- function(x) {
  if (is.null(x) || length(x) == 0L || all(is.na(x))) {
    return("NULL")
  }
  as.character(x[[1L]])
}

.format_hooks <- function(hooks) {
  if (is.null(hooks) || length(hooks) == 0L) {
    return("none")
  }
  paste(.stable_sort_character(unique(hooks)), collapse = ",")
}

.format_rng_kind <- function(x) {
  if (is.null(x) || length(x) == 0L || all(is.na(x))) {
    return("NA")
  }
  paste(as.character(x), collapse = "/")
}

.stable_sort_character <- function(x) {
  sort(x, method = "radix")
}

.short_r_version <- function() {
  paste(R.version$major, R.version$minor, sep = ".")
}

.short_stored_r_version <- function(x) {
  if (is.null(x) || length(x) == 0L || all(is.na(x))) {
    return("NA")
  }
  value <- as.character(x[[1L]])
  sub("^R version ([0-9]+[.][0-9]+[.][0-9]+).*$", "\\1", value)
}
# nolint end
