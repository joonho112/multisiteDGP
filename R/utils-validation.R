# Every error raised by the package goes through `.abort()`. `message` says
# what is wrong. `info` gives the value received or the rule, and `fix` gives a
# remedy that `message` does not make obvious. Braces are doubled so that cli
# prints the text as written, and `call = NULL` keeps this helper out of the
# error header.
.abort <- function(message, info = NULL, fix = NULL, class = NULL) {
  bullets <- c("x" = message, "i" = info, ">" = fix)
  bullets <- gsub("}", "}}", gsub("{", "{{", bullets, fixed = TRUE), fixed = TRUE)
  cli::cli_abort(
    bullets,
    class = c(class, "multisitedgp_error"),
    call = NULL
  )
}

# For a numerical procedure that could not reach its target.
.abort_solver <- function(message, info = NULL, fix = NULL) {
  .abort(message, info, fix, class = "multisitedgp_solver_error")
}

# A short description of a value, for the line of an error that says what was
# received.
.describe_value <- function(x) {
  if (is.null(x)) {
    return("NULL")
  }
  if (is.function(x)) {
    return("a function")
  }
  if (is.data.frame(x)) {
    return("a data frame")
  }
  if (is.factor(x)) {
    return(sprintf("a factor of length %d", length(x)))
  }
  if (is.atomic(x) && is.null(dim(x))) {
    return(.describe_vector(x))
  }
  if (is.list(x) && !is.object(x)) {
    return(sprintf("a list of length %d", length(x)))
  }
  sprintf("an object of class %s", paste(class(x), collapse = "/"))
}

# A single value is shown itself, and a longer vector by its type and length.
.describe_vector <- function(x) {
  if (length(x) != 1L) {
    kind <- class(x)[[1L]]
    article <- if (grepl("^[aeiou]", kind)) "an" else "a"
    return(sprintf("%s %s vector of length %d", article, kind, length(x)))
  }
  if (is.character(x) && !is.na(x)) {
    shown <- if (nchar(x) > 40L) paste0(substr(x, 1L, 40L), "...") else x
    return(sprintf("\"%s\"", shown))
  }
  format(x)
}

.you_passed <- function(x) {
  sprintf("You passed %s.", .describe_value(x))
}

# The same line for a numeric argument that may hold several values.
.you_passed_values <- function(x) {
  if (length(x) == 1L) {
    return(.you_passed(x))
  }
  sprintf("Its values range from %s to %s.", format(min(x)), format(max(x)))
}

# What a user-supplied function returned, when `n` numbers were expected.
# `flaw` describes the values when the type and the length are right.
.describe_returned <- function(x, n, flaw) {
  if (is.numeric(x) && length(x) == n) {
    return(flaw)
  }
  .describe_value(x)
}

.abort_not_simulated_data <- function(x) {
  .abort(
    "`x` must be simulated data from `sim_multisite()` or `sim_meta()`.",
    .you_passed(x)
  )
}

.validate_scalar_integer <- function(x, arg) {
  if (length(x) != 1L || is.na(x) || !is.numeric(x) || !is.finite(x) || x != floor(x)) {
    .abort(
      sprintf("`%s` must be a single whole number.", arg),
      .you_passed(x)
    )
  }
  # as.integer() returns NA past the 32-bit range, and the NA would surface
  # later as a bare "missing value where TRUE/FALSE needed".
  if (abs(x) > .Machine$integer.max) {
    largest <- format(.Machine$integer.max, scientific = FALSE)
    .abort(
      sprintf("`%s` must be between -%s and %s, the range of integers in R.", arg, largest, largest),
      .you_passed(x)
    )
  }
  invisible(as.integer(x))
}

.validate_seed <- function(x, arg = "seed") {
  x <- .validate_scalar_integer(x, arg)
  if (x < 0L || x > .Machine$integer.max) {
    .abort(
      sprintf("`%s` must be between 0 and %s.", arg, format(.Machine$integer.max, scientific = FALSE)),
      .you_passed(x)
    )
  }
  invisible(x)
}

.validate_scalar_number <- function(x, arg) {
  if (length(x) != 1L || is.na(x) || !is.numeric(x) || !is.finite(x)) {
    .abort(
      sprintf("`%s` must be a single finite number.", arg),
      .you_passed(x)
    )
  }
  invisible(as.numeric(x))
}

.validate_scalar_logical <- function(x, arg) {
  if (length(x) != 1L || is.na(x) || !is.logical(x)) {
    .abort(
      sprintf("`%s` must be TRUE or FALSE.", arg),
      .you_passed(x)
    )
  }
  x
}

.validate_function_or_null <- function(x, arg) {
  if (!is.null(x) && !is.function(x)) {
    .abort(
      sprintf("`%s` must be NULL or a function.", arg),
      .you_passed(x)
    )
  }
  invisible(x)
}

# An empty list passes, so that a missing entry is reported by name.
.validate_named_list <- function(x, arg) {
  if (!is.list(x) || (length(x) > 0L && is.null(names(x)))) {
    .abort(
      sprintf("`%s` must be a named list.", arg),
      .you_passed(x)
    )
  }
  invisible(x)
}

.validate_j <- function(j) {
  j <- .validate_scalar_integer(j, "J")
  if (j < 10L) {
    .abort(
      "`J` must be at least 10.",
      .you_passed(j)
    )
  }
  invisible(j)
}

.validate_sigma_tau <- function(sigma_tau) {
  sigma_tau <- .validate_scalar_number(sigma_tau, "sigma_tau")
  if (sigma_tau <= 0) {
    .abort(
      "`sigma_tau` must be positive.",
      .you_passed(sigma_tau)
    )
  }
  invisible(sigma_tau)
}

.validate_nj_mean_nj_min <- function(nj_mean, nj_min, allow_equal = FALSE) {
  nj_mean <- .validate_scalar_number(nj_mean, "nj_mean")
  nj_min <- .validate_scalar_integer(nj_min, "nj_min")
  allow_equal <- .validate_scalar_logical(allow_equal, "allow_equal")
  if (nj_min < 1L) {
    .abort(
      "`nj_min` must be at least 1.",
      .you_passed(nj_min)
    )
  }
  invalid_relation <- if (isTRUE(allow_equal)) nj_mean < nj_min else nj_mean <= nj_min
  if (invalid_relation) {
    .abort(
      if (isTRUE(allow_equal)) {
        "`nj_mean` must be at least `nj_min`."
      } else {
        "`nj_mean` must be larger than `nj_min`."
      },
      sprintf("You passed `nj_mean = %s` and `nj_min = %s`.", format(nj_mean), nj_min),
      if (nj_mean == nj_min) sprintf("Set `cv = 0` to give every site the size %s.", nj_min)
    )
  }
  invisible(list(nj_mean = nj_mean, nj_min = nj_min))
}

.validate_engine_dependence <- function(engine, dependence, rank_corr = 0) {
  if (identical(engine, "censored") && !identical(dependence, "none")) {
    .abort(
      sprintf("`engine = \"censored\"` is not compatible with `dependence = \"%s\"`.", dependence),
      "The censored site-size generator allows only `dependence = \"none\"`.",
      "Use `engine = \"truncated_gamma\"` to keep the dependence."
    )
  }
  invisible(list(engine = engine, dependence = dependence, rank_corr = rank_corr))
}

.validate_g_fn_true_dist <- function(g_fn = NULL, true_dist = "Gaussian") {
  .validate_function_or_null(g_fn, "g_fn")
  if (!is.null(g_fn) && !true_dist %in% "User") {
    .abort(
      sprintf("`g_fn` and `true_dist = \"%s\"` were both supplied.", true_dist),
      "With `g_fn`, `true_dist` must be \"User\" or left out."
    )
  }
  invisible(list(g_fn = g_fn, true_dist = true_dist))
}

.validate_studentt_theta <- function(theta_g) {
  .validate_named_list(theta_g, "theta_G")
  if (!"nu" %in% names(theta_g)) {
    .abort(
      "`true_dist = \"StudentT\"` requires `theta_G$nu`.",
      fix = "For example, `theta_G = list(nu = 5)`."
    )
  }
  .validate_studentt_df(theta_g$nu, "theta_G$nu")
}

# `arg` is the name under which the user gave the degrees of freedom: `nu` in
# gen_effects_studentt() and `theta_G$nu` in a design.
.validate_studentt_df <- function(nu, arg) {
  nu <- .validate_scalar_number(nu, arg)
  if (nu <= 2) {
    .abort(
      sprintf("`%s` must be greater than 2.", arg),
      .you_passed(nu)
    )
  }
  invisible(nu)
}

.validate_corr_exclusive <- function(rank_corr = 0, pearson_corr = 0, dependence = NULL) {
  rank_corr <- .validate_scalar_number(rank_corr, "rank_corr")
  pearson_corr <- .validate_scalar_number(pearson_corr, "pearson_corr")
  passed <- sprintf(
    "You passed `rank_corr = %s` and `pearson_corr = %s`.",
    format(rank_corr), format(pearson_corr)
  )
  if (abs(rank_corr) > 1 || abs(pearson_corr) > 1) {
    .abort(
      "`rank_corr` and `pearson_corr` must each be between -1 and 1.",
      passed
    )
  }
  rank_nonzero <- !isTRUE(all.equal(rank_corr, 0))
  pearson_nonzero <- !isTRUE(all.equal(pearson_corr, 0))
  if (rank_nonzero && pearson_nonzero) {
    .abort(
      "`rank_corr` and `pearson_corr` cannot both differ from 0.",
      passed,
      paste(
        "Give `rank_corr` with `dependence = \"rank\"` or `\"hybrid\"`,",
        "or `pearson_corr` with `dependence = \"copula\"`."
      )
    )
  }
  if (!is.null(dependence)) {
    if (dependence %in% c("rank", "hybrid") && pearson_nonzero) {
      .abort(
        sprintf("`pearson_corr` is not used when `dependence = \"%s\"`.", dependence),
        sprintf("You passed `pearson_corr = %s`.", format(pearson_corr)),
        "Give the target as `rank_corr`, or use `dependence = \"copula\"`."
      )
    }
    if (identical(dependence, "copula") && rank_nonzero) {
      .abort(
        "`rank_corr` is not used when `dependence = \"copula\"`.",
        sprintf("You passed `rank_corr = %s`.", format(rank_corr)),
        "Give the correlation as `pearson_corr`, or use `dependence = \"rank\"` or `\"hybrid\"`."
      )
    }
    if (identical(dependence, "none") && (rank_nonzero || pearson_nonzero)) {
      .abort(
        "`rank_corr` and `pearson_corr` are not used when `dependence = \"none\"`.",
        passed,
        paste(
          "Set `dependence` to \"rank\" or \"hybrid\" for `rank_corr`,",
          "or to \"copula\" for `pearson_corr`."
        )
      )
    }
  }
  invisible(list(rank_corr = rank_corr, pearson_corr = pearson_corr))
}
