sample_multisitedgp_frame <- function() {
  data.frame(
    site_index = 1:4,
    z_j = c(-1, -0.2, 0.4, 1.1),
    tau_j = c(-0.2, -0.04, 0.08, 0.22),
    tau_j_hat = c(-0.18, -0.01, 0.11, 0.19),
    se_j = c(0.2, 0.25, 0.3, 0.35),
    se2_j = c(0.04, 0.0625, 0.09, 0.1225),
    n_j = c(50L, 45L, 60L, 55L)
  )
}

test_that("the multisitedgp_data constructor returns a tibble subclass with its design and diagnostics", {
  des <- multisitedgp_design(seed = 1L)
  diagnostics <- list(I = 0.3, R = 2)
  dat <- multisitedgp_internal(".new_multisitedgp_data")(
    sample_multisitedgp_frame(),
    design = des,
    diagnostics = diagnostics
  )

  expect_identical(class(dat), c("multisitedgp_data", "tbl_df", "tbl", "data.frame"))
  expect_true(is_multisitedgp_data(dat))
  expect_s3_class(dat, "tbl_df")
  expect_identical(attr(dat, "design"), des)
  expect_identical(attr(dat, "diagnostics"), diagnostics)
})

test_that("multisitedgp_meta inherits from multisitedgp_data", {
  des <- multisitedgp_design(paradigm = "direct", I = 0.5, R = 1.5)
  frame <- sample_multisitedgp_frame()
  frame$n_j <- rep(NA_integer_, nrow(frame))
  meta <- multisitedgp_internal(".new_multisitedgp_meta")(frame, design = des)

  expect_identical(
    class(meta),
    c("multisitedgp_meta", "multisitedgp_data", "tbl_df", "tbl", "data.frame")
  )
  expect_true(is_multisitedgp_data(meta))
  expect_s3_class(meta, "multisitedgp_meta")
})

test_that("the data constructor rejects missing or mistyped columns", {
  missing_col <- sample_multisitedgp_frame()
  missing_col$se2_j <- NULL
  expect_multisitedgp_error(
    multisitedgp_internal(".new_multisitedgp_data")(missing_col)
  )

  bad_type <- sample_multisitedgp_frame()
  bad_type$site_index <- as.numeric(bad_type$site_index)
  expect_multisitedgp_error(
    multisitedgp_internal(".new_multisitedgp_data")(bad_type)
  )

  bad_numeric <- sample_multisitedgp_frame()
  bad_numeric$tau_j_hat <- as.character(bad_numeric$tau_j_hat)
  expect_multisitedgp_error(
    multisitedgp_internal(".new_multisitedgp_data")(bad_numeric)
  )

  bad_n <- sample_multisitedgp_frame()
  bad_n$n_j <- as.numeric(bad_n$n_j)
  expect_multisitedgp_error(
    multisitedgp_internal(".new_multisitedgp_data")(bad_n)
  )
})

test_that("data constructors reject mismatched design paradigms", {
  direct <- multisitedgp_design(paradigm = "direct", I = 0.5, R = 1.5)
  site_size <- multisitedgp_design(paradigm = "site_size")
  frame <- sample_multisitedgp_frame()

  expect_multisitedgp_error(
    multisitedgp_internal(".new_multisitedgp_data")(frame, design = direct)
  )
  expect_multisitedgp_error(
    multisitedgp_internal(".new_multisitedgp_meta")(frame, design = site_size)
  )
})

test_that("row subset preserves class and design but drops stale diagnostics", {
  des <- multisitedgp_design(seed = 1L)
  dat <- multisitedgp_internal(".new_multisitedgp_data")(
    sample_multisitedgp_frame(),
    design = des,
    diagnostics = list(I = 0.3)
  )

  expect_warning(
    row_subset <- dat[1:2, ],
    "Diagnostics dropped"
  )
  expect_true(is_multisitedgp_data(row_subset))
  expect_identical(attr(row_subset, "design"), des)
  expect_null(attr(row_subset, "diagnostics"))
  expect_equal(nrow(row_subset), 2L)

  expect_warning(
    same_rows <- dat[TRUE, ],
    "Diagnostics dropped"
  )
  expect_null(attr(same_rows, "diagnostics"))
})

test_that("row subset preserves multisitedgp_meta subclass", {
  des <- multisitedgp_design(paradigm = "direct", I = 0.5, R = 1.5)
  frame <- sample_multisitedgp_frame()
  frame$n_j <- rep(NA_integer_, nrow(frame))
  meta <- multisitedgp_internal(".new_multisitedgp_meta")(
    frame,
    design = des,
    diagnostics = list(I = 0.5)
  )

  expect_warning(
    row_subset <- meta[1:2, ],
    "Diagnostics dropped"
  )
  expect_s3_class(row_subset, "multisitedgp_meta")
  expect_true(is_multisitedgp_data(row_subset))
  expect_null(attr(row_subset, "diagnostics"))
})

test_that("a column subset that keeps every standard column preserves class and diagnostics", {
  dat <- multisitedgp_internal(".new_multisitedgp_data")(
    sample_multisitedgp_frame(),
    design = multisitedgp_design(),
    diagnostics = list(I = 0.3)
  )
  col_subset <- dat[, names(dat)]

  expect_true(is_multisitedgp_data(col_subset))
  expect_identical(attr(col_subset, "diagnostics"), list(I = 0.3))

  vector_subset <- dat[, "tau_j_hat", drop = TRUE]
  expect_type(vector_subset, "double")
  expect_false(is_multisitedgp_data(vector_subset))
})

test_that("a column subset that drops a standard column returns a plain tibble", {
  dat <- multisitedgp_internal(".new_multisitedgp_data")(
    sample_multisitedgp_frame(),
    design = multisitedgp_design(),
    diagnostics = list(I = 0.3)
  )
  thin <- dat[, c("site_index", "tau_j_hat")]

  expect_s3_class(thin, "tbl_df")
  expect_false(is_multisitedgp_data(thin))
  expect_null(attr(thin, "design"))
  expect_null(attr(thin, "diagnostics"))
})

test_that("as_tibble strips multisiteDGP class and attributes by default", {
  dat <- multisitedgp_internal(".new_multisitedgp_data")(
    sample_multisitedgp_frame(),
    design = multisitedgp_design(),
    diagnostics = list(I = 0.3)
  )
  plain <- tibble::as_tibble(dat)
  preserved <- tibble::as_tibble(dat, .preserve_class = TRUE)

  expect_s3_class(plain, "tbl_df")
  expect_false(is_multisitedgp_data(plain))
  expect_null(attr(plain, "design"))
  expect_null(attr(plain, "diagnostics"))
  expect_true(is_multisitedgp_data(preserved))
  expect_identical(preserved, dat)
})

test_that("dplyr row slicing drops stale diagnostics", {
  dat <- multisitedgp_internal(".new_multisitedgp_data")(
    sample_multisitedgp_frame(),
    design = multisitedgp_design(),
    diagnostics = list(I = 0.3)
  )

  expect_warning(
    filtered <- dplyr::filter(dat, site_index <= 2L),
    "Diagnostics dropped"
  )
  expect_true(is_multisitedgp_data(filtered))
  expect_null(attr(filtered, "diagnostics"))
  expect_equal(nrow(filtered), 2L)
})

# A simulated data set from site sizes and one with standard errors set
# directly. Both have the copula diagnostics, which only the simulation can
# store: `summary()` cannot compute them again from the columns.
simulated_pair <- function() {
  list(
    site_size = sim_multisite(J = 25L, dependence = "copula", pearson_corr = 0.4, seed = 41L),
    direct = sim_meta(J = 25L, I = 0.5, R = 3, dependence = "copula", pearson_corr = 0.4, seed = 42L)
  )
}

# The value of an expression together with the messages of its warnings.
value_and_warnings <- function(expr) {
  warnings <- character()
  value <- withCallingHandlers(expr, warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  list(value = value, warnings = warnings)
}

expect_same_attributes <- function(out, x) {
  expect_identical(class(out), class(x))
  expect_true(identical(attr(out, "design"), attr(x, "design")))
  expect_true(identical(attr(out, "diagnostics"), attr(x, "diagnostics")))
}

test_that("selecting or reordering all columns keeps the class, the design and the diagnostics", {
  selections <- list(
    one_index = function(x) x[names(x)],
    two_index = function(x) x[, names(x)],
    reordered = function(x) x[rev(names(x))],
    select_all = function(x) dplyr::select(x, dplyr::everything()),
    select_reordered = function(x) dplyr::select(x, dplyr::all_of(rev(names(x))))
  )
  for (x in simulated_pair()) {
    expect_false(is.null(attr(x, "diagnostics")$dependence_diagnostics$achieved_latent_pearson))
    for (select in selections) {
      result <- value_and_warnings(select(x))
      expect_length(result$warnings, 0L)
      expect_same_attributes(result$value, x)
    }
  }
})

test_that("a mutate that leaves the seven columns as they are keeps the diagnostics", {
  for (x in simulated_pair()) {
    expect_same_attributes(dplyr::mutate(x, extra = site_index %% 2L), x)
    expect_same_attributes(dplyr::mutate(x, se2_j = se2_j), x)
  }
})

test_that("a mutate that changes one of the seven columns drops the diagnostics, and summary computes them again", {
  for (x in simulated_pair()) {
    changed <- dplyr::mutate(x, se_j = se_j * 2, se2_j = se2_j * 4)
    expect_identical(class(changed), class(x))
    expect_true(identical(attr(changed, "design"), attr(x, "design")))
    expect_null(attr(changed, "diagnostics"))
    line <- grep("informativeness I", capture.output(summary(changed)), value = TRUE, fixed = TRUE)
    expect_length(line, 1L)
    expect_match(line, sprintf("%.3f", informativeness(changed)), fixed = TRUE)

    changed_effect <- dplyr::mutate(x, z_j = z_j * 2, tau_j = tau_j * 2)
    expect_identical(class(changed_effect), class(x))
    expect_null(attr(changed_effect, "diagnostics"))
  }
})

test_that("assignment to one of the seven columns drops the diagnostics, and other assignments keep them", {
  for (x in simulated_pair()) {
    kept <- x
    kept$extra <- 1
    kept[["label"]] <- "a"
    kept[, "flag"] <- TRUE
    kept$se2_j <- x$se2_j
    expect_same_attributes(kept, x)

    by_dollar <- x
    by_dollar$se2_j <- x$se2_j * 4
    by_name <- x
    by_name[["tau_j_hat"]] <- 0
    by_cell <- x
    by_cell[1L, "tau_j_hat"] <- 9
    by_column <- x
    by_column[, "z_j"] <- -x$z_j
    for (changed in list(by_dollar, by_name, by_cell, by_column)) {
      expect_identical(class(changed), class(x))
      expect_true(identical(attr(changed, "design"), attr(x, "design")))
      expect_null(attr(changed, "diagnostics"))
    }

    # The summary then describes the data as they are.
    rescaled <- x
    rescaled$se2_j <- x$se2_j * 4
    rescaled$se_j <- sqrt(rescaled$se2_j)
    line <- grep("informativeness I", capture.output(summary(rescaled)), value = TRUE, fixed = TRUE)
    expect_match(line, sprintf("%.3f", informativeness(rescaled)), fixed = TRUE)

    removed <- x
    removed$tau_j <- NULL
    expect_false(is_multisitedgp_data(removed))
    expect_null(attr(removed, "design"))
    expect_null(attr(removed, "diagnostics"))
  }
})

test_that("an assignment keeps attributes that the user has set", {
  x <- simulated_pair()$site_size
  attr(x, "note") <- "pilot"
  x$extra <- 1
  expect_identical(attr(x, "note"), "pilot")
  x$tau_j_hat <- x$tau_j_hat + 1
  expect_identical(attr(x, "note"), "pilot")
  expect_null(attr(x, "diagnostics"))
  expect_s3_class(x, "multisitedgp_data")
})

test_that("an assignment that changes a value gives no warning", {
  x <- simulated_pair()$site_size
  result <- value_and_warnings({
    x$tau_j_hat <- x$tau_j_hat + 1
    x
  })
  expect_length(result$warnings, 0L)
  expect_null(attr(result$value, "diagnostics"))
})

test_that("a row selection warns once, and later steps do not bring the diagnostics back", {
  slices <- list(
    bracket = function(x) x[1:12, ],
    filter = function(x) dplyr::filter(x, site_index <= 12L),
    slice = function(x) dplyr::slice(x, 1:12),
    arrange = function(x) dplyr::arrange(x, dplyr::desc(site_index))
  )
  for (x in simulated_pair()) {
    for (slice in slices) {
      result <- value_and_warnings(slice(x))
      expect_length(result$warnings, 1L)
      expect_match(result$warnings, "Diagnostics dropped", fixed = TRUE)
      expect_identical(class(result$value), class(x))
      expect_null(attr(result$value, "diagnostics"))

      later <- value_and_warnings(dplyr::filter(
        dplyr::select(dplyr::mutate(result$value, extra = 1), dplyr::everything()),
        site_index <= 10L
      ))
      expect_length(later$warnings, 0L)
      expect_identical(class(later$value), class(x))
      expect_null(attr(later$value, "diagnostics"))
    }
  }
})

test_that("removing one of the seven columns with `[` or mutate returns a plain tibble", {
  for (x in simulated_pair()) {
    for (out in list(x[setdiff(names(x), "se2_j")], dplyr::mutate(x, se2_j = NULL))) {
      expect_false(is_multisitedgp_data(out))
      expect_s3_class(out, "tbl_df")
      expect_null(attr(out, "design"))
      expect_null(attr(out, "diagnostics"))
    }
  }
})

test_that("renaming one of the seven columns returns a plain tibble, and exchanging two names drops the diagnostics", {
  for (x in simulated_pair()) {
    renamed <- x
    names(renamed)[names(renamed) == "tau_j_hat"] <- "effect"
    by_colnames <- x
    colnames(by_colnames)[colnames(by_colnames) == "tau_j_hat"] <- "effect"
    by_function <- dplyr::rename_with(x, function(name) sub("^tau_j_hat$", "effect", name))
    by_verb <- dplyr::rename(x, effect = tau_j_hat)
    for (out in list(renamed, by_colnames, by_function, by_verb)) {
      expect_false(is_multisitedgp_data(out))
      expect_s3_class(out, "tbl_df")
      expect_named(out, sub("^tau_j_hat$", "effect", names(x)))
      expect_null(attr(out, "design"))
      expect_null(attr(out, "diagnostics"))
    }

    # The same seven names over other values: the stored diagnostics describe
    # the columns as they were.
    exchanged <- x
    names(exchanged)[match(c("tau_j", "tau_j_hat"), names(x))] <- c("tau_j_hat", "tau_j")
    by_verb <- dplyr::rename(x, tau_j = tau_j_hat, tau_j_hat = tau_j)
    by_select <- dplyr::select(x, site_index, z_j, tau_j = tau_j_hat, tau_j_hat = tau_j, dplyr::everything())
    for (out in list(exchanged, by_verb, by_select)) {
      expect_identical(class(out), class(x))
      expect_identical(out$tau_j, x$tau_j_hat)
      expect_true(identical(attr(out, "design"), attr(x, "design")))
      expect_null(attr(out, "diagnostics"))
    }

    same <- x
    names(same) <- names(x)
    expect_same_attributes(same, x)

    other <- x
    other$extra <- 1
    names(other)[names(other) == "extra"] <- "note"
    expect_same_attributes(other, x)
  }
})

test_that("names that a tibble accepts but simulated data cannot have give a plain tibble", {
  for (x in simulated_pair()) {
    plain <- tibble::as_tibble(x)
    # Missing names, as `unname()` and `stack()` set them on the way.
    expect_identical(unname(x), unname(plain))
    expect_identical(utils::stack(x), utils::stack(plain))
    expect_identical(Reduce(`+`, unname(x)), Reduce(`+`, unname(plain)))

    duplicated_name <- x
    names(duplicated_name)[names(x) == "z_j"] <- "site_index"
    empty_name <- x
    names(empty_name)[names(x) == "z_j"] <- ""
    for (out in list(duplicated_name, empty_name)) {
      expect_false(is_multisitedgp_data(out))
      expect_s3_class(out, "tbl_df")
      expect_null(attr(out, "design"))
      expect_identical(out[[2L]], x$z_j)
    }
  }
})

test_that("print and summary set stored diagnostics aside when the number of rows has changed", {
  stored_diagnostics <- multisitedgp_internal(".stored_diagnostics")
  reporting_table <- multisitedgp_internal(".diagnostics_table_for_reporting")
  sigma_tau_row <- function(x) {
    tab <- reporting_table(x)
    tab[tab$diagnostic == "sigma_tau", ]
  }
  one_site <- list(z_j = 9, tau_j = 1.8, tau_j_hat = 1.8, se_j = 0.3, se2_j = 0.09)

  for (x in simulated_pair()) {
    sigma_tau <- attr(x, "design")$sigma_tau
    expect_identical(reporting_table(x), attr(x, "diagnostics")$target_vs_realized)

    # rbind() keeps the attributes of its first argument, so the diagnostics of
    # 25 sites stay on data that have 50.
    simulate <- if (inherits(x, "multisitedgp_meta")) sim_meta else sim_multisite
    stacked <- rbind(x, simulate(attr(x, "design"), seed = 43L))
    expect_identical(nrow(stacked), 50L)
    expect_identical(attr(stacked, "diagnostics")$J, 25L)
    expect_null(stored_diagnostics(stacked))
    expect_equal(sigma_tau_row(stacked)$realized, sigma_tau * stats::sd(stacked$z_j), tolerance = tol_deterministic)
    # The second line of print() shows the value of the 50 rows.
    expect_match(
      capture.output(print(stacked, n = 1L))[[2L]],
      sprintf("realized %.3f", sigma_tau * stats::sd(stacked$z_j)),
      fixed = TRUE
    )
    # The stored latent correlation of the copula belongs to the 25 sites, so
    # its row has no value and summary() leaves it out.
    latent <- reporting_table(stacked)
    latent <- latent[latent$basis == "copula_latent", ]
    expect_true(all(is.na(latent$realized)))
    expect_length(grep("copula latent", capture.output(summary(stacked)), fixed = TRUE), 0L)
    expect_length(grep("copula latent", capture.output(summary(x)), fixed = TRUE), 1L)

    grown <- do.call(tibble::add_row, c(list(x, site_index = 26L, n_j = x$n_j[[1L]]), one_site))
    expect_equal(sigma_tau_row(grown)$realized, sigma_tau * stats::sd(grown$z_j), tolerance = tol_deterministic)
    line <- grep("sigma_tau", capture.output(summary(grown)), value = TRUE, fixed = TRUE)
    expect_match(line, sprintf("%.3f", sigma_tau * stats::sd(grown$z_j)), fixed = TRUE)
  }
})

test_that("stored diagnostics that are not a list or name no number of sites are left to the old rules", {
  stored_diagnostics <- multisitedgp_internal(".stored_diagnostics")
  x <- sim_multisite(J = 25L, seed = 1L)

  # Fewer rows than the stored diagnostics describe.
  fewer <- multisitedgp_internal(".new_multisitedgp_data")(
    tibble::as_tibble(x)[1:10, ],
    design = attr(x, "design"),
    diagnostics = attr(x, "diagnostics")
  )
  expect_null(stored_diagnostics(fewer))

  # No number of sites in the list: nothing to compare, so the list is used.
  without_count <- x
  diagnostics <- attr(x, "diagnostics")
  diagnostics$J <- NULL
  diagnostics$J_note <- 3
  attr(without_count, "diagnostics") <- diagnostics
  expect_identical(stored_diagnostics(without_count), diagnostics)

  # Not a list: ignored, and print() computes the table from the data.
  not_a_list <- x
  attr(not_a_list, "diagnostics") <- "none"
  expect_null(stored_diagnostics(not_a_list))
  expect_match(capture.output(print(not_a_list, n = 1L))[[2L]], "sigma_tau", fixed = TRUE)
})

test_that("the methods of the data class are registered and not exported", {
  # An S3 method that is exported as a function instead of being registered
  # appears among the exports under its full name.
  exports <- getNamespaceExports("multisiteDGP")
  expect_length(grep("[.]multisitedgp_(data|meta)$", exports), 0L)
  for (generic in c("[", "$<-", "[[<-", "[<-", "names<-", "print", "summary")) {
    expect_true(is.function(utils::getS3method(generic, "multisitedgp_data", optional = TRUE)), info = generic)
  }
})

test_that("print of a data object without diagnostics shows the first line and the tibble", {
  dat <- multisitedgp_internal(".new_multisitedgp_data")(sample_multisitedgp_frame())
  printed <- capture.output(print(dat))

  expect_identical(printed[[1L]], "# multisitedgp_data: 4 sites")
  expect_match(printed[[2L]], "# A tibble: 4", fixed = TRUE)
})

test_that("print.multisitedgp_data prints a short header, then the tibble, and returns invisibly", {
  out <- sim_multisite(J = 25L, seed = 6601L)

  printed <- capture.output(returned <- print(out, n = 3L))
  informativeness <- grep("# informativeness I:", printed, value = TRUE, fixed = TRUE)

  expect_type(getS3method("print", "multisitedgp_data"), "closure")
  expect_identical(returned, out)
  expect_identical(
    printed[[1L]],
    "# multisitedgp_data: 25 sites, standard errors from site sizes, seed 6601"
  )
  expect_true(any(grepl("# sigma_tau: target 0.200, realized", printed, fixed = TRUE)))
  expect_true(any(grepl("# rank correlation (tau, se): target 0.000, realized", printed, fixed = TRUE)))
  expect_length(informativeness, 1L)
  expect_false(grepl("target", informativeness, fixed = TRUE))
  expect_true(any(grepl("# A tibble: 25", printed, fixed = TRUE)))
  expect_true(any(grepl("site_index", printed, fixed = TRUE)))
})

test_that("print.multisitedgp_data prints no status words and no closing hint", {
  out <- sim_multisite(J = 25L, seed = 6601L)

  printed <- capture.output(print(out, n = 3L))

  expect_false(any(grepl("pass|warn|fail", printed, ignore.case = TRUE)))
  expect_false(any(grepl("summary", printed, fixed = TRUE)))
  expect_false(any(grepl("paradigm", printed, ignore.case = TRUE)))
})

test_that("print.multisitedgp_data uses the same first line for `sim_meta()` output", {
  out <- sim_meta(I = 0.4, R = 3, seed = 6602L)

  printed <- capture.output(returned <- print(out, n = 3L))

  expect_identical(returned, out)
  expect_identical(
    printed[[1L]],
    "# multisitedgp_data: 50 sites, standard errors set directly, seed 6602"
  )
  expect_true(any(grepl("# informativeness I: target 0.400, realized 0.400", printed, fixed = TRUE)))
  expect_true(any(grepl("tau_j_hat", printed, fixed = TRUE)))
})

test_that("print.multisitedgp_data shows only the realized value when a row has no target", {
  out <- sim_multisite(J = 25L, seed = 6606L)
  diagnostics <- attr(out, "diagnostics", exact = TRUE)
  tab <- diagnostics$target_vs_realized
  row <- tab$diagnostic == "realized_rank_corr" & tab$basis == "residual"
  tab$target[row] <- NA_real_
  tab$status[row] <- NA_character_
  diagnostics$target_vs_realized <- tab
  attr(out, "diagnostics") <- diagnostics

  printed <- capture.output(print(out, n = 1L))
  rank_line <- grep("rank correlation (tau, se):", printed, value = TRUE, fixed = TRUE)

  expect_length(rank_line, 1L)
  expect_false(grepl("target", rank_line, fixed = TRUE))
  expect_false(grepl("realized", rank_line, fixed = TRUE))
  expect_false(grepl("N/A", rank_line, fixed = TRUE))
})

test_that("print.multisitedgp_data validates the row display count", {
  out <- sim_multisite(J = 25L, seed = 6605L)

  expect_multisitedgp_error(print(out, n = 0L))
  expect_multisitedgp_error(print(out, n = NA_integer_))
})

test_that("summary formatter reports unavailable diagnostics for bare data objects", {
  dat <- multisitedgp_internal(".new_multisitedgp_data")(sample_multisitedgp_frame())
  summary_lines <- multisitedgp_internal(".format_multisitedgp_summary")(dat)

  expect_identical(
    summary_lines,
    c("multisiteDGP simulation: 4 sites", "Diagnostics are not available for this object.")
  )
})

test_that("summary.multisitedgp_data prints the four blocks and the status counts", {
  out <- sim_meta(I = 0.4, R = 3, seed = 6603L)

  printed <- capture.output(returned <- summary(out))

  expect_type(getS3method("summary", "multisitedgp_data"), "closure")
  expect_identical(returned, out)
  expect_identical(
    printed[[1L]],
    "multisiteDGP simulation: 50 sites, standard errors set directly, seed 6603"
  )
  expect_identical(
    printed[printed %in% c("Scale", "Dependence", "Shape", "Precision")],
    c("Scale", "Dependence", "Shape", "Precision")
  )
  expect_false(any(grepl("feasibility", printed, ignore.case = TRUE)))
  expect_match(printed[[length(printed)]], "^[0-9]+ pass, [0-9]+ warn, [0-9]+ fail$")
  expect_false(any(grepl("N/A|PASS|WARN|FAIL", printed)))
})

test_that("summary.multisitedgp_data shows the target, the p-value and the status of a row", {
  out <- sim_meta(I = 0.4, R = 3, seed = 6603L)

  printed <- capture.output(summary(out))
  informativeness <- grep("informativeness I", printed, value = TRUE, fixed = TRUE)
  ks <- grep("KS distance", printed, value = TRUE, fixed = TRUE)
  marginal <- grep("rank correlation, marginal", printed, value = TRUE, fixed = TRUE)

  expect_match(informativeness, "target 0.400", fixed = TRUE)
  expect_match(informativeness, "pass", fixed = TRUE)
  expect_match(ks, "p = ", fixed = TRUE)
  expect_false(grepl("target", ks, fixed = TRUE))
  expect_false(grepl("target", marginal, fixed = TRUE))
})

test_that("summary.multisitedgp_data counts the statuses of the diagnostics table", {
  out <- sim_multisite(J = 25L, seed = 6607L)
  status <- attr(out, "diagnostics", exact = TRUE)$target_vs_realized$status

  printed <- capture.output(summary(out))

  expect_identical(
    printed[[length(printed)]],
    sprintf(
      "%d pass, %d warn, %d fail",
      sum(status == "pass", na.rm = TRUE),
      sum(status == "warn", na.rm = TRUE),
      sum(status == "fail", na.rm = TRUE)
    )
  )
})

test_that("summary.multisitedgp_data title includes preset only when present", {
  out <- sim_multisite(J = 25L, seed = 6607L)

  expect_identical(
    capture.output(summary(out))[[1L]],
    "multisiteDGP simulation: 25 sites, standard errors from site sizes, seed 6607"
  )

  design <- attr(out, "design", exact = TRUE)
  attr(design, "preset_name") <- "preset_education_modest"
  attr(out, "design") <- design

  expect_identical(
    capture.output(summary(out))[[1L]],
    "multisiteDGP simulation (preset_education_modest): 25 sites, standard errors from site sizes, seed 6607"
  )
})

test_that("summary.multisitedgp_data leaves out the rows that have no realized value", {
  out <- sim_multisite(
    J = 25L,
    true_dist = "Mixture",
    theta_G = list(delta = 5, eps = 0.3, ups = 2),
    seed = 6609L
  )

  printed <- capture.output(summary(out))

  expect_false(any(grepl("KS distance", printed, fixed = TRUE)))
  expect_false("Shape" %in% printed)
  expect_true("Precision" %in% printed)
  expect_false(any(grepl("N/A", printed, fixed = TRUE)))
})

test_that("summary.multisitedgp_data labels the copula latent correlation and shows its target", {
  out <- sim_multisite(J = 50L, dependence = "copula", pearson_corr = 0.3, seed = 6608L)

  printed <- capture.output(summary(out))
  latent <- grep("Pearson correlation, copula latent", printed, value = TRUE, fixed = TRUE)

  expect_length(latent, 1L)
  expect_match(latent, "target 0.300", fixed = TRUE)
})

test_that("summary recomputes diagnostics for row-subset objects", {
  out <- sim_multisite(J = 25L, seed = 6604L)
  sub <- suppressWarnings(out[seq_len(12L), ])

  printed <- capture.output(returned <- summary(sub))

  expect_identical(returned, sub)
  expect_null(attr(sub, "diagnostics", exact = TRUE))
  expect_true(any(grepl("multisiteDGP simulation: 12 sites", printed, fixed = TRUE)))
  expect_true("Scale" %in% printed)
})

test_that("print and summary report residual and marginal correlations for a covariate design", {
  data_x <- tibble::tibble(x = seq(-1, 1, length.out = 50L))
  df <- sim_multisite(
    formula = ~ x,
    beta = 1.5,
    data = data_x,
    dependence = "rank",
    rank_corr = 0.3,
    seed = 18L
  )

  printed <- capture.output(print(df, n = 1L))
  summarized <- capture.output(summary(df))

  # With covariates the correlation with z_j is not the correlation with
  # tau_j, so the header names z.
  expect_true(any(grepl("rank correlation (z, se): target 0.300, realized", printed, fixed = TRUE)))
  expect_false(any(grepl("(tau, se)", printed, fixed = TRUE)))
  expect_true(any(grepl("rank correlation, residual", summarized, fixed = TRUE)))
  expect_true(any(grepl("rank correlation, marginal", summarized, fixed = TRUE)))
  expect_true(any(grepl("Pearson correlation, residual", summarized, fixed = TRUE)))
  expect_true(any(grepl("Pearson correlation, marginal", summarized, fixed = TRUE)))
})
