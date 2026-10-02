design_lines <- function(design) {
  strsplit(format(design), "\n", fixed = TRUE)[[1L]]
}

test_that("multisitedgp_design creates the default immutable config record", {
  des <- multisitedgp_design()

  expect_s3_class(des, "multisitedgp_design")
  expect_s3_class(des, "list")
  expect_true(is_multisitedgp_design(des))
  expect_false(is_multisitedgp_design(list()))
  expect_identical(
    names(des),
    c(
      "J", "paradigm", "true_dist", "tau", "sigma_tau", "theta_G",
      "formula", "beta", "data", "g_fn", "g_returns", "nj_mean", "cv",
      "nj_min", "p", "R2", "var_outcome", "engine", "I", "R", "shuffle",
      "se_fn", "se_args", "dependence_spec", "obs_spec", "seed",
      "record_permutation"
    )
  )
  expect_identical(des$J, 50L)
  expect_identical(des$paradigm, "site_size")
  expect_identical(des$true_dist, "Gaussian")
  expect_identical(des$sigma_tau, 0.20)
  expect_identical(des$engine, "truncated_gamma")
  expect_identical(des$dependence_spec$method, "none")
  expect_identical(
    names(des$dependence_spec),
    c("method", "rank_corr", "pearson_corr", "dependence_fn", "hybrid_init", "hybrid_polish", "max_iter", "tol")
  )
  expect_identical(des$dependence_spec$hybrid_init, "copula")
  expect_identical(des$dependence_spec$hybrid_polish, "hill_climb")
  expect_identical(names(des$obs_spec), "obs_fn")
  expect_false(des$record_permutation)
})

test_that("record_permutation default follows dependence mode", {
  no_dep <- multisitedgp_design(dependence = "none")
  rank_dep <- multisitedgp_design(dependence = "rank", rank_corr = 0.3)
  explicit_false <- multisitedgp_design(dependence = "rank", rank_corr = 0.3, record_permutation = FALSE)

  expect_false(no_dep$record_permutation)
  expect_true(rank_dep$record_permutation)
  expect_false(explicit_false$record_permutation)
})

test_that("constructor exposes hybrid dependence controls in the design spec", {
  des <- multisitedgp_design(
    dependence = "hybrid",
    rank_corr = 0.3,
    hybrid_init = "rank",
    hybrid_polish = "none"
  )

  expect_identical(des$dependence_spec$method, "hybrid")
  expect_identical(des$dependence_spec$rank_corr, 0.3)
  expect_identical(des$dependence_spec$hybrid_init, "rank")
  expect_identical(des$dependence_spec$hybrid_polish, "none")
})

test_that("g_fn defaults true_dist to User but rejects explicit ambiguity", {
  user_design <- multisitedgp_design(g_fn = function(j) rep(0, j))

  expect_identical(user_design$true_dist, "User")
  expect_multisitedgp_error(
    multisitedgp_design(true_dist = "Gaussian", g_fn = function(j) rep(0, j)),
    regexp = "were both supplied"
  )
})

test_that("direct designs require I and reject explicit site-size margin args", {
  direct <- multisitedgp_design(paradigm = "direct", I = 0.5, R = 1.5)

  expect_identical(direct$paradigm, "direct")
  expect_identical(direct$I, 0.5)
  expect_identical(direct$R, 1.5)
  expect_multisitedgp_error(
    multisitedgp_design(paradigm = "direct"),
    regexp = "`I` is required"
  )
  expect_multisitedgp_error(
    multisitedgp_design(paradigm = "direct", I = 0.5, nj_mean = 40),
    regexp = "Site-size arguments are not used when `paradigm = \"direct\"`"
  )
})

test_that("constructor uses classed validation for early scalar and matching errors", {
  expect_multisitedgp_error(
    multisitedgp_design(J = 5L),
    regexp = "`J` must be at least 10"
  )
  expect_multisitedgp_error(
    multisitedgp_design(engine = "censored", dependence = "rank", rank_corr = 0.3),
    regexp = "is not compatible with"
  )
  expect_multisitedgp_error(
    multisitedgp_design(paradigm = "unknown"),
    regexp = "`paradigm` must be one of"
  )
  expect_multisitedgp_error(
    multisitedgp_design(hybrid_init = "bad"),
    regexp = "`hybrid_init` must be one of"
  )
  expect_multisitedgp_error(
    multisitedgp_design(hybrid_polish = "bad"),
    regexp = "`hybrid_polish` must be one of"
  )
})

test_that("constructor rejects dependence targets that the selected method ignores", {
  expect_multisitedgp_error(
    multisitedgp_design(dependence = "hybrid", pearson_corr = 0.3)
  )
  expect_multisitedgp_error(
    multisitedgp_design(dependence = "rank", pearson_corr = 0.3)
  )
  expect_multisitedgp_error(
    multisitedgp_design(dependence = "copula", rank_corr = 0.3)
  )
  expect_multisitedgp_error(
    multisitedgp_design(dependence = "none", rank_corr = 0.3)
  )
})

test_that("site-size designs reject I and se_fn", {
  expect_multisitedgp_error(
    multisitedgp_design(I = 0.5),
    regexp = "`I` is for `paradigm = \"direct\"`"
  )
  expect_multisitedgp_error(
    multisitedgp_design(se_fn = function(j) rep(0.1, j)),
    regexp = "`se_fn` is for `paradigm = \"direct\"`"
  )
})

test_that("direct-only arguments are refused on a site-size design", {
  expect_multisitedgp_error(
    multisitedgp_design(J = 10L, sigma_tau = 0.2, nj_mean = 100, R = 2),
    regexp = "`R` is for `paradigm = \"direct\"`"
  )
  # `shuffle` defaults to TRUE, so it is refused when it differs from the
  # default, the same rule as for `R` (default 1).
  expect_multisitedgp_error(
    multisitedgp_design(J = 10L, sigma_tau = 0.2, nj_mean = 100, shuffle = FALSE),
    regexp = "`shuffle` is for `paradigm = \"direct\"`"
  )
})

test_that("direct-paradigm ranges are enforced", {
  direct <- function(...) {
    multisitedgp_design(paradigm = "direct", J = 10L, sigma_tau = 0.2, ...)
  }
  for (bad in list(list(I = 0), list(I = 1), list(I = 1.5))) {
    expect_multisitedgp_error(do.call(direct, c(bad, list(R = 2))))
  }
  expect_multisitedgp_error(direct(I = 0.5, R = 0.5))
  expect_no_error(direct(I = 0.5, R = 2))
})

test_that("constructor rejects out-of-range cv, p, R2 and max_iter", {
  expect_multisitedgp_error(
    multisitedgp_design(cv = -0.1)
  )
  expect_multisitedgp_error(
    multisitedgp_design(p = 1)
  )
  expect_multisitedgp_error(
    multisitedgp_design(R2 = 1)
  )
  expect_multisitedgp_error(
    multisitedgp_design(max_iter = 10L)
  )
})

test_that("theta_G must be a named list", {
  expect_multisitedgp_error(
    multisitedgp_design(J = 10L, sigma_tau = 0.2, nj_mean = 100, theta_G = "nu = 5")
  )
  expect_multisitedgp_error(
    multisitedgp_design(J = 10L, sigma_tau = 0.2, nj_mean = 100,
                        true_dist = "StudentT", theta_G = list(5))
  )
})

test_that("StudentT design validates theta_G nu at the public boundary", {
  expect_multisitedgp_error(
    multisitedgp_design(true_dist = "StudentT", theta_G = list(other = 1))
  )
  student_t <- multisitedgp_design(true_dist = "StudentT", theta_G = list(nu = 5))

  expect_identical(student_t$true_dist, "StudentT")
  expect_identical(student_t$theta_G$nu, 5)
})

test_that("se_args and g_args must be lists", {
  expect_multisitedgp_error(
    multisitedgp_design(J = 10L, sigma_tau = 0.2, nj_mean = 100,
                        se_args = "not a list")
  )
  expect_multisitedgp_error(
    gen_effects_user(J = 10L, g_fn = function(J) stats::rnorm(J),
                     g_args = "not a list")
  )
})

test_that("a non-positive dependence tolerance is refused", {
  expect_multisitedgp_error(
    multisitedgp_design(J = 10L, sigma_tau = 0.2, nj_mean = 100,
                        dependence = "rank", rank_corr = 0.2, tol = 0)
  )
})

test_that("validate_multisitedgp_design accepts complete designs and rejects malformed objects", {
  des <- multisitedgp_design()

  expect_identical(validate_multisitedgp_design(des), invisible(des))
  expect_multisitedgp_error(
    validate_multisitedgp_design(list())
  )

  broken <- des
  broken$dependence_spec <- NULL
  expect_multisitedgp_error(
    validate_multisitedgp_design(broken)
  )

  extra <- des
  extra$unexpected <- TRUE
  expect_multisitedgp_error(
    validate_multisitedgp_design(extra)
  )

  malformed <- des
  names(malformed$dependence_spec)[1] <- "bad_method"
  expect_multisitedgp_error(
    validate_multisitedgp_design(malformed)
  )
})

test_that("a design with malformed nested specs fails validation", {
  design <- multisitedgp_design(J = 10L, sigma_tau = 0.2, nj_mean = 100)
  design$dependence_spec <- "not a list"
  expect_multisitedgp_error(validate_multisitedgp_design(design))
})

test_that("update_multisitedgp_design is functional and understands flat nested fields", {
  des <- multisitedgp_design(seed = 1L)
  updated <- update_multisitedgp_design(
    des,
    J = 100L,
    sigma_tau = 0.25,
    dependence = "hybrid",
    rank_corr = 0.3,
    hybrid_init = "rank",
    hybrid_polish = "none"
  )

  expect_s3_class(updated, "multisitedgp_design")
  expect_identical(des$J, 50L)
  expect_identical(des$sigma_tau, 0.20)
  expect_identical(des$dependence_spec$method, "none")
  expect_identical(updated$J, 100L)
  expect_identical(updated$sigma_tau, 0.25)
  expect_identical(updated$dependence_spec$method, "hybrid")
  expect_identical(updated$dependence_spec$rank_corr, 0.3)
  expect_identical(updated$dependence_spec$hybrid_init, "rank")
  expect_identical(updated$dependence_spec$hybrid_polish, "none")
  expect_true(updated$record_permutation)

  explicit <- multisitedgp_design(dependence = "rank", rank_corr = 0.3, record_permutation = FALSE)
  still_explicit <- update_multisitedgp_design(explicit, dependence = "copula", pearson_corr = 0.3, rank_corr = 0)
  expect_false(still_explicit$record_permutation)

  user_update <- update_multisitedgp_design(des, g_fn = function(j) rep(0, j))
  expect_identical(user_update$true_dist, "User")
})

test_that("update_multisitedgp_design uses value-based record_permutation semantics", {
  default_rank <- multisitedgp_design(dependence = "rank", rank_corr = 0.3)
  expect_false(update_multisitedgp_design(default_rank, dependence = "none", rank_corr = 0)$record_permutation)

  explicit_true_same_value <- multisitedgp_design(dependence = "rank", rank_corr = 0.3, record_permutation = TRUE)
  same_value_updated <- update_multisitedgp_design(explicit_true_same_value, dependence = "none", rank_corr = 0)
  expect_false(same_value_updated$record_permutation)

  explicit_true_nondefault <- multisitedgp_design(dependence = "none", record_permutation = TRUE)
  nondefault_updated <- update_multisitedgp_design(explicit_true_nondefault, dependence = "rank", rank_corr = 0.3)
  expect_true(nondefault_updated$record_permutation)
})

test_that("update_multisitedgp_design rejects unknown fields", {
  expect_multisitedgp_error(
    update_multisitedgp_design(multisitedgp_design(), dependence_spec = list(method = "rank"))
  )
})

test_that("update_multisitedgp_design requires a name for every argument", {
  design <- multisitedgp_design()
  expect_rng_neutral_error(update_multisitedgp_design(design, 5), regexp = "given by name")
  expect_multisitedgp_error(update_multisitedgp_design(design, 5, 6), regexp = "given by name")
  expect_multisitedgp_error(update_multisitedgp_design(design, 5, J = 30), regexp = "given by name")
  expect_identical(update_multisitedgp_design(design, J = 30)$J, 30L)
})

test_that("update_multisitedgp_design with no updates round-trips the design", {
  design <- multisitedgp_design(J = 10L, sigma_tau = 0.2, nj_mean = 100, seed = 1L)
  expect_identical(update_multisitedgp_design(design), design)
})

test_that("format and print show a site-size design under plain headings", {
  des <- multisitedgp_design(J = 100L, sigma_tau = 0.25, seed = 1L)
  formatted <- format(des)
  printed <- capture.output(print(des))
  lines <- design_lines(des)

  expect_type(formatted, "character")
  expect_length(formatted, 1L)
  expect_identical(lines[[1L]], "<multisitedgp_design>")
  expect_identical(lines[[2L]], "J = 100 sites, standard errors from site sizes, seed 1")
  headings <- c(
    "Site effects", "Site sizes and standard errors", "Standard errors", "Dependence", "Observations"
  )
  expect_identical(
    lines[lines %in% headings],
    c("Site effects", "Site sizes and standard errors", "Dependence")
  )
  expect_true(all(c(
    "  true_dist: Gaussian", "  tau: 0", "  sigma_tau: 0.25",
    "  engine: truncated_gamma", "  nj_mean: 50", "  cv: 0.5", "  nj_min: 5",
    "  p: 0.5", "  R2: 0", "  var_outcome: 1", "  dependence: none"
  ) %in% lines))
  expect_false(any(grepl("Paradigm|Engine|Lifecycle|Layer|rank_corr|hybrid|_fn|theta_G|sim_multisite", lines)))
  expect_identical(printed, lines)
  expect_identical(capture.output(returned <- print(des)), lines)
  expect_identical(returned, des)
})

test_that("format shows a direct design under the standard-errors heading", {
  lines <- design_lines(multisitedgp_design(paradigm = "direct", J = 30L, I = 0.3, R = 2))

  expect_identical(lines[[2L]], "J = 30 sites, standard errors set directly, no seed")
  expect_true("Standard errors" %in% lines)
  expect_true(all(c("  I: 0.3", "  R: 2", "  shuffle: TRUE") %in% lines))
  expect_false(any(grepl("nj_mean|engine|se_fn|Site sizes", lines)))
})

test_that("format shows the dependence fields that the chosen method uses", {
  dependence_fields <- function(...) {
    lines <- design_lines(multisitedgp_design(...))
    lines[seq(match("Dependence", lines) + 1L, length(lines))]
  }

  expect_identical(
    dependence_fields(dependence = "rank", rank_corr = 0.3),
    c("  dependence: rank", "  rank_corr: 0.3")
  )
  expect_identical(
    dependence_fields(dependence = "copula", pearson_corr = -0.2),
    c("  dependence: copula", "  pearson_corr: -0.2")
  )
  expect_identical(
    dependence_fields(dependence = "hybrid", rank_corr = 0.3),
    c("  dependence: hybrid", "  rank_corr: 0.3", "  hybrid_init: copula", "  hybrid_polish: hill_climb")
  )
})

test_that("format lists the optional fields only when they are set", {
  mixture <- design_lines(multisitedgp_design(true_dist = "Mixture", theta_G = list(delta = 5, eps = 0.3, ups = 2)))
  expect_true("  theta_G: delta = 5, eps = 0.3, ups = 2" %in% mixture)
  expect_false(any(grepl("g_returns|formula|beta|g_fn", mixture)))

  user <- design_lines(multisitedgp_design(
    g_fn = function(J) stats::rnorm(J),
    g_returns = "raw",
    dependence = "rank",
    rank_corr = 0.3,
    dependence_fn = function(...) NULL,
    obs_fn = function(tau_j, se2_j) tau_j
  ))
  expect_true(all(c(
    "  true_dist: User", "  g_fn: <function>", "  g_returns: raw",
    "  dependence_fn: <function>", "Observations", "  obs_fn: <function>"
  ) %in% user))

  direct <- design_lines(multisitedgp_design(
    paradigm = "direct",
    I = 0.3,
    se_fn = function(J, ...) list(se2_j = rep(0.04, J))
  ))
  expect_true("  se_fn: <function>" %in% direct)
})

test_that("the print method shows formulas, vectors and functions", {
  design <- multisitedgp_design(
    J = 10L, sigma_tau = 0.2, nj_mean = 100, seed = 1L,
    formula = ~ prior, beta = 0.3, data = school(10L),
    obs_fn = function(tau_j, se2_j) tau_j
  )
  out <- paste(capture.output(print(design)), collapse = "\n")

  expect_match(out, "formula: ~prior", fixed = TRUE)
  expect_match(out, "beta: 0.3", fixed = TRUE)
  expect_match(out, "obs_fn: <function>", fixed = TRUE)
})

test_that("print of a design shows numbers with at most six significant digits", {
  lines <- design_lines(multisitedgp_design(
    tau = 1 / 3, sigma_tau = 0.189020952141661, nj_mean = 123456.789,
    true_dist = "StudentT", theta_G = list(nu = 2.123456789)
  ))
  expect_true(all(c(
    "  tau: 0.333333", "  sigma_tau: 0.189021", "  nj_mean: 123457", "  theta_G: nu = 2.12346"
  ) %in% lines))

  # Whole numbers and short decimals are printed as they are.
  lines <- design_lines(multisitedgp_design(
    J = 30L, sigma_tau = 0.25, nj_mean = 1234567, cv = 0.5,
    formula = ~ prior + urban, beta = c(0.3, 1 / 3), data = school(30L)
  ))
  expect_true(all(c(
    "  sigma_tau: 0.25", "  nj_mean: 1234567", "  cv: 0.5", "  nj_min: 5", "  beta: 0.3, 0.333333"
  ) %in% lines))
})
