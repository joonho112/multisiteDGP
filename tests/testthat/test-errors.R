internal <- function(name) {
  getFromNamespace(name, "multisiteDGP")
}

test_that("abort helpers set the error classes and build the message", {
  err <- expect_multisitedgp_error(
    internal(".abort")("problem", "context", "Try a fix.")
  )
  expect_false(inherits(err, "multisitedgp_solver_error"))
  expect_match(conditionMessage(err), "problem")
  expect_match(conditionMessage(err), "context")
  expect_match(conditionMessage(err), "Try a fix")

  solver_err <- expect_multisitedgp_error(
    internal(".abort_solver")("no solution", "context", "Try a fix."),
    "multisitedgp_solver_error"
  )
  expect_match(conditionMessage(solver_err), "no solution")

  message_only <- expect_multisitedgp_error(internal(".abort")("problem"))
  expect_false(grepl("\n", conditionMessage(message_only), fixed = TRUE))
})

test_that("the J lower bound is validated", {
  expect_multisitedgp_error(
    internal(".validate_j")(5L)
  )
  expect_identical(internal(".validate_j")(10L), invisible(10L))
})

test_that("sigma_tau positivity is validated", {
  expect_multisitedgp_error(
    internal(".validate_sigma_tau")(0)
  )
  expect_identical(internal(".validate_sigma_tau")(0.2), invisible(0.2))
})

test_that("the nj_mean and nj_min relation is validated, with the deterministic equality exception", {
  expect_multisitedgp_error(
    internal(".validate_nj_mean_nj_min")(10, 0L)
  )
  expect_multisitedgp_error(
    internal(".validate_nj_mean_nj_min")(10, 15L)
  )
  expect_multisitedgp_error(
    internal(".validate_nj_mean_nj_min")(10, 10L)
  )
  expect_silent(internal(".validate_nj_mean_nj_min")(10, 10L, allow_equal = TRUE))
  expect_silent(internal(".validate_nj_mean_nj_min")(40, 5L))
})

test_that("the censored site-size generator with dependence is rejected", {
  expect_multisitedgp_error(
    internal(".validate_engine_dependence")("censored", "rank", rank_corr = 0.3),
    regexp = "is not compatible with"
  )
  expect_silent(internal(".validate_engine_dependence")("censored", "none"))
})

test_that("g_fn and true_dist coherence is validated", {
  expect_multisitedgp_error(
    internal(".validate_function_or_null")(1, "g_fn"),
    regexp = "must be NULL or a function"
  )
  expect_multisitedgp_error(
    internal(".validate_g_fn_true_dist")(function(j) stats::rnorm(j), "Gaussian"),
    regexp = "were both supplied"
  )
  expect_silent(internal(".validate_g_fn_true_dist")(function(j) stats::rnorm(j), "User"))
})

test_that("StudentT theta_G nu is validated", {
  err <- expect_multisitedgp_error(
    internal(".validate_studentt_theta")(list(other = 1))
  )
  expect_match(conditionMessage(err), "theta_G\\$nu")
  expect_multisitedgp_error(
    internal(".validate_studentt_theta")(list(nu = 2))
  )
  expect_identical(internal(".validate_studentt_theta")(list(nu = 5)), invisible(5))
})

test_that("rank and Pearson correlation exclusivity is validated", {
  expect_multisitedgp_error(
    internal(".validate_corr_exclusive")(rank_corr = 2, pearson_corr = 0)
  )
  expect_multisitedgp_error(
    internal(".validate_corr_exclusive")(rank_corr = 0.3, pearson_corr = 0.3)
  )
  expect_silent(internal(".validate_corr_exclusive")(rank_corr = 0.3, pearson_corr = 0))
  expect_silent(internal(".validate_corr_exclusive")(rank_corr = 0, pearson_corr = 0.3))
  expect_silent(internal(".validate_corr_exclusive")(rank_corr = 0.3, pearson_corr = 0, dependence = "hybrid"))
  expect_silent(internal(".validate_corr_exclusive")(rank_corr = 0, pearson_corr = 0.3, dependence = "copula"))
  expect_multisitedgp_error(
    internal(".validate_corr_exclusive")(rank_corr = 0, pearson_corr = 0.3, dependence = "hybrid")
  )
  expect_multisitedgp_error(
    internal(".validate_corr_exclusive")(rank_corr = 0.3, pearson_corr = 0, dependence = "copula")
  )
  expect_multisitedgp_error(
    internal(".validate_corr_exclusive")(rank_corr = 0.3, pearson_corr = 0, dependence = "none")
  )
})

test_that("integer arguments past 2^31 abort instead of coercing to NA", {
  # as.integer() returns NA beyond the 32-bit range. Without a range check the
  # NA would surface later as a bare "missing value where TRUE/FALSE needed"
  # instead of a multisitedgp_error.
  expect_multisitedgp_error(
    multisitedgp_design(J = 2^35, sigma_tau = 0.2, nj_mean = 100, seed = 1L)
  )
  expect_multisitedgp_error(
    multisitedgp_design(J = 10L, sigma_tau = 0.2, nj_mean = 100, seed = 2^40)
  )
  expect_multisitedgp_error(
    .solve_truncated_gamma(n_bar = 100, cv = 0.5, n_min = 2^40)
  )

  # The boundary itself is still admissible.
  expect_no_error(
    multisitedgp_design(J = 10L, sigma_tau = 0.2, nj_mean = 100,
                        seed = .Machine$integer.max)
  )
})

test_that("a message prints braces as they are", {
  err <- expect_multisitedgp_error(internal(".abort")("a {b} c", "d } e"))
  expect_match(conditionMessage(err), "a {b} c", fixed = TRUE)
  expect_match(conditionMessage(err), "d } e", fixed = TRUE)

  # The message of an error raised in a user-supplied function is passed on.
  err <- expect_multisitedgp_error(
    gen_effects_user(J = 10L, g_fn = function(J) stop("unmatched { brace"))
  )
  expect_match(conditionMessage(err), "`g_fn` stopped with an error when called with `J = 10`", fixed = TRUE)
  expect_match(conditionMessage(err), "unmatched { brace", fixed = TRUE)
})

test_that("a message says which values are allowed and what was passed", {
  dat <- sim_multisite(J = 20L, seed = 1L)
  cases <- list(
    list(call = function() multisitedgp_design(engine = "x"),
         rule = "`engine` must be one of \"truncated_gamma\" or \"censored\".", passed = "You passed \"x\"."),
    list(call = function() multisitedgp_design(dependence = 1),
         rule = "`dependence` must be one of \"none\", \"rank\", \"copula\", or \"hybrid\".", passed = "You passed 1."),
    list(call = function() plot_effects(dat, type = "x"),
         rule = "`type` must be one of \"caterpillar\" or \"density\".", passed = "You passed \"x\"."),
    list(call = function() plot_funnel(dat, envelope = "yes"),
         rule = "`envelope` must be TRUE or FALSE.", passed = "You passed \"yes\"."),
    list(call = function() sim_multisite(J = 5L),
         rule = "`J` must be at least 10.", passed = "You passed 5."),
    list(call = function() sim_multisite(sigma_tau = 0),
         rule = "`sigma_tau` must be positive.", passed = "You passed 0."),
    list(call = function() sim_multisite(J = c(20, 30)),
         rule = "`J` must be a single whole number.", passed = "You passed a numeric vector of length 2."),
    list(call = function() sim_multisite(J = 20L, seed = "a"),
         rule = "`seed` must be a single whole number.", passed = "You passed \"a\"."),
    list(call = function() plot_funnel(1:3),
         rule = "`x` must be simulated data from `sim_multisite()` or `sim_meta()`.",
         passed = "You passed an integer vector of length 3."),
    list(call = function() design_grid(J = 20, base_design = list(), seed_root = 1L),
         rule = "`base_design` must be a design from `multisitedgp_design()` or a preset.",
         passed = "You passed a list of length 0.")
  )
  for (case in cases) {
    msg <- conditionMessage(expect_multisitedgp_error(case$call()))
    expect_match(msg, case$rule, fixed = TRUE)
    expect_match(msg, case$passed, fixed = TRUE)
    # Nothing about how the package works inside.
    expect_no_match(msg, "is matched before|is checked before|design object")
  }
})

test_that("a missing entry of theta_G is reported by name", {
  required <- list(
    StudentT = "`theta_G$nu`", SkewN = "`theta_G$slant`", ALD = "`theta_G$rho`",
    Mixture = "`theta_G$delta`, `theta_G$eps`, and `theta_G$ups`", PointMassSlab = "`theta_G$pi0`"
  )
  for (true_dist in names(required)) {
    msg <- conditionMessage(expect_multisitedgp_error(multisitedgp_design(true_dist = true_dist)))
    expect_match(msg, sprintf("`true_dist = \"%s\"` requires %s", true_dist, required[[true_dist]]), fixed = TRUE)
    expect_no_match(msg, "must be a named list", fixed = TRUE)
  }

  # Something that is not a list is still reported as such.
  expect_multisitedgp_error(
    internal(".validate_studentt_theta")(5),
    regexp = "`theta_G` must be a named list"
  )
})

test_that("a generator names its own argument and a design names the theta_G entry", {
  msg <- conditionMessage(expect_multisitedgp_error(gen_effects_studentt(J = 10L, nu = 2)))
  expect_match(msg, "`nu` must be greater than 2.", fixed = TRUE)
  expect_match(msg, "You passed 2.", fixed = TRUE)
  expect_no_match(msg, "theta_G", fixed = TRUE)

  in_design <- list(
    list(true_dist = "StudentT", theta_G = list(nu = 2), names = "`theta_G$nu` must be greater than 2."),
    list(true_dist = "ALD", theta_G = list(rho = 0.99), names = "`theta_G$rho` must be strictly between"),
    list(true_dist = "SkewN", theta_G = list(slant = "a"), names = "`theta_G$slant` must be a single finite number."),
    list(true_dist = "Mixture", theta_G = list(delta = 5, eps = 2, ups = 2), names = "`theta_G$eps` must be strictly"),
    list(true_dist = "PointMassSlab", theta_G = list(pi0 = 1), names = "`theta_G$pi0` must be strictly between")
  )
  for (case in in_design) {
    err <- expect_multisitedgp_error(multisitedgp_design(true_dist = case$true_dist, theta_G = case$theta_G))
    expect_match(conditionMessage(err), case$names, fixed = TRUE)
  }

  direct <- list(
    list(call = function() gen_effects_ald(J = 10L, rho = 0.99), names = "`rho` must be strictly between"),
    list(call = function() gen_effects_mixture(J = 10L, delta = 5, eps = 2, ups = 2), names = "`eps` must be strictly"),
    list(call = function() gen_effects_pmslab(J = 10L, pi0 = 1), names = "`pi0` must be strictly between")
  )
  for (case in direct) {
    msg <- conditionMessage(expect_multisitedgp_error(case$call()))
    expect_match(msg, case$names, fixed = TRUE)
    expect_no_match(msg, "theta_G", fixed = TRUE)
  }
})

test_that("site-size messages name the arguments of gen_site_sizes()", {
  internal_names <- "n_bar|n_min\\b|\\btol\\b|max_starts|max_iter|[Tt]runcated"

  err <- expect_multisitedgp_error(
    sim_multisite(J = 20L, nj_mean = 10, cv = 1, seed = 1L),
    "multisitedgp_solver_error"
  )
  msg <- conditionMessage(err)
  expect_match(msg, "`cv = 1` cannot be reached with `nj_mean = 10` and `nj_min = 5`.", fixed = TRUE)
  expect_match(msg, "`cv` is too large for the ratio `nj_min / nj_mean`, which is 0.5.", fixed = TRUE)
  expect_match(msg, "Use a smaller `cv` or a smaller `nj_min`.", fixed = TRUE)
  expect_no_match(msg, internal_names)

  # The censored generator has the same rule, so the message does not name a
  # generator.
  for (engine in c("truncated_gamma", "censored")) {
    msg <- conditionMessage(expect_multisitedgp_error(
      sim_multisite(nj_mean = 5, nj_min = 5, engine = engine)
    ))
    expect_match(msg, "`nj_mean` must be larger than `nj_min`.", fixed = TRUE)
    expect_match(msg, "You passed `nj_mean = 5` and `nj_min = 5`.", fixed = TRUE)
    expect_no_match(msg, internal_names)
  }

  warned <- tryCatch(sim_multisite(J = 20L, nj_mean = 8, cv = 0.1, seed = 1L), warning = function(w) w)
  expect_s3_class(warned, "warning")
  expect_match(conditionMessage(warned), "`nj_mean` is less than twice `nj_min`", fixed = TRUE)
  expect_no_match(conditionMessage(warned), internal_names)
})

test_that("a message about a user-supplied function names the function and what it returned", {
  effects <- withr::with_seed(1L, gen_effects(J = 20L))
  sites <- withr::with_seed(1L, gen_site_sizes(effects, J = 20L))
  cases <- list(
    list(call = function() gen_effects_user(J = 20L, g_fn = function(J) stats::rnorm(J - 1L)),
         rule = "`g_fn` must return `J` finite numbers.",
         returned = "Called with `J = 20`, it returned a numeric vector of length 19."),
    list(call = function() gen_se_direct(effects, J = 20L, I = 0.3, se_fn = function(J) list(se2_j = rep(-1, J))),
         rule = "`se_fn` must return `se2_j` as one positive finite number per site.",
         returned = "For 20 sites it returned values that are missing, infinite, or not positive."),
    list(call = function() gen_se_direct(effects, J = 20L, I = 0.3, se_fn = function(J) rep(0.1, J)),
         rule = "`se_fn` must return a list or data frame with an element `se2_j`.",
         returned = "It returned a numeric vector of length 20."),
    list(call = function() gen_observations(sites, obs_fn = function(tau_j, se2_j) "a"),
         rule = "`obs_fn` must return one finite number per site.",
         returned = "For 20 sites it returned \"a\"."),
    list(call = function() align_rank_corr(sites, rank_corr = 0.3, dependence_fn = function(z_j, se2_j, target) se2_j),
         rule = "`dependence_fn` must return `list(se2_j = se2_j[perm], perm = perm)`.",
         returned = "It returned a numeric vector of length 20.")
  )
  for (case in cases) {
    msg <- conditionMessage(expect_multisitedgp_error(case$call()))
    expect_match(msg, case$rule, fixed = TRUE)
    expect_match(msg, case$returned, fixed = TRUE)
  }
})

test_that("the line that says what was passed describes values of every kind", {
  describe <- multisitedgp_internal(".describe_value")

  expect_identical(describe(NULL), "NULL")
  expect_identical(describe(mean), "a function")
  expect_identical(describe(data.frame(x = 1)), "a data frame")
  expect_identical(describe(factor(c("a", "b"))), "a factor of length 2")
  expect_identical(describe(list(1, 2, 3)), "a list of length 3")
  expect_identical(describe(stats::lm(y ~ 1, data.frame(y = 1:3))), "an object of class lm")
  expect_identical(describe(5), "5")
  expect_identical(describe("x"), "\"x\"")
  expect_identical(describe(1:3), "an integer vector of length 3")
  expect_identical(describe(c(0.5, 1)), "a numeric vector of length 2")
})

test_that("with cv = 0 the mean site size may equal the minimum but not fall below it", {
  expect_no_error(multisitedgp_design(nj_mean = 20, nj_min = 20L, cv = 0))
  expect_error(
    multisitedgp_design(nj_mean = 10, nj_min = 20L, cv = 0),
    "`nj_mean` must be at least `nj_min`",
    class = "multisitedgp_error"
  )
})
