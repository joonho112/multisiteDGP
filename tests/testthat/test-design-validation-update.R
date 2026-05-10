test_that("validate_multisitedgp_design accepts complete designs and rejects malformed objects", {
  des <- multisitedgp_design()

  expect_identical(validate_multisitedgp_design(des), invisible(des))
  expect_multisitedgp_error(
    validate_multisitedgp_design(list()),
    "multisitedgp_arg_error"
  )

  broken <- des
  broken$dependence_spec <- NULL
  expect_multisitedgp_error(
    validate_multisitedgp_design(broken),
    "multisitedgp_arg_error"
  )

  extra <- des
  extra$unexpected <- TRUE
  expect_multisitedgp_error(
    validate_multisitedgp_design(extra),
    "multisitedgp_arg_error"
  )

  malformed <- des
  names(malformed$dependence_spec)[1] <- "bad_method"
  expect_multisitedgp_error(
    validate_multisitedgp_design(malformed),
    "multisitedgp_arg_error"
  )
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
    update_multisitedgp_design(multisitedgp_design(), dependence_spec = list(method = "rank")),
    "multisitedgp_arg_error"
  )
})

test_that("direct designs require I and reject explicit site-size margin args", {
  direct <- multisitedgp_design(paradigm = "direct", I = 0.5, R = 1.5)

  expect_identical(direct$paradigm, "direct")
  expect_identical(direct$I, 0.5)
  expect_identical(direct$R, 1.5)
  expect_multisitedgp_error(
    multisitedgp_design(paradigm = "direct"),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    multisitedgp_design(paradigm = "direct", I = 0.5, nj_mean = 40),
    "multisitedgp_coherence_error"
  )
})

test_that("site-size designs reject direct precision hooks", {
  expect_multisitedgp_error(
    multisitedgp_design(I = 0.5),
    "multisitedgp_coherence_error"
  )
  expect_multisitedgp_error(
    multisitedgp_design(se_fn = function(j) rep(0.1, j)),
    "multisitedgp_coherence_error"
  )
})

test_that("constructor validates key Tier 2 ranges", {
  expect_multisitedgp_error(
    multisitedgp_design(cv = -0.1),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    multisitedgp_design(p = 1),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    multisitedgp_design(R2 = 1),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    multisitedgp_design(max_iter = 10L),
    "multisitedgp_arg_error"
  )
})

test_that("g_fn defaults true_dist to User but rejects explicit ambiguity", {
  user_design <- multisitedgp_design(g_fn = function(j) rep(0, j))

  expect_identical(user_design$true_dist, "User")
  expect_multisitedgp_error(
    multisitedgp_design(true_dist = "Gaussian", g_fn = function(j) rep(0, j)),
    "multisitedgp_coherence_error"
  )
})

test_that("StudentT design validates theta_G nu at the public boundary", {
  expect_multisitedgp_error(
    multisitedgp_design(true_dist = "StudentT", theta_G = list(other = 1)),
    "multisitedgp_arg_error"
  )
  student_t <- multisitedgp_design(true_dist = "StudentT", theta_G = list(nu = 5))

  expect_identical(student_t$true_dist, "StudentT")
  expect_identical(student_t$theta_G$nu, 5)
})

test_that("target_marginal_rho is an explicit v1 abort", {
  expect_multisitedgp_error(
    multisitedgp_design(target_marginal_rho = 0.3),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    update_multisitedgp_design(multisitedgp_design(), target_marginal_rho = 0.3),
    "multisitedgp_arg_error"
  )
})
