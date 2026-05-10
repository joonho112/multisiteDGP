# nolint start: object_usage_linter
scenario_relaxed_thresholds <- function(...) {
  modifyList(
    modifyList(
      default_thresholds(),
      list(
        mean_shrinkage_min = 0,
        feasibility_min = 0,
        R_max = 1e6,
        bhattacharyya_min = 0,
        ks_max = 1
      )
    ),
    list(...)
  )
}

scenario_metric_frame <- function(
  mean_shrinkage,
  feasibility_efron,
  R_hat,
  bhattacharyya,
  ks
) {
  n <- length(mean_shrinkage)
  tibble::tibble(
    cell_id = 1L,
    rep = seq_len(n),
    seed = seq_len(n),
    I_hat = rep(0.30, n),
    R_hat = R_hat,
    mean_shrinkage = mean_shrinkage,
    feasibility_efron = feasibility_efron,
    feasibility_morris = feasibility_efron,
    bhattacharyya = bhattacharyya,
    ks = ks
  )
}

test_that("Step 7.6 scenario_audit is exported and summarizes cells", {
  exports <- getNamespaceExports("multisiteDGP")
  expect_true("scenario_audit" %in% exports)

  grid <- design_grid(
    paradigm = "direct",
    J = c(10L, 30L),
    I = 0.2,
    R = 1,
    shuffle = FALSE,
    seed_root = 7601L
  )
  audit <- scenario_audit(
    grid,
    M = 2L,
    thresholds = scenario_relaxed_thresholds(feasibility_min = 5)
  )

  expect_s3_class(audit, "tbl_df")
  expect_equal(nrow(audit), nrow(grid))
  expect_true(all(c(
    "cell_id", "J", "I", "R", "M", "seed_root", "design_hash",
    "status", "pass", "n_violations", "fail_reasons", "warn_reasons",
    "med_I_hat", "med_R_hat", "med_mean_shrinkage",
    "med_feasibility_efron", "med_bhattacharyya", "med_ks"
  ) %in% names(audit)))
  expect_identical(audit$cell_id, seq_len(nrow(grid)))
  expect_identical(audit$M, c(2L, 2L))
  expect_identical(audit$status, c("FAIL", "PASS"))
  expect_identical(audit$pass, c(FALSE, TRUE))
  expect_match(audit$fail_reasons[[1L]], "feasibility", fixed = TRUE)
  expect_identical(audit$fail_reasons[[2L]], "")
  expect_true(all(is.finite(audit$med_I_hat)))
  expect_true(all(grepl("^[0-9a-f]+$", audit$design_hash)))
})

test_that("scenario metric summarizer classifies PASS, WARN, and FAIL gates", {
  summarize <- multisitedgp_internal(".summarize_scenario_metrics")
  thresholds <- scenario_relaxed_thresholds(
    mean_shrinkage_min = 0.50,
    feasibility_min = 10,
    R_max = 2,
    bhattacharyya_min = 0.80,
    ks_max = 0.20
  )

  pass <- summarize(
    scenario_metric_frame(
      mean_shrinkage = rep(0.70, 5L),
      feasibility_efron = rep(20, 5L),
      R_hat = rep(1.2, 5L),
      bhattacharyya = rep(0.90, 5L),
      ks = rep(0.10, 5L)
    ),
    thresholds = thresholds
  )
  warn <- summarize(
    scenario_metric_frame(
      mean_shrinkage = c(0.20, 0.70, 0.70, 0.70, 0.70),
      feasibility_efron = rep(20, 5L),
      R_hat = rep(1.2, 5L),
      bhattacharyya = rep(0.90, 5L),
      ks = rep(0.10, 5L)
    ),
    thresholds = thresholds
  )
  fail <- summarize(
    scenario_metric_frame(
      mean_shrinkage = rep(0.20, 5L),
      feasibility_efron = rep(5, 5L),
      R_hat = rep(Inf, 5L),
      bhattacharyya = rep(0.50, 5L),
      ks = rep(0.50, 5L)
    ),
    thresholds = thresholds
  )

  expect_identical(pass$status, "PASS")
  expect_true(pass$pass)
  expect_identical(pass$fail_reasons, "")
  expect_identical(pass$warn_reasons, "")

  expect_identical(warn$status, "WARN")
  expect_true(warn$pass)
  expect_match(warn$warn_reasons, "mean_shrinkage", fixed = TRUE)
  expect_identical(warn$fail_reasons, "")

  expect_identical(fail$status, "FAIL")
  expect_false(fail$pass)
  expect_gt(fail$n_violations, 0L)
  expect_match(fail$fail_reasons, "R:median", fixed = TRUE)
  expect_identical(fail$warn_reasons, "")
})

test_that("scenario_audit is deterministic and RNG-neutral", {
  grid <- design_grid(
    paradigm = "direct",
    J = c(20L, 25L),
    I = 0.4,
    R = 1,
    shuffle = FALSE,
    seed_root = 7602L
  )

  set.seed(7602L)
  before <- get(".Random.seed", envir = .GlobalEnv)
  audit_one <- scenario_audit(grid, M = 2L, thresholds = scenario_relaxed_thresholds())
  after <- get(".Random.seed", envir = .GlobalEnv)
  audit_two <- scenario_audit(grid, M = 2L, thresholds = scenario_relaxed_thresholds())

  expect_identical(after, before)
  expect_identical(audit_one, audit_two)
})

test_that("scenario_audit routes both front-door paradigms", {
  site_grid <- design_grid(
    J = 12L,
    sigma_tau = 0.2,
    nj_mean = 50,
    seed_root = 7603L
  )
  direct_grid <- design_grid(
    paradigm = "direct",
    J = 12L,
    I = 0.5,
    R = 1,
    shuffle = FALSE,
    seed_root = 7604L
  )

  site_audit <- scenario_audit(site_grid, M = 1L, thresholds = scenario_relaxed_thresholds())
  direct_audit <- scenario_audit(direct_grid, M = 1L, thresholds = scenario_relaxed_thresholds())

  expect_equal(nrow(site_audit), 1L)
  expect_equal(nrow(direct_audit), 1L)
  expect_identical(site_audit$cell_id, 1L)
  expect_identical(direct_audit$cell_id, 1L)
  expect_true(is.logical(site_audit$pass))
  expect_true(is.logical(direct_audit$pass))
})

test_that("scenario_audit can use fixed design seeds when grid seed stream is off", {
  base <- multisitedgp_design(seed = 7605L)
  grid <- design_grid(
    J = c(12L, 14L),
    base_design = base,
    seed_stream = FALSE
  )

  audit <- scenario_audit(grid, M = 1L, thresholds = scenario_relaxed_thresholds())

  expect_equal(nrow(audit), 2L)
  expect_identical(audit$seed_root, c(7605L, 7605L))
})

test_that("scenario_audit validates inputs with classed errors", {
  grid <- design_grid(J = 12L, seed_root = 7606L)
  seedless <- design_grid(J = 12L, seed_stream = FALSE)
  malformed <- grid
  malformed$design[[1L]] <- list()

  expect_multisitedgp_error(
    scenario_audit(data.frame(x = 1), M = 1L),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    scenario_audit(seedless, M = 1L),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    scenario_audit(malformed, M = 1L),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    scenario_audit(grid, M = 0L),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    scenario_audit(grid, M = 1.5),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    scenario_audit(grid, M = 1L, thresholds = list(not_a_gate = 1)),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    scenario_audit(grid, M = 1L, thresholds = list(R_max = c(1, 2))),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    scenario_audit(grid, M = 1L, thresholds = list(ks_max = 2)),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    scenario_audit(grid, M = 1L, parallel = NA),
    "multisitedgp_arg_error"
  )
})

test_that("Step 7.6 scenario_audit traceability status is synchronized", {
  trace_dir <- test_path("../../tools/traceability")
  skip_if_not(dir.exists(trace_dir), "Development-only traceability ledgers are not shipped in the package tarball.")

  api <- read.csv(file.path(trace_dir, "api-index.csv"), stringsAsFactors = FALSE)
  row <- match("API036", api$id)

  expect_identical(api$status[row], "implemented")
  expect_identical(api$owner_step[row], "Step 7.6")
  expect_match(api$unresolved_conflict[row], "cell-level summary", fixed = TRUE)
})
# nolint end
