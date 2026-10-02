test_that("without dependence the sites keep their order and no permutation is recorded", {
  df <- sim_default(seed = 820701L, J = 50L, dependence = "none")
  diagnostics <- attr(df, "diagnostics", exact = TRUE)

  expect_identical(df$site_index, seq_len(50L))
  expect_identical(diagnostics$dependence_method, "none")
  expect_identical(diagnostics$permutation, "identity")
  expect_null(attr(df, "permutation_perm", exact = TRUE))
})

test_that("without dependence the rank correlation of tau_j and se2_j stays within the permutation envelope", {
  # 2000 samples of 200 sites, 100 from each of 20 simulations of 20000 sites,
  # against 10000 permutations. The 95th percentile of M values of |rho| has a
  # relative standard error of 0.95 / sqrt(M): 2.1 percent for the samples and
  # 1.0 percent for the envelope, so 2.3 percent for their ratio. The margin of
  # 10 percent in permutation_envelope_x is 4 standard errors.
  J <- 200L
  samples_per_simulation <- 100L
  seeds <- replication_seeds(20L, 820401L)
  actual <- unlist(lapply(seeds, function(seed) {
    df <- sim_default(seed = seed, J = J * samples_per_simulation, dependence = "none")
    vapply(consecutive_samples(seq_len(nrow(df)), J), function(rows) {
      stats::cor(df$tau_j[rows], df$se2_j[rows], method = "spearman")
    }, numeric(1))
  }))
  # The rank correlation of two independent continuous variables is distributed
  # as the correlation of 1:J with a random permutation of 1:J.
  envelope <- withr::with_seed(
    1820401L,
    as.vector(stats::cor(seq_len(J), replicate(10000L, sample.int(J))))
  )

  q95_actual <- unname(stats::quantile(abs(actual), 0.95))
  q95_envelope <- unname(stats::quantile(abs(envelope), 0.95))
  expect_lte(q95_actual, q95_envelope * permutation_envelope_x)
})
