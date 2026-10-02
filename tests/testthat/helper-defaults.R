# The default design, written out argument by argument so that a test can
# override one argument and leave the rest.
default_design_args <- function(...) {
  utils::modifyList(
    list(
      J = 50L,
      true_dist = "Gaussian",
      tau = 0,
      sigma_tau = 0.20,
      nj_mean = 50,
      cv = 0.50,
      nj_min = 5L,
      p = 0.5,
      R2 = 0,
      engine = "truncated_gamma",
      dependence = "none"
    ),
    list(...)
  )
}

sim_default <- function(seed = NULL, ...) {
  args <- default_design_args(...)
  do.call(sim_multisite, c(args, list(seed = seed)))
}

# Seeds for the replications of a Monte Carlo test. The caller's RNG state is
# left as it was.
replication_seeds <- function(n, seed) {
  withr::with_seed(seed, sample.int(.Machine$integer.max, n))
}

# Consecutive samples of `size` values cut from the sites of one simulation.
# Without dependence and without covariates the sites of a simulation are
# independent draws, so each sample is distributed as a simulation of `size`
# sites, and a few large simulations give as many samples as many small ones.
consecutive_samples <- function(x, size) {
  stopifnot(length(x) %% size == 0L)
  unname(split(x, rep(seq_len(length(x) / size), each = size)))
}

# Sample skewness: the third central moment over the 3/2 power of the second.
sample_skewness <- function(x) {
  centered <- x - mean(x)
  mean(centered^3) / mean(centered^2)^1.5
}

# Site-level covariates for the tests that pass `formula`, `beta` and `data`:
# a standardized prior score and an urban indicator.
school <- function(n = 20L) {
  data.frame(prior = as.numeric(scale(seq_len(n))),
             urban = rep(c(0, 1), length.out = n))
}
