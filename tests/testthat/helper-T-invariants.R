# nolint start: object_usage_linter
t_invariant_default_args <- function(...) {
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
      engine = "A2_modern",
      dependence = "none",
      framing = "superpopulation"
    ),
    list(...)
  )
}

t_invariant_sim_default <- function(seed = NULL, ...) {
  args <- t_invariant_default_args(...)
  do.call(sim_multisite, c(args, list(seed = seed)))
}

t_invariant_seed_stream <- function(n, seed) {
  withr::with_seed(seed, sample.int(.Machine$integer.max, n))
}

t_invariant_jebs_seed_file <- function(seed) {
  test_path(sprintf("_snaps/golden/jebs_appendix_mixture_seed%d.rds", seed))
}

t_invariant_jebs_plain_frame <- function(x) {
  data.frame(
    site_index = as.integer(x$site_index),
    z_j = as.numeric(x$z_j),
    tau_j = as.numeric(x$tau_j),
    tau_j_hat = as.numeric(x$tau_j_hat),
    se_j = as.numeric(x$se_j),
    se2_j = as.numeric(x$se2_j),
    n_j = as.integer(x$n_j)
  )
}
# nolint end
