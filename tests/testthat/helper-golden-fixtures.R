# nolint start: object_usage_linter
golden_live_preset_specs <- function() {
  list(
    preset_jebs_paper.rds = sim_multisite(preset_jebs_paper(), seed = 4719L),
    preset_jebs_strict.rds = sim_multisite(preset_jebs_strict(), seed = 4719L),
    preset_education_modest.rds = sim_multisite(preset_education_modest(), seed = 12345L),
    preset_walters_2024.rds = sim_multisite(preset_walters_2024(), seed = 1L),
    preset_small_area_estimation.rds = sim_meta(preset_small_area_estimation(), seed = 42L)
  )
}

golden_jebs_object <- function(J, nj_mean, cv, seed) {
  multisitedgp_internal(".with_reproducible_seed")(seed, {
    eps <- 0.3
    delta <- 5
    ups <- 2
    sigma_tau <- 0.15
    scale <- sqrt((1 - eps) + eps * ups^2 + eps * (1 - eps) * delta^2)
    component_one <- stats::runif(J) < (1 - eps)
    tau_j <- component_one * stats::rnorm(J, -eps * delta / scale, sqrt(1 / scale^2)) +
      (1 - component_one) *
        stats::rnorm(J, (1 - eps) * delta / scale, sqrt(ups^2 / scale^2))
    tau_j <- tau_j * sigma_tau

    shape <- 1 / cv^2
    rate <- shape / nj_mean
    n_j <- round(pmax(5L, stats::rgamma(J, shape = shape, rate = rate)), 0)
    se2_j <- 4 / n_j
    permutation <- base::sample.int(J, size = J)
    n_j <- n_j[permutation]
    se2_j <- se2_j[permutation]
    tau_j_hat <- vapply(
      seq_len(J),
      function(idx) stats::rnorm(1L, tau_j[[idx]], sqrt(se2_j[[idx]])),
      numeric(1)
    )

    out <- data.frame(
      site_index = seq_len(J),
      z_j = tau_j / sigma_tau,
      tau_j = tau_j,
      tau_j_hat = tau_j_hat,
      se_j = sqrt(se2_j),
      se2_j = se2_j,
      n_j = as.integer(n_j)
    )
    out[c("site_index", "z_j", "tau_j", "tau_j_hat", "se_j", "se2_j", "n_j")]
  })
}

golden_live_jebs_specs <- function() {
  seeds <- c(42L, 1L, 2024L, 12345L)
  regular <- lapply(seeds, function(seed) {
    golden_jebs_object(J = 100L, nj_mean = 80, cv = 0.50, seed = seed)
  })
  names(regular) <- sprintf("jebs_appendix_mixture_seed%d.rds", seeds)
  c(
    regular,
    list(
      jebs_appendix_floor_active_seed42.rds =
        golden_jebs_object(J = 300L, nj_mean = 10, cv = 0.75, seed = 42L)
    )
  )
}
# nolint end
