library(multisiteDGP)

dat <- sim_multisite(preset_education_modest(), seed = 50421L)
sigma_tau <- attr(dat, "design")$sigma_tau

if (requireNamespace("metafor", quietly = TRUE)) {
  mf <- as_metafor(dat)
} else {
  mf <- tibble::tibble(
    yi = dat$tau_j_hat,
    vi = dat$se2_j,
    sei = dat$se_j
  )
}

if (requireNamespace("baggr", quietly = TRUE)) {
  bg <- as_baggr(dat, include_truth = TRUE)
} else {
  bg <- tibble::tibble(
    tau = dat$tau_j_hat,
    se = dat$se_j,
    tau_true = dat$tau_j
  )
}

i_before <- compute_I(dat$se2_j, sigma_tau = sigma_tau)
i_after <- compute_I(mf$vi, sigma_tau = sigma_tau)

stopifnot(all(c("yi", "vi", "sei") %in% names(mf)))
stopifnot(all(c("tau", "se", "tau_true") %in% names(bg)))
stopifnot(isTRUE(all.equal(i_before, i_after, tolerance = 1e-12)))

cat(
  sprintf(
    "COOKBOOK_RESULT R5.4 status=PASS I_delta=%.3e metafor_guard=%s baggr_guard=%s\n",
    abs(i_before - i_after),
    requireNamespace("metafor", quietly = TRUE),
    requireNamespace("baggr", quietly = TRUE)
  )
)
