library(multisiteDGP)

beta_diff_g <- function(J, ...) {
  flip <- stats::rbinom(J, 1, 0.5)
  raw <- ifelse(flip == 1, stats::rbeta(J, 2, 5), -stats::rbeta(J, 5, 2))
  (raw - mean(raw)) / stats::sd(raw)
}

design <- multisitedgp_design(
  J = 60L,
  true_dist = "User",
  g_fn = beta_diff_g,
  g_returns = "standardized",
  sigma_tau = 0.20
)
dat <- sim_multisite(design, seed = 71823L)
realized_sigma <- stats::sd(dat$z_j) * design$sigma_tau

stopifnot(is_multisitedgp_data(dat))
stopifnot(is.finite(realized_sigma))
stopifnot(abs(mean(dat$z_j)) < 0.25)

cat(
  sprintf(
    "COOKBOOK_RESULT R5.5 status=PASS realized_sigma=%.3f I=%.3f\n",
    realized_sigma,
    informativeness(dat)
  )
)
