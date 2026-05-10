library(multisiteDGP)

custom_rank <- function(z_j, se2_j, target, ...) {
  se_order <- order(se2_j)
  z_order <- order(z_j)
  perm <- integer(length(se2_j))
  if (target >= 0) {
    perm[z_order] <- se_order
  } else {
    perm[z_order] <- rev(se_order)
  }
  list(se2_j = se2_j[perm], perm = as.integer(perm))
}

design <- multisitedgp_design(
  J = 40L,
  sigma_tau = 0.20,
  dependence = "rank",
  rank_corr = 0.40,
  dependence_fn = custom_rank,
  seed = 4719L
)
dat <- sim_multisite(design)
rho <- realized_rank_corr(dat)

bad_dep <- function(z_j, se2_j, target, ...) {
  list(se2_j = se2_j * 2, perm = seq_along(se2_j))
}
bad_design <- update_multisitedgp_design(design, dependence_fn = bad_dep)
bad_caught <- tryCatch(
  {
    sim_multisite(bad_design)
    FALSE
  },
  error = function(e) TRUE
)

stopifnot(is.finite(rho))
stopifnot(isTRUE(bad_caught))

cat(
  sprintf(
    "COOKBOOK_RESULT R5.6 status=PASS rho=%.3f bad_callback_caught=%s\n",
    rho,
    bad_caught
  )
)
