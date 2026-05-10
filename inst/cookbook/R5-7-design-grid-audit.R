library(multisiteDGP)

grid <- design_grid(
  paradigm = "direct",
  I = c(0.20, 0.40),
  R = c(1, 3),
  J = c(14L, 30L),
  sigma_tau = 0.20,
  true_dist = "Gaussian",
  seed_root = 20260507L
)

audit <- scenario_audit(grid, M = 1L, parallel = FALSE)

stopifnot(inherits(grid, "multisitedgp_design_grid"))
stopifnot(nrow(grid) == 8L)
stopifnot(nrow(audit) == nrow(grid))
stopifnot("pass" %in% names(audit))

cat(
  sprintf(
    "COOKBOOK_RESULT R5.7 status=PASS cells=%d audit_hash=%s\n",
    nrow(audit),
    canonical_hash(audit)
  )
)
