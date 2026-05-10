library(multisiteDGP)

design <- preset_education_modest(J = 14L, nj_mean = 60)
dat <- sim_multisite(design, seed = 12345L)

checks <- data.frame(
  seed = 12345L + 0:2,
  I = vapply(12345L + 0:2, function(s) informativeness(sim_multisite(design, seed = s)), numeric(1)),
  feasibility = vapply(
    12345L + 0:2,
    function(s) feasibility_index(sim_multisite(design, seed = s), warn = FALSE),
    numeric(1)
  )
)

stopifnot(is.finite(informativeness(dat)))
stopifnot(is.finite(mean_shrinkage(dat)))
stopifnot(all(is.finite(checks$I)))
stopifnot(all(is.finite(checks$feasibility)))

cat(
  sprintf(
    "COOKBOOK_RESULT R5.9 status=PASS J=%d mean_I=%.3f mean_feasibility=%.3f\n",
    design$J,
    mean(checks$I),
    mean(checks$feasibility)
  )
)
