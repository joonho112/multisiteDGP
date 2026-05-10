library(multisiteDGP)

design <- preset_education_modest(
  sigma_tau = 0.25,
  R2 = 0.40
)
dat <- sim_multisite(design, seed = 91207L)
fi <- feasibility_index(dat, warn = FALSE)

stopifnot(is_multisitedgp_design(design))
stopifnot(is_multisitedgp_data(dat))
stopifnot(is.finite(fi))
stopifnot(attr(dat, "design")$R2 == 0.40)

cat(
  sprintf(
    "COOKBOOK_RESULT R5.2 status=PASS sigma_tau=%.2f R2=%.2f feasibility=%.3f\n",
    attr(dat, "design")$sigma_tau,
    attr(dat, "design")$R2,
    fi
  )
)
