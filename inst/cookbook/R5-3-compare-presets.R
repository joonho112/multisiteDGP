library(multisiteDGP)

small <- sim_multisite(preset_education_small(J = 40L), seed = 33113L)
substantial <- sim_multisite(preset_education_substantial(J = 40L), seed = 33113L)

small_i <- informativeness(small)
substantial_i <- informativeness(substantial)

stopifnot(is.finite(small_i))
stopifnot(is.finite(substantial_i))
stopifnot(substantial_i > small_i)

cat(
  sprintf(
    "COOKBOOK_RESULT R5.3 status=PASS small_I=%.3f substantial_I=%.3f\n",
    small_i,
    substantial_i
  )
)
