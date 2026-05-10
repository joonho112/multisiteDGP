library(multisiteDGP)

hashes <- vapply(
  seq_len(3),
  function(i) canonical_hash(sim_multisite(preset_education_modest(), seed = 12345L)),
  character(1)
)

dat <- sim_multisite(preset_education_modest(), seed = 12345L)
reordered <- dat[, sort(names(dat))]

stopifnot(length(unique(hashes)) == 1L)
stopifnot(identical(canonical_hash(dat), canonical_hash(reordered)))

cat(
  sprintf(
    "COOKBOOK_RESULT R5.8 status=PASS unique_hashes=%d hash=%s\n",
    length(unique(hashes)),
    hashes[[1L]]
  )
)
