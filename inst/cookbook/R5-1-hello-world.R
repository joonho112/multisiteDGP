library(multisiteDGP)

dat <- sim_multisite(preset_jebs_paper(), seed = 4719L)
diag <- attr(dat, "diagnostics")
effect_plot <- plot_effects(dat, caption = FALSE)

stopifnot(is_multisitedgp_data(dat))
stopifnot(inherits(effect_plot, "ggplot"))
stopifnot(is.finite(diag$I_hat))
stopifnot(nrow(dat) == 50L)

cat(
  sprintf(
    "COOKBOOK_RESULT R5.1 status=PASS J=%d I=%.3f hash=%s\n",
    nrow(dat),
    diag$I_hat,
    canonical_hash(dat)
  )
)
