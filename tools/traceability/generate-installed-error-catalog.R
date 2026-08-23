#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg) == 1L) {
  normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE)
} else {
  normalizePath("tools/traceability/generate-installed-error-catalog.R", mustWork = TRUE)
}
package_root <- normalizePath(file.path(dirname(script_path), "..", ".."), mustWork = TRUE)
source_path <- file.path(package_root, "tools", "traceability", "error-index.csv")
output_path <- file.path(package_root, "inst", "extdata", "error-catalog.csv")

catalog <- read.csv(source_path, stringsAsFactors = FALSE)
api <- c(
  E01 = "multisitedgp_design()", E02 = "multisitedgp_design()",
  E03 = "multisitedgp_design(); gen_site_sizes()",
  E04 = "multisitedgp_design(); sim_multisite()",
  E05 = "sim_meta()", E06 = "multisitedgp_design(); gen_effects()",
  E07 = "gen_effects(); gen_effects_studentt()",
  E08 = "multisitedgp_design()", E09 = "sim_meta()",
  E10 = "sim_multisite()", E11 = "sim_meta()",
  E12 = "gen_effects_ald()", E13 = "gen_effects_studentt()",
  E14 = "gen_effects_user()", E15 = "gen_site_sizes()",
  E16 = "gen_se_direct()", E17 = "align_rank_corr()",
  E18 = "gen_observations()", E19 = "gen_effects_skewn()",
  E20 = "gen_effects_ald()", E21 = "gen_effects_pmslab()",
  E22 = "gen_effects(); gen_effects_dpm()",
  E23 = "multisitedgp_design()", E24 = "reserved v2 PSD validator",
  E25 = "align_rank_corr(); align_hybrid_corr()",
  E26 = "as_metafor()", E27 = "as_baggr()",
  E28 = "feasibility_index()", E29 = "multisitedgp_design()",
  E30 = "as_metafor(); as_baggr(); as_multisitepower()"
)
remedy <- c(
  E01 = "Use integer J >= 10.",
  E02 = "Use sigma_tau >= 0; use 0 only for a degenerate no-heterogeneity design.",
  E03 = "Use nj_mean > nj_min, except cv = 0 may use equality.",
  E04 = "Use A1_legacy with dependence = 'none', or switch to A2_modern.",
  E05 = "Remove site-size arguments and specify the direct path with I and R.",
  E06 = "Use true_dist = 'User' (or bridged 'DPM') when supplying g_fn.",
  E07 = "Pass theta_G = list(nu = value) with nu > 2.",
  E08 = "Use one target scale: rank_corr or pearson_corr, not both.",
  E09 = "Use sim_multisite() for nj_mean/cv/nj_min or remove those arguments.",
  E10 = "Use sim_meta() for I/R designs, or remove I from sim_multisite().",
  E11 = "Pass I in (0, 1) to sim_meta().",
  E12 = "Use ALD rho strictly inside (0.05, 0.95).",
  E13 = "Use Student-t nu > 2.",
  E14 = "Use a g_fn that returns a finite numeric vector of exactly length J.",
  E15 = "Use a design inside the A2 feasible region; do not bypass failed moment verification.",
  E16 = "Use I strictly between 0 and 1.",
  E17 = "Use a feasible target/tolerance, more variable margins, or dependence = 'none'.",
  E18 = "Use an obs_fn that returns a finite numeric vector of exactly length J.",
  E19 = "Use install.packages('sn'), or choose another shape.",
  E20 = "Use install.packages('LaplacesDemon'), or choose another shape.",
  E21 = "Use pi0 strictly between 0 and 1.",
  E22 = "Use true_dist = 'User' with g_fn, or pass a g_fn bridge to the reserved DPM route.",
  E23 = "Use target_marginal_rho = NULL in v0.2.0.",
  E24 = "Use this row only as a reserved marker; re-evaluate it when the marginal-target API is implemented.",
  E25 = "Use a dependence_fn that returns an exact permutation of the upstream precision multiset.",
  E26 = "Use install.packages('metafor') before an adapter path that requires it.",
  E27 = "Use install.packages('baggr') before an adapter path that requires it.",
  E28 = "Try increasing site information/J or reconsider partial pooling; this ID is a warning, not an abort.",
  E29 = "Use paradigm = 'direct' with se_fn, or remove se_fn from a site-size design.",
  E30 = "Remove or rename input columns reserved by the target adapter before conversion."
)

stopifnot(identical(sort(names(api)), sort(catalog$id)))
stopifnot(identical(sort(names(remedy)), sort(catalog$id)))
stopifnot(all(grepl("^(Try|Use|Pass|Remove)\\b", remedy)))
stopifnot(all(grepl("[.]$", remedy)))

installed <- data.frame(
  id = catalog$id,
  condition = catalog$scenario,
  class = catalog$class,
  active_v0_2 = catalog$active_v1,
  api = unname(api[catalog$id]),
  remedy = unname(remedy[catalog$id]),
  status = catalog$status,
  stringsAsFactors = FALSE
)
write.csv(installed, output_path, row.names = FALSE, quote = TRUE)
message("Wrote installed error catalog to: inst/extdata/error-catalog.csv")
