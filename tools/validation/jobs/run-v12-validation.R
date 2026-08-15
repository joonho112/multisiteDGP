#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg) == 1L) {
  normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE)
} else {
  normalizePath("tools/validation/jobs/run-v12-validation.R", mustWork = TRUE)
}
source(file.path(dirname(script_path), "..", "R", "validation-harness.R"))

paths <- validation_paths(script_path)
validation_load_package(paths$package_root)

experiment_id <- "V12"
mode <- Sys.getenv("MULTISITEDGP_VALIDATION_MODE", unset = "full")
seed_root <- validation_env_int("MULTISITEDGP_VALIDATION_SEED_ROOT", 911201L)
lee_seed <- validation_env_int("MULTISITEDGP_VALIDATION_LEE_SEED", 4719L)
resume <- validation_env_flag("MULTISITEDGP_VALIDATION_RESUME", default = TRUE)
overwrite <- validation_env_flag("MULTISITEDGP_VALIDATION_OVERWRITE", default = FALSE)
attempt_site_lme4 <- validation_env_flag("MULTISITEDGP_VALIDATION_ATTEMPT_SITE_LME4", default = FALSE)
run_id <- validation_run_id(experiment_id, mode)

result_path <- file.path(paths$generated_dir, paste0(run_id, "-results.csv"))
summary_path <- file.path(paths$generated_dir, paste0(run_id, "-summary.csv"))
figure_path <- file.path(paths$generated_dir, paste0(run_id, "-jebs-shrinkage.png"))
started_at <- Sys.time()
acceptance_rule_version <- "phase9-v12-pattern-evidence-v2"

if (isTRUE(resume) && !isTRUE(overwrite) && validation_existing_run_complete(result_path, summary_path)) {
  summary <- utils::read.csv(summary_path, stringsAsFactors = FALSE)
  status <- if (nrow(summary) == 1L && isTRUE(summary$acceptance_pass[[1L]])) "pass" else "fail"
  ended_at <- Sys.time()
  validation_record_manifest(paths, run_id, experiment_id, mode, status, started_at, ended_at, seed_root, summary$n_sites[[1L]], script_path, result_path, summary_path, "Existing V12 output reused.")
  message("Resumed existing V12 output: ", result_path)
  validation_maybe_stop_for_blocker(status, "V12 validation failed in resumed output.")
  quit(status = 0)
}

# Pinned against hash schema v3. canonical_hash() carries the schema version in
# its payload, so a schema change moves this value even when the data is
# untouched — which is exactly what v1 -> v3 did (Phase 4, defect D-002). The
# previous pin, a96eaabd1c022e32, was a v1 value.
#
# If this check fails, read schema_matches_expected first. Schema moved is a
# documented decision; data moved is a regression, and the authority on that is
# the golden .rds set in tests/testthat/_snaps/golden, which compares exactly
# and does not depend on the schema. V02 sidesteps this entirely by hashing
# those fixtures at run time instead of pinning a literal.
expected_hash <- "df7a9af6d6f144c1"
expected_hash_schema <- "multisiteDGP-canonical-hash-v3"
actual_hash_schema <- multisiteDGP:::.hash_schema_version()
design <- multisiteDGP::preset_jebs_paper()
dat <- multisiteDGP::sim_multisite(design, seed = lee_seed)
provenance <- attr(dat, "provenance", exact = TRUE)
diagnostics <- attr(dat, "diagnostics", exact = TRUE)
retention <- multisiteDGP::compute_shrinkage(dat$se2_j, sigma_tau = design$sigma_tau)
posterior_mean <- design$tau + retention * (dat$tau_j_hat - design$tau)
raw_distance <- abs(dat$tau_j_hat - design$tau)
posterior_distance <- abs(posterior_mean - design$tau)
shrinkage_amount <- abs(dat$tau_j_hat - posterior_mean)
shrinkage_fraction <- ifelse(raw_distance > sqrt(.Machine$double.eps), shrinkage_amount / raw_distance, NA_real_)

lme4_available <- requireNamespace("lme4", quietly = TRUE)
lme4_status <- if (lme4_available) "not_applicable_site_summary_no_ipd" else "skipped_missing_lme4"
lme4_message <- if (lme4_available) {
  paste(
    "V12 validates site-summary shrinkage-pattern evidence.",
    "The Lee Figure 4(b) lme4 model requires individual-level input or",
    "digitized figure targets, neither of which is part of the repo-local v1 evidence."
  )
} else {
  "lme4 is not installed; V12 lme4 exact reproduction is deferred pending external IPD/reference targets."
}
lme4_singular <- NA
if (isTRUE(attempt_site_lme4) && lme4_available) {
  site_df <- as.data.frame(dat)
  site_df$site_factor <- factor(site_df$site_index)
  fit <- try(
    lme4::lmer(
      tau_j_hat ~ 1 + (1 | site_factor),
      weights = 1 / se2_j,
      data = site_df,
      REML = TRUE
    ),
    silent = TRUE
  )
  if (inherits(fit, "try-error")) {
    lme4_status <- "failed_site_level_one_row_per_site"
    lme4_message <- attr(fit, "condition")$message
  } else {
    lme4_status <- "completed_site_level_fit"
    lme4_singular <- lme4::isSingular(fit)
    lme4_message <- "Site-level lmer completed, but this is not the original individual-level Lee et al. model."
  }
}

plot_data <- data.frame(
  site_index = dat$site_index,
  se2_j = dat$se2_j,
  se_j = dat$se_j,
  tau_j = dat$tau_j,
  tau_j_hat = dat$tau_j_hat,
  posterior_mean = posterior_mean,
  retention = retention,
  shrinkage_fraction = shrinkage_fraction,
  stringsAsFactors = FALSE
)
plot_data$site_order <- seq_len(nrow(plot_data))
plot_data <- plot_data[order(plot_data$se2_j, decreasing = TRUE), , drop = FALSE]
plot_data$site_order <- seq_len(nrow(plot_data))

long_plot <- rbind(
  data.frame(site_order = plot_data$site_order, effect = plot_data$tau_j_hat, series = "Observed", stringsAsFactors = FALSE),
  data.frame(site_order = plot_data$site_order, effect = plot_data$posterior_mean, series = "Shrinkage target", stringsAsFactors = FALSE),
  data.frame(site_order = plot_data$site_order, effect = plot_data$tau_j, series = "Latent truth", stringsAsFactors = FALSE)
)
p <- ggplot2::ggplot(long_plot, ggplot2::aes(x = site_order, y = effect, color = series)) +
  ggplot2::geom_hline(yintercept = design$tau, linewidth = 0.6, linetype = "dashed", color = "grey55") +
  ggplot2::geom_line(linewidth = 0.65, alpha = 0.85) +
  ggplot2::geom_point(size = 1.7, alpha = 0.9) +
  ggplot2::scale_color_manual(values = c("Observed" = "#2A6FBB", "Shrinkage target" = "#C44E52", "Latent truth" = "#4C9F70")) +
  ggplot2::labs(
    title = "JEBS UX preset shrinkage pattern",
    subtitle = sprintf("preset_jebs_paper(seed = %s) | J = %s | sigma_tau = %.2f | canonical_hash=%s", lee_seed, nrow(dat), design$sigma_tau, provenance$canonical_hash),
    x = "Site ordered by sampling variance (largest to smallest)",
    y = "Site effect",
    color = NULL,
    caption = "Pattern-level validation only: preset_jebs_paper uses UX sigma_tau=0.20; exact Lee Figure 4(b) parity belongs to preset_jebs_strict/V02."
  ) +
  ggplot2::theme_minimal(base_size = 13) +
  ggplot2::theme(legend.position = "bottom")
plot_device <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else "png"
ggplot2::ggsave(figure_path, p, width = 8, height = 5, units = "in", dpi = 150, device = plot_device)

results <- data.frame(
  result_schema_version = "phase9-validation-v1",
  run_id = run_id,
  experiment_id = experiment_id,
  mode = mode,
  site_index = dat$site_index,
  lee_seed = lee_seed,
  tau_j = dat$tau_j,
  tau_j_hat = dat$tau_j_hat,
  se_j = dat$se_j,
  se2_j = dat$se2_j,
  n_j = dat$n_j,
  retention = retention,
  posterior_mean = posterior_mean,
  raw_distance = raw_distance,
  posterior_distance = posterior_distance,
  shrinkage_amount = shrinkage_amount,
  shrinkage_fraction = shrinkage_fraction,
  canonical_hash = provenance$canonical_hash,
  stringsAsFactors = FALSE
)
result_path <- validation_write_csv(results, result_path)

finite_shrinkage <- all(is.finite(retention)) && all(retention >= 0 & retention <= 1)
monotone_retention <- isTRUE(all.equal(
  stats::cor(dat$se2_j, retention, method = "spearman"),
  -1,
  tolerance = 1e-12
))
posterior_contracts <- all(posterior_distance <= raw_distance + 1e-12)
mean_abs_improvement <- mean(posterior_distance) <= mean(raw_distance)
figure_render_pass <- file.exists(figure_path) && file.info(figure_path)$size > 5000
lee_reference_available <- FALSE
pattern_evidence_pass <- nrow(dat) == 50L &&
  identical(provenance$canonical_hash, expected_hash) &&
  finite_shrinkage &&
  monotone_retention &&
  posterior_contracts &&
  mean_abs_improvement &&
  figure_render_pass
external_reference_deferred <- !lee_reference_available
acceptance_pass <- pattern_evidence_pass && external_reference_deferred

summary <- data.frame(
  run_id = run_id,
  experiment_id = experiment_id,
  mode = mode,
  acceptance_rule_version = acceptance_rule_version,
  seed_root = seed_root,
  lee_seed = lee_seed,
  n_sites = nrow(dat),
  expected_sites = 50L,
  preset_sigma_tau = design$sigma_tau,
  canonical_hash = provenance$canonical_hash,
  expected_hash = expected_hash,
  hash_matches_expected = identical(provenance$canonical_hash, expected_hash),
  hash_schema = actual_hash_schema,
  expected_hash_schema = expected_hash_schema,
  schema_matches_expected = identical(actual_hash_schema, expected_hash_schema),
  diagnostics_I = diagnostics$I_hat,
  diagnostics_R = diagnostics$R_hat,
  diagnostics_rho_S = diagnostics$rho_S_residual,
  mean_retention = mean(retention),
  min_retention = min(retention),
  max_retention = max(retention),
  mean_raw_abs_effect = mean(raw_distance),
  mean_posterior_abs_effect = mean(posterior_distance),
  finite_shrinkage = finite_shrinkage,
  monotone_retention_vs_se2 = monotone_retention,
  posterior_contracts_toward_tau = posterior_contracts,
  mean_abs_improvement = mean_abs_improvement,
  lme4_available = lme4_available,
  attempt_site_lme4 = attempt_site_lme4,
  lme4_status = lme4_status,
  lme4_singular = lme4_singular,
  lme4_message = lme4_message,
  lee_figure_numeric_reference_available = lee_reference_available,
  lee_figure_numeric_reference_gate = "deferred_external_reference",
  external_reference_deferred = external_reference_deferred,
  figure_path = .relative_to_root(figure_path, paths$package_root),
  figure_sha256 = validation_file_hash(figure_path),
  figure_render_pass = figure_render_pass,
  pattern_evidence_pass = pattern_evidence_pass,
  acceptance_pass = acceptance_pass,
  acceptance_note = "V12 accepts reproducible qualitative shrinkage-pattern evidence for the UX-anchored preset_jebs_paper() artifact. Exact Lee Figure 4(b) / lme4 reproduction is deferred until repo-controlled individual-level Lee model input or digitized numeric targets are available.",
  stringsAsFactors = FALSE
)
summary_path <- validation_write_csv(summary, summary_path)

status <- if (isTRUE(summary$acceptance_pass)) "pass" else "fail"
ended_at <- Sys.time()
validation_record_manifest(paths, run_id, experiment_id, mode, status, started_at, ended_at, seed_root, nrow(dat), script_path, result_path, summary_path, "V12 JEBS qualitative pattern evidence; exact Lee lme4/Figure target deferred pending external reference.")
print(summary)
message("V12 status: ", status)
validation_maybe_stop_for_blocker(status, "V12 validation failed acceptance criteria.")
