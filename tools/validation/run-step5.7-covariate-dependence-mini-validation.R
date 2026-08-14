#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg) == 1L) {
  normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE)
} else {
  normalizePath("tools/validation/run-step5.7-covariate-dependence-mini-validation.R", mustWork = TRUE)
}

package_root <- normalizePath(file.path(dirname(script_path), "..", ".."), mustWork = TRUE)
output_dir <- file.path(package_root, "tools", "validation", "generated")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("pkgload is required to load multisiteDGP from source.", call. = FALSE)
}
pkgload::load_all(package_root, quiet = TRUE)

shape_specs <- list(
  Gaussian = list(true_dist = "Gaussian", theta_G = list(), g_fn = NULL, audit_g = TRUE),
  StudentT = list(true_dist = "StudentT", theta_G = list(nu = 5), g_fn = NULL, audit_g = TRUE),
  SkewN = list(true_dist = "SkewN", theta_G = list(slant = 0.7), g_fn = NULL, audit_g = TRUE),
  ALD = list(true_dist = "ALD", theta_G = list(rho = 0.5), g_fn = NULL, audit_g = TRUE),
  Mixture = list(true_dist = "Mixture", theta_G = list(delta = 5, eps = 0.3, ups = 2), g_fn = NULL, audit_g = TRUE),
  PointMassSlab = list(
    true_dist = "PointMassSlab",
    theta_G = list(pi0 = 0.3, slab_shape = "Gaussian"),
    g_fn = NULL,
    audit_g = TRUE
  ),
  User = list(
    true_dist = "User",
    theta_G = list(),
    g_fn = function(...) {
      args <- list(...)
      n_sites <- args$J
      (stats::rchisq(n_sites, df = 3) - 3) / sqrt(6)
    },
    audit_g = FALSE
  )
)

rank_corr_target <- -0.30
n_sites <- 200L
seeds <- c(5701L, 5702L, 5703L)
sigma_tau <- 0.20
beta <- 1.5
tolerance_continuous <- 0.02
tolerance_ties <- 0.05

realized_spearman <- getFromNamespace(".realized_spearman", "multisiteDGP")

run_one <- function(shape, spec, seed) {
  withr::with_seed(seed, {
    data_x <- tibble::tibble(x_site = seq(-1, 1, length.out = n_sites))
    effects <- multisiteDGP::gen_effects(
      J = n_sites,
      true_dist = spec$true_dist,
      tau = 0,
      sigma_tau = sigma_tau,
      theta_G = spec$theta_G,
      formula = ~ x_site,
      beta = beta,
      data = data_x,
      g_fn = spec$g_fn,
      audit_g = spec$audit_g
    )
    margins <- multisiteDGP::gen_site_sizes(
      effects,
      J = n_sites,
      nj_mean = 80,
      cv = 0.50,
      nj_min = 5L,
      p = 0.5,
      R2 = 0,
      engine = "A2_modern"
    )
    dependent <- multisiteDGP::align_rank_corr(
      margins,
      rank_corr = rank_corr_target,
      max_iter = 20000L,
      tol = if (identical(shape, "PointMassSlab")) tolerance_ties else tolerance_continuous
    )
    observed <- multisiteDGP::gen_observations(dependent)
    diag <- attr(dependent, "dependence_diagnostics", exact = TRUE)
    residual_spearman <- realized_spearman(observed$z_j, observed$se2_j)
    marginal_spearman <- realized_spearman(observed$tau_j, observed$se2_j)
    tolerance <- if (identical(shape, "PointMassSlab")) tolerance_ties else tolerance_continuous

    data.frame(
      shape = shape,
      seed = seed,
      status = "completed",
      J = n_sites,
      sigma_tau = sigma_tau,
      beta = beta,
      dependence_method = "rank",
      target_residual_spearman = rank_corr_target,
      tolerance = tolerance,
      achieved_residual_spearman = unname(residual_spearman),
      achieved_marginal_spearman = unname(marginal_spearman),
      residual_abs_error = abs(unname(residual_spearman) - rank_corr_target),
      residual_pass = abs(unname(residual_spearman) - rank_corr_target) <= tolerance,
      marginal_diff_from_target = unname(marginal_spearman) - rank_corr_target,
      two_number_distinct = abs(unname(marginal_spearman) - unname(residual_spearman)) >= 0.05,
      se2_multiset_preserved = identical(unname(sort(margins$se2_j)), unname(sort(dependent$se2_j))),
      n_unique_z = length(unique(observed$z_j)),
      has_tau_j_hat = "tau_j_hat" %in% names(observed),
      tau_j_hat_finite = all(is.finite(observed$tau_j_hat)),
      diagnostics_target_type = diag$target_type,
      diagnostics_converged = isTRUE(diag$converged),
      diagnostics_iterations = diag$iterations,
      stringsAsFactors = FALSE
    )
  })
}

rows <- list()
idx <- 0L
for (shape in names(shape_specs)) {
  for (seed in seeds) {
    idx <- idx + 1L
    rows[[idx]] <- run_one(shape, shape_specs[[shape]], seed)
  }
}

rows[[length(rows) + 1L]] <- data.frame(
  shape = "DPM",
  seed = NA_integer_,
  status = "skipped_v1_stub",
  J = n_sites,
  sigma_tau = sigma_tau,
  beta = beta,
  dependence_method = "rank",
  target_residual_spearman = rank_corr_target,
  tolerance = NA_real_,
  achieved_residual_spearman = NA_real_,
  achieved_marginal_spearman = NA_real_,
  residual_abs_error = NA_real_,
  residual_pass = NA,
  marginal_diff_from_target = NA_real_,
  two_number_distinct = NA,
  se2_multiset_preserved = NA,
  n_unique_z = NA_integer_,
  has_tau_j_hat = NA,
  tau_j_hat_finite = NA,
  diagnostics_target_type = "skipped",
  diagnostics_converged = NA,
  diagnostics_iterations = NA_integer_,
  stringsAsFactors = FALSE
)

results <- do.call(rbind, rows)
csv_path <- file.path(output_dir, "step5.7-covariate-dependence-mini-validation.csv")
write.csv(results, csv_path, row.names = FALSE, quote = TRUE)

summary_by_shape <- stats::aggregate(
  cbind(residual_pass, two_number_distinct, se2_multiset_preserved, tau_j_hat_finite) ~ shape + status,
  data = results[results$status == "completed", ],
  FUN = mean
)
summary_path <- file.path(output_dir, "step5.7-covariate-dependence-mini-validation-summary.csv")
write.csv(summary_by_shape, summary_path, row.names = FALSE, quote = TRUE)

print(results)
print(summary_by_shape)
message("Wrote results to: ", csv_path)
message("Wrote summary to: ", summary_path)

if (!all(results$residual_pass[results$status == "completed"])) {
  stop("At least one completed shape/seed missed the residual Spearman target.", call. = FALSE)
}
if (!all(results$se2_multiset_preserved[results$status == "completed"])) {
  stop("At least one completed shape/seed violated SE2 multiset preservation.", call. = FALSE)
}
complete <- results$status == "completed"
if (!all(results$has_tau_j_hat[complete] & results$tau_j_hat_finite[complete])) {
  stop("At least one completed shape/seed failed observation-layer output checks.", call. = FALSE)
}
