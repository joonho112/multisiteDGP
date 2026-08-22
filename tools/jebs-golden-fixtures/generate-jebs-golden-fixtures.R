#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
trailing_args <- commandArgs(trailingOnly = TRUE)
write_rds <- "--write-rds" %in% trailing_args
script_path <- if (length(file_arg) == 1L) {
  normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE)
} else {
  normalizePath("tools/jebs-golden-fixtures/generate-jebs-golden-fixtures.R", mustWork = TRUE)
}

package_root <- normalizePath(file.path(dirname(script_path), "..", ".."), mustWork = TRUE)
project_root <- normalizePath(file.path(package_root, ".."), mustWork = TRUE)
source_qmd <- file.path(
  project_root,
  "dev",
  "Bayes-deconvolution",
  "posts",
  "2024-01-21_JEBS_Software_Appendix",
  "2024-01-21_JEBS_Software_Appendix_E.qmd"
)
artifact_dir <- file.path(package_root, "tools", "jebs-golden-fixtures", "generated")
dir.create(artifact_dir, recursive = TRUE, showWarnings = FALSE)

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("pkgload is required to load multisiteDGP for canonical_hash().", call. = FALSE)
}
pkgload::load_all(package_root, quiet = TRUE)
RNGkind("Mersenne-Twister", "Inversion", "Rejection")

hash_schema_version <- multisitedgp_internal(".hash_schema_version")()
generator_sha256 <- unname(tools::sha256sum(script_path))

rds_sha256 <- function(object, path = NULL) {
  out_path <- if (is.null(path)) {
    tempfile(fileext = ".rds")
  } else {
    path
  }
  saveRDS(object, out_path, version = 2)
  hash <- unname(tools::sha256sum(out_path))
  if (is.null(path)) {
    unlink(out_path)
  }
  hash
}

jebs_prior_g_mixture <- function(J, sigma_tau, delta, eps, ups) {
  a <- sqrt((1 - eps) + eps * ups^2 + eps * (1 - eps) * delta^2)
  ind <- stats::runif(J) < (1 - eps)
  tau_j <- ind * stats::rnorm(J, -eps * delta / a, sqrt(1 / a^2)) +
    (1 - ind) * stats::rnorm(J, (1 - eps) * delta / a, sqrt(ups^2 / a^2))
  tau_j * sigma_tau
}

jebs_nj_se2j_vec_gamma <- function(J, nj_mean, cv, nj_min, p, R2) {
  nj_vec <- if (identical(cv, 0) || isTRUE(all.equal(cv, 0))) {
    rep(nj_mean, J)
  } else {
    a <- 1 / cv^2
    b <- a / nj_mean
    nj_raw_gamma <- stats::rgamma(n = J, shape = a, rate = b)
    round(pmax(nj_min, nj_raw_gamma), 0)
  }

  varY <- 1
  kappa <- varY * (1 / p + 1 / (1 - p)) * (1 - R2)
  data.frame(
    n_j = nj_vec,
    se2_j = kappa * (1 / nj_vec)
  )
}

jebs_tau_j_hat <- function(tau_j, df_se2) {
  shuffled <- df_se2[base::sample.int(nrow(df_se2), size = nrow(df_se2)), , drop = FALSE]
  shuffled$tau_j <- tau_j
  shuffled$tau_j_hat <- vapply(
    seq_along(tau_j),
    function(idx) stats::rnorm(1L, mean = tau_j[[idx]], sd = sqrt(shuffled$se2_j[[idx]])),
    numeric(1)
  )
  row.names(shuffled) <- NULL
  shuffled
}

normalize_for_multisiteDGP <- function(raw_observed, sigma_tau) {
  data.frame(
    site_index = seq_len(nrow(raw_observed)),
    z_j = raw_observed$tau_j / sigma_tau,
    tau_j = raw_observed$tau_j,
    n_j = as.integer(raw_observed$n_j),
    se_j = sqrt(raw_observed$se2_j),
    se2_j = raw_observed$se2_j,
    tau_j_hat = raw_observed$tau_j_hat
  )
}

strict_fixture <- list(
  J = 100L,
  tau = 0,
  sigma_tau = 0.15,
  true_dist = "Mixture",
  delta = 5,
  eps = 0.3,
  ups = 2,
  nj_mean = 80,
  cv = 0.50,
  nj_min = 5L,
  p = 0.5,
  R2 = 0,
  engine = "A1_legacy",
  dependence = "none"
)
seeds <- c(42L, 1L, 2024L, 12345L)
fixture_specs <- lapply(seq_along(seeds), function(idx) {
  utils::modifyList(strict_fixture, list(
    fixture_id = sprintf("F%02d", idx),
    seed = seeds[[idx]],
    fixture_file = sprintf("jebs_appendix_mixture_seed%d.rds", seeds[[idx]]),
    fixture_type = "JEBS appendix A1 mixture package-normalized"
  ))
})
fixture_specs[[length(fixture_specs) + 1L]] <- utils::modifyList(strict_fixture, list(
  fixture_id = "F10",
  seed = 42L,
  fixture_file = "jebs_appendix_floor_active_seed42.rds",
  fixture_type = "JEBS appendix A1 mixture floor-active authority",
  J = 300L,
  nj_mean = 10,
  cv = 0.75
))

qmd_hash <- digest::digest(normalizePath(source_qmd, mustWork = TRUE), file = TRUE, algo = "sha256")
seed_policy <- "single_stream_package_T1a"
rng_sequence_expected <- paste(
  "runif(J)",
  "rnorm(J) component_1",
  "rnorm(J) component_2",
  "rgamma(J)",
  "sample.int(J) shuffle",
  "rnorm(1) repeated J times",
  sep = " -> "
)
package_versions <- paste(
  sprintf(
    "%s=%s",
    c("digest", "tibble", "withr"),
    vapply(c("digest", "tibble", "withr"), function(pkg) {
      as.character(utils::packageVersion(pkg))
    }, character(1))
  ),
  collapse = "; "
)

manifest <- vector("list", length(fixture_specs))
for (idx in seq_along(fixture_specs)) {
  fixture <- fixture_specs[[idx]]
  seed <- fixture$seed
  set.seed(seed)
  tau_j <- jebs_prior_g_mixture(
    J = fixture$J,
    sigma_tau = fixture$sigma_tau,
    delta = fixture$delta,
    eps = fixture$eps,
    ups = fixture$ups
  )
  df_se2 <- jebs_nj_se2j_vec_gamma(
    J = fixture$J,
    nj_mean = fixture$nj_mean,
    cv = fixture$cv,
    nj_min = fixture$nj_min,
    p = fixture$p,
    R2 = fixture$R2
  )
  raw_observed <- jebs_tau_j_hat(tau_j = tau_j, df_se2 = df_se2)
  normalized <- normalize_for_multisiteDGP(raw_observed, sigma_tau = fixture$sigma_tau)

  fixture_file <- fixture$fixture_file
  fixture_path <- file.path(artifact_dir, fixture_file)
  rds_hash <- if (isTRUE(write_rds)) {
    rds_sha256(normalized, fixture_path)
  } else {
    rds_sha256(normalized)
  }

  manifest[[idx]] <- data.frame(
    fixture_id = fixture$fixture_id,
    seed = seed,
    fixture_file = fixture_file,
    fixture_type = fixture$fixture_type,
    J = fixture$J,
    tau = fixture$tau,
    sigma_tau = fixture$sigma_tau,
    true_dist = fixture$true_dist,
    delta = fixture$delta,
    eps = fixture$eps,
    ups = fixture$ups,
    nj_mean = fixture$nj_mean,
    cv = fixture$cv,
    nj_min = fixture$nj_min,
    p = fixture$p,
    R2 = fixture$R2,
    engine = fixture$engine,
    dependence = fixture$dependence,
    status = if (isTRUE(write_rds)) "rds-generated-dev-only" else "manifest-hash-recorded",
    seed_policy = seed_policy,
    qmd_demo_seed_policy = "stage_reset_seed_2562_before_each_stage",
    rng_sequence_expected = rng_sequence_expected,
    nj_min_policy = "paper and JEBS appendix lower limit: n_j = 5",
    source_qmd = source_qmd,
    source_qmd_sha256 = qmd_hash,
    source_mixture_lines = "62-78",
    source_site_size_lines = "101-128",
    source_observation_lines = "141-154",
    source_functions = "gen_priorG_mixture; gen_nj_se2j_vec_gamma; gen_tau_j_hat",
    generation_script = script_path,
    output_schema = "site_index,z_j,tau_j,n_j,se2_j,se_j,tau_j_hat",
    hash_algo = "xxhash64",
    hash_schema_version = hash_schema_version,
    rng_kind = paste(RNGkind(), collapse = "/"),
    rng_policy = "generator-pinned",
    generator_sha256 = generator_sha256,
    component_tau_hash = canonical_hash(tau_j),
    component_site_size_hash = canonical_hash(df_se2),
    component_observed_raw_hash = canonical_hash(raw_observed),
    canonical_hash = canonical_hash(normalized),
    rds_sha256 = rds_hash,
    generated_R_version = R.version.string,
    generated_platform = R.version$platform,
    package_versions = package_versions,
    os_policy = "portable schema-v4 canonical numerical hash; binary SHA verifies only the generated artifact",
    stringsAsFactors = FALSE
  )
}

manifest_df <- do.call(rbind, manifest)
write.csv(
  manifest_df,
  file.path(package_root, "tools", "jebs-golden-fixtures", "jebs-golden-fixture-manifest.csv"),
  row.names = FALSE,
  quote = TRUE
)

message("Wrote JEBS golden fixture manifest to: tools/jebs-golden-fixtures/jebs-golden-fixture-manifest.csv")
if (isTRUE(write_rds)) {
  message("Wrote generated reference RDS files to: tools/jebs-golden-fixtures/generated")
} else {
  message("Did not write RDS files. Pass --write-rds for dev-only generated references.")
}
