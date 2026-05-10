#!/usr/bin/env Rscript
# nolint start: object_name_linter, object_usage_linter

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg) == 1L) {
  normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE)
} else {
  normalizePath("tests/data-raw/generate_golden_fixtures.R", mustWork = TRUE)
}

package_root <- normalizePath(file.path(dirname(script_path), "..", ".."), mustWork = TRUE)
golden_dir <- file.path(package_root, "tests", "testthat", "_snaps", "golden")
manifest_dir <- file.path(package_root, "inst", "extdata", "golden")
dir.create(golden_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(manifest_dir, recursive = TRUE, showWarnings = FALSE)

is_linux_x86_64 <- function() {
  platform <- tolower(R.version$platform)
  identical(tolower(Sys.info()[["sysname"]]), "linux") &&
    grepl("x86_64|amd64", platform)
}

if (!is_linux_x86_64() &&
    !identical(Sys.getenv("MULTISITEDGP_ALLOW_NON_LINUX_GOLDEN_REGEN"), "true")) {
  stop(
    paste(
      "Golden fixtures embed canonical_hash/design_hash values.",
      "Regenerate on Linux x86_64/amd64, or set",
      "MULTISITEDGP_ALLOW_NON_LINUX_GOLDEN_REGEN=true for exploratory local regeneration.",
      "Do not commit non-Linux hash-only diffs."
    ),
    call. = FALSE
  )
}

if (!is_linux_x86_64()) {
  warning(
    paste(
      "Non-Linux golden-fixture regeneration is exploratory only;",
      "canonical_hash/design_hash diffs should not be committed."
    ),
    call. = FALSE
  )
}

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("pkgload is required to generate golden fixtures.", call. = FALSE)
}
pkgload::load_all(package_root, quiet = TRUE)

rds_sha256 <- function(object, path) {
  saveRDS(object, path, version = 2)
  unname(tools::sha256sum(path))
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

  kappa <- (1 / p + 1 / (1 - p)) * (1 - R2)
  data.frame(
    n_j = nj_vec,
    se2_j = kappa / nj_vec
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

canonical_fixture_columns <- function(x) {
  x[c("site_index", "z_j", "tau_j", "tau_j_hat", "se_j", "se2_j", "n_j")]
}

normalize_jebs_output <- function(raw_observed, sigma_tau) {
  canonical_fixture_columns(data.frame(
    site_index = seq_len(nrow(raw_observed)),
    z_j = raw_observed$tau_j / sigma_tau,
    tau_j = raw_observed$tau_j,
    tau_j_hat = raw_observed$tau_j_hat,
    se_j = sqrt(raw_observed$se2_j),
    se2_j = raw_observed$se2_j,
    n_j = as.integer(raw_observed$n_j)
  ))
}

fixture_row <- function(
  fixture_id,
  fixture_file,
  fixture_type,
  object,
  seed,
  source_call,
  source_policy,
  generator
) {
  path <- file.path(golden_dir, fixture_file)
  source_canonical_hash <- canonical_hash(object)
  rds_hash <- rds_sha256(object, path)
  restored <- readRDS(path)

  data.frame(
    fixture_id = fixture_id,
    fixture_file = fixture_file,
    fixture_type = fixture_type,
    seed = seed,
    source_call = source_call,
    source_policy = source_policy,
    generator = generator,
    storage_path = file.path("tests", "testthat", "_snaps", "golden", fixture_file),
    object_class = paste(class(object), collapse = "/"),
    nrow = if (inherits(object, "data.frame")) nrow(object) else NA_integer_,
    ncol = if (inherits(object, "data.frame")) ncol(object) else NA_integer_,
    hash_algo = "xxhash64",
    canonical_hash = canonical_hash(restored),
    source_canonical_hash = source_canonical_hash,
    rds_sha256 = rds_hash,
    generated_R_version = R.version.string,
    generated_platform = R.version$platform,
    package_versions = paste(
      sprintf(
        "%s=%s",
        c("digest", "tibble", "withr"),
        vapply(c("digest", "tibble", "withr"), function(pkg) as.character(utils::packageVersion(pkg)), character(1))
      ),
      collapse = "; "
    ),
    os_policy = "Linux x86-64 strict; macOS/Windows numerical equivalence per blueprint ch18 sec18.12",
    stringsAsFactors = FALSE
  )
}

jebs_fixture <- list(
  J = 100L,
  sigma_tau = 0.15,
  delta = 5,
  eps = 0.3,
  ups = 2,
  nj_mean = 80,
  cv = 0.50,
  nj_min = 4L,
  p = 0.5,
  R2 = 0
)

jebs_seeds <- c(42L, 1L, 2024L, 12345L)
jebs_rows <- lapply(seq_along(jebs_seeds), function(idx) {
  seed <- jebs_seeds[[idx]]
  set.seed(seed)
  tau_j <- jebs_prior_g_mixture(
    J = jebs_fixture$J,
    sigma_tau = jebs_fixture$sigma_tau,
    delta = jebs_fixture$delta,
    eps = jebs_fixture$eps,
    ups = jebs_fixture$ups
  )
  se2 <- jebs_nj_se2j_vec_gamma(
    J = jebs_fixture$J,
    nj_mean = jebs_fixture$nj_mean,
    cv = jebs_fixture$cv,
    nj_min = jebs_fixture$nj_min,
    p = jebs_fixture$p,
    R2 = jebs_fixture$R2
  )
  object <- normalize_jebs_output(
    jebs_tau_j_hat(tau_j = tau_j, df_se2 = se2),
    sigma_tau = jebs_fixture$sigma_tau
  )
  fixture_row(
    fixture_id = sprintf("F%02d", idx),
    fixture_file = sprintf("jebs_appendix_mixture_seed%d.rds", seed),
    fixture_type = "JEBS appendix seed",
    object = object,
    seed = seed,
    source_call = sprintf("Lee 2024 JEBS appendix normalized output, seed = %d", seed),
    source_policy = "direct appendix-code reproduction; plain canonical data frame",
    generator = "tests/data-raw/generate_golden_fixtures.R"
  )
})

preset_specs <- list(
  F05 = list(
    file = "preset_jebs_paper.rds",
    type = "preset output",
    seed = 4719L,
    call = "preset_jebs_paper() |> sim_multisite(seed = 4719)",
    object = sim_multisite(preset_jebs_paper(), seed = 4719L)
  ),
  F06 = list(
    file = "preset_jebs_strict.rds",
    type = "preset output",
    seed = 4719L,
    call = "preset_jebs_strict() |> sim_multisite(seed = 4719)",
    object = sim_multisite(preset_jebs_strict(), seed = 4719L)
  ),
  F07 = list(
    file = "preset_education_modest.rds",
    type = "preset output",
    seed = 12345L,
    call = "preset_education_modest() |> sim_multisite(seed = 12345)",
    object = sim_multisite(preset_education_modest(), seed = 12345L)
  ),
  F08 = list(
    file = "preset_walters_2024.rds",
    type = "preset output",
    seed = 1L,
    call = "preset_walters_2024() |> sim_multisite(seed = 1)",
    object = sim_multisite(preset_walters_2024(), seed = 1L)
  ),
  F09 = list(
    file = "preset_small_area_estimation.rds",
    type = "preset output",
    seed = 42L,
    call = "preset_small_area_estimation() |> sim_meta(seed = 42)",
    object = sim_meta(preset_small_area_estimation(), seed = 42L)
  )
)

preset_rows <- Map(
  f = function(fixture_id, spec) {
    fixture_row(
      fixture_id = fixture_id,
      fixture_file = spec$file,
      fixture_type = spec$type,
      object = spec$object,
      seed = spec$seed,
      source_call = spec$call,
      source_policy = "package preset output with full multisiteDGP attributes",
      generator = "tests/data-raw/generate_golden_fixtures.R"
    )
  },
  fixture_id = names(preset_specs),
  spec = preset_specs
)

manifest <- do.call(rbind, c(jebs_rows, preset_rows))
write.csv(
  manifest,
  file.path(manifest_dir, "golden-fixture-manifest.csv"),
  row.names = FALSE,
  quote = TRUE
)

writeLines(
  c(
    "# Golden Fixture Policy",
    "",
    "This directory stores binary RDS test references for the Step 8.1 golden",
    "fixture inventory.",
    "",
	    "Regenerate this directory with:",
	    "",
	    "```sh",
	    "Rscript tests/data-raw/generate_golden_fixtures.R",
	    "```",
	    "",
	    "Authoritative regeneration is Linux x86_64/amd64 only. Non-Linux",
	    "regeneration requires MULTISITEDGP_ALLOW_NON_LINUX_GOLDEN_REGEN=true",
	    "and is exploratory only. Non-Linux fixture, manifest, or hash-only",
	    "diffs must not be committed as authoritative.",
	    "",
	    "For the v0.1 bootstrap manifest, local macOS/aarch64 provenance remains",
	    "documented in `inst/extdata/golden/golden-fixture-manifest.csv` until a",
	    "GitHub Actions Linux x86_64/amd64 artifact pass confirms equality or",
	    "regenerates the fixtures and manifest as the Linux-baseline authority.",
	    "",
	    "The authoritative v1 inventory is nine files:",
    "",
    "- four JEBS appendix normalized seed fixtures;",
    "- five package preset output fixtures.",
    "",
    "The shipped metadata manifest lives in `inst/extdata/golden/`."
  ),
  file.path(golden_dir, "README.md"),
  useBytes = TRUE
)

message("Wrote 9 golden RDS files to: tests/testthat/_snaps/golden")
message("Wrote manifest to: inst/extdata/golden/golden-fixture-manifest.csv")
# nolint end
