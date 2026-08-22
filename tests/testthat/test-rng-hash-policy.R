# nolint start: object_usage_linter, object_name_linter
sample_hash_frame <- function() {
  data.frame(
    site_index = 1:4,
    z_j = c(-1, -0.2, 0.4, 1.1),
    tau_j = c(-0.2, -0.04, 0.08, 0.22),
    tau_j_hat = c(-0.18, -0.01, 0.11, 0.19),
    se_j = c(0.2, 0.25, 0.3, 0.35),
    se2_j = c(0.04, 0.0625, 0.09, 0.1225),
    n_j = c(50L, 45L, 60L, 55L)
  )
}

new_hash_data <- function(design = multisitedgp_design(seed = 42L)) {
  multisitedgp_internal(".new_multisitedgp_data")(
    sample_hash_frame(),
    design = design,
    diagnostics = list(
      I_hat = 0.20,
      R_hat = 1.33,
      rho_S_residual = 0.10,
      rho_S_marginal = 0.11,
      elapsed = "not-hashed"
    )
  )
}

test_that("local seed stream is deterministic and restores caller RNG", {
  local_seed_stream <- multisitedgp_internal(".local_seed_stream")

  set.seed(99L)
  before <- .Random.seed
  seeds_one <- local_seed_stream(5L, 123L)
  after <- .Random.seed
  seeds_two <- local_seed_stream(5L, 123L)

  expect_identical(before, after)
  expect_identical(seeds_one, seeds_two)
  expect_type(seeds_one, "integer")
  expect_length(seeds_one, 5L)

  expect_multisitedgp_error(
    local_seed_stream(0L, 123L),
    "multisitedgp_arg_error"
  )
  expect_multisitedgp_error(
    local_seed_stream(2L, NULL),
    "multisitedgp_arg_error"
  )
})

test_that("design seed validation rejects out-of-range seeds", {
  expect_multisitedgp_error(
    multisitedgp_design(seed = -1L),
    "multisitedgp_arg_error"
  )
})

test_that("canonical hash is stable under column order row names and attrs", {
  dat <- new_hash_data()
  reordered <- dat[, rev(names(dat))]
  with_extra_attr <- dat
  attr(with_extra_attr, "transient") <- Sys.time()
  with_column_attr <- dat
  attr(with_column_attr$tau_j_hat, "transient") <- Sys.time()

  plain_rows <- as.data.frame(dat)
  row.names(plain_rows) <- paste0("row-", seq_len(nrow(plain_rows)))
  attr(plain_rows, "design") <- attr(dat, "design", exact = TRUE)
  attr(plain_rows, "diagnostics") <- attr(dat, "diagnostics", exact = TRUE)
  attr(plain_rows, "multisitedgp_version") <- attr(dat, "multisitedgp_version", exact = TRUE)
  attr(plain_rows, "paradigm") <- attr(dat, "paradigm", exact = TRUE)

  expect_identical(canonical_hash(dat), canonical_hash(reordered))
  expect_identical(canonical_hash(dat), canonical_hash(with_extra_attr))
  expect_identical(canonical_hash(dat), canonical_hash(with_column_attr))
  expect_identical(canonical_hash(dat), canonical_hash(plain_rows))

  old_lineage <- dat
  attr(old_lineage, "multisitedgp_version") <- "0.0.0.9000"
  expect_identical(canonical_hash(dat), canonical_hash(old_lineage))
})

test_that("canonical hash column ordering is locale independent", {
  stable_sort <- multisitedgp_internal(".stable_sort_character")
  dat <- sample_hash_frame()
  colliding_names <- c("se_j", "se2_j", "site_index", "tau_j")

  expect_identical(stable_sort(colliding_names), sort(colliding_names, method = "radix"))
  expect_identical(
    canonical_hash(dat),
    canonical_hash(dat[, sample(names(dat))])
  )
})

test_that("canonical hash ignores diagnostics entirely under schema v4", {
  # v1 and v2 hashed an allowlist of numeric diagnostics. v3 and v4 drop them:
  # are derived from the hashed data and the hashed design, so they carry no
  # provenance the hash lacks, and their cor()/sd() accumulation order varies
  # by platform, which is what made the cross-platform contract unachievable
  # (defect ledger D-002).
  dat <- new_hash_data()
  extra <- dat
  attr(extra, "diagnostics")$elapsed <- Sys.time()
  attr(extra, "diagnostics")$note <- "ignore me"
  changed <- dat
  attr(changed, "diagnostics")$I_hat <- 0.21

  expect_identical(canonical_hash(dat), canonical_hash(extra))
  expect_identical(canonical_hash(dat), canonical_hash(changed))
})

test_that("a diagnostic still moves the hash when the caller asks for it", {
  # Opting in explicitly is the escape hatch for anyone who wants a particular
  # diagnostic pinned; it is simply not part of the default contract.
  dat <- new_hash_data()
  changed <- dat
  attr(changed, "diagnostics")$I_hat <- 0.21

  expect_false(identical(
    canonical_hash(dat, diagnostics_to_include = "I_hat"),
    canonical_hash(changed, diagnostics_to_include = "I_hat")
  ))
})

test_that("opt-in diagnostics use the canonical numerical precision", {
  dat <- new_hash_data()
  sub_precision <- dat
  attr(sub_precision, "diagnostics")$I_hat <- 0.20 + 1e-11

  expect_identical(
    canonical_hash(dat, diagnostics_to_include = "I_hat"),
    canonical_hash(sub_precision, diagnostics_to_include = "I_hat")
  )
})

test_that("data still drives the hash", {
  dat <- new_hash_data()
  changed <- dat
  # Above the nine-digit rounding, so a genuine numerical change is still seen.
  changed$tau_j_hat[1] <- changed$tau_j_hat[1] * (1 + 1e-6)

  expect_false(identical(canonical_hash(dat), canonical_hash(changed)))
})

test_that("the hash defines numerical equivalence rather than raw identity", {
  dat <- new_hash_data()
  sub_precision <- dat
  sub_precision$tau_j_hat[1] <- sub_precision$tau_j_hat[1] + 1e-11

  expect_false(identical(as.data.frame(dat), as.data.frame(sub_precision)))
  expect_identical(canonical_hash(dat), canonical_hash(sub_precision))

  boundary <- 1.234567895
  below <- data.frame(x = boundary - .Machine$double.eps)
  above <- data.frame(x = boundary + .Machine$double.eps)
  expect_lt(abs(below$x - above$x), 1e-14)
  expect_false(identical(canonical_hash(below), canonical_hash(above)))
})

test_that("factor semantics are part of schema v4", {
  base <- data.frame(group = factor(c("a", "b"), levels = c("a", "b")))
  relabelled <- data.frame(group = factor(c("x", "y"), levels = c("x", "y")))
  reversed_levels <- data.frame(group = factor(c("a", "b"), levels = c("b", "a")))
  ordered <- data.frame(group = ordered(c("a", "b"), levels = c("a", "b")))

  expect_false(identical(canonical_hash(base), canonical_hash(relabelled)))
  expect_false(identical(canonical_hash(base), canonical_hash(reversed_levels)))
  expect_false(identical(canonical_hash(base), canonical_hash(ordered)))
})

test_that("canonical hash supports explicit column and diagnostic subsets", {
  dat <- new_hash_data()
  subset_hash <- canonical_hash(
    dat,
    columns_to_include = c("tau_j_hat", "site_index"),
    diagnostics_to_include = "I_hat"
  )

  expect_type(subset_hash, "character")
  expect_match(subset_hash, "^[0-9a-f]+$")
  expect_multisitedgp_error(
    canonical_hash(dat, columns_to_include = "missing_column"),
    "multisitedgp_arg_error"
  )
})

test_that("canonical design hashing excludes function bodies and environments", {
  fn_one <- local({
    offset <- 1
    function(J, ...) rep(offset, J)
  })
  fn_two <- local({
    offset <- 999
    function(J, ...) rep(offset * 2, J)
  })
  design_one <- multisitedgp_design(g_fn = fn_one, seed = 42L)
  design_two <- multisitedgp_design(g_fn = fn_two, seed = 42L)
  dat_one <- new_hash_data(design_one)
  dat_two <- new_hash_data(design_two)

  expect_identical(canonical_hash(design_one), canonical_hash(design_two))
  expect_identical(canonical_hash(dat_one), canonical_hash(dat_two))
  expect_identical(attr(dat_one, "provenance")$custom_hooks, "g_fn")
  expect_match(attr(dat_one, "provenance")$function_exclusion_policy, "excluded")

  se_fn_one <- function(J) list(se2_j = rep(0.04, J), n_j = NULL)
  se_fn_two <- function(J) list(se2_j = rep(0.99, J), n_j = NULL)
  se_design_one <- multisitedgp_design(paradigm = "direct", I = 0.4, se_fn = se_fn_one, seed = 42L)
  se_design_two <- multisitedgp_design(paradigm = "direct", I = 0.4, se_fn = se_fn_two, seed = 42L)
  se_frame <- sample_hash_frame()
  se_frame$n_j <- rep(NA_integer_, nrow(se_frame))
  se_dat <- multisitedgp_internal(".new_multisitedgp_meta")(se_frame, design = se_design_one)
  expect_identical(canonical_hash(se_design_one), canonical_hash(se_design_two))
  expect_identical(attr(se_dat, "provenance")$custom_hooks, "se_fn")

  dep_fn_one <- function(z, se2, target, ...) order(seq_along(z))
  dep_fn_two <- function(z, se2, target, ...) rev(order(seq_along(z)))
  dep_design_one <- multisitedgp_design(dependence = "rank", rank_corr = 0.2, dependence_fn = dep_fn_one, seed = 42L)
  dep_design_two <- multisitedgp_design(dependence = "rank", rank_corr = 0.2, dependence_fn = dep_fn_two, seed = 42L)
  expect_identical(canonical_hash(dep_design_one), canonical_hash(dep_design_two))
  expect_identical(attr(new_hash_data(dep_design_one), "provenance")$custom_hooks, "dependence_fn")

  obs_fn_one <- function(tau_j, se2_j, ...) tau_j
  obs_fn_two <- function(tau_j, se2_j, ...) tau_j + 999
  obs_design_one <- multisitedgp_design(obs_fn = obs_fn_one, seed = 42L)
  obs_design_two <- multisitedgp_design(obs_fn = obs_fn_two, seed = 42L)
  expect_identical(canonical_hash(obs_design_one), canonical_hash(obs_design_two))
  expect_identical(attr(new_hash_data(obs_design_one), "provenance")$custom_hooks, "obs_fn")
})

test_that("canonical hash is final wrapper provenance and same-seed stable", {
  hashes_a <- vapply(
    seq_len(5L),
    function(i) canonical_hash(sim_multisite(J = 25L, seed = 6501L)),
    character(1)
  )
  hashes_b <- vapply(
    seq_len(5L),
    function(i) canonical_hash(sim_meta(J = 25L, I = 0.4, R = 3, seed = 6502L)),
    character(1)
  )
  out <- sim_multisite(J = 25L, seed = 6503L)
  provenance <- attr(out, "provenance", exact = TRUE)
  design <- attr(out, "design", exact = TRUE)

  expect_length(unique(hashes_a), 1L)
  expect_length(unique(hashes_b), 1L)
  expect_identical(provenance$canonical_hash, canonical_hash(out))
  expect_identical(provenance$design_hash, canonical_hash(design))

  set.seed(6504L)
  before <- .Random.seed
  invisible(sim_multisite(J = 25L, seed = 6505L))
  invisible(sim_meta(J = 25L, I = 0.4, R = 3, seed = 6506L))
  after <- .Random.seed
  expect_identical(after, before)
})

test_that("seeded wrappers pin RNG kind and restore the caller stream", {
  original_kind <- RNGkind()
  original_has_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  original_seed <- if (original_has_seed) .Random.seed else NULL
  on.exit({
    do.call(RNGkind, as.list(original_kind))
    if (original_has_seed) {
      assign(".Random.seed", original_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
  set.seed(7101L)
  le_kind <- RNGkind()
  le_seed <- .Random.seed
  from_lecuyer <- sim_multisite(J = 20L, seed = 7102L)
  expect_identical(RNGkind(), le_kind)
  expect_identical(.Random.seed, le_seed)

  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(7103L)
  mt_kind <- RNGkind()
  mt_seed <- .Random.seed
  from_mt <- sim_multisite(J = 20L, seed = 7102L)
  expect_identical(RNGkind(), mt_kind)
  expect_identical(.Random.seed, mt_seed)

  expect_identical(as.data.frame(from_lecuyer), as.data.frame(from_mt))
  expect_identical(canonical_hash(from_lecuyer), canonical_hash(from_mt))
  provenance <- attr(from_lecuyer, "provenance", exact = TRUE)
  expect_identical(
    unname(provenance$rng_kind),
    c("Mersenne-Twister", "Inversion", "Rejection")
  )
  expect_identical(provenance$rng_policy, "package-pinned")
})

test_that("seeded wrappers do not manufacture caller RNG state", {
  original_kind <- RNGkind()
  original_has_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  original_seed <- if (original_has_seed) .Random.seed else NULL
  on.exit({
    do.call(RNGkind, as.list(original_kind))
    if (original_has_seed) {
      assign(".Random.seed", original_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    rm(".Random.seed", envir = .GlobalEnv)
  }
  before_kind <- RNGkind()
  invisible(sim_meta(J = 10L, I = 0.4, R = 2, seed = 7104L))

  expect_identical(RNGkind(), before_kind)
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})

test_that("canonical_hash is the contract where raw digest is fragile", {
  dat <- new_hash_data()
  reordered <- dat[, rev(names(dat))]
  with_extra_attr <- dat
  attr(with_extra_attr, "transient") <- Sys.time()

  expect_false(identical(
    digest::digest(dat, algo = "xxhash64"),
    digest::digest(reordered, algo = "xxhash64")
  ))
  expect_false(identical(
    digest::digest(dat, algo = "xxhash64"),
    digest::digest(with_extra_attr, algo = "xxhash64")
  ))
  expect_identical(canonical_hash(dat), canonical_hash(reordered))
  expect_identical(canonical_hash(dat), canonical_hash(with_extra_attr))
})

test_that("provenance_string is exported for data design and default paths", {
  expect_true("provenance_string" %in% getNamespaceExports("multisiteDGP"))

  out <- sim_multisite(J = 25L, seed = 6507L)
  design <- attr(out, "design", exact = TRUE)
  data_line <- provenance_string(out)
  design_line <- provenance_string(design)
  default_line <- provenance_string(data.frame(x = 1:3))

  expect_type(data_line, "character")
  expect_length(data_line, 1L)
  expect_match(data_line, "multisiteDGP", fixed = TRUE)
  expect_match(data_line, "paradigm=site_size", fixed = TRUE)
  expect_match(data_line, "seed=6507", fixed = TRUE)
  expect_match(data_line, sprintf("canonical_hash=%s", canonical_hash(out)), fixed = TRUE)
  expect_match(data_line, sprintf("design_hash=%s", canonical_hash(design)), fixed = TRUE)
  expect_match(data_line, "hash_schema=multisiteDGP-canonical-hash-v4", fixed = TRUE)
  expect_match(data_line, "producer_R=", fixed = TRUE)
  expect_match(data_line, "producer_platform=", fixed = TRUE)
  expect_match(data_line, "producer_rng=Mersenne-Twister/Inversion/Rejection", fixed = TRUE)
  expect_match(data_line, "verifier_R=", fixed = TRUE)
  expect_match(data_line, "hooks=none", fixed = TRUE)

  expect_match(design_line, "object=multisitedgp_design", fixed = TRUE)
  expect_match(design_line, sprintf("design_hash=%s", canonical_hash(design)), fixed = TRUE)
  expect_match(default_line, "object=data.frame", fixed = TRUE)
  expect_match(default_line, sprintf("canonical_hash=%s", canonical_hash(data.frame(x = 1:3))), fixed = TRUE)
})

test_that("provenance_string distinguishes producer and verifier runtimes", {
  out <- sim_multisite(J = 10L, seed = 6510L)
  provenance <- attr(out, "provenance", exact = TRUE)
  provenance$R_version <- "R version 3.6.3 (2020-02-29)"
  attr(out, "provenance") <- provenance

  expect_warning(
    line <- provenance_string(out),
    "Stored producer runtime differs"
  )
  expect_match(line, "producer_R=3.6.3", fixed = TRUE)
  expect_match(line, sprintf("verifier_R=%s.%s", R.version$major, R.version$minor), fixed = TRUE)
  expect_no_match(line, "producer_R=4.6.0", fixed = TRUE)
})

test_that("provenance_string records custom hook presence without hashing bodies", {
  fn_one <- function(J, ...) rep(0, J)
  fn_two <- function(J, ...) rep(999, J)
  design_one <- multisitedgp_design(g_fn = fn_one, seed = 6508L)
  design_two <- multisitedgp_design(g_fn = fn_two, seed = 6508L)

  expect_identical(canonical_hash(design_one), canonical_hash(design_two))
  expect_match(provenance_string(design_one), "hooks=g_fn", fixed = TRUE)
})

test_that("provenance_string recomputes row-subset hash instead of using stale provenance", {
  out <- sim_multisite(J = 25L, seed = 6509L)
  subset <- suppressWarnings(out[1:10, ])
  provenance <- attr(subset, "provenance", exact = TRUE)
  line <- provenance_string(subset)

  expect_null(provenance$canonical_hash)
  expect_true(provenance$row_subset)
  expect_match(line, sprintf("canonical_hash=%s", canonical_hash(subset)), fixed = TRUE)
  expect_no_match(line, sprintf("canonical_hash=%s", canonical_hash(out)), fixed = TRUE)
})
# nolint end
