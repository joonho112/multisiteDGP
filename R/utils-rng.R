# nolint start: object_name_linter, object_usage_linter
.reproducible_rng_kind <- function() {
  c(
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
}

.with_reproducible_seed <- function(seed, code) {
  seed <- .validate_seed(seed, "seed")
  caller_kind <- RNGkind()
  caller_has_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  caller_seed <- if (caller_has_seed) {
    get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  } else {
    NULL
  }

  on.exit({
    do.call(RNGkind, as.list(caller_kind))
    if (caller_has_seed) {
      assign(".Random.seed", caller_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  do.call(RNGkind, as.list(unname(.reproducible_rng_kind())))
  set.seed(seed)
  force(code)
}

.provenance_rng_kind <- function(seed = NULL) {
  if (is.null(seed)) {
    return(stats::setNames(RNGkind(), names(.reproducible_rng_kind())))
  }
  .reproducible_rng_kind()
}

.local_seed_stream <- function(n, seed_root) {
  n <- .validate_scalar_integer(n, "n")
  if (n < 1L) {
    .abort_arg(
      "`n` must be at least 1.",
      "Seed streams allocate one deterministic seed per requested design row.",
      "Use `n = 1L` or a larger positive integer."
    )
  }
  if (is.null(seed_root)) {
    .abort_arg(
      "`seed_root` is required when a deterministic seed stream is requested.",
      "multisiteDGP never manufactures seeds from the caller's global RNG state.",
      "Pass `seed_root = 12345L` or set seed streaming to `FALSE`."
    )
  }
  seed_root <- .validate_seed(seed_root, "seed_root")
  .with_reproducible_seed(seed_root, sample.int(.Machine$integer.max, n))
}
# nolint end
