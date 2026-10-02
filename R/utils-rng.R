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

.local_seed_stream <- function(n, seed_root) {
  n <- .validate_scalar_integer(n, "n")
  if (n < 1L) {
    .abort("`n` must be at least 1.", .you_passed(n))
  }
  if (is.null(seed_root)) {
    .abort(
      "`seed_root` is required when `seed_stream = TRUE`.",
      fix = "Set `seed_stream = FALSE` for a grid without seeds."
    )
  }
  seed_root <- .validate_seed(seed_root, "seed_root")
  .with_reproducible_seed(seed_root, sample.int(.Machine$integer.max, n))
}
