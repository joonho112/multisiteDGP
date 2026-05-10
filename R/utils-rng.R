# nolint start: object_name_linter, object_usage_linter
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
  withr::with_seed(seed_root, sample.int(.Machine$integer.max, n))
}
# nolint end
