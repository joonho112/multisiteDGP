# canonical_hash 의 정규화 계층.
#
# `.canonicalize_for_hash()` 는 해시 payload 를 만들기 전에 객체를 정규화한다.
# 함수 본문·환경·수식·언어 객체를 제외하거나 문자열로 바꾸는 분기가 여기 있고,
# 그것이 "해시는 스키마로 안정적이다" 라는 계약의 실체다.
#
# v0.1.x 에서 이 분기 대부분이 커버되지 않았다 (utils-reproducibility.R 미커버
# 12줄). Phase 4 가 재현성 계약을 재설계하면서 이 코드를 신뢰해야 하므로,
# 재설계 전에 현재 동작을 고정한다. 결함 원장 D-002 관련.

canon <- function(...) .canonicalize_for_hash(...)

# ── 제외 분기: 해시가 무엇을 의도적으로 보지 않는가 ───────────────────

test_that("NULL canonicalizes to a tagged marker", {
  expect_identical(canon(NULL), list(kind = "NULL"))
})

test_that("functions are excluded by identity, not by body", {
  a <- canon(function(x) x + 1, path = c("design", "g_fn"))
  b <- canon(function(x) x * 999, path = c("design", "g_fn"))

  # 본문이 달라도 같은 표현으로 정규화된다 — 콜백 본문은 해시에 들어가지 않는다.
  expect_identical(a, b)
  expect_identical(a$kind, "function_excluded")
  expect_true(a$present)
  expect_identical(a$hook, "design.g_fn")
})

test_that("environments are excluded the same way", {
  e1 <- new.env()
  assign("x", 1, envir = e1)
  e2 <- new.env()
  assign("x", 2, envir = e2)

  expect_identical(canon(e1, path = "env"), canon(e2, path = "env"))
  expect_identical(canon(e1, path = "env")$kind, "environment_excluded")
})

# ── 문자열로 바뀌는 분기 ──────────────────────────────────────────────

test_that("formulas canonicalize to their deparsed text", {
  out <- canon(~ x1 + x2)

  expect_identical(out$kind, "formula")
  expect_true(grepl("x1 \\+ x2", out$expression))
})

test_that("language objects canonicalize to their deparsed text", {
  out <- canon(quote(a + b))

  expect_identical(out$kind, "language")
  expect_identical(out$expression, "a + b")
})

# ── 컨테이너 분기 ─────────────────────────────────────────────────────

test_that("named lists are sorted so element order cannot change the hash", {
  forward <- canon(list(alpha = 1, beta = 2))
  reversed <- canon(list(beta = 2, alpha = 1))

  expect_identical(forward, reversed)
  expect_identical(names(forward), c("alpha", "beta"))
})

test_that("unnamed list elements are addressed by position", {
  out <- canon(list(1, 2, 3))

  expect_length(out, 3L)
  expect_identical(out[[1]], 1)
  expect_identical(out[[3]], 3)
})

test_that("pairlists are canonicalized as lists", {
  pl <- as.pairlist(list(a = 1, b = 2))
  expect_true(is.pairlist(pl))

  expect_identical(canon(pl), canon(list(a = 1, b = 2)))
})

test_that("data frames sort their columns", {
  forward <- canon(data.frame(a = 1:2, b = 3:4))
  reversed <- canon(data.frame(b = 3:4, a = 1:2))

  expect_identical(forward, reversed)
  expect_identical(forward$kind, "data.frame")
  expect_identical(forward$columns_sorted, c("a", "b"))
})

# ── atomic 분기: 여기가 계약의 핵심이다 ───────────────────────────────

test_that("atomic vectors are stripped of names and attributes", {
  x <- c(first = 1, second = 2)
  attr(x, "note") <- "should not matter"

  expect_identical(canon(x), c(1, 2))
})

test_that("doubles are rounded so sub-1e-9 drift cannot move the hash", {
  # v3 dropped the derived diagnostics and rounded what remained; v4 retains
  # that precision policy. Limited fixtures had appeared byte-stable, but
  # broader CI cases still drifted below 1e-9.
  x1 <- c(1.2345678901234, 2, 3)
  x2 <- x1
  x2[1] <- x2[1] * (1 + 2 * .Machine$double.eps)

  expect_identical(canon(x1), canon(x2))
})

test_that("rounding keeps enough resolution to catch a real regression", {
  x1 <- c(1.2345678901234, 2, 3)
  x2 <- x1
  x2[1] <- x2[1] * (1 + 1e-8)

  expect_false(identical(canon(x1), canon(x2)))
})

test_that("integer and logical columns keep their type", {
  expect_identical(canon(1:5), 1:5)
  expect_identical(canon(c(TRUE, FALSE, NA)), c(TRUE, FALSE, NA))
})

test_that("the diagnostics allowlist is empty in schema v4", {
  # The diagnostics are computed from the hashed data and the hashed design, so
  # they add no provenance — only cor()/sd() accumulation noise, which is what
  # made the cross-platform contract unachievable (D-002).
  expect_identical(.canonical_diagnostics_allowlist(), character())

  dat <- sim_multisite(J = 10L, sigma_tau = 0.2, nj_mean = 100, seed = 1L)
  payload <- .canonical_hash_payload(dat, algo = "xxhash64")
  expect_length(payload$diagnostics_numeric, 0L)
})

test_that("a caller can still pin diagnostics explicitly", {
  dat <- sim_multisite(J = 10L, sigma_tau = 0.2, nj_mean = 100, seed = 1L)

  with_diag <- canonical_hash(dat, diagnostics_to_include = "I_hat")
  without <- canonical_hash(dat)

  expect_type(with_diag, "character")
  expect_false(identical(with_diag, without))
})

test_that("the hash schema version is v4", {
  expect_identical(.hash_schema_version(), "multisiteDGP-canonical-hash-v4")
})

# ── payload 조립 ──────────────────────────────────────────────────────

test_that("a non-data-frame object takes the generic payload branch", {
  payload <- .canonical_hash_payload(list(a = 1, b = 2), algo = "xxhash64")

  expect_identical(payload$object_type, "list")
  expect_true(all(c("hash_schema_version", "object", "manifest") %in% names(payload)))
  expect_null(payload$columns_sorted)
})

test_that("a design object takes the design payload branch", {
  design <- multisitedgp_design(paradigm = "site_size", J = 10L, sigma_tau = 0.2,
                                nj_mean = 100, seed = 1L)
  payload <- .canonical_hash_payload(design, algo = "xxhash64")

  expect_identical(payload$object_type, "multisitedgp_design")
  expect_true("design" %in% names(payload))
})

test_that("the diagnostics allowlist keeps only finite numeric scalars", {
  keys <- .canonical_diagnostics_allowlist()
  diagnostics <- c(
    stats::setNames(as.list(seq_along(keys)), keys),
    list(not_allowlisted = 99, non_finite = Inf, vector_valued = c(1, 2))
  )
  out <- .canonicalize_diagnostics(diagnostics)

  expect_setequal(names(out), keys)
  expect_false("not_allowlisted" %in% names(out))
  expect_false("non_finite" %in% names(out))
  expect_false("vector_valued" %in% names(out))
})

test_that("canonicalize_diagnostics tolerates a missing diagnostics slot", {
  expect_null(.canonicalize_diagnostics(NULL))
  expect_null(.canonicalize_diagnostics("not a list"))
})

# ── 공개 함수의 인자 검증 ─────────────────────────────────────────────

test_that("canonical_hash rejects unknown columns_to_include", {
  dat <- sim_multisite(J = 10L, sigma_tau = 0.2, nj_mean = 100, seed = 1L)

  expect_error(
    canonical_hash(dat, columns_to_include = c("tau_j", "no_such_column")),
    class = "multisitedgp_arg_error"
  )
})

test_that("canonical_hash honours an explicit column subset", {
  dat <- sim_multisite(J = 10L, sigma_tau = 0.2, nj_mean = 100, seed = 1L)

  subset_hash <- canonical_hash(dat, columns_to_include = c("tau_j", "se2_j"))
  full_hash <- canonical_hash(dat)

  expect_type(subset_hash, "character")
  expect_false(identical(subset_hash, full_hash))
  # 열 순서를 바꿔도 같은 해시가 나와야 한다 (안정 정렬).
  expect_identical(
    subset_hash,
    canonical_hash(dat, columns_to_include = c("se2_j", "tau_j"))
  )
})

test_that("canonical_hash rejects a non-string algo", {
  dat <- sim_multisite(J = 10L, sigma_tau = 0.2, nj_mean = 100, seed = 1L)

  expect_error(canonical_hash(dat, algo = 1L), class = "multisitedgp_arg_error")
})
