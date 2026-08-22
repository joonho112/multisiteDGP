# 오류 메시지 3단 구조의 정적 검사.
#
# expect_multisitedgp_error() 는 fix 라인이 Try|Use|Pass|Remove 로 시작하는지
# 검사하지만, 그 분기가 **실행될 때만** 검사한다. 실제로 규약을 어긴 메시지 6 건이
# 오랫동안 통과하고 있었다 — 전부 테스트가 한 번도 도달하지 않은 분기였다
# (D-033). 커버리지로 잡으려면 모든 오류 분기에 테스트가 있어야 하는데, 그것은
# 이 검사가 보장하는 것보다 훨씬 비싸다.
#
# 여기서는 R 소스를 파싱해 .abort_*() 호출의 세 번째 인자를 직접 본다. 분기가
# 실행되지 않아도 잡힌다.

# fix 라인은 문자열 리터럴이거나 sprintf(fmt, ...) 다. 후자면 fmt 를 본다.
fix_line_literal <- function(node) {
  if (is.character(node) && length(node) == 1L) {
    return(node)
  }
  if (is.call(node) && identical(as.character(node[[1L]]), "sprintf")) {
    fmt <- node[[2L]]
    if (is.character(fmt) && length(fmt) == 1L) {
      return(fmt)
    }
  }
  NULL
}

# fix 인자를 R의 exact-name/positional matching 규칙으로 찾는다. named 호출을
# 버리던 이전 검사는 실제 245개 call 중 대부분을 우연히 positional처럼 읽었다.
abort_fix_argument <- function(args, formal_names) {
  nms <- names(args)
  if (is.null(nms)) nms <- rep("", length(args))
  nms[is.na(nms)] <- ""
  if (any(nms == "fix")) {
    return(args[[which(nms == "fix")[[1L]]]])
  }

  used <- intersect(nms[nzchar(nms)], formal_names)
  available <- formal_names[!formal_names %in% used]
  for (idx in which(!nzchar(nms))) {
    if (length(available) == 0L) break
    assigned <- available[[1L]]
    available <- available[-1L]
    if (identical(assigned, "fix")) return(args[[idx]])
  }
  NULL
}

primary_abort_helpers <- c(
  ".abort_multisitedgp" = "message,info,fix,class",
  ".abort_arg" = "message,info,fix",
  ".abort_coherence" = "message,info,fix",
  ".abort_engine_dependence" = "message,info,fix",
  ".abort_solver" = "message,info,fix",
  ".abort_dependence_solver" = "message,info,fix",
  ".abort_marginal_violation" = "message,info,fix"
)

is_abort_helper_definition <- function(node) {
  if (!is.call(node) || !identical(node[[1L]], as.name("<-"))) {
    return(FALSE)
  }
  target <- node[[2L]]
  value <- node[[3L]]
  is.name(target) && grepl("^[.]abort_", as.character(target)) &&
    is.call(value) && identical(value[[1L]], as.name("function"))
}

abort_helper_definitions <- function(files) {
  definitions <- character()
  visit <- function(node) {
    if (is_abort_helper_definition(node)) {
      definitions <<- c(definitions, as.character(node[[2L]]))
    }
    walk_children(node, visit)
  }
  for (file in files) for (expr in parse(file, keep.source = FALSE)) visit(expr)
  unique(definitions)
}

# 모든 .abort_* call을 static, dynamic propagation, delegated wrapper, 또는
# unclassified로 분류한다. 마지막 분류는 테스트 실패다.
abort_call_record <- function(node, definitions) {
  if (!is.call(node)) {
    return(NULL)
  }
  fn <- node[[1L]]
  if (!is.name(fn) || !grepl("^[.]abort_", as.character(fn))) {
    return(NULL)
  }
  fn <- as.character(fn)
  args <- as.list(node)[-1L]

  if (!fn %in% names(primary_abort_helpers)) {
    return(list(
      helper = fn,
      kind = if (fn %in% definitions) "delegated" else "unclassified",
      fix = NULL
    ))
  }

  formal_names <- strsplit(primary_abort_helpers[[fn]], ",", fixed = TRUE)[[1L]]
  fix_expr <- abort_fix_argument(args, formal_names)
  if (is.null(fix_expr)) {
    return(list(helper = fn, kind = "unclassified", fix = NULL))
  }
  literal <- fix_line_literal(fix_expr)
  if (!is.null(literal)) {
    return(list(helper = fn, kind = "static", fix = literal))
  }
  if (is.name(fix_expr) && identical(as.character(fix_expr), "fix")) {
    return(list(helper = fn, kind = "dynamic", fix = NULL))
  }
  list(helper = fn, kind = "unclassified", fix = NULL)
}

# 자식 노드를 순회한다. 기본값 없는 형식인자는 빈 심볼이라 변수에 묶거나 인자로
# 넘기는 순간 "argument is missing" 이 난다. `[` 는 원소를 강제하지 않으므로
# 한 원소짜리 슬라이스로 비교해 건너뛴다.
walk_children <- function(node, visit) {
  if (!is.call(node) && !is.pairlist(node) && !is.expression(node)) {
    return(invisible(NULL))
  }
  parts <- as.list(node)
  empty <- list(quote(expr = ))
  for (i in seq_along(parts)) {
    if (identical(parts[i], empty)) next
    visit(parts[[i]])
  }
  invisible(NULL)
}

collect_abort_sites <- function() {
  files <- list.files(testthat::test_path("..", "..", "R"),
                      pattern = "[.]R$", full.names = TRUE)
  skip_if(length(files) == 0L, "R/ not reachable from the test directory")
  definitions <- abort_helper_definitions(files)

  out <- list()
  current <- NULL
  visit <- function(node) {
    record <- abort_call_record(node, definitions)
    if (!is.null(record)) {
      record$file <- current
      out[[length(out) + 1L]] <<- record
    }
    walk_children(node, visit)
  }
  for (f in files) {
    current <- basename(f)
    for (expr in parse(f, keep.source = FALSE)) visit(expr)
  }
  out
}

collect_fix_lines <- function() {
  Filter(function(site) identical(site$kind, "static"), collect_abort_sites())
}

offending <- function(found, predicate) {
  bad <- Filter(function(x) predicate(x$fix), found)
  vapply(bad, function(x) sprintf("%s: %s", x$file, x$fix), character(1))
}

test_that("the static scan finds the error calls it is supposed to guard", {
  sites <- collect_abort_sites()

  # 이 검사가 조용히 일부 call을 누락하면 아무것도 보장하지 못한다.
  expect_gt(length(sites), 240L)
  expect_gt(sum(vapply(sites, function(x) x$kind == "static", logical(1))), 200L)
  expect_gt(sum(vapply(sites, function(x) x$kind == "dynamic", logical(1))), 0L)
  expect_gt(sum(vapply(sites, function(x) x$kind == "delegated", logical(1))), 0L)
  expect_identical(
    vapply(
      Filter(function(x) x$kind == "unclassified", sites),
      function(x) sprintf("%s: %s", x$file, x$helper),
      character(1)
    ),
    character()
  )
})

test_that("named and dynamic fix arguments are classified fail-closed", {
  named <- quote(.abort_arg(message = "bad", info = "why", fix = sprintf("Use `%s`.", arg)))
  dynamic <- quote(.abort_multisitedgp(message, info, fix, "multisitedgp_arg_error"))
  unknown <- quote(.abort_unregistered("bad"))

  expect_identical(abort_call_record(named, character())$kind, "static")
  expect_identical(abort_call_record(named, character())$fix, "Use `%s`.")
  expect_identical(abort_call_record(dynamic, character())$kind, "dynamic")
  expect_identical(abort_call_record(unknown, character())$kind, "unclassified")
})

test_that("every fix line opens with an imperative from the documented set", {
  # 3 단 구조의 세 번째 줄은 "무엇을 하라" 다. 동사를 좁게 고정해 두면 메시지가
  # 조언("Consider ...") 이나 진단("Check ...") 으로 흘러가지 않는다.
  expect_identical(
    offending(collect_fix_lines(), function(fix) !grepl("^(Try|Use|Pass|Remove)\\b", fix)),
    character()
  )
})

test_that("no fix line is empty or missing its terminating period", {
  expect_identical(
    offending(collect_fix_lines(), function(fix) {
      !nzchar(trimws(fix)) || !grepl("[.]$", trimws(fix))
    }),
    character()
  )
})

test_that("dynamic fix propagation and installed remedies execute as advertised", {
  abort_arg <- multisitedgp_internal(".abort_arg")
  condition <- rlang::catch_cnd(abort_arg(
    "Bad sample value.",
    "The sample value violates the demonstration contract.",
    "Use a finite sample value."
  ))

  expect_s3_class(condition, "multisitedgp_arg_error")
  expect_match(conditionMessage(condition), "Use a finite sample value.", fixed = TRUE)

  catalog <- error_catalog()
  expect_true(all(grepl("^(Try|Use|Pass|Remove)\\b", catalog$remedy)))
  expect_true(all(grepl("[.]$", catalog$remedy)))
})
