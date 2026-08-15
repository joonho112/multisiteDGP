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

# .abort_*(message, info, fix) 호출이면 fix 라인을, 아니면 NULL 을 돌려준다.
abort_call_fix <- function(node) {
  if (!is.call(node)) {
    return(NULL)
  }
  fn <- node[[1L]]
  if (!is.name(fn) || !grepl("^[.]abort_", as.character(fn))) {
    return(NULL)
  }
  args <- as.list(node)[-1L]
  nms <- names(args)
  if (!is.null(nms)) {
    args <- args[!nzchar(nms)]
  }
  if (length(args) < 3L) {
    return(NULL)
  }
  fix_line_literal(args[[3L]])
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

collect_fix_lines <- function() {
  files <- list.files(testthat::test_path("..", "..", "R"),
                      pattern = "[.]R$", full.names = TRUE)
  skip_if(length(files) == 0L, "R/ not reachable from the test directory")

  out <- list()
  current <- NULL
  visit <- function(node) {
    fix <- abort_call_fix(node)
    if (!is.null(fix)) {
      out[[length(out) + 1L]] <<- list(file = current, fix = fix)
    }
    walk_children(node, visit)
  }
  for (f in files) {
    current <- basename(f)
    for (expr in parse(f, keep.source = FALSE)) visit(expr)
  }
  out
}

offending <- function(found, predicate) {
  bad <- Filter(function(x) predicate(x$fix), found)
  vapply(bad, function(x) sprintf("%s: %s", x$file, x$fix), character(1))
}

test_that("the static scan finds the error calls it is supposed to guard", {
  # 이 검사가 조용히 0 건을 스캔하면 아무것도 보장하지 못한다.
  expect_gt(length(collect_fix_lines()), 200L)
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
