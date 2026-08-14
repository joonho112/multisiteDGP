# 테스트 게이트 정책 (v0.2.0, 결정 D3)
#
# v0.1.x 는 slow / property 테스트를 환경변수 뒤에 숨겼다. 그 결과 기본
# `devtools::test()` 가 통계 불변량 30 건을 skip 한 채 green 을 보고했다.
#
# 실측 결과 게이트를 전부 켜는 비용은 +94.6 초(18.1 s -> 112.7 s)뿐이었다.
# 따라서 v0.2.0 은 표본 크기나 격자를 줄이지 않고 게이트를 제거했다.
# 근거는 decisions 폴더의 D3 테스트 게이트 정책 문서에 있다.
#
# 남은 게이트는 둘뿐이다.
#   - skip_if_not_installed()          soft dependency guard (testthat 기본 제공)
#   - skip_if_not_linux_strict_hash()  아래. 재현성 계약(D1)에 종속된다.

skip_if_not_linux_strict_hash <- function() {
  platform <- tolower(R.version$platform)
  testthat::skip_if_not(
    identical(tolower(Sys.info()[["sysname"]]), "linux") &&
      grepl("x86_64|amd64", platform),
    "Strict canonical_hash equality is Linux x86_64/amd64 baseline only."
  )
}
