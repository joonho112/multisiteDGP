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
# 남은 게이트는 하나뿐이다.
#   - skip_if_not_installed()  soft dependency guard (testthat 기본 제공)
#
# v0.1.x 에는 skip_if_not_linux_strict_hash() 가 있었다. canonical_hash 동등성을
# Linux x86_64 에서만 검사하고 나머지 플랫폼은 면제했다. 해시 스키마 v3 가
# 파생 진단값을 payload 에서 빼면서 해시가 실제로 이식 가능해졌고,
# tools/cross-os-reproducibility-policy.md 는 "플랫폼 위계 없음" 을 계약으로
# 명시한다. 그 계약을 CI 5 칸 중 1 칸에서만 검사하는 gate 는 계약을 검증하지
# 않는 것과 같으므로 제거했다. T1a 는 이제 모든 플랫폼에서 실행된다.
