# `main` branch protection 설정 요청 (PI 조치 필요)

**요청자**: Claude Opus 5 · **날짜**: 2026-08-15 · **근거**: 계획서 Phase 6 Step 6.6

## 왜 필요한가

CI 가 3 개월간 red 였던 구조적 이유는 **red 인 CI 가 아무것도 막지 않았기** 때문입니다.
실패해도 병합이 되므로 신호가 비용을 만들지 않았고, 비용이 없는 신호는 결국 무시됩니다.
workflow 를 green 으로 만드는 것만으로는 이 구조가 재발합니다.

## 요청 내용

GitHub 저장소 Settings → Branches → Add branch protection rule

- **Branch name pattern**: `main`
- [x] Require a pull request before merging
- [x] Require status checks to pass before merging
  - 필수 체크: `R-CMD-check (linux-release)`, `R-CMD-check (linux-devel)`,
    `R-CMD-check (linux-oldrel)`, `R-CMD-check (macos-release)`,
    `R-CMD-check (windows-release)`, `lint`, `test-coverage`
- [x] Require branches to be up to date before merging

`extended-tests` 는 스케줄 실행이므로 필수 체크에 넣지 않습니다 — 대신 실패 시
GitHub Issue 를 자동 생성하도록 Phase 6 에서 설정했습니다.

## 이 조치는 PI 권한입니다

저장소 설정 변경은 관리자 권한이 필요하고, 저는 수행하지 않습니다.
설정 후 이 문서에 날짜를 적어 주시면 Phase 6 종료 조건이 닫힙니다.

**설정 완료일**: ______
