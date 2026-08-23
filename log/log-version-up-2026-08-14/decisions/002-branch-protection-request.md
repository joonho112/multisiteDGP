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

---

## 결정 (2026-08-22, PI)

**설정하지 않는다.** 요청은 닫는다.

### 무엇을 결정했나

`main` 에 branch protection rule 을 걸지 않는다. PR 강제도, 필수 status check 도
설정하지 않는다. 위 요청서의 체크 목록은 이후 참고용으로 남겨 둔다.

### 이 결정이 남기는 것

요청서가 지적한 구조는 그대로 남는다 — **red 인 CI 는 여전히 아무것도 막지 않는다.**
CI 가 실패해도 `main` 에 직접 push 할 수 있고, 실패한 커밋 위에 다음 커밋을 얹을 수 있다.
v0.2.0 작업에서 CI 가 실제로 결함 세 건(D-060, D-062, D-063)을 잡았지만, 그것은
**내가 매번 결과를 읽고 멈췄기 때문**이지 설정이 막았기 때문이 아니다.

이것은 반대 의견이 아니라 결정의 범위를 정확히 적는 것이다. 단일 개발자 저장소에서
PR 강제의 비용이 이득보다 크다는 판단은 PI 의 권한이고, 그 판단에는 근거가 있다.

### 대신 무엇이 남아 있나

설정 대신 절차로 남는 것들:

- CI 는 `main` push 와 `main` 대상 PR 에서 자동 실행된다. 신호 자체는 있다.
- feature branch 에서는 `workflow_dispatch` 로 수동 실행한다 (v0.2.0 작업에서 사용한 방식).
- validation 13 종과 source digest 게이트는 CI 와 독립적으로 동작한다.
  `test-traceability.R` 은 증거의 digest 가 현재 트리와 맞지 않으면 실패한다.

마지막 항목이 실질적인 방어선이다. 설정이 아니라 테스트이므로 우회하려면
테스트를 고쳐야 하고, 그것은 diff 에 남는다.
