# 돌아오셨을 때 먼저 읽을 것

**2026-08-15 · Phase 8 완료 시점 상태입니다.**

---

## 1. 한눈에

| | |
|---|---|
| **완료 Phase** | 1 · 2 · 3 · 4 · 5 · 6 · 7 · 8 |
| **남은 Phase** | 9(문서·버전) · 10(외부 검토) · 11(릴리스) |
| **결함 원장** | 36 행 · **fixed 34** · open 2 (둘 다 Phase 9 배정) |
| **테스트** | **4882 통과** · 0 실패 · **0 skip** · 커버리지 **96.42 %** |
| **lint** | 0 위반 |
| **`R CMD check --as-cran`** | **0 error · 0 warning · 0 note** |
| **CI** | `main` 6/6 green (Phase 5·7 병합 완료) · Phase 8 브랜치 실행 중 |
| **Validation** | **V0–V12 열세 실험 전부 `full` 통과** (2026-08-15) |
| **PI 조치 대기** | `main` branch protection 1 건 (아래 §5) |

---

## 2. 진행 상황

| Phase | 상태 | 로그 |
|---|---|---|
| 1 작업 기반 복구 | 완료 | [`002`](002_phase01-foundation-recovery.html) |
| 2 결함 인벤토리 감사 | 완료 | [`003`](003_phase02-defect-audit.html) |
| 3 v0.2.0 범위 결정 | 완료 | [`004`](004_phase03-scope-decisions.html) |
| 4 재현성 계약 + solver | 완료 | [`006`](006_phase04-solver-hardening-partial.html) |
| 5 테스트 스위트 | **완료** (6/6 Step) | [`005`](005_phase05-test-suite-rebuild-partial.html) · [**`009`**](009_phase05-coverage-completion.html) |
| 6 CI 복구 | 완료 (branch protection 대기) | [`007`](007_phase06-ci-restoration.html) |
| 7 기능 결함 수정 | 완료 | [`008`](008_phase07-defect-remediation.html) |
| **8 통계 검증 재실행** | **완료** | [**`010`**](010_phase08-statistical-revalidation.html) |
| 9–11 | 미착수 | — |

---

## 3. 지금까지 무엇이 바뀌었나

| 지표 | 시작 | 지금 |
|---|--:|--:|
| git 관리 | 없음 | 추적 중, 커밋 40+ |
| green workflow | **0 / 6** | **6 / 6** |
| lint 위반 (CI 조건) | 440 | **0** |
| 기본 테스트 skip | **30** | **0** |
| 기본 테스트 실패 (Linux) | 15 | **0** |
| `R CMD check` NOTE | 1 | **0** |
| 커버리지 | 90.89 % | **96.42 %** |
| 결함 원장 | 없음 | **35 행 · 33 해소** |

---

## 4. 가장 중요한 세 가지

### ① CI 3 개월 red 는 결함 **네 개가 겹친 것**이었다

하나의 버그가 여러 곳에 나타난 것이 아니다. `.lintr` 미추적 · `multisitepower` 가 어디서도 해결 불가 · 재현성 계약이 달성 불가능 · `covr` 의 `xml2` 누락 — **넷이 서로 독립**이라 하나를 고쳐도 나머지가 red 였다. 자세한 것은 [Phase 6 로그](007_phase06-ci-restoration.html).

### ② 재현성 계약은 이제 실제로 이식 가능하다

해시 스키마 v3 가 파생 진단(`cor()`/`sd()` 로 계산되어 플랫폼마다 누적 순서가 다른 값)을 payload 에서 제거했다. 그 결과 **macOS ARM 에서 Linux 생성 fixture 와 해시가 일치**한다 — 네 seed 전부.

그래서 Phase 7 에서 **T1a 의 Linux 전용 skip 을 제거**했다. 정책 문서가 "플랫폼 위계 없음" 을 선언하는데 테스트는 5 칸 중 1 칸에서만 검사하고 있었다. 스위트의 마지막 skip 이 사라졌다.

::: 확인됨
**세 플랫폼 전부 통과했습니다** (run 31889543618 — Windows · macOS · Linux release/devel/oldrel). Linux 에서 생성된 golden fixture 에 대해 macOS 와 Windows 가 같은 `canonical_hash()` 를 냅니다. 재현성 계약은 이제 선언이 아니라 **검증된 사실**입니다.
:::

### ③ 실행된 적 없는 오류 분기가 결함 6 건을 숨기고 있었다

Phase 7 이 찾은 가장 나쁜 두 건이 전부 "테스트가 한 번도 도달하지 않은 `.abort_*()` 분기" 에 있었다. Phase 5 Step 5.6·5.7 이 같은 자리를 체계적으로 뒤지자 **4 건이 더** 나왔다 — 그중 P1 이 둘이다.

가장 뚜렷한 것: 이름 붙인 `beta` 로 공변량 설계를 하면 절편을 명시하지 않는 한 항상 거부되고, 메시지는 절편을 *"비-절편 계수 누락"* 이라고 부른다(D-035). 대소문자 하나 차이였다. 공변량 경로는 커버리지 79.7 % 로 패키지 최하위였다.

같은 일이 재발하지 않도록 **AST 정적 검사**를 넣었다 — 오류 메시지 223 개의 fix 라인을 소스에서 직접 읽으므로, 분기가 실행되지 않아도 규약 위반이 잡힌다.

### ④ 통계적 주장은 그대로다 — 그리고 자동 검사가 못 보는 것이 있었다

Phase 4·5·7 이 재현성 계약·solver·오류 경로·광고 표면을 바꿨지만 **V0–V12 열세 실험이 전부 통과**하고, JEBS 부록 계산이 **비트 단위로** 재현된다(4 seed × 7 열 `identical()`). 하네스는 수정 없이 돌았다.

다만 V10 의 자동 검사는 *"그림이 렌더링됐고 5 KB 를 넘는다"* 만 본다. 직접 눈으로 보니 caterpillar 의 Y축 사이트 라벨이 J = 50 에서 겹쳐 판독 불가였다(D-036). funnel 과 dependence 는 정상이었다.

### ⑤ `scenario_audit()` 이 7 종 중 4 종에서 쓸 수 없었다

계획서 Step 7.5 의 완료 조건("문서와 동작이 일치") 을 **검증하다** 발견했다. m2 vignette 은 SkewN·ALD·Mixture·PointMassSlab 에서 Group C 만 `not_available` 로 폴백한다고 서술했는데, 실제로는 **감사 전체가 abort** 했고 문서가 말한 `target_source` 열은 존재조차 하지 않았다. 원장 D-031 (P1), Phase 7 에서 수정 + 회귀 테스트 42 건.

문서만 읽고 넘어갔다면 놓쳤을 결함이다.

---

## 5. PI 조치가 필요한 것 — 1 건

### `main` branch protection 설정

요청 문서: [`decisions/002-branch-protection-request.md`](decisions/002-branch-protection-request.md)

**왜 중요한가.** workflow 를 green 으로 만드는 것만으로는 3 개월 red 가 재발합니다. red 인 CI 가 병합을 막지 않으면 실패에 비용이 없고, 비용 없는 신호는 결국 무시됩니다. Phase 6 의 진짜 목표 — "red 가 방치되지 않는 구조" — 의 마지막 조각입니다.

저장소 설정 변경은 관리자 권한이라 제가 수행하지 않았습니다. 필수 체크 목록까지 문서에 적어 두었습니다.

---

## 6. 판단이 갈릴 수 있는 지점 — 1 건

**`scenario_audit()` 에서 Group C 를 측정할 수 없는 셀의 `status`.**

현재는 `PASS` 로 두고 `target_source = "not_available"` 열로 구별합니다. 진단군 하나를 통째로 건너뛴 셀이 그것을 통과한 셀과 같은 `status` 를 갖습니다.

- **`PASS` 유지 (현재)** — 해당 4 종을 의도적으로 쓰는 사용자에게 영구 WARN 은 "WARN 은 무시해도 된다" 를 학습시킵니다. 대신 roxygen 과 m2 에 "`pass` 와 `target_source` 를 함께 읽으라" 를 명시했습니다
- **`WARN` 으로 승격** — 감사가 불완전했다는 사실이 `status` 하나로 드러납니다. `.summarize_scenario_metrics()` 의 `group_c` 분기에 warn 사유 한 줄을 추가하면 됩니다

뒤집기를 원하시면 말씀해 주십시오.

---

## 7. 다음에 할 일

| 순서 | 내용 |
|---|---|
| 1 | **Phase 8 브랜치 병합 여부 결정** (PI) |
| 2 | Phase 9 — 문서 델타, `0.2.0` 버전 범프, `NEWS.md` breaking change 절 (`upstream` 제거 · `target_source` 열 추가), 해소된 Known limitations 3 건 갱신, D-013 · D-020 |
| 3 | Phase 10 외부 검토 → Phase 11 릴리스 |

---

## 8. 안전 상태

| 항목 | 상태 |
|---|---|
| 원격 push | 승인됨(D5). `main` 은 9d39131 |
| `pkgdown` 공개 사이트 | **릴리스 전용** — 병합으로 갱신되지 않습니다 |
| 되돌리기 | `git checkout pre-upgrade-baseline` 으로 전체 원복 |
| 수치 출력 | golden fixture 열 값 **바이트 동일** — 시뮬레이션 결과는 바뀌지 않았습니다 |

---

## 9. 파일 지도

```
log-version-up-2026-08-14/
├─ STATUS.md                          ← 이 문서
├─ 001_plan-master-version-upgrade/   계획서 (Quarto Book)
├─ 002_phase01-*  003_phase02-*  004_phase03-*
├─ 005_phase05-*  006_phase04-*
├─ 007_phase06-ci-restoration.*       CI 복구
├─ 008_phase07-defect-remediation.*   기능 결함 수정 (최신)
├─ defect-ledger.csv                  31 행
├─ decisions/
│  ├─ 000-PI-approval-packet.html
│  ├─ 000-pi-approval-record.md       D1-D5 승인
│  ├─ 001-pi-decisions-gate-B.md      D6-D9
│  ├─ 002-branch-protection-request.md  ★ PI 조치 대기
│  └─ D1-D4-*.md  branch-strategy.md
└─ evidence/phase01 … phase07/
```
