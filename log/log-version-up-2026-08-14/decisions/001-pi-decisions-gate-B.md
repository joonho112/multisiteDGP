# PI 결정 기록 — 게이트 G-B

- **일자:** 2026-08-14
- **결정자:** JoonHo Lee (PI)
- **대상:** `000-PI-approval-packet.md` 의 결정 5 건
- **방식:** 대화형 확인

**5 건 전부 권고안 채택.**

---

## D5 — Linux 검증 수단

**결정: A — 원격 push 승인.**

작업 브랜치와 태그를 `joonho112/multisiteDGP` 에 push 한다. 노출 범위는 2026-08-14 에 승인된 추적 범위(패키지 + `tools/` + `log/` + 개발설정, 695 파일)와 동일하다.

**해소되는 것**

| 막혀 있던 것 | 상태 |
|---|---|
| Phase 2 Step 2.1 · 2.3 의 `partial` 항목 | 해소 가능 |
| Phase 4 Step 4.1 (플랫폼 간 드리프트 실측) | 해소 |
| Phase 4 Step 4.3 · 4.4 · 4.5 (Linux 베이스라인 생성) | 해소 |
| Phase 6 전체 (CI green 확인) | 해소 |
| 커밋 12 건이 로컬에만 있는 디스크 사고 위험 | 해소 |

**실행 방식.** CI workflow 는 `push: branches: [main, master]` 에서만 자동 실행되므로, 작업 브랜치를 push 한 뒤 `workflow_dispatch` 로 명시 실행한다. `main` 은 Phase 종료 시 병합한다 (브랜치 전략대로).

원장 D-019 → **fixed**.

## D1 — 재현성 계약

**결정: 권고 채택 — Step 4.1 에서 O4 를 시간 제한 두고 조사하고, 닫히지 않으면 O2 로 전환.**

- **O4** — engine A2 가 solver 해를 반환할 때 유효숫자 **10 자리**로 양자화. 비트 단위 해시 계약을 유지한 채 달성 가능해진다
- **시간 제한** — Linux 대조 **2 회** 안에 드리프트가 완전히 사라지지 않으면 O2 로 전환
- **O2 fallback** — 해시 payload 조립 전 유효숫자 **9 자리**로 정규화
- **O1 기각** — 문제를 macOS·Windows 로 이전시킬 뿐

**공통 후속.** 어느 쪽이든 fixture 9 개와 snapshot 전면 재생성, 정책 문서 개정, NEWS breaking change 명시가 따른다. O4 를 택하면 JEBS 재현 재검증(Phase 8)이 추가된다.

**D-007 (`test-golden.R` 바이너리 비교)** 는 O4 성공 시 자연 해소되고, O2 로 전환하면 `canonical_hash()` 기반 비교로 바꾼다.

## D2 — 광고-구현 갭

**결정: IES 자료에 "eight distribution shapes" 언급 없음 → 7 로 축소.**

Phase 2 가 확정한 18 곳을 정정한다.

| 파일 | 건수 |
|---|--:|
| `DESCRIPTION` | 1 |
| `NEWS.md` | 2 (v0.1.0 이력 항목은 보존, v0.2.0 에서 정정 명시) |
| `README.Rmd` → `README.md` 재생성 | 5 |
| `index.md` | 3 |
| `_pkgdown.yml` | 2 |

`vignette("m2-g-distribution-catalog")` 도 함께 확인한다.

**`target_marginal_rho`** 는 유지 + E23 메시지 명확화, **`upstream`** 은 문서에서 숨기거나 제거 (Step 7.4 에서 blueprint 의 용도 확인 후 결정).

## D3 — 테스트 게이트 정책

**결정: 승인 + Phase 5 잔여 Step 진행.**

Phase 5 에서 잠정 적용한 게이트 제거 29 건을 확정한다. 실측 결과 skip 30 → 1, 106 초, 실패 0, 플레이키 없음.

**잔여 Step 5.6** 을 완료한다.

1. 원장 각 P0/P1 행에 대응하는 회귀 테스트
2. **확장 스위트 재정의** — 게이트가 사라져 `extended-tests` 가 기본 스위트와 같은 것을 돌리게 됐다. Phase 6 이 workflow 를 손보기 전에 존재 이유를 다시 정해야 한다

Phase 5 Step 구성 축소(8 → 6) 승인.

## D4 — 버전과 범위

**결정: `0.2.0`, 원장 22 행 전건 v0.2.0 포함.**

PI 가 별도 이견을 제시하지 않아 권고 기본값을 적용한다. `NEWS.md` v0.2.0 항목 최상단에 breaking change 절을 둔다 — 재현성 계약 변경, shape 개수 정정, 기본 테스트 스위트 소요 시간 증가.

**D-013 (tarball 9.2 MB)** 은 문서화된 한계로 남기고 진행한다. v0.2.0 은 CRAN 제출 범위가 아니다.

---

## 게이트 G-B 통과

**Phase 4 진입 가능.** D1 결정과 D5 해소가 모두 이뤄졌다.

## 다음 실행 순서

| 순서 | 작업 | 근거 |
|---|---|---|
| 1 | 브랜치·태그 push, workflow 실행 | D5 |
| 2 | CI 결과로 Phase 2 `partial` 항목 종결 | D5 |
| 3 | Phase 5 Step 5.6 완료 | D3 |
| 4 | Phase 4 (Step 4.1 O4 조사부터) | D1 |
| 5 | Phase 7 (D2 실행 포함) | D2 |
| 6 | Phase 6 → 8 → 9 → 10 → 11 | 계획서 |

Phase 4 · 5 · 7 은 병렬 가능하다.

---

# 추가 결정 — 2026-08-14 (첫 Linux CI 이후)

첫 Linux CI 가 D-024(solver 실현 가능 경계의 플랫폼 의존)를 드러내면서 D1 의 전제가 흔들렸다. 네 건을 추가로 결정했다.

## D6 — D-024 처리 위치

**결정: Phase 4 를 두 갈래로 확장.**

Step 4.1 을 **① 해시 계약**과 **② solver 견고화** 두 갈래로 나눈다. 둘은 같은 뿌리(`nleqslv` 의 플랫폼별 수렴 차이)에서 나오고, solver 를 먼저 안정화하면 해시 드리프트도 줄어들 수 있으므로 순서가 맞다.

**Phase 4 가 7 → 9 Step 으로 늘고 약 40 분이 추가된다.**

## D7 — solver 견고화 방향

**결정: 적응적 tolerance.**

`post-solve` 검증이 `tol = 1e-6` 을 조건과 무관하게 고정 적용하는 것이 실패 지점이다. `alpha ≈ 40000` 영역에서는 `trunc_gamma_moments()` 의 잔차 평가 자체가 그 정밀도를 낼 수 없다 (`lgamma`·`pgamma`·`exp`·`log` 가 전부 libm 이고 조건수가 나쁘다).

::: {.callout-note title="권고와 다른 선택 — 보완 방침"}
planning agent 의 권고는 **실현 가능 영역 축소**였다. PI 는 적응적 tolerance 를 택했다. 기존 설계가 계속 동작한다는 것이 장점이고, 약점은 "어느 수준까지 느슨해도 되는지의 근거" 였다.

그 약점을 다음으로 보완한다 — **tolerance 를 임의로 잡지 않고 모멘트 평가의 조건수에서 유도한다.**

1. Step 4.1b 가 `trunc_gamma_moments()` 잔차 평가의 **달성 가능 정밀도**를 `(n_bar, cv, n_min)` 격자에서 측정한다 (섭동에 대한 잔차 민감도)
2. tolerance 를 그 정밀도의 상수배로 정의한다 — 손으로 고른 값이 아니라 **측정된 노이즈 바닥에서 유도된 값**
3. 유도식과 상수를 roxygen 과 `cross-os-reproducibility-policy.md` 에 명시한다
4. macOS·Linux 양쪽에서 같은 판정이 나오는지 회귀 테스트로 고정한다

이렇게 하면 "느슨하게 했다" 가 아니라 "평가 가능한 정밀도만큼만 요구한다" 가 된다.
:::

## D8 — main 병합 시점

**결정: PR 을 열어둔다.**

`main` 을 건드리지 않으면서 `pull_request` 트리거로 CI 가 자동 실행되고, 누적 변경을 한 곳에서 검토할 수 있다. Phase 종료마다 PR 에 커밋이 쌓이고, 최종 병합은 Phase 11 릴리스 시점에 한다.

## D9 — 자율 실행

**결정: 계속 진행.**

실행 순서: **D-025 (macOS·Windows 의존성 설치 실패) → Phase 4 두 갈래 → Phase 5 잔여 → Phase 7.**

D-025 를 먼저 처리하는 이유는 그것이 풀려야 이후 모든 검증이 5 개 플랫폼 전부에서 돌기 때문이다.
