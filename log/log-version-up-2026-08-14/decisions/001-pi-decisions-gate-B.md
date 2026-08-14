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
