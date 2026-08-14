# 돌아오셨을 때 먼저 읽을 것

**2026-08-14 자율 실행 구간 종료 시점 상태입니다.**

---

## 1. 지금 필요한 것: 결정 5 건

**→ [`decisions/000-PI-approval-packet.html`](decisions/000-PI-approval-packet.html) 를 열어주세요.** 10 분이면 읽고 결정하실 수 있게 썼습니다.

| # | 결정 | 권고 | 막는 것 |
|---|---|---|---|
| **D5** | **Linux 검증 수단** | **원격 push 승인** | **Phase 4 · 6 전체** |
| D1 | 재현성 계약 | Step 4.1 에서 O4 조사 → 실패 시 O2 | Phase 4 |
| D2 | 광고-구현 갭 | 이미 승인. IES 자료에 "eight shapes" 언급 여부만 확인 | Phase 7 일부 |
| D3 | 테스트 게이트 | 축소 없이 전부 제거 — **이미 잠정 적용함** | Phase 5 |
| D4 | 버전·범위 | `0.2.0`, 원장 22 행 전건 포함 | 없음 |

**D5 가 가장 급합니다.** Docker 가 없고 push 가 보류라 Linux 에서 아무것도 실행할 수 없어 Phase 4 와 6 이 막혀 있습니다.

::: 주의
**D3 는 승인 전에 잠정 적용했습니다.** 근거가 실측으로 확고하고(게이트 전부 켜는 비용이 +95 초뿐) 커밋 하나를 되돌리면 원복되기 때문입니다. 기각하시면 `git revert 8c4dda9` 하면 됩니다.
:::

---

## 2. 진행 상황

| Phase | 상태 | 로그 |
|---|---|---|
| 1 작업 기반 복구 | **완료** | [`002_phase01-foundation-recovery.html`](002_phase01-foundation-recovery.html) |
| 2 결함 인벤토리 감사 | **완료** | [`003_phase02-defect-audit.html`](003_phase02-defect-audit.html) |
| 3 v0.2.0 범위 결정 | **완료 — 게이트 G-B 대기** | [`004_phase03-scope-decisions.html`](004_phase03-scope-decisions.html) |
| 4 재현성 계약 | **막힘** (D-019) | — |
| 5 테스트 스위트 | **부분 (4/6 Step)** | [`005_phase05-test-suite-rebuild-partial.html`](005_phase05-test-suite-rebuild-partial.html) |
| 6–11 | 미착수 | — |

---

## 3. 가장 중요한 세 가지 발견

### ① 우려했던 "3 개월 격차" 는 격차가 아니었습니다

원격 추적 파일 235 개 중 **226 개가 바이트 동일**하고, 로컬에서 원격으로 가야 할 변경은 **하나도 없었습니다.** 다른 9 개는 전부 원격이 최신이었고, 그중 `CONTRIBUTING.md`·`NEWS.md`·`references.bib` 는 공개 릴리스 때 내부 경로를 지운 **정제본이 로컬로 돌아오지 않은 것**이었습니다. 모르고 편집하셨다면 정제가 되돌려질 뻔했습니다.

### ② red CI 4 개가 근본 원인 2 개로 환원됩니다

- **RC-1** — macOS 에서 생성된 golden fixture·snapshot 이 체크인되어 있는데, 이를 검증하는 테스트 5 개 파일이 **게이트 없이** 모든 플랫폼에서 실행됩니다. `extended-tests`·`R-CMD-check`·`test-coverage` 를 설명합니다
- **RC-2** — `lint` 는 위반 1 건도 허용하지 않는데, **`.lintr` 이 원격에 추적되지 않아** CI 가 기본 규칙으로 돌았습니다. 위반이 65 가 아니라 **440** 이었습니다 (Phase 1 에서 `.lintr` 추적으로 이미 65 로 낮췄습니다)

### ③ 재현성 계약은 버그가 아니라 **달성 불가능한 계약**이었습니다

기본 engine A2 가 `nleqslv` 를 `ftol = 1e-12` 로 돌립니다. 5 개 시작점이 **모두 수렴 조건을 만족하면서 서로 약 5,000 ULP 떨어진 점**에 안착합니다. 반면 해시는 double 을 반올림하지 않아 **1 ULP** 에도 뒤집힙니다.

**계약이 요구하는 정밀도가 계산이 제공하는 정밀도보다 3~4 자릿수 엄격합니다.** 어느 플랫폼에서 재생성해도 다른 플랫폼에서는 반드시 깨집니다. 그래서 D1 에서 O1(Linux 재생성)을 **기각 권고**했습니다 — 문제를 macOS 로 옮길 뿐입니다.

---

## 4. 눈에 보이는 개선

| 지표 | 시작 | 지금 |
|---|--:|--:|
| git 관리 | 없음 | 629 파일 추적, 커밋 12 건 |
| 기본 테스트 skip | **30** | **1** |
| 기본 테스트 실패 | 0 | 0 |
| 기본 테스트 시간 | 18.1 s | 106.0 s (예산 180 s) |
| 커버리지 | 90.89 % | **92.78 %** |
| `layer2-engine-a2.R` | 84.8 % | **95.5 %** |
| `utils-reproducibility.R` | 91.0 % | **95.5 %** |
| 작업 디렉터리 | 462 MB | 417 MB |
| 결함 원장 | 없음 | **22 행** (P0 9 · P1 6 · P2 7) |

`R CMD check --as-cran` (vignette 포함)은 시작 시점부터 **0 error / 0 warning / 1 note** 였습니다. NOTE 는 `README.Rmd` 가 최상위에 있다는 것뿐입니다.

---

## 5. 안전 상태

| 항목 | 상태 |
|---|---|
| **원격 push** | **하지 않았습니다** — 커밋 12 건 전부 로컬 |
| **외부 저장소 생성** | 하지 않았습니다 — 아카이브는 로컬 git repo 로만 |
| 패키지 코드(`R/`) 변경 | **0 줄** |
| `DESCRIPTION` 변경 | 1 줄 (`hedgehog` 제거) |
| 되돌리기 | `git checkout pre-upgrade-baseline` 으로 전체 원복 |

::: 위험
Phase 1~5 산출물이 **전부 로컬 디스크에만** 있습니다. git 저장소가 생겨 실수로 인한 손실은 복구되지만 **디스크 고장에는 대비가 없습니다.** D5 승인이 이것도 해소합니다.
:::

---

## 6. 결정이 늦어져도 진행 가능한 것

| 결정 | 승인 시 열리는 것 |
|---|---|
| **D5 만** | Phase 4 · 6 |
| **D3 만** | Phase 5 잔여 Step 5.6 (이미 잠정 적용 중이므로 확인만) |
| **D2 만** | Phase 7 Step 7.3 |
| 아무것도 안 해도 | Phase 7 의 Step 7.4 · 7.5 · 7.7 은 진행 가능 |

---

## 7. 파일 지도

```
log-version-up-2026-08-14/
├─ STATUS.md                          ← 이 문서
├─ 001_plan-master-version-upgrade/   계획서 (Quarto Book)
├─ 002_phase01-*.qmd/html             Phase 1 로그
├─ 003_phase02-*.qmd/html             Phase 2 로그 (가장 내용이 많습니다)
├─ 004_phase03-*.qmd/html             Phase 3 로그
├─ 005_phase05-*.qmd/html             Phase 5 로그 (부분)
├─ defect-ledger.csv                  결함 원장 22 행
├─ decisions/
│  ├─ 000-PI-approval-packet.html     ★ 먼저 읽으실 것
│  ├─ D1-reproducibility-contract.md
│  ├─ D2-advertised-surface.md
│  ├─ D3-test-gate-policy.md
│  ├─ D4-release-scope.md
│  ├─ 000-pi-approval-record.md       2026-08-14 승인 기록
│  └─ branch-strategy.md
├─ evidence/phase01/                  베이스라인 측정 + 재실행 스크립트
├─ evidence/phase02/                  감사 증거 14 파일 + 스크립트 7
├─ evidence/phase05/                  게이트 제거 전후
└─ artifacts/                         되돌리기용 보존
```
